;;; gent-providers.el --- LLM backend implementations for gent -*- lexical-binding: t; -*-

;;; Commentary:

;; An OpenAI-compatible backend implementing the `gent-llm' interface,
;; with streaming SSE requests through `gent-llm-curl-executable'.
;; Dumile and OpenCode Go constructors supply service-specific endpoints
;; and headers.  Callers supply models, credentials, and request settings.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'map)
(require 'subr-x)
(require 'gent-llm)

(defcustom gent-llm-curl-executable "curl"
  "The curl program used to make requests."
  :type 'string
  :group 'tools)

(cl-defstruct (gent-openai-backend (:include gent-llm-generic-backend)
                                  (:constructor gent-make-openai-backend)
                                  (:copier gent-copy-openai-backend))
  "A model at an OpenAI-compatible API endpoint, with its request settings."
  (name nil :documentation "Name shown to the user.")
  (url-base nil :documentation "Base URL of the API.")
  (api-path "/chat/completions"
            :documentation "Path appended to `url-base'.")
  (api-key nil :documentation "API key as a string, or a function returning one.")
  (headers nil
           :documentation "Extra HTTP header strings, or a function returning them.")
  (session-id-header nil
                     :documentation "HTTP header for the request's session ID.
When non-nil, requests must supply a session ID.  Nil omits the header.")
  (model nil :documentation "Model id sent with each request.")
  (timeout 600 :documentation "Request timeout in seconds.")
  (max-tokens 4096
              :documentation "Maximum tokens to generate, or nil to let the server decide.")
  (temperature nil :documentation "Sampling temperature, or nil to omit it.")
  (reasoning-effort "low"
                    :documentation "Sent as `reasoning_effort': \"none\" turns reasoning off,
nil omits the parameter."))

(cl-defmethod gent-llm-copy-backend ((backend gent-openai-backend))
  "Return a copy of BACKEND whose settings can be changed independently."
  (gent-copy-openai-backend backend))

(cl-defmethod gent-llm-backend-model ((backend gent-openai-backend))
  "Return the model ID used by BACKEND."
  (gent-openai-backend-model backend))

(cl-defmethod (setf gent-llm-backend-model) (model (backend gent-openai-backend))
  "Set BACKEND's model ID to MODEL."
  (setf (gent-openai-backend-model backend) model))

(cl-defmethod gent-llm-backend-reasoning-effort ((backend gent-openai-backend))
  "Return BACKEND's reasoning effort, or nil to omit it."
  (gent-openai-backend-reasoning-effort backend))

(cl-defmethod (setf gent-llm-backend-reasoning-effort) (effort (backend gent-openai-backend))
  "Set BACKEND's reasoning effort to EFFORT, or nil to omit it."
  (setf (gent-openai-backend-reasoning-effort backend) effort))

(defun gent-openai--api-key (backend)
  "Return the API key of BACKEND, or nil if it has none."
  (let ((key (gent-openai-backend-api-key backend)))
    (if (functionp key) (funcall key) key)))

(defun gent-openai--url (backend)
  "Return the full chat completions URL of BACKEND."
  (concat (string-remove-suffix "/" (gent-openai-backend-url-base backend))
          (gent-openai-backend-api-path backend)))

(defun gent-openai--headers (backend &optional session-id)
  "Return the HTTP headers for a request to BACKEND with SESSION-ID.
Require SESSION-ID when BACKEND specifies a session ID header."
  (gent-llm--validate-session-id session-id)
  (let ((session-header (gent-openai-backend-session-id-header backend)))
    (when (and session-header (null session-id))
      (error "Gent LLM: backend requires :session-id for %s" session-header))
    (let ((key (gent-openai--api-key backend))
          (extra (gent-openai-backend-headers backend)))
      (append (list "Content-Type: application/json")
              (and key (not (string-empty-p key))
                   (list (format "Authorization: Bearer %s" key)))
              (and session-header
                   (list (format "%s: %s" session-header session-id)))
              (if (functionp extra) (funcall extra) extra)))))

(defun gent-openai--payload (backend messages prompt tools tool-choice)
  "Build the request payload for BACKEND, MESSAGES, PROMPT and TOOLS.
Include TOOL-CHOICE as the tool selection policy when non-nil."
  (let ((payload `((model . ,(gent-openai-backend-model backend))
                   (messages . ,(gent-llm--normalize-messages messages prompt))
                   (stream . t)))
        (effort (gent-openai-backend-reasoning-effort backend))
        (max-tokens (gent-openai-backend-max-tokens backend))
        (temperature (gent-openai-backend-temperature backend)))
    (when effort
      (push `(reasoning_effort . ,effort) payload))
    (when max-tokens
      (push `(max_tokens . ,max-tokens) payload))
    (when temperature
      (push `(temperature . ,temperature) payload))
    (when tools
      (push `(tools . ,(vconcat (mapcar #'gent-llm--normalize-tool
                                        (append tools nil))))
            payload))
    (when tool-choice
      (push `(tool_choice . ,tool-choice) payload))
    payload))

(cl-defmethod gent-llm-backend-preview ((backend gent-openai-backend) messages tools)
  `((endpoint . ,(gent-openai--url backend))
    (timeout . ,(gent-openai-backend-timeout backend))
    (payload . ,(gent-openai--payload backend messages nil tools nil))))

(defun gent-openai--merge-tool-call (acc tool-call)
  "Merge streamed TOOL-CALL fragment into ACC, returning ACC.
ACC is a vector indexed by the fragment's `index'.  Argument
fragments are concatenated."
  (let* ((index (or (map-elt tool-call 'index) 0))
         (function (map-elt tool-call 'function)))
    (while (<= (length acc) index)
      (setq acc (vconcat acc (vector nil))))
    (let ((slot (aref acc index)))
      (unless slot
        (setq slot (list (cons 'id nil)
                         (cons 'type "function")
                         (cons 'function (list (cons 'name nil)
                                               (cons 'arguments "")))))
        (aset acc index slot))
      (let ((id (map-elt tool-call 'id)))
        (when id
          (setcdr (assq 'id slot) id)))
      (when function
        (let ((function-slot (cdr (assq 'function slot)))
              (name (map-elt function 'name))
              (arguments (map-elt function 'arguments)))
          (when name
            (setcdr (assq 'name function-slot) name))
          (when arguments
            (setcdr (assq 'arguments function-slot)
                    (concat (or (cdr (assq 'arguments function-slot)) "")
                            arguments))))))
    acc))

(defun gent-openai--tool-calls (acc)
  "Return ACC vector as a list of clean OpenAI-shaped tool calls."
  (delq nil (append acc nil)))

(defun gent-openai--error-message (error)
  "Return a readable message for the streamed ERROR object."
  (or (and (consp error) (stringp (map-elt error 'message)) (map-elt error 'message))
      (and (stringp error) error)
      (format "%S" error)))

(defun gent-openai--filter (state)
  "Parse streamed SSE output in STATE.
Return an alist of ((:filtered . TEXT) (:reasoning . REASONING)
\(:pending . PARTIAL-LINE) (:tool-calls . ACC) (:finish-reason . REASON)
\(:error . MESSAGE)).  MESSAGE is the first error the server streamed."
  (let* ((raw (concat (or (map-elt state :pending) "") ""))
         (complete (string-suffix-p "\n" raw))
         (lines (split-string raw "\n"))
         (pending (and (not complete)
                       (not (string-empty-p raw))
                       (car (last lines))))
         (lines (if pending (butlast lines) lines))
         (tool-calls (or (map-elt state :tool-calls) []))
         (finish-reason (map-elt state :finish-reason))
         (error (map-elt state :error))
         (text "")
         (reasoning ""))
    (dolist (line lines)
      (when (string-prefix-p "data:" line)
        (let ((payload (string-trim (substring line 5))))
          (unless (or (string-empty-p payload)
                      (equal payload "[DONE]"))
            (let* ((obj (gent-llm--json-parse-string payload))
                   (choices (and obj (map-elt obj 'choices)))
                   (choice (and choices (> (length choices) 0) (aref choices 0)))
                   (delta (and choice (map-elt choice 'delta)))
                   (content (and delta (map-elt delta 'content)))
                   (reason (and delta (map-elt delta 'reasoning_content)))
                   (calls (and delta (map-elt delta 'tool_calls)))
                   (stop (and choice (map-elt choice 'finish_reason))))
              (when (and obj (map-elt obj 'error) (not error))
                (setq error (gent-openai--error-message (map-elt obj 'error))))
              (when stop
                (setq finish-reason stop))
              (when content
                (setq text (concat text content)))
              (when reason
                (setq reasoning (concat reasoning reason)))
              (dolist (call (append calls nil))
                (setq tool-calls (gent-openai--merge-tool-call
                                  tool-calls call))))))))
    `((:filtered . ,text)
      (:reasoning . ,reasoning)
      (:pending . ,(and pending (not (string-empty-p pending)) pending))
      (:tool-calls . ,tool-calls)
      (:finish-reason . ,finish-reason)
      (:error . ,error))))

(defun gent-openai--write-payload (payload)
  "Write PAYLOAD as JSON to a new temporary file and return its name."
  (let ((file (make-temp-file "gent-llm-" nil ".json")))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file
        (insert (json-serialize payload :false-object :json-false))))
    file))

(defun gent-openai--curl-config (headers)
  "Return a curl config file that sends HEADERS.
Headers go through curl's standard input, so API keys are not
visible in the process list."
  (mapconcat (lambda (header)
               (format "header = \"%s\"\n"
                       (replace-regexp-in-string
                        "[\"\\]" (lambda (c) (concat "\\" c)) header t t)))
             headers ""))

(cl-defmethod gent-llm-backend-chat ((backend gent-openai-backend)
                                   &key session-id messages prompt tools tool-choice
                                   on-token on-reasoning on-tool-calls on-done on-error)
  "Stream a completion from BACKEND using its OpenAI-compatible endpoint.
SESSION-ID identifies the conversation.  MESSAGES, PROMPT, TOOLS and
TOOL-CHOICE describe the request.  ON-TOKEN, ON-REASONING,
ON-TOOL-CALLS, ON-DONE and ON-ERROR are callbacks.
All arguments and the returned process follow `gent-llm-chat'."
  (let* ((headers (gent-openai--headers backend session-id))
         (data-file (gent-openai--write-payload
                     (gent-openai--payload backend messages prompt tools tool-choice)))
         (pending nil)
         (output "")
         (full "")
         (tool-calls [])
         (finish-reason nil)
         (stream-error nil)
         (parse (lambda (text)
                  (let ((result (gent-openai--filter
                                 `((:pending . ,text)
                                   (:tool-calls . ,tool-calls)
                                   (:finish-reason . ,finish-reason)
                                   (:error . ,stream-error)))))
                    (setq pending (map-elt result :pending))
                    (setq tool-calls (map-elt result :tool-calls))
                    (setq finish-reason (or (map-elt result :finish-reason)
                                            finish-reason))
                    (setq stream-error (map-elt result :error))
                    (let ((reasoning (map-elt result :reasoning)))
                      (when (and on-reasoning (not (string-empty-p reasoning)))
                        (funcall on-reasoning reasoning)))
                    (let ((chunk (map-elt result :filtered)))
                      (unless (string-empty-p chunk)
                        (setq full (concat full chunk))
                        (when on-token
                          (funcall on-token chunk)))))))
         (process
          (make-process
           :name "gent-llm"
           :command (list gent-llm-curl-executable (gent-openai--url backend)
                          "--fail-with-body" "--no-progress-meter" "--no-buffer"
                          "-m" (number-to-string (gent-openai-backend-timeout backend))
                          "--data-binary" (concat "@" data-file)
                          "-K" "-")
           :connection-type 'pipe
           :coding 'utf-8
           :noquery t
           :filter (lambda (_process chunk)
                     (setq output (concat output chunk))
                     (funcall parse (concat pending chunk)))
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (ignore-errors (delete-file data-file))
               (when pending
                 (funcall parse (concat pending "\n")))
               (pcase (process-status process)
                 ('signal nil)
                 ((guard (and stream-error (zerop (process-exit-status process))))
                  (when on-error
                    (funcall on-error (format "gent-llm: server error: %s" stream-error))))
                 ((guard (zerop (process-exit-status process)))
                  (let ((calls (gent-openai--tool-calls tool-calls)))
                    (when (and on-tool-calls calls)
                      (funcall on-tool-calls calls finish-reason)))
                  (when on-done
                    (funcall on-done full)))
                 (_
                  (when on-error
                    (funcall on-error
                             (format "gent-llm: request failed (exit %s)%s"
                                     (process-exit-status process)
                                     (let ((out (string-trim output)))
                                       (if (string-empty-p out)
                                           ""
                                         (format ": %s" out)))))))))))))
    (process-send-string process (gent-openai--curl-config
                                  headers))
    (process-send-eof process)
    process))

(defun gent-llm-make-dumile-backend (&rest args)
  "Create a Dumile backend with keyword arguments ARGS.

ARGS are passed to `gent-make-openai-backend', including :model, :api-key
\(a string or function), and request settings."
  (apply #'gent-make-openai-backend
         :name "dumile" :url-base "https://dumile.local/v1" args))

(defun gent-llm-make-opencode-go-backend (&rest args)
  "Create an OpenCode Go backend with keyword arguments ARGS.

ARGS are passed to `gent-make-openai-backend', including :model,
:api-key \(a string or function), and request settings.  Requests must
supply :session-id, reused for the same conversation.  It is sent as
x-opencode-session for routing and prompt caching.  See
https://opencode.ai/v2/docs/console/api/inference/ and
https://opencode.ai/v2/docs/console/go/#where-can-i-use-it."
  (apply #'gent-make-openai-backend
         :name "opencodego" :url-base "https://opencode.ai/inference/go/v1"
         :session-id-header "x-opencode-session"
         args))

(provide 'gent-providers)
;;; gent-providers.el ends here
