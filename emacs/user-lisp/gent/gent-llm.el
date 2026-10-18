;;; gent-llm.el --- Streaming LLM queries for gent -*- lexical-binding: t; -*-

;;; Commentary:

;; A small OpenAI-compatible chat client with streaming support.
;;
;; A `gent-llm-provider' describes an endpoint (URL, API key, default
;; timeout) and a `gent-llm-backend' a model on that provider with its
;; request settings (max tokens, temperature, reasoning effort):
;;
;;   (setq gent-llm-default-backend
;;         (gent-llm-make-backend
;;          :provider (gent-llm-make-provider
;;                     :name "openai"
;;                     :url-base "https://api.openai.com/v1"
;;                     :api-key (lambda () (getenv "OPENAI_API_KEY")))
;;          :model "gpt-4o"))
;;
;; `gent-llm-chat' sends a chat request to `:backend' (default
;; `gent-llm-default-backend') and calls `:on-token' for each streamed
;; chunk, `:on-reasoning' for each chunk of reasoning, `:on-done' with
;; the full text, or `:on-error' on failure.
;;
;;   (gent-llm-chat
;;    :prompt "Say hello in three words."
;;    :on-token (lambda (chunk) (insert chunk))
;;    :on-done  (lambda (full) (message "done: %s" full)))
;;
;; Tool calls are supported too.  Declare tools with `gent-llm-make-tool'
;; and receive assembled calls (raw JSON argument strings) via `:on-tool-calls':
;;
;;   (gent-llm-chat
;;    :prompt "What's the weather in Paris?"
;;    :tools (list (gent-llm-make-tool
;;                  :name "get_weather"
;;                  :description "Get current weather"
;;                  :parameters '((type . "object")
;;                                (properties . ((location . ((type . "string")))))
;;                                (required . ["location"]))))
;;    :on-tool-calls (lambda (calls finish-reason) ...)
;;    :on-done       (lambda (full) ...))
;;
;; This library only makes the request and reads the response; it never
;; executes the tools it is told about.
;;
;; Requests are always streamed (SSE) through `gent-llm-curl-executable'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)

(defcustom gent-llm-curl-executable "curl"
  "The curl program used to make requests."
  :type 'string
  :group 'tools)

(cl-defstruct (gent-llm-provider (:constructor gent-llm-make-provider)
                                 (:copier gent-llm-copy-provider))
  "An OpenAI-compatible API endpoint."
  (name nil :documentation "Name shown to the user.")
  (url-base nil :documentation "Base URL of the API.")
  (api-path "/chat/completions"
            :documentation "Path appended to `url-base'.")
  (api-key nil :documentation "API key as a string, or a function returning one.")
  (headers nil
           :documentation "Extra HTTP header strings, or a function returning them.")
  (default-timeout 600
                   :documentation "Request timeout in seconds, unless the backend sets one."))

(cl-defstruct (gent-llm-backend (:constructor gent-llm-make-backend)
                                (:copier gent-llm-copy-backend))
  "A model served by a `gent-llm-provider', with its request settings."
  (provider nil :documentation "The `gent-llm-provider' to send requests to.")
  (model nil :documentation "Model id sent with each request.")
  (timeout nil
           :documentation "Request timeout in seconds, or nil for the provider's default.")
  (max-tokens 4096
              :documentation "Maximum tokens to generate, or nil to let the server decide.")
  (temperature nil :documentation "Sampling temperature, or nil to omit it.")
  (reasoning-effort "low"
                    :documentation "Sent as `reasoning_effort': \"none\" turns reasoning off,
nil omits the parameter."))

(defvar gent-llm-default-backend nil
  "The `gent-llm-backend' used when `gent-llm-chat' is not given one.")

(defun gent-llm--json-parse-string (string)
  "Parse STRING as JSON, returning nil on failure."
  (if (fboundp 'json-parse-string)
      (condition-case nil
          (json-parse-string string :object-type 'alist)
        (error nil))
    (condition-case nil
        (json-read-from-string string)
      (error nil))))

(defun gent-llm--api-key (provider)
  "Return the API key of PROVIDER, or nil if it has none."
  (let ((key (gent-llm-provider-api-key provider)))
    (if (functionp key) (funcall key) key)))

(defun gent-llm--url (provider)
  "Return the full chat completions URL of PROVIDER."
  (concat (string-remove-suffix "/" (gent-llm-provider-url-base provider))
          (gent-llm-provider-api-path provider)))

(defun gent-llm--headers (provider)
  "Return the HTTP headers for a request to PROVIDER."
  (let ((key (gent-llm--api-key provider))
        (extra (gent-llm-provider-headers provider)))
    (append (list "Content-Type: application/json")
            (and key (not (string-empty-p key))
                 (list (format "Authorization: Bearer %s" key)))
            (if (functionp extra) (funcall extra) extra))))

(defun gent-llm--timeout (backend)
  "Return the request timeout of BACKEND, in seconds."
  (or (gent-llm-backend-timeout backend)
      (gent-llm-provider-default-timeout (gent-llm-backend-provider backend))))

(defun gent-llm--normalize-message (message)
  "Return MESSAGE as a clean, tool-aware alist.
Preserves `role', `content', `tool_calls', `tool_call_id' and `name'."
  (let ((role (map-elt message 'role)))
    (when role
      (let ((out (list (cons 'role (format "%s" role))))
            (content (map-elt message 'content)))
        (cond ((stringp content)
               (push (cons 'content (substring-no-properties content)) out))
              (content
               (push (cons 'content (substring-no-properties
                                     (format "%s" content)))
                     out)))
        (let ((tool-calls (map-elt message 'tool_calls)))
          (when tool-calls
            (push (cons 'tool_calls (vconcat tool-calls)) out)))
        (let ((tool-call-id (map-elt message 'tool_call_id)))
          (when tool-call-id
            (push (cons 'tool_call_id (format "%s" tool-call-id)) out)))
        (let ((name (map-elt message 'name)))
          (when name
            (push (cons 'name (format "%s" name)) out)))
        (nreverse out)))))

(defun gent-llm--normalize-messages (messages prompt)
  "Return a vector of message alists from MESSAGES and PROMPT."
  (let ((msgs (cond ((null messages) nil)
                    ((vectorp messages) (append messages nil))
                    ((listp messages) messages)
                    (t (error "gent-llm: invalid `messages'")))))
    (setq msgs (delq nil (mapcar #'gent-llm--normalize-message msgs)))
    (when (and prompt (not (string-empty-p prompt)))
      (setq msgs (append msgs
                         (list (gent-llm--normalize-message
                                `((role . "user") (content . ,prompt)))))))
    (vconcat msgs)))

(cl-defun gent-llm-make-tool (&key name description parameters)
  "Build an OpenAI tool definition for NAME, DESCRIPTION and PARAMETERS.
PARAMETERS is a JSON-schema alist, for example:

  ((type . \"object\")
   (properties . ((location . ((type . \"string\")))))
   (required . [\"location\"]))"
  `((type . "function")
    (function . ((name . ,name)
                 (description . ,description)
                 (parameters . ,parameters)))))

(defun gent-llm--normalize-tool (tool)
  "Return TOOL as an OpenAI tool-definition alist.
Accepts either `gent-llm-make-tool' output or a plist with
`:name', `:description' and `:parameters'."
  (cond
   ((and (listp tool) (assq 'function tool))
    tool)
   ((and (listp tool) (plist-member tool :name))
    (gent-llm-make-tool :name (plist-get tool :name)
                         :description (plist-get tool :description)
                         :parameters (plist-get tool :parameters)))
   (t
    (error "gent-llm: invalid tool definition: %S" tool))))

(defun gent-llm--payload (backend messages prompt tools tool-choice)
  "Build the request payload alist for BACKEND, MESSAGES, PROMPT and TOOLS."
  (let ((payload `((model . ,(gent-llm-backend-model backend))
                   (messages . ,(gent-llm--normalize-messages messages prompt))
                   (stream . t)))
        (effort (gent-llm-backend-reasoning-effort backend))
        (max-tokens (gent-llm-backend-max-tokens backend))
        (temperature (gent-llm-backend-temperature backend)))
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

(defun gent-llm--merge-tool-call (acc tool-call)
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

(defun gent-llm--tool-calls (acc)
  "Return ACC vector as a list of clean OpenAI-shaped tool calls."
  (delq nil (append acc nil)))

(defun gent-llm--filter (state)
  "Parse streamed SSE output in STATE.
Return an alist of ((:filtered . TEXT) (:reasoning . REASONING)
\(:pending . PARTIAL-LINE) (:tool-calls . ACC) (:finish-reason . REASON))."
  (let* ((raw (concat (or (map-elt state :pending) "") ""))
         (complete (string-suffix-p "\n" raw))
         (lines (split-string raw "\n"))
         (pending (and (not complete)
                       (not (string-empty-p raw))
                       (car (last lines))))
         (lines (if pending (butlast lines) lines))
         (tool-calls (or (map-elt state :tool-calls) []))
         (finish-reason (map-elt state :finish-reason))
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
              (when stop
                (setq finish-reason stop))
              (when content
                (setq text (concat text content)))
              (when reason
                (setq reasoning (concat reasoning reason)))
              (dolist (call (append calls nil))
                (setq tool-calls (gent-llm--merge-tool-call
                                  tool-calls call))))))))
    `((:filtered . ,text)
      (:reasoning . ,reasoning)
      (:pending . ,(and pending (not (string-empty-p pending)) pending))
      (:tool-calls . ,tool-calls)
      (:finish-reason . ,finish-reason))))

(defun gent-llm--write-payload (payload)
  "Write PAYLOAD as JSON to a new temporary file and return its name."
  (let ((file (make-temp-file "gent-llm-" nil ".json")))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file
        (insert (if (fboundp 'json-serialize)
                    (json-serialize payload)
                  (json-encode payload)))))
    file))

(defun gent-llm--curl-config (headers)
  "Return a curl config file that sends HEADERS.
Headers go through curl's standard input, so API keys are not
visible in the process list."
  (mapconcat (lambda (header)
               (format "header = \"%s\"\n"
                       (replace-regexp-in-string
                        "[\"\\]" (lambda (c) (concat "\\" c)) header t t)))
             headers ""))

(cl-defun gent-llm-chat (&key backend messages prompt tools tool-choice
                               on-token on-reasoning on-tool-calls on-done on-error)
  "Stream a chat completion from BACKEND.

BACKEND is a `gent-llm-backend'; it defaults to `gent-llm-default-backend'.

MESSAGES is a list or vector of alists.  Besides ((role . R)
(content . C)), tool-aware messages are supported: an assistant turn
may carry `tool_calls', and a tool turn may carry `tool_call_id' and
`content'.  PROMPT, when given, is appended as a user message.

TOOLS is a list of `gent-llm-make-tool' definitions (or equivalent
plists); TOOL-CHOICE is passed through as `tool_choice' when non-nil.

Streamed text chunks are passed to ON-TOKEN and chunks of the model's
reasoning to ON-REASONING; reasoning is not part of the full text.
Assembled tool calls are passed to ON-TOOL-CALLS as (CALLS
FINISH-REASON), the full text to ON-DONE, and any failure message to
ON-ERROR.  This library does not execute the tools; callers must do
that themselves.  Return the curl process; killing it cancels the
request without calling ON-ERROR."
  (let* ((backend (or backend gent-llm-default-backend
                      (error "gent-llm: no backend; set `gent-llm-default-backend'")))
         (provider (gent-llm-backend-provider backend))
         (data-file (gent-llm--write-payload
                     (gent-llm--payload backend messages prompt tools tool-choice)))
         (pending nil)
         (output "")
         (full "")
         (tool-calls [])
         (finish-reason nil)
         (parse (lambda (text)
                  (let ((result (gent-llm--filter
                                 `((:pending . ,text)
                                   (:tool-calls . ,tool-calls)
                                   (:finish-reason . ,finish-reason)))))
                    (setq pending (map-elt result :pending))
                    (setq tool-calls (map-elt result :tool-calls))
                    (setq finish-reason (or (map-elt result :finish-reason)
                                            finish-reason))
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
           :command (list gent-llm-curl-executable (gent-llm--url provider)
                          "--fail-with-body" "--no-progress-meter" "--no-buffer"
                          "-m" (number-to-string (gent-llm--timeout backend))
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
                 ((guard (zerop (process-exit-status process)))
                  (let ((calls (gent-llm--tool-calls tool-calls)))
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
    (process-send-string process (gent-llm--curl-config
                                  (gent-llm--headers provider)))
    (process-send-eof process)
    process))

(provide 'gent-llm)
;;; gent-llm.el ends here
