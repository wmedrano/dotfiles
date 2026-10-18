;;; valet-llm.el --- Streaming LLM queries for valet -*- lexical-binding: t; -*-

;;; Commentary:

;; A small OpenAI-compatible chat client with streaming support.
;;
;; `valet-llm-chat' sends a chat request and calls `:on-token' for each
;; streamed chunk, `:on-done' with the full text, or `:on-error' on failure.
;;
;;   (valet-llm-chat
;;    :prompt "Say hello in three words."
;;    :on-token (lambda (chunk) (insert chunk))
;;    :on-done  (lambda (full) (message "done: %s" full)))
;;
;; Tool calls are supported too.  Declare tools with `valet-llm-make-tool'
;; and receive assembled calls (raw JSON argument strings) via `:on-tool-calls':
;;
;;   (valet-llm-chat
;;    :prompt "What's the weather in Paris?"
;;    :tools (list (valet-llm-make-tool
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
;; Requests are always streamed (SSE).  The backend is
;; `shell-maker-make-http-request', so `shell-maker' must be available.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)
(require 'shell-maker)

(defgroup valet-llm nil
  "Streaming LLM queries for `valet'."
  :group 'tools)

(defcustom valet-llm-api-url-base "https://dumile.local/v1"
  "Base URL of the OpenAI-compatible API."
  :type 'string
  :group 'valet-llm)

(defcustom valet-llm-api-path "/chat/completions"
  "Path appended to `valet-llm-api-url-base'."
  :type 'string
  :group 'valet-llm)

(defcustom valet-llm-model "halogen-qwen3.8-flash-next"
  "Model id sent with each request."
  :type 'string
  :group 'valet-llm)

(defcustom valet-llm-timeout 600
  "Request timeout, in seconds."
  :type 'number
  :group 'valet-llm)

(defcustom valet-llm-max-tokens 4096
  "Maximum tokens to generate, or nil to let the server decide."
  :type '(choice (integer :tag "Tokens") (const :tag "Server default" nil))
  :group 'valet-llm)

(defcustom valet-llm-temperature nil
  "Sampling temperature, or nil to omit it."
  :type '(choice (number :tag "Value") (const :tag "Unset" nil))
  :group 'valet-llm)

(defcustom valet-llm-enable-thinking nil
  "When nil, ask the server not to emit reasoning content."
  :type 'boolean
  :group 'valet-llm)

(defcustom valet-llm-auth-file (expand-file-name "~/.pi/agent/auth.json")
  "JSON file holding API keys, keyed by provider."
  :type 'file
  :group 'valet-llm)

(defcustom valet-llm-auth-provider "dumile"
  "Provider key to read from `valet-llm-auth-file'."
  :type 'string
  :group 'valet-llm)

(defcustom valet-llm-api-key nil
  "API key as a string, a function returning one, or nil.
When nil, `valet-llm--api-key' falls back to the VALET_LLM_API_KEY
environment variable and then `valet-llm-auth-file'."
  :type '(choice (string :tag "Key")
                 (function :tag "Function")
                 (const :tag "Auto" nil))
  :group 'valet-llm)

(defun valet-llm--json-parse-string (string)
  "Parse STRING as JSON, returning nil on failure."
  (if (fboundp 'json-parse-string)
      (condition-case nil
          (json-parse-string string :object-type 'alist)
        (error nil))
    (condition-case nil
        (json-read-from-string string)
      (error nil))))

(defun valet-llm--key-from-auth-file ()
  "Return the API key stored for `valet-llm-auth-provider', if any."
  (when (file-readable-p valet-llm-auth-file)
    (let* ((data (condition-case nil
                     (with-temp-buffer
                       (insert-file-contents valet-llm-auth-file)
                       (valet-llm--json-parse-string (buffer-string)))
                   (error nil)))
           (provider (and data
                          (map-elt data (intern valet-llm-auth-provider)))))
      (map-elt provider 'key))))

(defun valet-llm--api-key ()
  "Resolve the API key to use, or signal an error."
  (cond
   ((and (stringp valet-llm-api-key)
         (not (string-empty-p valet-llm-api-key)))
    valet-llm-api-key)
   ((functionp valet-llm-api-key)
    (funcall valet-llm-api-key))
   ((let ((env (getenv "VALET_LLM_API_KEY")))
      (and env (not (string-empty-p env)) env)))
   ((valet-llm--key-from-auth-file))
   (t
    (error "valet-llm: no API key; set `valet-llm-api-key', VALET_LLM_API_KEY, or %s"
           valet-llm-auth-file))))

(defun valet-llm--url ()
  "Return the full chat completions URL."
  (concat (string-remove-suffix "/" valet-llm-api-url-base)
          valet-llm-api-path))

(defun valet-llm--headers ()
  "Return the HTTP headers for a request."
  (list "Content-Type: application/json"
        (format "Authorization: Bearer %s" (valet-llm--api-key))))

(defun valet-llm--normalize-message (message)
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

(defun valet-llm--normalize-messages (messages prompt)
  "Return a vector of message alists from MESSAGES and PROMPT."
  (let ((msgs (cond ((null messages) nil)
                    ((vectorp messages) (append messages nil))
                    ((listp messages) messages)
                    (t (error "valet-llm: invalid `messages'")))))
    (setq msgs (delq nil (mapcar #'valet-llm--normalize-message msgs)))
    (when (and prompt (not (string-empty-p prompt)))
      (setq msgs (append msgs
                         (list (valet-llm--normalize-message
                                `((role . "user") (content . ,prompt)))))))
    (vconcat msgs)))

(cl-defun valet-llm-make-tool (&key name description parameters)
  "Build an OpenAI tool definition for NAME, DESCRIPTION and PARAMETERS.
PARAMETERS is a JSON-schema alist, for example:

  ((type . \"object\")
   (properties . ((location . ((type . \"string\")))))
   (required . [\"location\"]))"
  `((type . "function")
    (function . ((name . ,name)
                 (description . ,description)
                 (parameters . ,parameters)))))

(defun valet-llm--normalize-tool (tool)
  "Return TOOL as an OpenAI tool-definition alist.
Accepts either `valet-llm-make-tool' output or a plist with
`:name', `:description' and `:parameters'."
  (cond
   ((and (listp tool) (assq 'function tool))
    tool)
   ((and (listp tool) (plist-member tool :name))
    (valet-llm-make-tool :name (plist-get tool :name)
                         :description (plist-get tool :description)
                         :parameters (plist-get tool :parameters)))
   (t
    (error "valet-llm: invalid tool definition: %S" tool))))

(defun valet-llm--payload (messages prompt model tools tool-choice)
  "Build the request payload alist for MESSAGES, PROMPT, MODEL and TOOLS."
  (let ((payload `((model . ,(or model valet-llm-model))
                   (messages . ,(valet-llm--normalize-messages messages prompt))
                   (stream . t)
                   (chat_template_kwargs
                    . ((enable_thinking
                        . ,(if valet-llm-enable-thinking t :false)))))))
    (when valet-llm-max-tokens
      (push `(max_tokens . ,valet-llm-max-tokens) payload))
    (when valet-llm-temperature
      (push `(temperature . ,valet-llm-temperature) payload))
    (when tools
      (push `(tools . ,(vconcat (mapcar #'valet-llm--normalize-tool
                                        (append tools nil))))
            payload))
    (when tool-choice
      (push `(tool_choice . ,tool-choice) payload))
    payload))

(defun valet-llm--merge-tool-call (acc tool-call)
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

(defun valet-llm--tool-calls (acc)
  "Return ACC vector as a list of clean OpenAI-shaped tool calls."
  (delq nil (append acc nil)))

(defun valet-llm--filter (state)
  "Parse streamed SSE output in STATE.
Return an alist of ((:filtered . TEXT) (:pending . PARTIAL-LINE)
(:tool-calls . ACC) (:finish-reason . REASON))."
  (let* ((raw (concat (or (map-elt state :pending) "") ""))
         (complete (string-suffix-p "\n" raw))
         (lines (split-string raw "\n"))
         (pending (and (not complete)
                       (not (string-empty-p raw))
                       (car (last lines))))
         (lines (if pending (butlast lines) lines))
         (tool-calls (or (map-elt state :tool-calls) []))
         (finish-reason (map-elt state :finish-reason))
         (text ""))
    (dolist (line lines)
      (when (string-prefix-p "data:" line)
        (let ((payload (string-trim (substring line 5))))
          (unless (or (string-empty-p payload)
                      (equal payload "[DONE]"))
            (let* ((obj (valet-llm--json-parse-string payload))
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
              (when (and valet-llm-enable-thinking reason)
                (setq text (concat text reason)))
              (dolist (call (append calls nil))
                (setq tool-calls (valet-llm--merge-tool-call
                                  tool-calls call))))))))
    `((:filtered . ,text)
      (:pending . ,(and pending (not (string-empty-p pending)) pending))
      (:tool-calls . ,tool-calls)
      (:finish-reason . ,finish-reason))))

(cl-defun valet-llm-chat (&key messages prompt model tools tool-choice
                               on-token on-tool-calls on-done on-error)
  "Stream a chat completion from the configured LLM.

MESSAGES is a list or vector of alists.  Besides ((role . R)
(content . C)), tool-aware messages are supported: an assistant turn
may carry `tool_calls', and a tool turn may carry `tool_call_id' and
`content'.  PROMPT, when given, is appended as a user message.  MODEL
overrides `valet-llm-model'.

TOOLS is a list of `valet-llm-make-tool' definitions (or equivalent
plists); TOOL-CHOICE is passed through as `tool_choice' when non-nil.

Streamed text chunks are passed to ON-TOKEN, assembled tool calls to
ON-TOOL-CALLS as (CALLS FINISH-REASON), the full text to ON-DONE, and
any failure message to ON-ERROR.  This library does not execute the
tools; callers must do that themselves.  Return the request process,
if one is available (best effort)."
  (let ((full "")
        (tool-calls [])
        (finish-reason nil))
    (shell-maker-make-http-request
     :async t
     :url (valet-llm--url)
     :timeout valet-llm-timeout
     :headers (valet-llm--headers)
     :data (valet-llm--payload messages prompt model tools tool-choice)
     :filter (lambda (state)
               (let ((result (valet-llm--filter state)))
                 (setq tool-calls (map-elt result :tool-calls))
                 (setq finish-reason (or (map-elt result :finish-reason)
                                         finish-reason))
                 result))
     :on-output (lambda (chunk)
                  (when (and chunk (not (string-empty-p chunk)))
                    (setq full (concat full chunk))
                    (when on-token
                      (funcall on-token chunk))))
     :on-finished (lambda (result)
                    (let ((status (map-elt result :exit-status)))
                      (if (and (numberp status) (zerop status))
                          (progn
                            (let ((calls (valet-llm--tool-calls tool-calls)))
                              (when (and on-tool-calls calls)
                                (funcall on-tool-calls calls finish-reason)))
                            (when on-done
                              (funcall on-done full)))
                        (when on-error
                          (funcall on-error
                                   (format "valet-llm: request failed (exit %s)%s"
                                           status
                                           (let ((out (map-elt result :output)))
                                             (if (and out (not (string-empty-p out)))
                                                 (format ": %s" out)
                                               "")))))))))
    (and (boundp 'shell-maker--request-process)
         shell-maker--request-process)))

(provide 'valet-llm)
;;; valet-llm.el ends here
