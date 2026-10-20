;;; gent-llm.el --- Streaming LLM queries for gent -*- lexical-binding: t; -*-

;;; Commentary:

;; A generic streaming chat interface.
;;
;; Backends include `gent-llm-generic-backend' and implement the generic
;; chat, copy, and settings operations below.  `gent-providers' supplies
;; `gent-openai-backend' for OpenAI-compatible APIs:
;;
;;   (require 'gent-providers)
;;   (setq gent-llm-default-backend
;;         (gent-make-openai-backend
;;          :name "openai"
;;          :url-base "https://api.openai.com/v1"
;;          :api-key (lambda () (getenv "OPENAI_API_KEY"))
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
;; This library dispatches requests to backends; it never executes tools.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'json)
(require 'map)
(require 'subr-x)

(cl-defstruct (gent-llm-generic-backend (:constructor nil) (:copier nil))
  "Base for backends implementing the gent LLM interface.
Subtypes implement chat and copying, plus getters and setters for model
and reasoning effort, using the generic operations below.")

(cl-defgeneric gent-llm-backend-chat (backend &key session-id messages prompt tools tool-choice
                                            on-token on-reasoning on-tool-calls
                                            on-done on-error)
  "Stream a completion from BACKEND.
SESSION-ID identifies the conversation.  MESSAGES, PROMPT, TOOLS and
TOOL-CHOICE describe the request.  ON-TOKEN, ON-REASONING,
ON-TOOL-CALLS, ON-DONE and ON-ERROR are callbacks.
All arguments follow `gent-llm-chat'.  Return a process; killing it
cancels the request without calling ON-ERROR.")

(cl-defgeneric gent-llm-copy-backend (backend)
  "Return a copy of BACKEND whose settings can be changed independently.")

(cl-defgeneric gent-llm-backend-model (backend)
  "Return the model id used by BACKEND.")

(cl-defgeneric (setf gent-llm-backend-model) (model backend)
  "Set BACKEND's model id to MODEL.")

(cl-defgeneric gent-llm-backend-reasoning-effort (backend)
  "Return BACKEND's reasoning effort, or nil to omit it.")

(cl-defgeneric (setf gent-llm-backend-reasoning-effort) (effort backend)
  "Set BACKEND's reasoning effort to EFFORT, or nil to omit it.")

(cl-defgeneric gent-llm-backend-preview (backend messages tools)
  "Return safe request details for BACKEND, MESSAGES and TOOLS as an alist.
Optional keys are `endpoint', `timeout' and `payload'.  Implementations
must not perform requests, resolve credentials, or include authentication
headers.  The default returns nil for backends without a wire preview.")

(cl-defmethod gent-llm-backend-preview ((_backend gent-llm-generic-backend)
                                      _messages _tools)
  nil)

(defvar gent-llm-default-backend nil
  "The `gent-llm-generic-backend' used when `gent-llm-chat' is not given one.")

(defun gent-llm-make-session-id ()
  "Return a new opaque conversation ID.
Reuse this ID for every request in the same conversation."
  (md5 (format "%s:%s:%s" (emacs-pid) (current-time) (random))))

(defun gent-llm--validate-session-id (session-id)
  "Signal an error unless SESSION-ID is nil or a nonempty, single-line string."
  (when (and session-id
             (not (and (stringp session-id)
                       (not (string-empty-p session-id))
                       (not (string-match-p "[\r\n]" session-id)))))
    (error "Gent LLM: session ID must be a nonempty, single-line string")))

(defun gent-llm--json-parse-string (string)
  "Parse STRING as JSON, decoding null as nil and returning nil on failure."
  (if (fboundp 'json-parse-string)
      (condition-case nil
          (json-parse-string string :object-type 'alist :null-object nil)
        (error nil))
    (condition-case nil
        (let ((json-null nil))
          (json-read-from-string string))
      (error nil))))

(defun gent-llm--normalize-message (message)
  "Return MESSAGE as a clean, tool-aware alist.
Preserves `role', `content', `reasoning_content', `tool_calls',
`tool_call_id' and `name'."
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
        (let ((reasoning (map-elt message 'reasoning_content)))
          (when (stringp reasoning)
            (push (cons 'reasoning_content (substring-no-properties reasoning)) out)))
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
                    (t (error "Gent LLM: invalid `messages'")))))
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
    (error "Gent LLM: invalid tool definition: %S" tool))))

(cl-defun gent-llm-chat (&key backend session-id messages prompt tools tool-choice
                               on-token on-reasoning on-tool-calls on-done on-error)
  "Stream a chat completion from BACKEND.

BACKEND implements `gent-llm-generic-backend'; it defaults to
`gent-llm-default-backend'.

SESSION-ID is an optional nonempty, single-line string identifying the
conversation.  Reuse it across turns, tool calls and retries.  Backends
may use or ignore it; some require it.  Callers own its lifecycle and
can generate one with `gent-llm-make-session-id'.

MESSAGES is a list or vector of alists.  Besides ((role . R)
\(content . C)), tool-aware messages are supported: an assistant turn
may carry `tool_calls' and `reasoning_content', and a tool turn may
carry `tool_call_id' and `content'.  Preserve streamed reasoning in
the corresponding assistant message for APIs that require it on
continuation.  PROMPT, when given, is appended as a user message.

TOOLS is a list of `gent-llm-make-tool' definitions (or equivalent
plists); TOOL-CHOICE is passed through as `tool_choice' when non-nil.

Streamed text chunks are passed to ON-TOKEN and chunks of the model's
reasoning to ON-REASONING; reasoning is not part of the full text.
Assembled tool calls are passed to ON-TOOL-CALLS as (CALLS
FINISH-REASON), the full text to ON-DONE, and any failure message to
ON-ERROR.  This library does not execute the tools; callers must do
that themselves.  Return the request process; killing it cancels the
request without calling ON-ERROR."
  (gent-llm--validate-session-id session-id)
  (gent-llm-backend-chat
   (or backend gent-llm-default-backend
       (error "Gent LLM: no backend; set `gent-llm-default-backend'"))
   :session-id session-id
   :messages messages :prompt prompt :tools tools :tool-choice tool-choice
   :on-token on-token :on-reasoning on-reasoning :on-tool-calls on-tool-calls
   :on-done on-done :on-error on-error))

(provide 'gent-llm)
;;; gent-llm.el ends here
