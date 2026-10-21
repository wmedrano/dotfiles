;;; gent-session.el --- Session state for the gent coding agent -*- lexical-binding: t; -*-

;;; Commentary:

;; The `gent-session' struct holds the conversation and execution
;; state owned by one gent buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gent-prompt)
(require 'gent-tools)

(defconst gent-default-system-prompt
  `(("base" . ,gent-base-prompt)
    ("project" . gent-project-context)
    ("instructions" . gent-project-instructions))
  "Default value of the `system-prompt' slot of `gent-session'.
Parts are combined as described in `gent-session-system-prompt-string'.
Treat this as read-only; set the slot per session instead of mutating it.")

(cl-defstruct gent-session
  "Conversation and execution state owned by one gent buffer."
  (messages nil :documentation "Conversation messages, oldest first.")
  (id nil :documentation "Conversation ID, generated lazily.")
  (root nil :documentation "Project root directory.")
  (backend nil :documentation "Backend, or nil for `gent-llm-default-backend'.")
  (backend-name nil :documentation "Selected name in `gent-backends', or nil.")
  (system-prompt
   (copy-tree gent-default-system-prompt)
   :documentation "System prompt parts as an alist of (NAME . PART).
NAME is a descriptive string.  PART is a string, or a function called
with the project root that returns a string or nil.  The non-empty parts
are joined with blank lines, in order, by `gent-session-system-prompt-string'.")
  (request-process nil :documentation "Model request process, or nil.")
  (tool-cancel nil :documentation "Function cancelling the running tool call, or nil.")
  (trusted nil :documentation "Tool names approved with `always' in this conversation.")
  (queue nil :documentation "Prompts sent during a turn, oldest first, to run after it ends.")
  (state 'idle :documentation "Execution state: idle, running, or (awaiting-approval . PLIST).
The approval PLIST has :name, :kind, :text, :respond and :write keys.")
  (turn 0 :documentation "Generation counter advanced on start and cancellation.
Callbacks from earlier turns must stop when this changes."))

(defun gent-session-system-prompt-string (session root)
  "Return the system prompt of SESSION for a conversation in ROOT."
  (string-join
   (delq nil
         (mapcar (lambda (entry)
                   (let* ((part (cdr entry))
                          (text (if (functionp part) (funcall part root) part)))
                     (unless (string-empty-p (or text ""))
                       text)))
                 (gent-session-system-prompt session)))
   "\n\n"))

(defvar-local gent-session-current nil
  "Session owned by this buffer, created by `gent-mode'.")

(provide 'gent-session)
;;; gent-session.el ends here
