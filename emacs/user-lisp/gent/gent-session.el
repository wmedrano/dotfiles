;;; gent-session.el --- Session state for the gent coding agent -*- lexical-binding: t; -*-

;;; Commentary:

;; The `gent-session' struct holds the conversation and execution
;; state owned by one gent buffer.

;;; Code:

(require 'cl-lib)

(cl-defstruct gent-session
  "Conversation and execution state owned by one gent buffer."
  (messages nil :documentation "Conversation messages, oldest first.")
  (id nil :documentation "Conversation ID, generated lazily.")
  (root nil :documentation "Project root directory.")
  (backend nil :documentation "Backend, or nil for `gent-llm-default-backend'.")
  (backend-name nil :documentation "Selected name in `gent-backends', or nil.")
  (request-process nil :documentation "Model request process, or nil.")
  (tool-cancel nil :documentation "Function cancelling the running tool call, or nil.")
  (trusted nil :documentation "Tool names approved with `always' in this conversation.")
  (queue nil :documentation "Prompts sent during a turn, oldest first, to run after it ends.")
  (state 'idle :documentation "Execution state: idle, running, or (awaiting-approval . PLIST).
The approval PLIST has :name, :kind, :text, :respond and :write keys.")
  (turn 0 :documentation "Generation counter advanced on start and cancellation.
Callbacks from earlier turns must stop when this changes."))

(provide 'gent-session)
;;; gent-session.el ends here
