;;; gent-session.el --- Session state for the gent coding agent -*- lexical-binding: t; -*-

;;; Commentary:

;; The `gent-session' struct holds the conversation and execution
;; state owned by one gent buffer, along with the functions that
;; prepare its messages for a request: repair, compaction and token
;; estimates.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)
(require 'gent-tools)

(defcustom gent-context-max-tokens 100000
  "Rough token budget for a conversation.
When the estimated size of the conversation passes
`gent-context-compact-ratio' of this, older tool results are elided."
  :type 'integer
  :group 'gent)

(defcustom gent-context-compact-ratio 0.75
  "Fraction of `gent-context-max-tokens' at which old tool results are elided."
  :type 'number
  :group 'gent)

(defcustom gent-context-keep-tool-results 6
  "Number of most recent tool results never elided by compaction."
  :type 'integer
  :group 'gent)

(defconst gent-session-base-prompt
  "You are gent, a coding agent running inside Emacs.  You help the user \
understand and change the project they are working on.

- Use the tools to look at the project before answering questions about it; \
do not guess file contents.
- Use run_shell with ls, find, rg or grep to explore, and read_file to read.
- Make focused changes with edit_file.  Every change and command is shown to \
the user, who may reject it; if they leave a note, follow it.
- Keep answers short and concrete.  Refer to code as path:line."
  "Instructions that open the default system prompt.")

(defun gent-session-project-context (root)
  "Get the system prompt for the project at ROOT."
    (format "Working directory: %s\nAvailable tools: %s"
            root
            (string-join (gent-tools-names) ", ")))

(defun gent-session-project-instructions (root)
  "Return the context from the AGENTS.md file in ROOT."
  (let* ((max-chars 20000)
         (file (expand-file-name "AGENTS.md" root)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file nil 0 max-chars)
        (format "# %s\n\n%s" "AGENTS.md" (buffer-string))))))

(defconst gent-default-system-prompt
  `(("base" . ,gent-session-base-prompt)
    ("project" . gent-session-project-context)
    ("instructions" . gent-session-project-instructions))
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
  (state 'idle :documentation "Execution state: where the session is in a turn.
A turn starts when the user sends a prompt and ends when the model
answers without tool calls, fails, or is interrupted.  The state is
one of these phases:

  `idle'               No turn is in progress.  A sent prompt starts one.
  `running'            A model request or tool call is in progress.
                       Sent prompts are queued until the turn ends.
  `awaiting-approval'  The turn is paused until the user approves or
                       rejects a change proposed by a tool.

The `awaiting-approval' phase is stored as (awaiting-approval . PLIST),
where PLIST describes the pending change:

  :name     Name of the tool that proposed it.
  :kind     `diff' for file changes, `command' for shell commands.
  :text     The diff or command shown to the user.
  :respond  Function called with the user's answer; see `gent-tools-call'.
  :write    Function that writes output to the session's display.

Use `gent-session-phase' and `gent-session-pending' to read the state
and `gent-session-transition' to change it.")
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

(defun gent-session-phase (session)
  "Return SESSION's phase: `idle', `running' or `awaiting-approval'.
The phase is the `state' slot without the approval data, so it can
be compared with `eq'.  See `gent-session' for what each phase means."
  (let ((state (gent-session-state session)))
    (if (consp state) (car state) state)))

(defun gent-session-pending (session)
  "Return the plist describing SESSION's pending change, or nil.
The plist is non-nil only in the `awaiting-approval' phase; its keys
are described in the `state' slot of `gent-session'."
  (let ((state (gent-session-state session)))
    (when (eq (car-safe state) 'awaiting-approval)
      (cdr state))))

(defun gent-session-transition (session event &optional approval)
  "Move SESSION to the phase that follows EVENT, and return the new state.
EVENT is one of:

  `start'           idle -> running               a prompt starts a turn
  `await-approval'  running -> awaiting-approval  a tool proposes a change
  `answer'          awaiting-approval -> running  the user approves or rejects
  `finish'          running -> idle               the turn ends
  `interrupt'       any phase -> idle             the user cancels the turn

For `await-approval', APPROVAL is the plist describing the change; see
the `state' slot of `gent-session'.  Signal an error when EVENT is not
allowed in the current phase, or when APPROVAL is missing."
  (let* ((phase (gent-session-phase session))
         (state
          (pcase (cons phase event)
            ('(idle . start) 'running)
            ('(running . await-approval)
             (unless approval (error "Gent: missing approval data"))
             (cons 'awaiting-approval approval))
            ('(awaiting-approval . answer) 'running)
            ('(running . finish) 'idle)
            (`(,_ . interrupt) 'idle)
            (_ (error "Gent: cannot %s while %s" event phase)))))
    (setf (gent-session-state session) state)))

;;; Messages

(defun gent-session-ensure-id (session)
  "Return SESSION's conversation ID, generating one if needed.
Reuse this ID for every request in the same conversation."
  (or (gent-session-id session)
      (setf (gent-session-id session)
            (md5 (format "%s:%s:%s" (emacs-pid) (current-time) (random))))))

(defun gent-session-append (session message)
  "Append MESSAGE to SESSION's conversation."
  (setf (gent-session-messages session)
        (append (gent-session-messages session) (list message))))

(defun gent-session-system-message (session)
  "Return the system message for SESSION's conversation."
  (let ((root (gent-session-root session)))
    `((role . "system")
      (content . ,(gent-session-system-prompt-string session root)))))

(defun gent-session--message-chars (message)
  "Return the approximate size of MESSAGE in characters."
  (+ (length (or (map-elt message 'content) ""))
     (length (or (map-elt message 'reasoning_content) ""))
     (apply #'+ (mapcar (lambda (call)
                          (length (or (map-elt (map-elt call 'function) 'arguments) "")))
                        (append (map-elt message 'tool_calls) nil)))))

(defun gent-session-estimate-tokens (messages)
  "Return a rough token count for MESSAGES."
  (/ (apply #'+ (mapcar #'gent-session--message-chars messages)) 4))

(defun gent-session-overhead-tokens (system &optional tools)
  "Return the estimated tokens of SYSTEM and TOOLS, defaulting to the registry."
  (/ (+ (gent-session--message-chars system)
        (length (json-encode (or tools (gent-tools-definitions)))))
     4))

(defun gent-session-context-tokens (session)
  "Return the estimated tokens of SESSION's next request."
  (+ (gent-session-overhead-tokens (gent-session-system-message session))
     (gent-session-estimate-tokens (gent-session-messages session))))

(defconst gent-session--elided "[output elided to save context]"
  "Content that replaces tool results removed by compaction.")

(defun gent-session-compact (messages &optional overhead)
  "Return MESSAGES with old tool results elided when they are too large.
Tool results are elided oldest first, keeping the last
`gent-context-keep-tool-results', until the estimate plus OVERHEAD
tokens fits under `gent-context-compact-ratio' of
`gent-context-max-tokens'."
  (let ((limit (* gent-context-compact-ratio gent-context-max-tokens))
        (tokens (+ (or overhead 0) (gent-session-estimate-tokens messages))))
    (if (<= tokens limit)
        messages
      (let* ((tool-count (cl-count "tool" messages
                                   :key (lambda (m) (map-elt m 'role))
                                   :test #'equal))
             (elidable (- tool-count gent-context-keep-tool-results)))
        (mapcar (lambda (message)
                  (if (and (> elidable 0)
                           (> tokens limit)
                           (equal (map-elt message 'role) "tool"))
                      (progn
                        (cl-decf elidable)
                        (if (equal (map-elt message 'content) gent-session--elided)
                            message
                          (cl-decf tokens (/ (- (gent-session--message-chars message)
                                                (length gent-session--elided))
                                             4))
                          (cons (cons 'content gent-session--elided)
                                (map-delete (copy-alist message) 'content))))
                    message))
                messages)))))

(defun gent-session-repair (messages)
  "Return MESSAGES with a result for every tool call.
A cancelled turn can leave tool calls without results, which the API
rejects; those get a placeholder result right after the call."
  (let ((answered (delq nil (mapcar (lambda (m) (map-elt m 'tool_call_id)) messages)))
        (out nil))
    (dolist (message messages)
      (push message out)
      (dolist (call (append (map-elt message 'tool_calls) nil))
        (let ((id (map-elt call 'id)))
          (unless (member id answered)
            (push `((role . "tool") (tool_call_id . ,id)
                    (content . "Cancelled by the user."))
                  out)))))
    (nreverse out)))

(defun gent-session-prepare-turn (messages prompt)
  "Return repaired MESSAGES followed by user PROMPT, without changing MESSAGES."
  (append (gent-session-repair messages)
          (list `((role . "user") (content . ,prompt)))))

(defun gent-session-prepare-request (session messages)
  "Prepare MESSAGES for a request from SESSION without changing them.
Return a plist with system-prefixed :messages, :tools, estimated :tokens,
and :elided tool result IDs (or message positions) changed by compaction."
  (let* ((system (gent-session-system-message session))
         (tools (gent-tools-definitions))
         (overhead (gent-session-overhead-tokens system tools))
         (compacted (gent-session-compact messages overhead)))
    (list :messages (cons system compacted)
          :tools tools
          :tokens (+ overhead (gent-session-estimate-tokens compacted))
          :elided (cl-loop for before in messages
                           for after in compacted
                           for index from 1
                           unless (equal before after)
                           collect (or (map-elt before 'tool_call_id) index)))))

(defvar-local gent-session-current nil
  "Session owned by this buffer, created by `gent-mode'.")

(provide 'gent-session)
;;; gent-session.el ends here
