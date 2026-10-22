;;; gent.el --- A gentlemanly coding agent -*- lexical-binding: t; -*-

;;; Commentary:

;; A coding agent.  `M-x gent' opens one chat buffer per project.
;; Type a prompt after the `gent>' prompt at the bottom and press RET;
;; it is sent to the LLM along with the conversation so far; when the model calls tools (see
;; `gent-tools.el'), gent runs them, sends the results back and repeats
;; until the model answers without tool calls.
;;
;; Edits and shell commands need your approval.  gent never grabs the
;; keyboard for it: the turn pauses, the diff or command is shown in
;; the gent buffer, and nothing happens until you answer with one of
;; these commands, which work from any buffer:
;;
;;   C-c C-a  `gent-approve'            approve (C-u: trust the tool for the session)
;;   C-c C-r  `gent-reject'             reject, with an optional note for the model
;;   C-c C-v  `gent-review'             show the full diff in a review buffer
;;
;; Other commands in the gent buffer:
;;
;;   RET      `gent-send'               send the prompt (C-j for a newline)
;;   C-c C-d  `gent-send-dry-run'       preview the next request without sending
;;   M-p/M-n  `gent-previous-input'     recall earlier prompts
;;   C-c C-c  `gent-interrupt'          cancel the current turn
;;   C-c C-t  `gent-toggle-details'     show or hide reasoning and tool output
;;   C-c C-e  `gent-set-reasoning-effort' change how hard the model thinks
;;   C-c C-k  `gent-reset'              forget the conversation (asks first)
;;   C-c C-m  `gent-set-backend'        select a configured backend
;;
;; `gent-batch-run' runs a single prompt through the full loop from the
;; command line.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'face-remap)
(require 'json)
(require 'map)
(require 'project)
(require 'ring)
(require 'subr-x)
(require 'gent-llm)
(require 'gent-session)
(require 'gent-tools)
(require 'markdown-ts-mode)

(defgroup gent nil
  "A gentlemanly coding agent."
  :group 'tools)

(defvar gent-backends nil
  "Alist of (NAME . BACKEND) entries offered by `gent-set-backend'.
Each BACKEND is a fully configured `gent-llm-generic-backend', including
its model and provider settings.  Selection copies it for the current
conversation; registry entries and `gent-llm-default-backend' are unchanged.")

(defcustom gent-max-iterations 1000
  "Maximum number of model requests in a single turn."
  :type 'integer
  :group 'gent)

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

(defcustom gent-tool-display-max-chars 4000
  "Maximum characters of a tool result shown in the gent buffer."
  :type 'integer
  :group 'gent)

(defcustom gent-approval-max-lines 40
  "Maximum lines of a pending change shown inline in the gent buffer.
Use `gent-review' to see all of it."
  :type 'integer
  :group 'gent)

(defcustom gent-prompt "gent> "
  "Prompt shown before the input area of a gent buffer."
  :type 'string
  :group 'gent)

(defface gent-prompt-face
  '((t :inherit minibuffer-prompt))
  "Face for the input prompt in the gent buffer."
  :group 'gent)

(defface gent-user-face
  '((t :inherit bold))
  "Face for the user's prompts in the gent buffer."
  :group 'gent)

(defface gent-tool-call-face
  '((t :inherit font-lock-function-name-face))
  "Face for tool call headers in the gent buffer."
  :group 'gent)

(defface gent-reasoning-face
  '((t :inherit shadow :slant italic))
  "Face for the model's reasoning in the gent buffer."
  :group 'gent)

(defface gent-tool-output-face
  '((t :inherit shadow))
  "Face for tool output in the gent buffer."
  :group 'gent)

(defface gent-approval-face
  '((t :inherit warning))
  "Face for the line asking to approve a pending change."
  :group 'gent)

(defun gent--buffer-session (buffer)
  "Return BUFFER's session, or nil if it has none."
  (buffer-local-value 'gent-session-current buffer))

(defun gent--state ()
  "Return the current session's execution phase, without its approval data."
  (let ((state (gent-session-state gent-session-current)))
    (if (consp state) (car state) state)))

(defun gent--session-pending (session)
  "Return SESSION's approval data, or nil when it is not awaiting approval."
  (let ((state (gent-session-state session)))
    (when (eq (car-safe state) 'awaiting-approval)
      (cdr state))))

(defun gent--transition (event &optional approval)
  "Apply EVENT to the current session's execution state.
start: idle -> running; await-approval: running -> awaiting-approval;
answer: awaiting-approval -> running; finish: running -> idle.
interrupt returns any state to idle.  APPROVAL carries the pending
tool details for await-approval.  Invalid transitions signal an error."
  (let* ((session gent-session-current)
         (phase (gent--state))
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

(defvar-local gent--review-source nil
  "In a review buffer, the gent buffer whose pending change it shows.")

(defvar-local gent--output-marker nil
  "Where output is inserted in a gent buffer, just before the prompt.")

(defvar-local gent--input-marker nil
  "Start of the editable input area of a gent buffer.")

(defvar-local gent--history nil
  "Ring of prompts sent from the gent buffer, most recent first.")

(defvar-local gent--history-index nil
  "Index in `gent--history' of the prompt being recalled, or nil.")

;;; Messages

(defun gent--ensure-session-id ()
  "Return this conversation's ID, generating one if needed."
  (let ((session gent-session-current))
    (or (gent-session-id session)
        (setf (gent-session-id session) (gent-llm-make-session-id)))))

(defun gent--root ()
  "Return the project root for the current buffer."
  (or (gent-session-root gent-session-current)
      (let ((project (project-current)))
        (expand-file-name (if project (project-root project) default-directory)))))

(defun gent--system-message (root)
  "Return the system message for a conversation in ROOT."
  `((role . "system")
    (content . ,(gent-session-system-prompt-string gent-session-current root))))

(defun gent--message-chars (message)
  "Return the approximate size of MESSAGE in characters."
  (+ (length (or (map-elt message 'content) ""))
     (length (or (map-elt message 'reasoning_content) ""))
     (apply #'+ (mapcar (lambda (call)
                          (length (or (map-elt (map-elt call 'function) 'arguments) "")))
                        (append (map-elt message 'tool_calls) nil)))))

(defun gent--estimate-tokens (messages)
  "Return a rough token count for MESSAGES."
  (/ (apply #'+ (mapcar #'gent--message-chars messages)) 4))

(defconst gent--elided "[output elided to save context]"
  "Content that replaces tool results removed by compaction.")

(defun gent--compact (messages &optional overhead)
  "Return MESSAGES with old tool results elided when they are too large.
Tool results are elided oldest first, keeping the last
`gent-context-keep-tool-results', until the estimate plus OVERHEAD
tokens fits under `gent-context-compact-ratio' of
`gent-context-max-tokens'."
  (let ((limit (* gent-context-compact-ratio gent-context-max-tokens))
        (tokens (+ (or overhead 0) (gent--estimate-tokens messages))))
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
                        (if (equal (map-elt message 'content) gent--elided)
                            message
                          (cl-decf tokens (/ (- (gent--message-chars message)
                                                (length gent--elided))
                                             4))
                          (cons (cons 'content gent--elided)
                                (map-delete (copy-alist message) 'content))))
                    message))
                messages)))))

(defun gent--repair (messages)
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

(defun gent--append (message)
  "Append MESSAGE to the conversation in the current buffer."
  (let ((session gent-session-current))
    (setf (gent-session-messages session)
          (append (gent-session-messages session) (list message)))))

(defun gent--overhead-tokens (system &optional tools)
  "Return the estimated tokens of SYSTEM and TOOLS, defaulting to the registry."
  (/ (+ (gent--message-chars system)
        (length (json-encode (or tools (gent-tools-definitions)))))
     4))

(defun gent--prepare-turn (messages prompt)
  "Return repaired MESSAGES followed by user PROMPT, without changing MESSAGES."
  (append (gent--repair messages)
          (list `((role . "user") (content . ,prompt)))))

(defun gent--prepare-request (messages)
  "Prepare MESSAGES for a request from this buffer without changing them.
Return a plist with system-prefixed :messages, :tools, estimated :tokens,
and :elided tool result IDs (or message positions) changed by compaction."
  (let* ((system (gent--system-message (gent--root)))
         (tools (gent-tools-definitions))
         (overhead (gent--overhead-tokens system tools))
         (compacted (gent--compact messages overhead)))
    (list :messages (cons system compacted)
          :tools tools
          :tokens (+ overhead (gent--estimate-tokens compacted))
          :elided (cl-loop for before in messages
                           for after in compacted
                           for index from 1
                           unless (equal before after)
                           collect (or (map-elt before 'tool_call_id) index)))))

(defun gent--update-mode-line ()
  "Show the estimated context size in the mode line."
  (let ((tokens (+ (gent--overhead-tokens (gent--system-message (gent--root)))
                   (gent--estimate-tokens (gent-session-messages gent-session-current)))))
    (setq mode-line-process
          (list (format " ~%.1fk/%dk" (/ tokens 1000.0) (/ gent-context-max-tokens 1000))
                (unless (eq (gent--state) 'idle) " …")
                (when-let* ((queue (gent-session-queue gent-session-current)))
                  (format " +%d queued" (length queue)))
                (when (gent--session-pending gent-session-current)
                  (propertize " [approve?]" 'face 'gent-approval-face)))))
  (force-mode-line-update))

;;; Display

(defun gent--header-button (label command help)
  "Return LABEL as a header button invoking COMMAND, with tooltip HELP."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (with-selected-window (posn-window (event-start event))
                    (call-interactively command))))
    (propertize label 'local-map map 'mouse-face 'highlight 'help-echo help)))

(defun gent--header-hat (shape)
  "Return hat SHAPE colored for the current execution state."
  (propertize shape 'face
              (pcase (gent--state)
                ('idle 'shadow)
                ('running 'gent-prompt-face)
                ('awaiting-approval 'gent-approval-face))))

(defun gent--header-line ()
  "Return the upper header row: hat crown, project and status.
Displayed in the tab line, above `gent--header-details'."
  (let* ((session gent-session-current)
         (root (or (gent-session-root session) default-directory)))
    (concat
     " " (gent--header-hat "  ████  ") " gent · "
     (propertize (file-name-nondirectory (directory-file-name root))
                 'face 'bold 'help-echo root)
     "  │  "
     (pcase (gent--state)
       ('awaiting-approval (propertize "Approval needed" 'face 'gent-approval-face))
       ('running "Working")
       ('idle "Ready"))
     (when-let* ((queue (gent-session-queue session)))
       (format " · %d queued" (length queue)))
     " ")))

(defun gent--header-details ()
  "Return the lower header row: hat brim, model and reasoning effort."
  (let* ((backend (or (gent-session-backend gent-session-current)
                      gent-llm-default-backend))
         (effort (and backend (gent-llm-backend-reasoning-effort backend))))
    (concat
     " " (gent--header-hat "▀▀▀▀▀▀▀▀") " Model: "
     (gent--header-button
      (if backend (gent-llm-backend-model backend) "No backend")
      #'gent-set-backend "mouse-1: Select backend (C-c C-m)")
     "  │  Reasoning: "
     (if backend
         (gent--header-button (or effort "default")
                              #'gent-set-reasoning-effort
                              "mouse-1: Set reasoning effort (C-c C-e)")
       "default")
     " ")))

(defun gent--format-arguments (arguments)
  "Return a one-line summary of the JSON ARGUMENTS string."
  (let ((args (ignore-errors (gent-tools--parse-arguments arguments))))
    (if (not (consp args))
        (or arguments "")
      (string-join
       (mapcar (lambda (pair)
                 (let ((value (format "%s" (cdr pair))))
                   (format "%s=%s" (car pair)
                           (truncate-string-to-width
                            (replace-regexp-in-string "\n" "⏎" value) 60 nil nil "…"))))
               args)
       " "))))

(defun gent--format-tool-call (name arguments)
  "Return the text shown for a call to tool NAME with ARGUMENTS."
  (propertize (format "\n⚙ %s %s\n" name (gent--format-arguments arguments))
              'face 'gent-tool-call-face
              'font-lock-face 'gent-tool-call-face))

(defun gent--format-tool-result (result)
  "Return the text shown for tool RESULT, hidden unless toggled."
  (let* ((lines (length (split-string result "\n")))
         (summary (format "  ↳ %d line%s (C-c C-t to toggle)\n"
                          lines (if (= lines 1) "" "s")))
         (body (concat (truncate-string-to-width
                        result gent-tool-display-max-chars nil nil "\n[…]")
                       "\n")))
    (concat (propertize summary 'face 'gent-tool-output-face
                        'font-lock-face 'gent-tool-output-face)
            (propertize body
                        'gent-details t
                        'face 'gent-tool-output-face
                        'font-lock-face 'gent-tool-output-face))))

(defun gent--diff-line-face (line)
  "Return the face for LINE of a unified diff, or nil."
  (cond ((string-prefix-p "@@" line) 'diff-hunk-header)
        ((or (string-prefix-p "+++" line) (string-prefix-p "---" line)) 'diff-file-header)
        ((string-prefix-p "+" line) 'diff-added)
        ((string-prefix-p "-" line) 'diff-removed)))

(defun gent--format-approval (kind text)
  "Return the text shown when a change of KIND described by TEXT awaits approval."
  (let* ((lines (split-string (string-trim-right text) "\n"))
         (shown (seq-take lines gent-approval-max-lines))
         (hidden (- (length lines) (length shown))))
    (concat
     (mapconcat (lambda (line)
                  (let ((face (and (eq kind 'diff) (gent--diff-line-face line))))
                    (if face
                        (propertize line 'face face 'font-lock-face face)
                      line)))
                shown "\n")
     (if (> hidden 0)
         (format "\n[%d more lines; C-c C-v to review]" hidden)
       "")
     "\n"
     (propertize "⏸ Awaiting approval: C-c C-a approve (C-u: always), C-c C-r reject, C-c C-v review\n"
                 'face 'gent-approval-face 'font-lock-face 'gent-approval-face))))

(defun gent--format-reasoning-header ()
  "Return the visible line that starts a block of reasoning."
  (propertize "\n💭 Reasoning (C-c C-t to toggle)\n"
              'face 'gent-reasoning-face
              'font-lock-face 'gent-reasoning-face))

(defun gent--format-reasoning (text)
  "Return reasoning TEXT, hidden unless toggled."
  (propertize text
              'gent-details t
              'face 'gent-reasoning-face
              'font-lock-face 'gent-reasoning-face))

(defun gent-toggle-details ()
  "Show or hide reasoning and tool output in the current buffer."
  (interactive)
  (let ((hidden (memq 'gent-details (ensure-list buffer-invisibility-spec))))
    (if hidden
        (remove-from-invisibility-spec 'gent-details)
      (add-to-invisibility-spec 'gent-details))
    (force-window-update)
    (message "gent: reasoning and tool output %s" (if hidden "shown" "hidden"))))

;;; Agent loop

(defun gent--live-p (buffer turn)
  "Return non-nil if TURN is still active and current in BUFFER."
  (and (buffer-live-p buffer)
       (let ((session (gent--buffer-session buffer)))
         (and session
              (not (eq (gent-session-state session) 'idle))
              (eql turn (gent-session-turn session))))))

(defun gent--start-turn (buffer prompt shell)
  "Add PROMPT to BUFFER's conversation and run the agent loop.
SHELL is an alist of output functions, `:write-output' and
`:finish-output'; see `gent--shell'."
  (with-current-buffer buffer
    (let ((session gent-session-current))
      (gent--transition 'start)
      (cl-incf (gent-session-turn session))
      (setf (gent-session-messages session)
            (gent--prepare-turn (gent-session-messages session) prompt))
      (gent--request buffer (gent-session-turn session) shell 1))))

(defun gent--finish (buffer shell success)
  "Finish the turn in BUFFER through SHELL with SUCCESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (gent--transition 'finish)
      (gent--update-mode-line)))
  (funcall (map-elt shell :finish-output) success))

(defun gent--request (buffer turn shell iteration)
  "Send BUFFER's conversation to the model for TURN.
SHELL receives the output.  ITERATION counts requests in this turn."
  (with-current-buffer buffer
    (let* ((session gent-session-current)
           (write (map-elt shell :write-output))
           (reasoning nil)
           (reasoning-content "")
           (end-reasoning (lambda ()
                            (when reasoning
                              (setq reasoning nil)
                              (funcall write (gent--format-reasoning "\n\n")))))
           (calls nil)
           (backend (or (gent-session-backend session) gent-llm-default-backend))
           (session-id (gent--ensure-session-id))
           (request (gent--prepare-request (gent-session-messages session))))
      (setf (gent-session-messages session)
            (cdr (plist-get request :messages)))
      (gent--update-mode-line)
      (condition-case err
          (setf (gent-session-request-process session)
           (gent-llm-chat
           :session-id session-id
           :messages (plist-get request :messages)
           :backend (gent-session-backend session)
           :tools (plist-get request :tools)
           :on-reasoning (lambda (chunk)
                           (when (gent--live-p buffer turn)
                             (setq reasoning-content (concat reasoning-content chunk))
                             (unless reasoning
                               (setq reasoning t)
                               (funcall write (gent--format-reasoning-header)))
                             (funcall write (gent--format-reasoning chunk))))
           :on-token (lambda (token)
                       (when (gent--live-p buffer turn)
                         (funcall end-reasoning)
                         (funcall write token)))
           :on-tool-calls (lambda (tool-calls _reason)
                            (setq calls tool-calls))
           :on-done (lambda (text)
                      (when (gent--live-p buffer turn)
                        (funcall end-reasoning)
                        (condition-case err
                            (gent--handle-response buffer turn shell iteration
                                                   text calls reasoning-content)
                          (error
                           (funcall write (format "\n\ngent: %s\n"
                                                  (error-message-string err)))
                           (gent--finish buffer shell nil)))))
           :on-error (lambda (message)
                       (when (gent--live-p buffer turn)
                         (funcall write (format "\n\n%s\n" message))
                         (funcall write
                                  (format "gent: request saved in %s\n"
                                          (buffer-name
                                           (with-current-buffer buffer
                                             (gent--save-failed-request
                                              backend session-id request message)))))
                         (gent--finish buffer shell nil)))))
        (error
         (funcall write (format "\n\ngent: %s\n" (error-message-string err)))
         (gent--finish buffer shell nil))))))

(defun gent--handle-response (buffer turn shell iteration text calls &optional reasoning)
  "Record the model's TEXT, tool CALLS and REASONING, then continue or finish.
BUFFER, TURN, SHELL and ITERATION are as in `gent--request'."
  (with-current-buffer buffer
    (gent--append `((role . "assistant")
                    (content . ,(or text ""))
                    ,@(when (and reasoning (not (string-empty-p reasoning)))
                        `((reasoning_content . ,reasoning)))
                    ,@(when calls `((tool_calls . ,(vconcat calls))))))
    (cond
     ((null calls)
      (gent--finish buffer shell t))
     ((>= iteration gent-max-iterations)
      (setf (gent-session-messages gent-session-current)
            (gent--repair (gent-session-messages gent-session-current)))
      (funcall (map-elt shell :write-output)
               (format "\n\ngent: stopped after %d requests (`gent-max-iterations').\n"
                       iteration))
      (gent--finish buffer shell nil))
     (t
      (gent--run-calls
       buffer turn shell calls
       (lambda ()
         (gent--request buffer turn shell (1+ iteration))))))))

(defun gent--run-calls (buffer turn shell calls then)
  "Run tool CALLS one at a time in BUFFER, then call THEN.
Each result is recorded as a tool message and shown through SHELL.
Stop silently if TURN is no longer current."
  (if (null calls)
      (funcall then)
    (let* ((call (car calls))
           (function (map-elt call 'function))
           (name (map-elt function 'name))
           (arguments (map-elt function 'arguments))
           (write (map-elt shell :write-output)))
      (funcall write (gent--format-tool-call name arguments))
      (with-current-buffer buffer
        (setf (gent-session-tool-cancel gent-session-current)
              (gent-tools-call
               name arguments (gent--root)
               (lambda (result)
                 ;; Let synchronous tools unwind before the next step.
                 (run-at-time
                  0 nil
                  (lambda ()
                    (when (gent--live-p buffer turn)
                      (with-current-buffer buffer
                        (setf (gent-session-tool-cancel gent-session-current) nil)
                        (gent--append `((role . "tool")
                                        (tool_call_id . ,(map-elt call 'id))
                                        (content . ,result))))
                      (funcall write (gent--format-tool-result result))
                      (gent--run-calls buffer turn shell (cdr calls) then)))))
               (lambda (_name kind text respond)
                 (when (gent--live-p buffer turn)
                   (gent--await-approval buffer write name kind text respond)))
               (lambda (name)
                 (and (buffer-live-p buffer)
                      (member name (gent-session-trusted
                                    (gent--buffer-session buffer)))))))))))

(defun gent--await-approval (buffer write name kind text respond)
  "Pause BUFFER's turn until the user answers a change by tool NAME.
KIND and TEXT describe the change and are shown through WRITE.
RESPOND is called with the answer by `gent-approve' or `gent-reject'."
  (with-current-buffer buffer
    (gent--transition
     'await-approval
     (list :name name :kind kind :text text :respond respond :write write))
    (gent--update-mode-line))
  (funcall write (gent--format-approval kind text))
  (unless (get-buffer-window buffer 'visible)
    (message "gent: %s awaits approval in %s (M-x gent-approve or gent-reject)"
             name (buffer-name buffer))))

;;; Approval

(defun gent--pending-in-buffer (buffer)
  "Return BUFFER's pending approval without initializing a session."
  (let ((session (gent--buffer-session buffer)))
    (and session (gent--session-pending session))))

(defun gent--pending-buffers ()
  "Return the gent buffers with a change awaiting approval."
  (seq-filter #'gent--pending-in-buffer (buffer-list)))

(defun gent--pending-buffer ()
  "Return the gent buffer whose pending change the user means.
That is the current gent buffer or the one the current review buffer
belongs to; otherwise the only buffer with a pending change, the one
for the current project, or one the user picks."
  (cond
   ((gent--pending-in-buffer (current-buffer)) (current-buffer))
   ((and gent--review-source (buffer-live-p gent--review-source)
         (gent--pending-in-buffer gent--review-source))
    gent--review-source)
   (t
    (let* ((buffers (gent--pending-buffers))
           (directory (expand-file-name default-directory)))
      (cond
       ((null buffers) (user-error "Nothing is awaiting approval"))
       ((null (cdr buffers)) (car buffers))
       ((seq-find (lambda (buffer)
                    (file-in-directory-p directory
                                         (gent-session-root
                                          (gent--buffer-session buffer))))
                  buffers))
       (t (get-buffer (completing-read "gent buffer: "
                                       (mapcar #'buffer-name buffers) nil t))))))))

(defun gent--answer (buffer answer)
  "Answer the change pending in BUFFER with ANSWER.
See `gent-tools-call' for the meaning of ANSWER."
  (with-current-buffer buffer
    (let* ((session gent-session-current)
           (pending (gent--session-pending session)))
      (unless pending
        (user-error "Nothing is awaiting approval in %s" (buffer-name buffer)))
      (when (eq answer 'always)
        (cl-pushnew (plist-get pending :name) (gent-session-trusted session)
                    :test #'equal))
      (gent--transition 'answer)
      (gent--update-mode-line)
      (gent--close-review buffer)
      (funcall (plist-get pending :write)
               (propertize (pcase answer
                             ('always (format "✓ Approved; %s is trusted for this session\n"
                                              (plist-get pending :name)))
                             (`(reject . ,(and note (pred (not string-empty-p))))
                              (format "✗ Rejected: %s\n" note))
                             ((or 'nil `(reject . ,_)) "✗ Rejected\n")
                             (_ "✓ Approved\n"))
                           'face 'gent-approval-face 'font-lock-face 'gent-approval-face))
      (funcall (plist-get pending :respond) answer))))

(defun gent-approve (&optional always)
  "Approve the change gent is waiting on.
With prefix argument ALWAYS, also trust the tool for the rest of the
session."
  (interactive "P")
  (gent--answer (gent--pending-buffer) (if always 'always t)))

(defun gent-reject (note &optional buffer)
  "Reject the change gent is waiting on in BUFFER.
NOTE, when not empty, is passed to the model to explain why."
  (interactive
   (let ((buffer (gent--pending-buffer)))
     (list (read-string "Reason for gent (optional): ") buffer)))
  (gent--answer (or buffer (gent--pending-buffer)) (cons 'reject note)))

(defvar-keymap gent-review-mode-map
  :doc "Keymap for `gent-review-mode'."
  "C-c C-c" #'gent-approve
  "C-c C-a" #'gent-approve
  "C-c C-k" #'gent-reject
  "C-c C-r" #'gent-reject)

(define-minor-mode gent-review-mode
  "Minor mode for reviewing a change gent is waiting on.
\\{gent-review-mode-map}"
  :lighter " gent-review")

(defun gent-review ()
  "Show the full change gent is waiting on, in a review buffer."
  (interactive)
  (let* ((source (gent--pending-buffer))
         (pending (gent--pending-in-buffer source))
         (review (get-buffer-create "*gent-review*")))
    (with-current-buffer review
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (plist-get pending :text))
        (if (eq (plist-get pending :kind) 'diff)
            (diff-mode)
          (fundamental-mode))
        (setq gent--review-source source)
        (gent-review-mode 1)
        (setq buffer-read-only t)
        (goto-char (point-min))
        (setq header-line-format
              (substitute-command-keys
               (format "gent %s: \\<gent-review-mode-map>\\[gent-approve] approve, \\[gent-reject] reject"
                       (plist-get pending :name))))))
    (pop-to-buffer review)))

(defun gent--close-review (source)
  "Close the review buffer if it shows SOURCE's pending change."
  (let ((review (get-buffer "*gent-review*")))
    (when (and review (eq (buffer-local-value 'gent--review-source review) source))
      (dolist (window (get-buffer-window-list review nil t))
        (quit-restore-window window 'kill))
      (when (buffer-live-p review)
        (kill-buffer review)))))

;;; Buffer

(defconst gent--read-only-properties
  '(read-only t front-sticky (read-only) rear-nonsticky t field output)
  "Text properties of the transcript and prompt.")

(defun gent--insert-output (buffer text)
  "Insert TEXT into the transcript of gent BUFFER, above the prompt.
The text is made read-only; the input area is left alone."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when gent--output-marker
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (save-excursion
            (goto-char gent--output-marker)
            (let ((start (point)))
              (insert text)
              (add-text-properties start (point) gent--read-only-properties)
              ;; Markdown fontification manages the `invisible' text property.
              ;; Use overlays so refontification cannot erase Gent's folding.
              (let ((end (point)))
                (while (< start end)
                  (let ((next (next-single-property-change start 'gent-details nil end)))
                    (when (get-text-property start 'gent-details)
                      (let ((overlay (make-overlay start next nil t nil)))
                        (overlay-put overlay 'invisible 'gent-details)
                        (overlay-put overlay 'evaporate t)))
                    (setq start next)))))))))))

(defun gent--shell (buffer)
  "Return the output functions used by turns started from BUFFER."
  (list (cons :write-output (lambda (text &optional _) (gent--insert-output buffer text)))
        (cons :finish-output (lambda (success) (gent--turn-done buffer success)))))

(defun gent--format-user (prompt)
  "Return the transcript text for a PROMPT the user sent."
  (concat "\n"
          (propertize gent-prompt 'face 'gent-prompt-face)
          (propertize prompt 'face 'gent-user-face)
          "\n\n"))

(defun gent--format-message (message)
  "Return the transcript text for MESSAGE of the conversation."
  (pcase (map-elt message 'role)
    ("user" (gent--format-user (map-elt message 'content)))
    ("tool" (gent--format-tool-result (or (map-elt message 'content) "")))
    ("assistant"
     (let ((calls (append (map-elt message 'tool_calls) nil)))
       (concat (or (map-elt message 'content) "")
               (mapconcat (lambda (call)
                            (let ((function (map-elt call 'function)))
                              (gent--format-tool-call (map-elt function 'name)
                                                      (map-elt function 'arguments))))
                          calls "")
               (if calls "" "\n"))))
    (_ "")))

(defun gent--draw (messages)
  "Redraw the current gent buffer showing MESSAGES, keeping the input."
  (let ((input (if gent--input-marker
                   (buffer-substring gent--input-marker (point-max))
                 ""))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (apply #'propertize
                   (concat "\n" (propertize gent-prompt 'face 'gent-prompt-face))
                   gent--read-only-properties))
    (setq gent--input-marker (point-marker))
    (setq gent--output-marker (copy-marker (point-min) t))
    (insert input)
    (gent--insert-output (current-buffer) (format "gent in %s\n" (gent--root)))
    (dolist (message messages)
      (gent--insert-output (current-buffer) (gent--format-message message)))
    (setq buffer-undo-list nil)
    (goto-char (point-max))))

(defun gent--input ()
  "Return the text in the input area."
  (buffer-substring-no-properties gent--input-marker (point-max)))

(defun gent--set-input (text)
  "Replace the text in the input area with TEXT."
  (delete-region gent--input-marker (point-max))
  (goto-char (point-max))
  (insert text))

(defun gent-send ()
  "Send the prompt in the input area.
While a turn is running the prompt is queued and sent after it."
  (interactive)
  (gent--ensure-buffer)
  (if (< (point) gent--input-marker)
      (goto-char (point-max))
    (let ((prompt (string-trim (gent--input))))
      (unless (string-empty-p prompt)
        (ring-insert gent--history prompt)
        (setq gent--history-index nil)
        (gent--set-input "")
        (setq buffer-undo-list nil)
        (if (eq (gent--state) 'idle)
            (gent--send-prompt prompt)
          (let ((session gent-session-current))
            (setf (gent-session-queue session)
                  (append (gent-session-queue session) (list prompt))))
          (gent--update-mode-line)
          (message "gent: queued until the current turn ends"))))))

(defun gent--send-prompt (prompt)
  "Show PROMPT in the transcript and start a turn for it."
  (let ((buffer (current-buffer)))
    (gent--insert-output buffer (gent--format-user prompt))
    (gent--start-turn buffer prompt (gent--shell buffer))))

;;; Request preview

(defvar-local gent--dry-run-source nil
  "Gent buffer whose request is displayed in this preview buffer.")

(defvar-keymap gent-dry-run-mode-map
  :doc "Keymap for request previews."
  :parent special-mode-map
  "g" #'gent-dry-run-refresh)

(define-derived-mode gent-dry-run-mode special-mode "Gent Dry Run"
  "Read-only preview of a Gent request.  Refresh with g; close with q.")

(defun gent-dry-run-refresh ()
  "Refresh this preview using its source buffer's current input."
  (interactive)
  (unless (buffer-live-p gent--dry-run-source)
    (user-error "The source Gent buffer no longer exists"))
  (with-current-buffer gent--dry-run-source
    (gent-send-dry-run)))

(defun gent--insert-debug-json (object)
  "Insert OBJECT as indented JSON in the current buffer."
  (let ((json-encoding-pretty-print t))
    (insert (json-encode object) "\n")))

(defun gent--save-failed-request (backend session-id request error)
  "Record REQUEST, which BACKEND rejected with ERROR, and return its buffer.
REQUEST is a plist from `gent--prepare-request' and SESSION-ID the
conversation it was sent for.  Each failure replaces the previous record
for this project."
  (let ((root (gent--root))
        (messages (plist-get request :messages))
        (tools (plist-get request :tools)))
    (with-current-buffer
        (get-buffer-create
         (format "*gent failed request: %s*"
                 (file-name-nondirectory (directory-file-name root))))
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Gent failed request\n\n"
                (format "Time: %s\nProject root: %s\nSession ID: %s\n"
                        (format-time-string "%F %T%z") root session-id)
                (format "Backend: %s\n" (type-of backend))
                (format "Error: %s\n" error))
        (let ((details (and backend
                            (condition-case err
                                (gent-llm-backend-preview backend messages tools)
                              (error `((preview-error . ,(error-message-string err))))))))
          (when-let* ((endpoint (map-elt details 'endpoint)))
            (insert (format "Endpoint: %s\n" endpoint)))
          (insert "\nRequest body\n\n")
          (cond ((map-elt details 'payload)
                 (gent--insert-debug-json (map-elt details 'payload)))
                ((map-elt details 'preview-error)
                 (insert (format "Preview failed: %s\n" (map-elt details 'preview-error)))
                 (gent--insert-debug-json (vconcat messages)))
                (t
                 (insert "This backend does not provide a wire preview; messages follow.\n\n")
                 (gent--insert-debug-json (vconcat messages)))))
        (goto-char (point-min))
        (set-buffer-modified-p nil))
      (current-buffer))))

(defun gent-send-dry-run ()
  "Preview the first request for the current input without sending it.
Leave input, conversation, history and execution state unchanged.
No tools, network requests or credential callbacks are run."
  (interactive)
  (gent--ensure-buffer)
  (let* ((session gent-session-current)
         (prompt (string-trim (gent--input))))
    (when (or (not (eq (gent--state) 'idle)) (gent-session-queue session))
      (user-error "Wait for the current turn and queued prompts before previewing"))
    (when (string-empty-p prompt)
      (user-error "No prompt to preview"))
    (let* ((source (current-buffer))
           (root (gent--root))
           (backend (gent--current-backend))
           (backend-name (gent-session-backend-name session))
           (session-id (gent-session-id session))
           (budget gent-context-max-tokens)
           (threshold (* gent-context-compact-ratio budget))
           (request (gent--prepare-request
                     (gent--prepare-turn
                      (copy-tree (gent-session-messages session) t) prompt)))
           (messages (plist-get request :messages))
           (tools (plist-get request :tools))
           (details (gent-llm-backend-preview backend messages tools))
           (buffer (get-buffer-create
                    (format "*gent dry run: %s*"
                            (file-name-nondirectory (directory-file-name root))))))
      (with-current-buffer buffer
        (gent-dry-run-mode)
        (setq gent--dry-run-source source)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Gent request preview\n\n"
                  "Snapshot of the first request.  g refreshes; q closes.\n"
                  "Later requests depend on model responses and tool results.\n\n"
                  (format "Source buffer: %s\nProject root: %s\n" (buffer-name source) root)
                  (format "Session ID: %s\n" (or session-id "(generated when sent)"))
                  (format "Backend: %s (%s)\n" (or backend-name "default") (type-of backend))
                  (format "Model: %s\nReasoning effort: %s\n"
                          (gent-llm-backend-model backend)
                          (or (gent-llm-backend-reasoning-effort backend) "(omitted)")))
          (when-let* ((endpoint (map-elt details 'endpoint)))
            (insert (format "Endpoint: %s\n" endpoint)))
          (when-let* ((timeout (map-elt details 'timeout)))
            (insert (format "Timeout: %s seconds\n" timeout)))
          (insert (format "Estimated context: %d tokens / %d budget\n"
                          (plist-get request :tokens) budget)
                  (format "Compaction threshold: %.0f tokens\n"
                          threshold)
                  (format "Tool results elided by this request: %S\n"
                          (plist-get request :elided))
                  "\nMessages (including system instructions)\n\n")
          (gent--insert-debug-json (vconcat messages))
          (insert "\nTool definitions\n\n")
          (gent--insert-debug-json (vconcat tools))
          (insert "\nRequest body\n\n")
          (if-let* ((payload (map-elt details 'payload)))
              (gent--insert-debug-json payload)
            (insert "This backend does not provide a wire preview.\n"))
          (goto-char (point-min))
          (set-buffer-modified-p nil)))
      (pop-to-buffer buffer
                     '((display-buffer-reuse-window display-buffer-pop-up-window))))))

(defun gent--turn-done (buffer _success)
  "Finish displaying the turn in BUFFER and send the next queued prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (eq (char-before gent--output-marker) ?\n)
        (gent--insert-output buffer "\n"))
      (if (gent-session-queue gent-session-current)
          (gent--send-prompt (pop (gent-session-queue gent-session-current)))
        (gent--update-mode-line)))))

(defun gent-previous-input (arg)
  "Replace the input with the ARGth previous prompt."
  (interactive "p")
  (gent--ensure-buffer)
  (when (ring-empty-p gent--history)
    (user-error "No previous input"))
  (let ((index (+ (or gent--history-index -1) arg)))
    (if (< index 0)
        (progn (setq gent--history-index nil)
               (gent--set-input ""))
      (setq gent--history-index (min index (1- (ring-length gent--history))))
      (gent--set-input (ring-ref gent--history gent--history-index)))))

(defun gent-next-input (arg)
  "Replace the input with the ARGth next prompt."
  (interactive "p")
  (gent-previous-input (- arg)))

(defvar-keymap gent-mode-map
  :doc "Keymap for `gent-mode'."
  "RET" #'gent-send
  "C-j" #'newline
  "S-<return>" #'newline
  "M-p" #'gent-previous-input
  "M-n" #'gent-next-input
  "C-c C-c" #'gent-interrupt
  "C-c C-d" #'gent-send-dry-run
  "C-c C-t" #'gent-toggle-details
  "C-c C-e" #'gent-set-reasoning-effort
  "C-c C-k" #'gent-reset
  "C-c C-m" #'gent-set-backend
  "C-c C-a" #'gent-approve
  "C-c C-r" #'gent-reject
  "C-c C-v" #'gent-review)

(define-derived-mode gent-mode markdown-ts-mode "Gent"
  "Major mode for chatting with gent.
Type a prompt at the bottom of the buffer and send it with \\[gent-send].
\\{gent-mode-map}"
  (setq gent-session-current (make-gent-session))
  ;; Each native bar is one line; together they form a pinned two-row header.
  ;; Match their fonts and backgrounds so the two halves of the hat align.
  (face-remap-add-relative 'header-line '(:underline nil :box nil) 'fixed-pitch)
  (face-remap-set-base 'tab-line 'header-line)
  (setq-local tab-line-format '(:eval (gent--header-line)))
  (setq-local header-line-format '(:eval (gent--header-details)))
  (setq gent--history (make-ring 100))
  (add-to-invisibility-spec 'gent-details)
  (visual-line-mode 1)
  (add-hook 'kill-buffer-hook #'gent--kill-request nil t))

(defun gent--kill-request ()
  "Stop the model request and tool call of the current buffer, if any."
  (let* ((session gent-session-current)
         (process (gent-session-request-process session))
         (cancel (gent-session-tool-cancel session)))
    (when (process-live-p process)
      (delete-process process))
    (when cancel
      (setf (gent-session-tool-cancel session) nil)
      (funcall cancel))))

(defun gent--buffer-p (&optional buffer)
  "Return non-nil if BUFFER (default: current) is a gent buffer."
  (with-current-buffer (or buffer (current-buffer))
    (derived-mode-p 'gent-mode)))

(defun gent--ensure-buffer ()
  "Signal an error unless the current buffer is a gent buffer."
  (unless (gent--buffer-p)
    (user-error "Not in a gent buffer")))

;;;###autoload
(defun gent ()
  "Start or switch to the gent buffer for the current project."
  (interactive)
  (let* ((project (project-current))
         (root (expand-file-name (if project (project-root project) default-directory)))
         (name (format "*gent %s*" (if project
                                       (project-name project)
                                     (file-name-nondirectory
                                      (directory-file-name root)))))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (gent--buffer-p)
        (gent-mode)
        (setq default-directory root)
        (setf (gent-session-root gent-session-current) root)
        (gent--draw nil)
        (gent--update-mode-line)))
    (pop-to-buffer-same-window buffer)
    buffer))

(defun gent-interrupt ()
  "Cancel the current gent turn, including running tool commands."
  (interactive)
  (gent--ensure-buffer)
  (let ((session gent-session-current))
    (cl-incf (gent-session-turn session))
    (when (gent--session-pending session)
      (gent--close-review (current-buffer)))
    (let ((active (not (eq (gent--state) 'idle))))
      (gent--transition 'interrupt)
      (when (and active (gent--buffer-p))
        (gent--insert-output (current-buffer) "\n[interrupted]\n")))
    (gent--kill-request)
    (setf (gent-session-messages session)
          (gent--repair (gent-session-messages session)))
    (setf (gent-session-queue session) nil)
    (gent--update-mode-line)))

(defun gent-reset (&optional no-confirm)
  "Forget the conversation in the current gent buffer and clear it.
Ask for confirmation when the conversation has messages, unless
NO-CONFIRM is non-nil.  The interactive prefix argument means
NO-CONFIRM, so \\[universal-argument] \\[gent-reset] resets without asking."
  (interactive "P")
  (gent--ensure-buffer)
  (let ((session gent-session-current))
    (if (and (not no-confirm)
             (gent-session-messages session)
             (not (y-or-n-p "Reset the gent conversation? ")))
        (message "gent: reset cancelled")
      (unless (eq (gent--state) 'idle)
        (gent-interrupt))
      (setf (gent-session-messages session) nil
            (gent-session-id session) nil
            (gent-session-trusted session) nil)
      (gent--draw nil)
      (gent--update-mode-line)
      (message "gent: conversation reset"))))

(defun gent--current-backend ()
  "Return the backend of the current gent buffer, or signal an error."
  (or (gent-session-backend gent-session-current) gent-llm-default-backend
      (user-error "Gent: no backend; set `gent-llm-default-backend'")))

(defun gent--copy-backend ()
  "Return a copy of the current backend, to change for this buffer only."
  (gent-llm-copy-backend (gent--current-backend)))

(defun gent--named-backend (name)
  "Return the configured backend NAME, or signal a user error."
  (or (alist-get name gent-backends nil nil #'equal)
      (user-error "Gent: unknown backend %S; configure `gent-backends'" name)))

(defun gent-set-backend (name)
  "Select NAME from `gent-backends' for the current conversation.
Copy the configured backend, including its model and provider settings.
Leave registry entries and `gent-llm-default-backend' unchanged."
  (interactive
   (progn
     (gent--ensure-buffer)
     (unless gent-backends
       (user-error "Gent: no backends configured; set `gent-backends'"))
     (list (completing-read "Backend: " gent-backends nil t
                            nil nil (gent-session-backend-name gent-session-current)))))
  (gent--ensure-buffer)
  (let ((session gent-session-current))
    (setf (gent-session-backend session)
          (gent-llm-copy-backend (gent--named-backend name))
          (gent-session-backend-name session) name))
  (force-mode-line-update)
  (message "gent: using %s" name))

(defun gent-set-reasoning-effort (effort)
  "Set the reasoning EFFORT for requests from the current gent buffer.
See `gent-llm-backend-reasoning-effort'."
  (interactive
   (progn
     (gent--ensure-buffer)
     (let ((current (gent-llm-backend-reasoning-effort (gent--current-backend))))
       (list (completing-read
              (format "Reasoning effort (current %s): " current)
              '("none" "minimal" "low" "medium" "high" "xhigh" "max")
              nil nil nil nil current)))))
  (gent--ensure-buffer)
  (let ((backend (gent--copy-backend)))
    (setf (gent-llm-backend-reasoning-effort backend) effort)
    (setf (gent-session-backend gent-session-current) backend))
  (force-mode-line-update)
  (message "gent: reasoning effort %s" effort))

;;; Batch

(defun gent-batch-run (prompt &optional approve root)
  "Run PROMPT through the agent loop and print the transcript.
For use with `emacs --batch'.  When APPROVE is non-nil every tool
call is approved, otherwise changes are rejected.  ROOT defaults to
`default-directory'.  Return the conversation."
  (let* ((done nil)
         (success nil)
         (shell (list (cons :write-output (lambda (text &optional _force) (princ text)))
                      (cons :finish-output (lambda (ok) (setq success ok done t))))))
    (with-temp-buffer
      (let ((session (setq gent-session-current (make-gent-session))))
        (setf (gent-session-root session) (expand-file-name (or root default-directory)))
        (gent--start-turn (current-buffer) prompt shell)
        (while (not done)
          (when (gent--session-pending session)
            (princ (if approve "[approved]\n" "[rejected]\n"))
            (gent--answer (current-buffer) (and approve t)))
          (accept-process-output nil 0.1))
        (princ (format "\n[%s, ~%d tokens]\n" (if success "done" "failed")
                       (gent--estimate-tokens (gent-session-messages session))))
        (gent-session-messages session)))))

(provide 'gent)
;;; gent.el ends here
