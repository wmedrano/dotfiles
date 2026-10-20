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
;;   M-p/M-n  `gent-previous-input'     recall earlier prompts
;;   C-c C-c  `gent-interrupt'          cancel the current turn
;;   C-c C-t  `gent-toggle-details'     show or hide reasoning and tool output
;;   C-c C-e  `gent-set-reasoning-effort' change how hard the model thinks
;;   C-c C-k  `gent-reset'              forget the conversation
;;   C-c C-m  `gent-set-model'          switch model (or provider)
;;
;; Sessions can be saved with `gent-save-session' and restored with
;; `gent-load-session'.  `gent-batch-run' runs a single prompt through
;; the full loop from the command line.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'map)
(require 'project)
(require 'ring)
(require 'subr-x)
(require 'gent-llm)
(require 'gent-tools)
(require 'markdown-ts-mode)

(defgroup gent nil
  "A gentlemanly coding agent."
  :group 'tools)

(defcustom gent-system-prompt
  "You are gent, a coding agent running inside Emacs.  You help the user \
understand and change the project they are working on.

- Use the tools to look at the project before answering questions about it; \
do not guess file contents.
- Use run_shell with ls, find, rg or grep to explore, and read_file to read.
- Make focused changes with edit_file.  Every change and command is shown to \
the user, who may reject it; if they leave a note, follow it.
- Keep answers short and concrete.  Refer to code as path:line."
  "System prompt sent as the first message of every request.
Details about the project root, the tools and the project's
AGENTS.md are appended to it."
  :type 'string
  :group 'gent)

(defcustom gent-max-iterations 1000
  "Maximum number of model requests in a single turn."
  :type 'integer
  :group 'gent)

(defcustom gent-project-instruction-files '("AGENTS.md")
  "Files in the project root that are added to the system prompt."
  :type '(repeat string)
  :group 'gent)

(defcustom gent-project-instructions-max-chars 20000
  "Maximum characters read from each of `gent-project-instruction-files'."
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

(defcustom gent-session-directory
  (locate-user-emacs-file "gent-sessions/")
  "Directory where `gent-save-session' stores sessions."
  :type 'directory
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

(defvar-local gent--pending nil
  "The tool call awaiting approval, or nil.
A plist with `:name', `:kind', `:text', `:respond' and `:write'.")

(defvar-local gent--review-source nil
  "In a review buffer, the gent buffer whose pending change it shows.")

(defvar-local gent--messages nil
  "Conversation history, a list of message alists, oldest first.")

(defvar-local gent--root nil
  "Project root the gent buffer works in.")

(defvar-local gent--backend nil
  "Backend used by this buffer, or nil for `gent-llm-default-backend'.")

(defvar-local gent--output-marker nil
  "Where output is inserted in a gent buffer, just before the prompt.")

(defvar-local gent--input-marker nil
  "Start of the editable input area of a gent buffer.")

(defvar-local gent--busy nil
  "Non-nil while a turn started from the input area is running.")

(defvar-local gent--queue nil
  "Prompts sent while busy, oldest first, to run after the current turn.")

(defvar-local gent--history nil
  "Ring of prompts sent from the gent buffer, most recent first.")

(defvar-local gent--history-index nil
  "Index in `gent--history' of the prompt being recalled, or nil.")

(defvar-local gent--request-process nil
  "Process of the model request in flight, if any.")

(defvar-local gent--turn 0
  "Counter identifying the current turn.
It changes whenever a turn starts or is cancelled, so callbacks of
a stale turn can tell they should stop.")

;;; Messages

(defun gent--root ()
  "Return the project root for the current buffer."
  (or gent--root
      (let ((project (project-current)))
        (expand-file-name (if project (project-root project) default-directory)))))

(defun gent--project-instructions (root)
  "Return the contents of the instruction files found in ROOT."
  (mapconcat
   (lambda (name)
     (let ((file (expand-file-name name root)))
       (when (file-readable-p file)
         (with-temp-buffer
           (insert-file-contents file nil 0 gent-project-instructions-max-chars)
           (format "\n\n# %s\n\n%s" name (buffer-string))))))
   gent-project-instruction-files ""))

(defun gent--system-message (root)
  "Return the system message for a conversation in ROOT."
  `((role . "system")
    (content . ,(concat gent-system-prompt
                        (format "\n\nProject root: %s\nAll relative paths are resolved against it.\nAvailable tools: %s"
                                root (string-join (gent-tools-names) ", "))
                        (gent--project-instructions root)))))

(defun gent--message-chars (message)
  "Return the approximate size of MESSAGE in characters."
  (+ (length (or (map-elt message 'content) ""))
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
  (setq gent--messages (append gent--messages (list message))))

(defun gent--overhead-tokens (system)
  "Return the estimated tokens of the SYSTEM message and tool definitions."
  (/ (+ (gent--message-chars system)
        (length (json-encode (gent-tools-definitions))))
     4))

(defun gent--update-mode-line ()
  "Show the estimated context size in the mode line."
  (let ((tokens (+ (gent--overhead-tokens (gent--system-message (gent--root)))
                   (gent--estimate-tokens gent--messages))))
    (setq mode-line-process
          (list (format " ~%.1fk/%dk" (/ tokens 1000.0) (/ gent-context-max-tokens 1000))
                (when gent--busy " …")
                (when gent--queue (format " +%d queued" (length gent--queue)))
                (when gent--pending
                  (propertize " [approve?]" 'face 'gent-approval-face)))))
  (force-mode-line-update))

;;; Display

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
                        'invisible 'gent-details
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
              'invisible 'gent-details
              'face 'gent-reasoning-face
              'font-lock-face 'gent-reasoning-face))

(defun gent-toggle-details ()
  "Show or hide reasoning and tool output in the current buffer."
  (interactive)
  (if (memq 'gent-details (ensure-list buffer-invisibility-spec))
      (remove-from-invisibility-spec 'gent-details)
    (add-to-invisibility-spec 'gent-details))
  (force-window-update))

;;; Agent loop

(defun gent--live-p (buffer turn)
  "Return non-nil if TURN is still the current turn of BUFFER."
  (and (buffer-live-p buffer)
       (eql turn (buffer-local-value 'gent--turn buffer))))

(defun gent--start-turn (buffer prompt shell)
  "Add PROMPT to BUFFER's conversation and run the agent loop.
SHELL is an alist of output functions, `:write-output' and
`:finish-output'; see `gent--shell'."
  (with-current-buffer buffer
    (setq gent--turn (1+ gent--turn))
    (setq gent--messages (gent--repair gent--messages))
    (gent--append `((role . "user") (content . ,prompt)))
    (gent--request buffer gent--turn shell 1)))

(defun gent--finish (buffer shell success)
  "Finish the turn in BUFFER through SHELL with SUCCESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (gent--update-mode-line)))
  (funcall (map-elt shell :finish-output) success))

(defun gent--request (buffer turn shell iteration)
  "Send BUFFER's conversation to the model for TURN.
SHELL receives the output.  ITERATION counts requests in this turn."
  (with-current-buffer buffer
    (let* ((write (map-elt shell :write-output))
           (reasoning nil)
           (end-reasoning (lambda ()
                            (when reasoning
                              (setq reasoning nil)
                              (funcall write (gent--format-reasoning "\n\n")))))
           (calls nil)
           (system (gent--system-message (gent--root))))
      (setq gent--messages (gent--compact gent--messages
                                          (gent--overhead-tokens system)))
      (gent--update-mode-line)
      (condition-case err
          (setq gent--request-process
           (gent-llm-chat
           :messages (cons system gent--messages)
           :backend gent--backend
           :tools (gent-tools-definitions)
           :on-reasoning (lambda (chunk)
                           (when (gent--live-p buffer turn)
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
                                                   text calls)
                          (error
                           (funcall write (format "\n\ngent: %s\n"
                                                  (error-message-string err)))
                           (gent--finish buffer shell nil)))))
           :on-error (lambda (message)
                       (when (gent--live-p buffer turn)
                         (funcall write (format "\n\n%s\n" message))
                         (gent--finish buffer shell nil)))))
        (error
         (funcall write (format "\n\ngent: %s\n" (error-message-string err)))
         (gent--finish buffer shell nil))))))

(defun gent--handle-response (buffer turn shell iteration text calls)
  "Record the model's TEXT and tool CALLS, then continue or finish.
BUFFER, TURN, SHELL and ITERATION are as in `gent--request'."
  (with-current-buffer buffer
    (gent--append `((role . "assistant")
                    (content . ,(or text ""))
                    ,@(when calls `((tool_calls . ,(vconcat calls))))))
    (cond
     ((null calls)
      (gent--finish buffer shell t))
     ((>= iteration gent-max-iterations)
      (setq gent--messages (gent--repair gent--messages))
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
        (gent-tools-call
         name arguments (gent--root)
         (lambda (result)
           ;; Let synchronous tools unwind before the next step.
           (run-at-time
            0 nil
            (lambda ()
              (when (gent--live-p buffer turn)
                (with-current-buffer buffer
                  (gent--append `((role . "tool")
                                  (tool_call_id . ,(map-elt call 'id))
                                  (content . ,result))))
                (funcall write (gent--format-tool-result result))
                (gent--run-calls buffer turn shell (cdr calls) then)))))
         (lambda (_name kind text respond)
           (when (gent--live-p buffer turn)
             (gent--await-approval buffer write name kind text respond))))))))

(defun gent--await-approval (buffer write name kind text respond)
  "Pause BUFFER's turn until the user answers a change by tool NAME.
KIND and TEXT describe the change and are shown through WRITE.
RESPOND is called with the answer by `gent-approve' or `gent-reject'."
  (with-current-buffer buffer
    (setq gent--pending (list :name name :kind kind :text text
                              :respond respond :write write))
    (gent--update-mode-line))
  (funcall write (gent--format-approval kind text))
  (unless (get-buffer-window buffer 'visible)
    (message "gent: %s awaits approval in %s (M-x gent-approve or gent-reject)"
             name (buffer-name buffer))))

;;; Approval

(defun gent--pending-buffers ()
  "Return the gent buffers with a change awaiting approval."
  (seq-filter (lambda (buffer) (buffer-local-value 'gent--pending buffer))
              (buffer-list)))

(defun gent--pending-buffer ()
  "Return the gent buffer whose pending change the user means.
That is the current gent buffer or the one the current review buffer
belongs to; otherwise the only buffer with a pending change, the one
for the current project, or one the user picks."
  (cond
   (gent--pending (current-buffer))
   ((and gent--review-source (buffer-live-p gent--review-source)
         (buffer-local-value 'gent--pending gent--review-source))
    gent--review-source)
   (t
    (let* ((buffers (gent--pending-buffers))
           (directory (expand-file-name default-directory)))
      (cond
       ((null buffers) (user-error "Nothing is awaiting approval"))
       ((null (cdr buffers)) (car buffers))
       ((seq-find (lambda (buffer)
                    (file-in-directory-p directory
                                         (buffer-local-value 'gent--root buffer)))
                  buffers))
       (t (get-buffer (completing-read "gent buffer: "
                                       (mapcar #'buffer-name buffers) nil t))))))))

(defun gent--answer (buffer answer)
  "Answer the change pending in BUFFER with ANSWER.
See `gent-tools-call' for the meaning of ANSWER."
  (with-current-buffer buffer
    (let ((pending gent--pending))
      (unless pending
        (user-error "Nothing is awaiting approval in %s" (buffer-name buffer)))
      (setq gent--pending nil)
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
         (pending (buffer-local-value 'gent--pending source))
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
              (add-text-properties start (point) gent--read-only-properties))))))))

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
        (if (not gent--busy)
            (gent--send-prompt prompt)
          (setq gent--queue (append gent--queue (list prompt)))
          (gent--update-mode-line)
          (message "gent: queued until the current turn ends"))))))

(defun gent--send-prompt (prompt)
  "Show PROMPT in the transcript and start a turn for it."
  (let ((buffer (current-buffer)))
    (gent--insert-output buffer (gent--format-user prompt))
    (setq gent--busy t)
    (gent--start-turn buffer prompt (gent--shell buffer))))

(defun gent--turn-done (buffer _success)
  "Mark the turn in BUFFER as done and send the next queued prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (eq (char-before gent--output-marker) ?\n)
        (gent--insert-output buffer "\n"))
      (setq gent--busy nil)
      (if gent--queue
          (gent--send-prompt (pop gent--queue))
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
  "C-c C-t" #'gent-toggle-details
  "C-c C-e" #'gent-set-reasoning-effort
  "C-c C-k" #'gent-reset
  "C-c C-m" #'gent-set-model
  "C-c C-a" #'gent-approve
  "C-c C-r" #'gent-reject
  "C-c C-v" #'gent-review)

(define-derived-mode gent-mode markdown-ts-mode "Gent"
  "Major mode for chatting with gent.
Type a prompt at the bottom of the buffer and send it with \\[gent-send].
\\{gent-mode-map}"
  (setq gent--history (make-ring 100))
  (add-to-invisibility-spec 'gent-details)
  (visual-line-mode 1)
  (add-hook 'kill-buffer-hook #'gent--kill-request nil t))

(defun gent--kill-request ()
  "Stop the model request of the current buffer, if any."
  (when (process-live-p gent--request-process)
    (delete-process gent--request-process)))

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
        (setq gent--root root)
        (gent--draw nil)
        (gent--update-mode-line)))
    (pop-to-buffer-same-window buffer)
    buffer))

(defun gent-interrupt ()
  "Cancel the current gent turn, including running tool commands."
  (interactive)
  (gent--ensure-buffer)
  (setq gent--turn (1+ gent--turn))
  (when gent--pending
    (setq gent--pending nil)
    (gent--close-review (current-buffer)))
  (gent-tools-kill-processes)
  (gent--kill-request)
  (setq gent--messages (gent--repair gent--messages))
  (when gent--busy
    (gent--insert-output (current-buffer) "\n[interrupted]\n"))
  (setq gent--busy nil)
  (setq gent--queue nil)
  (gent--update-mode-line))

(defun gent-reset ()
  "Forget the conversation in the current gent buffer and clear it."
  (interactive)
  (gent--ensure-buffer)
  (when (or gent--busy gent--pending)
    (gent-interrupt))
  (setq gent--messages nil)
  (setq gent-tools-session-trusted nil)
  (gent--draw nil)
  (gent--update-mode-line)
  (message "gent: conversation reset"))

(defun gent--current-backend ()
  "Return the backend of the current gent buffer, or signal an error."
  (or gent--backend gent-llm-default-backend
      (user-error "gent: no backend; set `gent-llm-default-backend'")))

(defun gent--copy-backend ()
  "Return a copy of the current backend, to change for this buffer only."
  (gent-llm-copy-backend (gent--current-backend)))

(defun gent-set-model (model &optional url-base)
  "Use MODEL for requests from the current gent buffer.
With a prefix argument, also prompt for the API URL-BASE, to switch
provider."
  (interactive
   (let ((backend (gent--current-backend)))
     (list (read-string (format "Model (default %s): "
                                (gent-llm-backend-model backend))
                        nil nil (gent-llm-backend-model backend))
           (when current-prefix-arg
             (read-string "API URL base: "
                          (gent-llm-provider-url-base
                           (gent-llm-backend-provider backend)))))))
  (gent--ensure-buffer)
  (let ((backend (gent--copy-backend)))
    (setf (gent-llm-backend-model backend) model)
    (when url-base
      (let ((provider (gent-llm-copy-provider (gent-llm-backend-provider backend))))
        (setf (gent-llm-provider-url-base provider) url-base)
        (setf (gent-llm-backend-provider backend) provider)))
    (setq gent--backend backend)
    (message "gent: using %s at %s" model
             (gent-llm-provider-url-base (gent-llm-backend-provider backend)))))

(defun gent-set-reasoning-effort (effort)
  "Set the reasoning EFFORT for requests from the current gent buffer.
See `gent-llm-backend-reasoning-effort'."
  (interactive
   (let ((current (gent-llm-backend-reasoning-effort (gent--current-backend))))
     (list (completing-read
            (format "Reasoning effort (current %s): " current)
            '("none" "minimal" "low" "medium" "high" "xhigh" "max")
            nil nil nil nil current))))
  (gent--ensure-buffer)
  (let ((backend (gent--copy-backend)))
    (setf (gent-llm-backend-reasoning-effort backend) effort)
    (setq gent--backend backend))
  (message "gent: reasoning effort %s" effort))

;;; Sessions

(defun gent-save-session (file)
  "Save the conversation in the current gent buffer to FILE."
  (interactive
   (progn
     (gent--ensure-buffer)
     (make-directory gent-session-directory t)
     (list (read-file-name "Save session to: " gent-session-directory nil nil
                           (format-time-string "%Y%m%d-%H%M%S.eld")))))
  (let ((messages gent--messages)
        (root (gent--root))
        (model (and gent--backend (gent-llm-backend-model gent--backend))))
    (with-temp-file file
      (let ((print-length nil) (print-level nil))
        (prin1 `((version . 1) (root . ,root) (model . ,model)
                 (messages . ,messages))
               (current-buffer))))
    (message "gent: saved %d messages to %s" (length messages) file)))

(defun gent-load-session (file)
  "Replace the conversation in the current gent buffer with FILE's."
  (interactive
   (progn
     (gent--ensure-buffer)
     (list (read-file-name "Load session: " gent-session-directory nil t))))
  (gent--ensure-buffer)
  (when (or gent--busy gent--pending)
    (gent-interrupt))
  (let ((data (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer)))))
    (setq gent--messages (map-elt data 'messages))
    (let ((model (map-elt data 'model)))
      (if model
          (let ((backend (gent--copy-backend)))
            (setf (gent-llm-backend-model backend) model)
            (setq gent--backend backend))
        (setq gent--backend nil)))
    (gent--draw gent--messages)
    (gent--update-mode-line)
    (message "gent: loaded %d messages from %s" (length gent--messages) file)))

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
      (setq gent--root (expand-file-name (or root default-directory)))
      (gent--start-turn (current-buffer) prompt shell)
      (while (not done)
        (when gent--pending
          (princ (if approve "[approved]\n" "[rejected]\n"))
          (gent--answer (current-buffer) (and approve t)))
        (accept-process-output nil 0.1))
      (princ (format "\n[%s, ~%d tokens]\n" (if success "done" "failed")
                     (gent--estimate-tokens gent--messages)))
      gent--messages)))

(provide 'gent)
;;; gent.el ends here
