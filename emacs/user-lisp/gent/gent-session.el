;;; gent-session.el --- Session state for the gent coding agent -*- lexical-binding: t; -*-

;;; Commentary:

;; The `gent-session' struct holds the conversation and execution
;; state owned by one gent buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gent-tools)

(defcustom gent-project-instruction-files '("AGENTS.md")
  "Files in the project root that are added to the system prompt."
  :type '(repeat string)
  :group 'gent)

(defcustom gent-project-instructions-max-chars 20000
  "Maximum characters read from each of `gent-project-instruction-files'."
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

(defun gent--project-context (root)
  "Return the project root and available tools for ROOT."
  (format "Project root: %s\nAll relative paths are resolved against it.\nAvailable tools: %s"
          root (string-join (gent-tools-names) ", ")))

(defun gent--project-instructions (root)
  "Return the contents of the instruction files found in ROOT, or nil."
  (let ((parts
         (delq nil
               (mapcar
                (lambda (name)
                  (let ((file (expand-file-name name root)))
                    (when (file-readable-p file)
                      (with-temp-buffer
                        (insert-file-contents file nil 0 gent-project-instructions-max-chars)
                        (format "# %s\n\n%s" name (buffer-string))))))
                gent-project-instruction-files))))
    (when parts
      (string-join parts "\n\n"))))

(cl-defstruct gent-session
  "Conversation and execution state owned by one gent buffer."
  (messages nil :documentation "Conversation messages, oldest first.")
  (id nil :documentation "Conversation ID, generated lazily.")
  (root nil :documentation "Project root directory.")
  (backend nil :documentation "Backend, or nil for `gent-llm-default-backend'.")
  (backend-name nil :documentation "Selected name in `gent-backends', or nil.")
  (system-prompt
   `(("base" . ,gent-session-base-prompt)
     ("project" . gent--project-context)
     ("instructions" . gent--project-instructions))
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
