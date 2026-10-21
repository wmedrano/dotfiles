;;; gent-prompt.el --- Default system prompt parts for gent -*- lexical-binding: t; -*-

;;; Commentary:

;; Building blocks for the default system prompt.  A `gent-session' has
;; a `system-prompt' slot holding an alist of (NAME . PART), where PART
;; is a string or a function called with the project root.  Users who
;; want a different prompt can keep, drop, or reorder these parts, or
;; add their own:
;;
;;   (setf (gent-session-system-prompt session)
;;         `(("base" . ,gent-base-prompt)
;;           ("project" . gent-project-context)
;;           ("mine" . "Answer in rhyme.")))
;;
;; `gent-default-system-prompt' (in `gent-session.el') holds the alist
;; of parts used by new sessions.

;;; Code:

(require 'subr-x)
(require 'gent-tools)

(defconst gent-base-prompt
  "You are gent, a coding agent running inside Emacs.  You help the user \
understand and change the project they are working on.

- Use the tools to look at the project before answering questions about it; \
do not guess file contents.
- Use run_shell with ls, find, rg or grep to explore, and read_file to read.
- Make focused changes with edit_file.  Every change and command is shown to \
the user, who may reject it; if they leave a note, follow it.
- Keep answers short and concrete.  Refer to code as path:line."
  "Instructions that open the default system prompt.")

(defun gent-project-context (root)
  "Return the project root and available tools for ROOT."
  (format "Project root: %s\nAll relative paths are resolved against it.\nAvailable tools: %s"
          root (string-join (gent-tools-names) ", ")))

(defun gent-project-instructions (root)
  "Return the contents of the project instruction file in ROOT, or nil."
  (let* ((name "AGENTS.md")
         (max-chars 20000)
         (file (expand-file-name name root)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file nil 0 max-chars)
        (format "# %s\n\n%s" name (buffer-string))))))

(provide 'gent-prompt)
;;; gent-prompt.el ends here
