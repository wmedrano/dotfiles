;;; monorepo.el --- Project backend scoped by a .monorepo file -*- lexical-binding: t; -*-

;;; Commentary:

;; A `project.el' backend for large repositories.  A directory containing
;; a `monorepo-file-name' file is a project root, and the file describes
;; what is in scope, one "KEY: VALUE" entry per line:
;;
;;   # Include a directory
;;   dir: services/foo
;;   # Include a single file
;;   file: BUILD
;;   # Assign a name to the project
;;   name: foo
;;   # Include the files printed by a shell command, one per line
;;   command: jj diff --name-only --ignore-working-copy
;;
;; Paths are relative to the root.  Commands run in the root each time
;; the project's files are listed, and must print paths relative to it.  Blank lines, lines starting with #,
;; unknown keys and paths that don't exist are ignored.  Commands such as
;; `project-find-file' only see the included directories and files.
;;
;; Add `monorepo-project-try' to `project-find-functions' to enable the
;; backend.  `monorepo-add-directory' and `monorepo-remove-directory' edit
;; the marker file's "dir" entries, `monorepo-add-command' previews and
;; adds a "command" entry, and `monorepo-find-file' opens it.

;;; Code:

(require 'cl-lib)
(require 'project)

(defgroup monorepo nil
  "Project backend scoped by a .monorepo file."
  :group 'project)

(defcustom monorepo-file-name ".monorepo"
  "Name of the file that marks a monorepo root and describes its scope."
  :type 'string
  :group 'monorepo)

(defcustom monorepo-command-preview-lines 10
  "Number of files shown when previewing a command in `monorepo-add-command'."
  :type 'natnum
  :group 'monorepo)

(cl-defstruct monorepo-project
  "A project rooted at a directory containing `monorepo-file-name'."
  (root nil :documentation "Absolute directory containing the marker file.")
  (name nil :documentation "Name from the marker file, or nil for the default.")
  (dirs nil :documentation "Absolute directories that are in scope.")
  (files nil :documentation "Absolute single files that are in scope.")
  (commands nil :documentation "Shell commands that print in-scope files."))

(defconst monorepo--entry-regexp
  "\\`[ \t]*\\([a-z]+\\):[ \t]*\\(.*?\\)[ \t]*\\'"
  "Regexp matching a \"KEY: VALUE\" line of the marker file.")

(defun monorepo--read-project (root)
  "Return a `monorepo-project' for ROOT, parsed from its marker file."
  (let (name dirs files commands)
    (with-temp-buffer
      (insert-file-contents (expand-file-name monorepo-file-name root))
      (dolist (line (split-string (buffer-string) "\n"))
        (when (string-match monorepo--entry-regexp line)
          (let ((key (match-string 1 line))
                (value (match-string 2 line)))
            (unless (string-empty-p value)
              (pcase key
                ("name" (setq name value))
                ("dir"
                 (let ((dir (file-name-as-directory (expand-file-name value root))))
                   (when (file-directory-p dir) (push dir dirs))))
                ("file"
                 (let ((file (expand-file-name value root)))
                   (when (file-regular-p file) (push file files))))
                ("command" (push value commands))))))))
    (make-monorepo-project :root root :name name
                           :dirs (nreverse dirs) :files (nreverse files)
                           :commands (nreverse commands))))

(defun monorepo--run-command (command root)
  "Run shell COMMAND in ROOT and return (STATUS . FILES).
STATUS is the exit status.  FILES are the absolute paths of the output
lines, which are relative to ROOT; lines that are not existing regular
files are dropped."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (process-file-shell-command command nil '(t nil))))
        (cons status
              (cl-loop for line in (split-string (buffer-string) "\n" t "[ \t\r]+")
                       for file = (expand-file-name line root)
                       when (file-regular-p file) collect file))))))

(defun monorepo--command-files (command root)
  "Return the absolute files printed by shell COMMAND run in ROOT.
If COMMAND fails, report it and return nil."
  (pcase-let ((`(,status . ,files) (monorepo--run-command command root)))
    (if (eql status 0)
        files
      (message "monorepo: `%s' failed with status %s" command status)
      nil)))

;;;###autoload
(defun monorepo-project-try (dir)
  "Return the monorepo project containing DIR, or nil."
  (when-let* ((root (locate-dominating-file dir monorepo-file-name)))
    (monorepo--read-project (file-name-as-directory (expand-file-name root)))))

;;;###autoload
(defun monorepo-buffer-p ()
  "Return non-nil if the current buffer is inside a monorepo project."
  (and (locate-dominating-file default-directory monorepo-file-name) t))

(cl-defmethod project-root ((project monorepo-project))
  (monorepo-project-root project))

(cl-defmethod project-name ((project monorepo-project))
  (or (monorepo-project-name project) (cl-call-next-method)))

(cl-defmethod project-files ((project monorepo-project) &optional dirs)
  (let ((root (monorepo-project-root project)))
    (if (and dirs (not (and (null (cdr dirs)) (file-equal-p (car dirs) root))))
        (cl-call-next-method)
      (let ((relative project-files-relative-names)
            (files (let ((project-files-relative-names nil))
                     (delete-dups
                      (nconc (mapcan (lambda (dir)
                                       (project--files-in-directory
                                        dir (project--dir-ignores project dir)))
                                     (monorepo-project-dirs project))
                             (copy-sequence (monorepo-project-files project))
                             (mapcan (lambda (command)
                                       (monorepo--command-files command root))
                                     (monorepo-project-commands project)))))))
        (if relative
            (mapcar (lambda (file) (file-relative-name file root)) files)
          files)))))

;;; Editing the marker file

(defun monorepo--find-root (&optional create)
  "Return the monorepo root enclosing `default-directory'.
If there is none, signal an error, or prompt for a new root when
CREATE is non-nil."
  (file-name-as-directory
   (expand-file-name
    (or (locate-dominating-file default-directory monorepo-file-name)
        (if create
            (read-directory-name (format "Create %s in: " monorepo-file-name)
                                 (when-let* ((project (project-current)))
                                   (project-root project))
                                 nil t)
          (user-error "No %s file found" monorepo-file-name))))))

(defun monorepo--relative-dir (dir root)
  "Return DIR relative to ROOT, without a trailing slash."
  (directory-file-name (file-relative-name (expand-file-name dir root) root)))

(defun monorepo--dir-entry (line root)
  "Return the directory of LINE relative to ROOT if it is a \"dir\" entry."
  (when (and (string-match monorepo--entry-regexp line)
             (equal (match-string 1 line) "dir")
             (not (string-empty-p (match-string 2 line))))
    (monorepo--relative-dir (match-string 2 line) root)))

(defun monorepo--dir-entries (root)
  "Return the directories listed in ROOT's marker file, relative to ROOT."
  (let ((file (expand-file-name monorepo-file-name root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (delq nil (mapcar (lambda (line) (monorepo--dir-entry line root))
                          (split-string (buffer-string) "\n")))))))

(defun monorepo--edit-marker (root function)
  "Call FUNCTION in a buffer holding ROOT's marker file, then save it."
  (let* ((file (expand-file-name monorepo-file-name root))
         (buffer (get-file-buffer file)))
    (when (and buffer (buffer-modified-p buffer))
      (user-error "Save %s before editing it" file))
    (with-temp-buffer
      (when (file-exists-p file)
        (insert-file-contents file))
      (funcall function)
      (write-region nil nil file nil 'silent))
    (when buffer
      (with-current-buffer buffer
        (revert-buffer t t t)))))

;;;###autoload
(defun monorepo-add-directory (dir &optional root)
  "Add DIR to the scope of the monorepo project at ROOT.
ROOT defaults to the monorepo enclosing `default-directory'.
Interactively, offer to create a marker file if there is none."
  (interactive
   (let ((root (monorepo--find-root t)))
     (list (read-directory-name "Add directory: " root nil t) root)))
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (monorepo--find-root)))))
         (dir (expand-file-name dir root))
         (relative (monorepo--relative-dir dir root)))
    (unless (file-in-directory-p dir root)
      (user-error "%s is not inside %s" dir root))
    (unless (file-directory-p dir)
      (user-error "%s is not a directory" dir))
    (if (member relative (monorepo--dir-entries root))
        (message "%s is already in the monorepo" relative)
      (monorepo--edit-marker
       root
       (lambda ()
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert "dir: " relative "\n")))
      (message "Added %s to the monorepo" relative))))

;;;###autoload
(defun monorepo-remove-directory (dir &optional root)
  "Remove DIR from the scope of the monorepo project at ROOT.
DIR is relative to ROOT, which defaults to the monorepo enclosing
`default-directory'."
  (interactive
   (let* ((root (monorepo--find-root))
          (entries (or (monorepo--dir-entries root)
                       (user-error "No directories in %s" monorepo-file-name)))
          (current (cl-find-if (lambda (entry)
                                 (file-in-directory-p
                                  default-directory (expand-file-name entry root)))
                               entries)))
     (list (completing-read (format-prompt "Remove directory" current)
                            entries nil t nil nil current)
           root)))
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (monorepo--find-root)))))
         (relative (monorepo--relative-dir dir root)))
    (unless (member relative (monorepo--dir-entries root))
      (user-error "%s is not in the monorepo" relative))
    (monorepo--edit-marker
     root
     (lambda ()
       (goto-char (point-min))
       (while (not (eobp))
         (if (equal (monorepo--dir-entry
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     root)
                    relative)
             (delete-region (line-beginning-position)
                            (min (point-max) (1+ (line-end-position))))
           (forward-line 1)))))
    (message "Removed %s from the monorepo" relative)))

(defun monorepo--command-entries (root)
  "Return the commands listed in ROOT's marker file."
  (let ((file (expand-file-name monorepo-file-name root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (cl-loop for line in (split-string (buffer-string) "\n")
                 when (and (string-match monorepo--entry-regexp line)
                           (equal (match-string 1 line) "command")
                           (not (string-empty-p (match-string 2 line))))
                 collect (match-string 2 line))))))

(defun monorepo--insert-command-preview (command root status files seconds)
  "Insert a preview of COMMAND run in ROOT into the current buffer.
STATUS is its exit status, FILES the absolute files it printed and
SECONDS the time it took to run."
  (insert (format "Command: %s\nRoot:    %s\n" command root))
  (unless (eql status 0)
    (insert (format "Status:  %s (failed)\n" status)))
  (insert (format "Files:   %d\nTime:    %.3fs\n\n" (length files) seconds))
  (dolist (file (take monorepo-command-preview-lines files))
    (insert "  " (file-relative-name file root) "\n"))
  (when (> (length files) monorepo-command-preview-lines)
    (insert (format "  ... and %d more\n"
                    (- (length files) monorepo-command-preview-lines)))))

;;;###autoload
(defun monorepo-add-command (command &optional root)
  "Preview shell COMMAND and offer to add it to the monorepo at ROOT.
COMMAND runs in ROOT and should print file paths relative to it, one
per line.  The preview shows some of the files, how many there are
and how long the command took.  ROOT defaults to the monorepo
enclosing `default-directory'.  Interactively, offer to create a
marker file if there is none."
  (interactive
   (let ((root (monorepo--find-root t)))
     (list (read-shell-command "Add command: ") root)))
  (let ((root (file-name-as-directory
               (expand-file-name (or root (monorepo--find-root)))))
        (command (string-trim command)))
    (when (string-empty-p command)
      (user-error "Empty command"))
    (when (member command (monorepo--command-entries root))
      (user-error "`%s' is already in the monorepo" command))
    (pcase-let* ((start (current-time))
                 (`(,status . ,files) (monorepo--run-command command root))
                 (seconds (float-time (time-since start)))
                 (buffer (get-buffer-create "*monorepo command preview*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (monorepo--insert-command-preview command root status files seconds)
          (goto-char (point-min)))
        (special-mode))
      (let ((window (display-buffer buffer)))
        (unwind-protect
            (if (not (y-or-n-p (format "Add `%s' to %s? " command monorepo-file-name)))
                (message "Did not add `%s'" command)
              (monorepo--edit-marker
               root
               (lambda ()
                 (goto-char (point-max))
                 (unless (bolp) (insert "\n"))
                 (insert "command: " command "\n")))
              (message "Added `%s' to the monorepo" command))
          (when (window-live-p window)
            (quit-window t window)))))))

;;;###autoload
(defun monorepo-find-file (&optional root)
  "Visit the marker file of the monorepo project at ROOT.

ROOT defaults to the monorepo enclosing `default-directory'.
Interactively, offer to create a marker file if there is none."
  (interactive (list (monorepo--find-root t)))
  (find-file (expand-file-name monorepo-file-name
                               (or root (monorepo--find-root)))))

(provide 'monorepo)
;;; monorepo.el ends here
