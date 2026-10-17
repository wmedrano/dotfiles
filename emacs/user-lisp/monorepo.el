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
;;
;; Paths are relative to the root.  Blank lines, lines starting with #,
;; unknown keys and paths that don't exist are ignored.  Commands such as
;; `project-find-file' only see the included directories and files.
;;
;; Add `monorepo-project-try' to `project-find-functions' to enable the
;; backend.

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

(cl-defstruct monorepo-project
  "A project rooted at a directory containing `monorepo-file-name'."
  (root nil :documentation "Absolute directory containing the marker file.")
  (name nil :documentation "Name from the marker file, or nil for the default.")
  (dirs nil :documentation "Absolute directories that are in scope.")
  (files nil :documentation "Absolute single files that are in scope."))

(defun monorepo--read-project (root)
  "Return a `monorepo-project' for ROOT, parsed from its marker file."
  (let (name dirs files)
    (with-temp-buffer
      (insert-file-contents (expand-file-name monorepo-file-name root))
      (dolist (line (split-string (buffer-string) "\n"))
        (when (string-match "\\`[ \t]*\\([a-z]+\\):[ \t]*\\(.*?\\)[ \t]*\\'" line)
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
                   (when (file-regular-p file) (push file files))))))))))
    (make-monorepo-project :root root :name name
                           :dirs (nreverse dirs) :files (nreverse files))))

;;;###autoload
(defun monorepo-project-try (dir)
  "Return the monorepo project containing DIR, or nil."
  (when-let* ((root (locate-dominating-file dir monorepo-file-name)))
    (monorepo--read-project (file-name-as-directory (expand-file-name root)))))

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
                             (copy-sequence (monorepo-project-files project)))))))
        (if relative
            (mapcar (lambda (file) (file-relative-name file root)) files)
          files)))))

(provide 'monorepo)
;;; monorepo.el ends here
