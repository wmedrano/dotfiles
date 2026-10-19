;;; file-extra.el --- Extra file utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive commands for working with file paths.
;;
;; Provides:
;; - `copy-filename': Copies the file name of the current buffer to the
;;   clipboard.  If `project-current' is valid, the path is relative to the
;;   project root; otherwise it is the full path.
;; - `copy-filename-absolute': Copies the absolute file path of the current
;;   buffer to the clipboard.

;;; Code:

(require 'project)

(defun file-extra--filename (&optional buffer)
  "Return the file name visited by BUFFER, or signal a `user-error'.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or buffer-file-name
        (buffer-file-name (buffer-base-buffer))
        (user-error "Current buffer is not visiting a file"))))

(defun file-extra--relative-or-absolute-filename (filename)
  "Return FILENAME relative to `project-current' root, or full path if none."
  (let* ((expanded (expand-file-name filename))
         (proj (or (project-current)
                   (project-current nil (file-name-directory (file-truename expanded))))))
    (if-let* ((proj)
              (root (expand-file-name (project-root proj)))
              ((file-in-directory-p expanded root)))
        (let ((rel (file-relative-name expanded root)))
          (if (string-prefix-p ".." rel)
              (file-relative-name (file-truename expanded) (file-truename root))
            rel))
      expanded)))

;;;###autoload
(defun copy-filename (&optional buffer)
  "Copy the file name of BUFFER to the clipboard.
If `project-current' is valid, the path is relative to the project root;
otherwise it is the full path.  BUFFER defaults to the current buffer.
Return the copied path."
  (interactive)
  (let* ((filename (file-extra--filename buffer))
         (path (with-current-buffer (or buffer (current-buffer))
                 (file-extra--relative-or-absolute-filename filename))))
    (kill-new path)
    (message "Copied %s" path)
    path))

;;;###autoload
(defun copy-filename-absolute (&optional buffer)
  "Copy the absolute file name of BUFFER to the clipboard.
BUFFER defaults to the current buffer.  Return the copied path."
  (interactive)
  (let* ((filename (file-extra--filename buffer))
         (path (with-current-buffer (or buffer (current-buffer))
                 (expand-file-name filename))))
    (kill-new path)
    (message "Copied %s" path)
    path))

(provide 'file-extra)
;;; file-extra.el ends here
