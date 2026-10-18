;;; eglot-extras.el --- Extra helpers for Eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that formats the buffer with the language server before
;; saving, using `eglot-format-buffer'.

;;; Code:

(require 'eglot)

(defun eglot-format-on-save-mode--format ()
  "Format the current buffer if it is managed by Eglot."
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (eglot-format-buffer)))

;;;###autoload
(define-minor-mode eglot-format-on-save-mode
  "Format the buffer with Eglot before saving."
  :lighter nil
  (if eglot-format-on-save-mode
      (add-hook 'before-save-hook #'eglot-format-on-save-mode--format nil t)
    (remove-hook 'before-save-hook #'eglot-format-on-save-mode--format t)))

(provide 'eglot-extras)
;;; eglot-extras.el ends here
