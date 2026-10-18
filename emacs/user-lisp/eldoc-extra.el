;;; eldoc-extra.el --- Extra helpers for ElDoc -*- lexical-binding: t; -*-

;;; Commentary:

;; Toggle the ElDoc documentation buffer in the current frame.

;;; Code:

(require 'eldoc)

;;;###autoload
(defun eldoc-extra-toggle-buffer ()
  "Show or hide the ElDoc documentation buffer in the current frame.
If no documentation buffer exists yet, report the same error as
`eldoc-doc-buffer'."
  (interactive)
  (let ((buffer (condition-case nil
                    (eldoc-doc-buffer)
                  (user-error nil))))
    (if (and buffer (get-buffer-window buffer))
        (quit-windows-on buffer)
      (call-interactively #'eldoc-doc-buffer))))

(provide 'eldoc-extra)
;;; eldoc-extra.el ends here
