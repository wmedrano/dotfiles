;;; wm-process --- Process utilities.
;;; Commentary:
;;; Code:

(require 'ansi-color)

(defun make-file-process (&rest args)
  "Like `make-process` but handles tramp connections as well.

ARGS - Arguments to `make-process` call."
  (if-let ((fnh (find-file-name-handler default-directory 'make-process)))
      (apply fnh #'make-process args)
    (apply #'make-process args)))


(defun process-append-colorized-text (proc string)
  "Add STRING to buffer for PROC."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (let ((begin (point)))
      (insert string)
      (ansi-color-apply-on-region begin (point)))))


(provide 'wm-process)
;;; wm-process.el ends here
