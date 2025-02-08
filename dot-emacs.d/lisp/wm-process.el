;;; wm-process --- Process utilities. -*- lexical-binding: t; -*-
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
    (save-excursion
      (goto-char (point-max))
      (let ((begin (point)))
        (insert string)
        (ansi-color-apply-on-region begin (point)))
      (goto-char 0)
      (while (re-search-forward ".*\r" nil :noerror)
        (replace-match ""))
      (goto-char 0)
      )))

(defun process-message-event (proc event)
  "Run message function with PROC and EVENT.

Useful as a sentinel that prints the status of a terminated process."
  (message "Process %s exited with: %s" proc event))

(provide 'wm-process)
;;; wm-process.el ends here
