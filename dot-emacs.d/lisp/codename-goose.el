;;; codename-goose --- Codename Goose interface for Emacs.    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'wm-process)
(require 'project)

(defcustom goose-auto-start t
  "Automatically start Goose when `goose-do' is called."
  :type 'boolean
  :group 'codename-goose)
(defcustom goose-proc-name "goose-session"
  "Name of the Goose process."
  :type 'string
  :group 'codename-goose)
(defcustom goose-proc-buffer-name "*goose*"
  "Name of the buffer for the Goose process."
  :type 'string
  :group 'codename-goose)
(defcustom goose-use-project-root t
  "Whether to use the project root as the Goose directory."
  :type 'boolean
  :group 'codename-goose)

(defun goose-colorize (proc string)
  "Insert STRING to buffer for PROC."
  (process-append-colorized-text proc string))

(defun goose-start ()
  "Start the goose process session."
  (interactive)
  (let ((previous-default-directory default-directory))
    (setq-local default-directory (project-root (project-current)))
    (make-file-process
     :name goose-proc-name
     :buffer goose-proc-buffer-name
     :command '("goose" "session")
     :filter #'goose-colorize)
    (setq-local default-directory previous-default-directory))
  (message "Goose session started successfully."))

(defun goose-maybe-start ()
  "Start Goose if it's not running and return the process."
  (when (and goose-auto-start (not (get-process goose-proc-name)))
    (goose-start))
  (let ((proc (get-process goose-proc-name)))
    (if (not proc) (error "Goose session not initialized, use `goose-start' to create a new Goose session"))
    proc))

(defun goose-do (message)
  "Sends a MESSAGE to the Goose process."
  (interactive (list (read-string "Enter message for Goose: ")))
  (let ((proc (goose-maybe-start)))
    (save-excursion
      (with-current-buffer goose-proc-buffer-name
        (goto-char (point-max))
        (insert "<" (current-time-string) ">" (propertize message 'face 'menu) "\n")))
    (process-send-string proc (concat message "\n"))
    (display-buffer goose-proc-buffer-name)))


(defun goose-do-with-file (message)
  "Sends a MESSAGE to the Goose process along the current file.

Gooses should only change the current file."
  (interactive (list (read-string "Enter message for Goose: ")))
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer not associated with a file.")
      (goose-do (concat message " Consider only the file " filename)))))

(defun goose-end ()
  "Kill the Goose process and buffer."
  (interactive)
  (let ((proc (get-process goose-proc-name)))
    (when proc
      (delete-process proc)
      (kill-buffer goose-proc-buffer-name))))

(defun goose-restart ()
  "Restart the Goose process."
  (interactive)
  (goose-end)
  (goose-start)
  (display-buffer goose-proc-buffer-name))

(defun goose-open-hints ()
  "Open the goosehints file."
  (interactive)
  (let ((proc (goose-maybe-start)))
    (with-current-buffer (process-buffer proc)
      (find-file ".goosehints"))))

(provide 'codename-goose)
;;; codename-goose.el ends here
