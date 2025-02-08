;;; package --- Emacs configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wm-process)

(defun podman-ps-sentinel (proc event)
    (message "Event was %s" event))

(defun podman-ps--exec ()
  "Run podman ps command."
  (json-parse-string
   (shell-command-to-string "podman ps --all --format json")
   :object-type 'alist :array-type 'list))

(let* ((data (podman-ps--exec))
       (data (mapcar (lambda (proc) (assoc 'Command proc)) data)))
  (message "%s" (type-of (nth 0 data))))

(provide 'podman)
;;; podman.el ends here
