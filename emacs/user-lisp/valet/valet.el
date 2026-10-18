;;; valet.el --- A gentlemanly coding agent shell -*- lexical-binding: t; -*-

;;; Commentary:

;; A `shell-maker' based shell that always replies with a fixed message
;; after a short delay.  Whatever you type, valet waits
;; `valet-response-delay' seconds and then thanks you.

;;; Code:

(require 'map)
(require 'shell-maker)

(defgroup valet nil
  "A gentlemanly coding agent."
  :group 'tools)

(defcustom valet-response-delay 2.0
  "Seconds to wait before `valet' replies."
  :type 'number
  :group 'valet)

(defun valet--execute-command (_command shell)
  "Reply with a fixed message after `valet-response-delay', ignoring COMMAND."
  (let ((write-output (map-elt shell :write-output))
        (finish-output (map-elt shell :finish-output)))
    (run-at-time valet-response-delay nil
                 (lambda ()
                   (funcall write-output "Thanks, maybe one day I can do that")
                   (funcall finish-output t)))))

(defvar valet--config
  (make-shell-maker-config
   :name "Valet"
   :execute-command #'valet--execute-command)
  "Shell configuration for `valet'.")

(shell-maker-define-major-mode valet--config)

;;;###autoload
(defun valet ()
  "Start a Valet shell."
  (interactive)
  (shell-maker-start valet--config))

(provide 'valet)
;;; valet.el ends here
