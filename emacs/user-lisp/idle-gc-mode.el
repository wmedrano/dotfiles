;;; idle-gc-mode.el --- Garbage collect when Emacs is idle -*- lexical-binding: t; -*-

;;; Commentary:

;; Global minor mode that runs the garbage collector once Emacs has been
;; idle for `idle-gc-mode-delay' seconds.

;;; Code:

(defgroup idle-gc-mode nil
  "Garbage collect when Emacs is idle."
  :group 'alloc)

(defcustom idle-gc-mode-delay 4
  "Seconds of idle time before `idle-gc-mode' runs the garbage collector."
  :type 'number
  :group 'idle-gc-mode)

(defvar idle-gc-mode--timer nil
  "Idle timer used by `idle-gc-mode'.")

;;;###autoload
(define-minor-mode idle-gc-mode
  "Run the garbage collector when Emacs has been idle for a while."
  :global t
  (when (timerp idle-gc-mode--timer)
    (cancel-timer idle-gc-mode--timer))
  (setq idle-gc-mode--timer
        (when idle-gc-mode
          (run-with-idle-timer idle-gc-mode-delay t #'garbage-collect))))

(provide 'idle-gc-mode)
;;; idle-gc-mode.el ends here
