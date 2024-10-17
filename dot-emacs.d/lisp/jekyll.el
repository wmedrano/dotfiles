;;; jekyll --- Jekyll interface for Emacs. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ansi-color)
(require 'wm-process)

(defvar jekyll-serve-url-opened nil)
(defvar jekyll-proc-buffer-name "*jekyll*")

(defun jekyll-sentinel-display-buffer-on-error (proc signal)
  "If SIGNAL is an error, display the buffer for PROC.

PROC - The process for the jekyll command.
SIGNAL - The signal from the process."
  (when (or (string-prefix-p "exited abnormally" signal)
            (string-prefix-p "failed with code" signal))
    (display-buffer (process-buffer proc))
    (message "jekyll: %s" signal)))

(defun jekyll-reset-buffer ()
  "Reset the jekyll buffer."
  (setq jekyll-serve-url-opened nil)
  (when-let* ((buffer (get-buffer jekyll-proc-buffer-name)))
    (with-current-buffer buffer
      (when-let* ((proc (get-buffer-process buffer)))
        (kill-process proc))
      (erase-buffer))))

(defun jekyll-serve-filter (proc string)
  "Filter for `jekyll-serve`.

PROC - The process for jekyll serve.
STRING - The output from jekyll serve."
  (with-current-buffer (process-buffer proc)
    (process-append-colorized-text proc string)
    (when (not jekyll-serve-url-opened)
      (save-excursion
        (goto-char 0)
        (when (re-search-forward "Server address: " nil t)
          (setq jekyll-serve-url-opened t)
          (browse-url-at-point))))))

(defun jekyll-serve ()
  "Run jekyll serve."
  (interactive)
  (jekyll-reset-buffer)
  (make-file-process :name "jekyll serve"
                     :buffer jekyll-proc-buffer-name
                     :command `("bundle" "exec" "jekyll" "serve" "--livereload")
                     :filter #'jekyll-serve-filter
                     :sentinel #'jekyll-sentinel-display-buffer-on-error))

(defun jekyll-build ()
  "Run jekyll build."
  (interactive)
  (jekyll-reset-buffer)
  (make-file-process :name "jekyll build"
                     :buffer jekyll-proc-buffer-name
                     :command `("bundle" "exec" "jekyll" "build")
                     :filter #'process-append-colorized-text
                     :sentinel #'jekyll-sentinel-display-buffer-on-error))

(provide 'jekyll)
;;; jekyll.el ends here
