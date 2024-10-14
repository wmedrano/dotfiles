;;; jekyll --- Jekyll interface for Emacs. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ansi-color)

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

(defun jekyll-append-colorized-text (string)
  "Insert STRING in the buffer and colorize based on ansi escape sequences."
  (goto-char (point-max))
  (let ((begin (point)))
    (insert string)
    (ansi-color-apply-on-region begin (point))))

(defun jekyll-reset-buffer ()
  "Reset the jekyll buffer."
  (setq jekyll-serve-url-opened nil)
  (when-let* ((buffer (get-buffer jekyll-proc-buffer-name)))
    (with-current-buffer buffer
      (when-let* ((proc (get-buffer-process buffer)))
        (kill-process proc))
      (erase-buffer))))

(defun jekyll-make-process (&rest args)
  "Like `make-process` but handles tramp connections as well.

ARGS - Arguments to `make-process` call."
  (if-let ((fnh (find-file-name-handler default-directory 'make-process)))
      (apply fnh #'make-process args)
    (apply #'make-process args)))

(defun jekyll-serve-filter (proc string)
  "Filter for `jekyll-serve`.

PROC - The process for jekyll serve.
STRING - The output from jekyll serve."
  (with-current-buffer (process-buffer proc)
    (jekyll-append-colorized-text string)
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
  (jekyll-make-process :name "jekyll serve"
                       :buffer jekyll-proc-buffer-name
                       :command `("bundle" "exec" "jekyll" "serve" "--livereload")
                       :filter #'jekyll-serve-filter
                       :sentinel #'jekyll-sentinel-display-buffer-on-error))

(defun jekyll-build-filter (proc string)
  "Filter for `jekyll-build`.

PROC - The process for jekyll build.
STRING - The output from jekyll build."
  (with-current-buffer (process-buffer proc)
    (jekyll-append-colorized-text string)))

(defun jekyll-build ()
  "Run jekyll build."
  (interactive)
  (jekyll-reset-buffer)
  (jekyll-make-process :name "jekyll build"
                       :buffer jekyll-proc-buffer-name
                       :command `("bundle" "exec" "jekyll" "build")
                       :filter #'jekyll-build-filter
                       :sentinel #'jekyll-sentinel-display-buffer-on-error))

(provide 'jekyll)
;;; jekyll.el ends here
