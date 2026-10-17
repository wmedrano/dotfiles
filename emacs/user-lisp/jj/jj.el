;;; jj.el --- Jujutsu integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Minimal Jujutsu (jj) integration.  `jj-log' runs `jj log' with
;; `default-directory' set to the repository root (as reported by `jj root')
;; and shows the result in a `jj-log-mode' buffer.  Each revision in the log
;; carries a `jj--revision' overlay property with parsed metadata.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup jj nil
  "Jujutsu (jj) integration."
  :group 'tools)

(defcustom jj-executable "jj"
  "Name of or path to the Jujutsu executable."
  :type 'string
  :group 'jj)

(defcustom jj-arguments '("--no-pager" "--color=always")
  "Common arguments passed to every `jj' invocation."
  :type '(repeat string)
  :group 'jj)

(defcustom jj-log-buffer-name "*jj-log*"
  "Name of the buffer showing `jj log' output."
  :type 'string
  :group 'jj)

(defconst jj--revision-fields
  '((:change-id              . "json(change_id)")
    (:description            . "json(description.first_line())")
    (:bookmarks              . "json(bookmarks.map(|b| b.name()))")
    (:current-working-copy-p . "json(stringify(current_working_copy))"))
  "Alist mapping revision field keywords to jj template expressions.")

(cl-defstruct (jj--revision (:constructor jj--make-revision))
  change-id
  description
  bookmarks
  current-working-copy-p)

(defun jj--root (&optional directory)
  "Return the Jujutsu repository root for DIRECTORY, or nil.
DIRECTORY defaults to `default-directory'.  Return nil when not inside
a Jujutsu repository or when the `jj' executable is unavailable."
  (let ((default-directory (file-name-as-directory
                            (or directory default-directory))))
    (with-temp-buffer
      (when (and (executable-find jj-executable)
                 (eql 0 (apply #'process-file
                               jj-executable nil (list t nil) nil
                               (append '("root") jj-arguments))))
        (let ((root (string-trim (buffer-string))))
          (unless (string-empty-p root) root))))))

(defun jj--log-template ()
  "Return the jj template used to render and annotate the log."
  (string-join (append (mapcar #'cdr jj--revision-fields)
                       '("builtin_log_compact"))
               "++"))

(defun jj--log-finalize ()
  "Parse revision metadata from the current buffer into overlays.
Each revision block gets a `jj--revision' overlay property holding a
`jj--revision' struct.  The JSON metadata is removed from the buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (save-excursion
      (while (search-forward "\"" nil t)
        (backward-char)
        (let* ((start         (line-beginning-position))
               (json-start    (point))
               (revision-args (cl-loop for (field . _template) in jj--revision-fields
                                       append (list field (json-parse-buffer))))
               (revision      (apply #'jj--make-revision revision-args)))
          (setf (jj--revision-current-working-copy-p revision)
                (string= "true" (jj--revision-current-working-copy-p revision)))
          (delete-region json-start (point))
          (forward-line 2)
          (overlay-put (make-overlay start (point))
                       'jj--revision
                       revision))))))

(defvar jj-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'jj-log)
    map)
  "Keymap for `jj-log-mode'.")

;;;###autoload
(define-derived-mode jj-log-mode special-mode "jj-log"
  "Major mode for displaying `jj log' output.

\\{jj-log-mode-map}"
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm) (jj-log))))

;;;###autoload
(defun jj-log ()
  "Show the output of `jj log' in a dedicated buffer.

The command runs in the Jujutsu repository root reported by `jj root',
so it works from any buffer inside the repository.  The result is shown
in `jj-log-buffer-name' using `jj-log-mode', with a `jj--revision'
overlay on each revision block."
  (interactive)
  (unless (executable-find jj-executable)
    (user-error "The `%s' executable was not found" jj-executable))
  (let* ((root (or (jj--root)
                   (user-error "Not inside a Jujutsu repository")))
         (buffer (get-buffer-create jj-log-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jj-log-mode)
        (setq-local default-directory root)
        (let ((coding-system-for-read 'utf-8)
              (default-directory root))
          (let ((status (apply #'process-file
                               jj-executable nil (list buffer nil) nil
                               (append (list "log" "-T" (jj--log-template))
                                       jj-arguments))))
            (ansi-color-apply-on-region (point-min) (point-max))
            (if (eql 0 status)
                (jj--log-finalize)
              (message "jj log exited with status %s" status))
            (goto-char (point-min))))))
    (pop-to-buffer buffer)))

(provide 'jj)
;;; jj.el ends here
