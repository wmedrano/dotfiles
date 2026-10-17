;;; jj.el --- Jujutsu integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Minimal Jujutsu (jj) integration.  `jj-log' runs `jj log' with
;; `default-directory' set to the repository root (as reported by `jj root')
;; and shows the result in a `jj-log-mode' buffer.  Each revision in the log
;; carries a `jj--revision' overlay property with parsed metadata.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'diff-mode)
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

(defcustom jj-diff-buffer-name "*jj-diff*"
  "Name of the buffer showing `jj diff' output."
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

(defun jj--call (directory destination &rest args)
  "Run `jj' with ARGS in DIRECTORY and return the exit status.
Output goes to DESTINATION, as in `process-file'.  `jj-arguments' are
placed before ARGS, so ARGS can override them."
  (let ((default-directory (file-name-as-directory directory)))
    (apply #'process-file
           jj-executable nil destination nil
           (append jj-arguments args))))

(defun jj--root (&optional directory)
  "Return the Jujutsu repository root for DIRECTORY, or nil.
DIRECTORY defaults to `default-directory'.  Return nil when not inside
a Jujutsu repository or when the `jj' executable is unavailable."
  (with-temp-buffer
    (when (and (executable-find jj-executable)
               (eql 0 (jj--call (or directory default-directory)
                                (list t nil) "root")))
      (let ((root (string-trim (buffer-string))))
        (unless (string-empty-p root) root)))))

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

(defun jj--revision-at-point ()
  "Return the `jj--revision' struct at point, or nil."
  (get-char-property (point) 'jj--revision))

(defun jj--run-on-revision (&rest args)
  "Run `jj' with ARGS followed by the change id of the revision at point.
Show the command output as a message and refresh the log on success."
  (let ((revision (jj--revision-at-point)))
    (unless revision
      (user-error "No revision at point"))
    (let* ((change-id (jj--revision-change-id revision))
           (root (or (jj--root)
                     (user-error "Not inside a Jujutsu repository"))))
      (with-temp-buffer
        (let* ((status (apply #'jj--call root (list (current-buffer) t)
                              (append args (list change-id))))
               (output (string-trim
                        (ansi-color-filter-apply (buffer-string)))))
          (unless (string-empty-p output)
            (message "%s" output))
          (when (eql 0 status)
            (jj-log)))))))

;;;###autoload
(defun jj-edit ()
  "Edit the revision at point with `jj edit'."
  (interactive)
  (jj--run-on-revision "edit"))

;;;###autoload
(defun jj-new ()
  "Create a new revision on top of the revision at point with `jj new -r'."
  (interactive)
  (jj--run-on-revision "new" "-r"))

(defvar-local jj-diff--revision nil
  "Change id of the revision rendered in the current `jj-diff-mode' buffer.")

(defun jj-diff--fill (change-id)
  "Render `jj diff --git -r CHANGE-ID' into the current buffer.
Return the process exit status.  The output is uncolored so that
`diff-mode' can font-lock it."
  (let* ((root (or (jj--root)
                   (user-error "Not inside a Jujutsu repository")))
         (root (file-name-as-directory root))
         (inhibit-read-only t))
    (erase-buffer)
    ;; Set the buffer-local outside any `let' binding of `default-directory',
    ;; otherwise `setq-local' would write the dynamic binding instead.
    (setq-local default-directory root)
    (setq-local jj-diff--revision change-id)
    (let ((status (jj--call root (list (current-buffer) t)
                            "diff" "--git" "-r" change-id
                            ;; `--color=never' overrides any `--color' in
                            ;; `jj-arguments'; `diff-mode' supplies the colors.
                            "--color=never")))
      (font-lock-ensure)
      (goto-char (point-min))
      status)))

(defun jj-diff--refresh ()
  "Re-render the diff buffer for `jj-diff--revision'."
  (interactive)
  (jj-diff--fill jj-diff--revision))

(defvar jj-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'jj-diff--refresh)
    map)
  "Keymap for `jj-diff-mode'.")

;;;###autoload
(define-derived-mode jj-diff-mode diff-mode "jj-diff"
  "Major mode for displaying `jj diff --git' output.

\\{jj-diff-mode-map}"
  (setq-local buffer-read-only t)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm) (jj-diff--refresh))))

;;;###autoload
(defun jj-diff ()
  "Show `jj diff --git -r REVISION' for the revision at point."
  (interactive)
  (let ((revision (jj--revision-at-point)))
    (unless revision
      (user-error "No revision at point"))
    (let ((buffer (get-buffer-create jj-diff-buffer-name)))
      (with-current-buffer buffer
        (jj-diff-mode)
        (let ((status (jj-diff--fill (jj--revision-change-id revision))))
          (unless (eql 0 status)
            (message "jj diff exited with status %s" status))))
      (pop-to-buffer buffer))))

(defun jj--revision-overlays ()
  "Return the `jj--revision' overlays in the current buffer, in order."
  (sort (cl-remove-if-not (lambda (o) (overlay-get o 'jj--revision))
                          (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun jj--revision-overlay-at-point ()
  "Return the `jj--revision' overlay containing point, or nil."
  (cl-find-if (lambda (o)
                (and (<= (overlay-start o) (point))
                     (< (point) (overlay-end o))))
              (jj--revision-overlays)))

(defun jj-log-next-revision ()
  "Move to the next revision in a `jj-log-mode' buffer."
  (interactive)
  (let ((next (cl-find-if (lambda (o) (> (overlay-start o) (point)))
                          (jj--revision-overlays))))
    (unless next
      (user-error "No next revision"))
    (goto-char (overlay-start next))))

(defun jj-log-prev-revision ()
  "Move to the previous revision in a `jj-log-mode' buffer."
  (interactive)
  (let* ((current (jj--revision-overlay-at-point))
         (limit (if current (overlay-start current) (point)))
         (prev (car (last (cl-remove-if (lambda (o)
                                          (>= (overlay-start o) limit))
                                        (jj--revision-overlays))))))
    (unless prev
      (user-error "No previous revision"))
    (goto-char (overlay-start prev))))

(defvar jj-log-revision-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'jj-edit)
    (define-key map (kbd "n") #'jj-new)
    (define-key map (kbd "d") #'jj-diff)
    map)
  "Keymap of actions on the revision at point, reached via RET.")

(defvar jj-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'jj-log)
    (define-key map (kbd "e") #'jj-edit)
    (define-key map (kbd "d") #'jj-diff)
    (define-key map (kbd "RET") jj-log-revision-map)
    (define-key map (kbd "<return>") jj-log-revision-map)
    (define-key map (kbd "n") #'jj-log-next-revision)
    (define-key map (kbd "p") #'jj-log-prev-revision)
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
        (let* ((coding-system-for-read 'utf-8)
               (status (jj--call root (list buffer nil)
                                 "log" "-T" (jj--log-template))))
          (ansi-color-apply-on-region (point-min) (point-max))
          (if (eql 0 status)
              (jj--log-finalize)
            (message "jj log exited with status %s" status))
          (goto-char (point-min)))))
    (pop-to-buffer buffer)))

;;;###autoload
(defalias 'jj #'jj-log
  "Show the output of `jj log'.  Alias for `jj-log'.")

(provide 'jj)
;;; jj.el ends here
