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
(require 'markdown-ts-mode)
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

(defcustom jj-describe-buffer-name "*jj-describe*"
  "Name of the buffer for editing a revision description."
  :type 'string
  :group 'jj)

(defcustom jj-process-buffer-name "*jj-process*"
  "Name of the buffer showing output of asynchronous `jj' commands."
  :type 'string
  :group 'jj)

(defcustom jj-git-push-remote nil
  "Remote that `jj git push' pushes to, or nil to use jj's default."
  :type '(choice (const :tag "jj default" nil) string)
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

DIRECTORY defaults to `default-directory'.  Return nil when not inside a
Jujutsu repository or when the `jj' executable is unavailable."
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

(defvar jj--last-output ""
  "Output of the most recent `jj--run' invocation.")

(defun jj--run (&rest args)
  "Run `jj' with ARGS and return whether it succeeded.

Show the command output as a message and refresh the log on success.
The output is also saved in `jj--last-output'."
  (let ((root (or (jj--root)
                  (user-error "Not inside a Jujutsu repository"))))
    (with-temp-buffer
      (let* ((status (apply #'jj--call root (list (current-buffer) t) args))
             (output (string-trim
                      (ansi-color-filter-apply (buffer-string)))))
        (setq jj--last-output output)
        (unless (string-empty-p output)
          (message "%s" output))
        (when (eql 0 status)
          (jj-log)
          t)))))

(defun jj--run-on-change-id (change-id &rest args)
  "Run `jj' with ARGS followed by CHANGE-ID and return whether it succeeded.

See `jj--run'."
  (apply #'jj--run (append args (list change-id))))

(defun jj--run-async (args callback)
  "Run `jj' with ARGS asynchronously in the repository root.

Output goes to `jj-process-buffer-name'.  When the process exits, call
CALLBACK with its exit status and its trimmed output, with the process
buffer current and `default-directory' set to the repository root."
  (let* ((root (file-name-as-directory
                (or (jj--root)
                    (user-error "Not inside a Jujutsu repository"))))
         (buffer (get-buffer-create jj-process-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local default-directory root))
    (let ((default-directory root))
      (make-process
       :name "jj"
       :buffer buffer
       :command (append (list jj-executable) jj-arguments args)
       :connection-type 'pipe
       :file-handler t
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (ansi-color-apply-on-region (point-min) (point-max)))
             (funcall callback
                      (process-exit-status process)
                      (string-trim (buffer-substring-no-properties
                                    (point-min) (point-max)))))))))))

(defun jj--run-on-revision (&rest args)
  "Run `jj' with ARGS followed by the change id of the revision at point."
  (let ((revision (jj--revision-at-point)))
    (unless revision
      (user-error "No revision at point"))
    (apply #'jj--run-on-change-id (jj--revision-change-id revision) args)))

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

(defun jj--bookmark-names ()
  "Return the names of the existing bookmarks in the repository."
  (let ((root (or (jj--root)
                  (user-error "Not inside a Jujutsu repository"))))
    (with-temp-buffer
      (when (eql 0 (jj--call root (list t nil)
                             "bookmark" "list" "--color=never"
                             "-T" "name ++ \"\\n\""))
        (delete-dups (split-string (buffer-string) "\n" t))))))

;;;###autoload
(defun jj-bookmark (&optional arg)
  "Set a bookmark on the revision at point with `jj bookmark set'.

Complete over the existing bookmarks, or enter a new name to create one.
If jj refuses to move the bookmark backwards or sideways, offer to retry
with `--allow-backwards'.  Once the bookmark is set, preview `jj git push'
of it and offer to push.  With prefix ARG, ask for the remote."
  (interactive "P")
  (unless (jj--revision-at-point)
    (user-error "No revision at point"))
  (let ((name (completing-read "Bookmark: " (jj--bookmark-names))))
    (when (string-empty-p name)
      (user-error "No bookmark name given"))
    (when (or (jj--run-on-revision "bookmark" "set" name "-r")
              (and (string-match-p "--allow-backwards" jj--last-output)
                   (y-or-n-p (format "Could not move `%s'.  Retry with --allow-backwards? "
                                     name))
                   (jj--run-on-revision "bookmark" "set" name
                                        "--allow-backwards" "-r")))
      ;; The bookmark is set; a failed or empty push preview is not an error.
      (condition-case err
          (jj--git-push (list "-b" name) (jj--git-push-read-remote arg))
        (user-error (message "%s" (error-message-string err)))))))

(defun jj--git-remote-names ()
  "Return the names of the Git remotes of the repository."
  (let ((root (or (jj--root)
                  (user-error "Not inside a Jujutsu repository"))))
    (with-temp-buffer
      (when (eql 0 (jj--call root (list t nil)
                             "git" "remote" "list" "--color=never"))
        (mapcar (lambda (line) (car (split-string line)))
                (split-string (buffer-string) "\n" t))))))

(defun jj--git-push-read-remote (prompt-p)
  "Return the remote to push to.

If PROMPT-P is non-nil, ask for one; otherwise use `jj-git-push-remote'."
  (if prompt-p
      (completing-read "Remote: " (jj--git-remote-names) nil t)
    jj-git-push-remote))

(defun jj--git-push-preview (args)
  "Return the summary of `jj ARGS --dry-run'.

Signal a `user-error' with jj's output if the dry run fails or there is
nothing to push."
  (let ((root (or (jj--root)
                  (user-error "Not inside a Jujutsu repository"))))
    (with-temp-buffer
      (let* ((status (apply #'jj--call root (list t t)
                            (append args '("--dry-run"))))
             (output (string-trim
                      (ansi-color-filter-apply (buffer-string)))))
        (when (or (not (eql 0 status))
                  (string-match-p "^Nothing changed\\." output))
          (user-error "%s" output))
        (string-trim
         (replace-regexp-in-string "^Dry-run requested, not pushing\\.$"
                                   "" output))))))

(defun jj--git-push (selector-args remote)
  "Run `jj git push SELECTOR-ARGS' to REMOTE after confirmation.

REMOTE nil means jj's default.  Show a `--dry-run' preview and ask
before pushing.  The push runs asynchronously."
  (let* ((remote-args (when remote (list "--remote" remote)))
         (args (append '("git" "push") selector-args remote-args))
         (preview (jj--git-push-preview args)))
    (when (y-or-n-p (format "%s\nPush? " preview))
      (message "Pushing...")
      (jj--run-async args
                     (lambda (status output)
                       (jj--git-push-finish selector-args remote
                                            status output))))))

(defun jj--git-push-finish (selector-args remote status output)
  "Handle the exit STATUS and OUTPUT of a push of SELECTOR-ARGS to REMOTE.

On success refresh the log.  If jj's safety checks ask for a fetch,
offer to fetch and retry the push."
  (cond
   ((eql 0 status)
    (message "%s" output)
    (jj--refresh-log default-directory))
   ((and (string-match-p "jj git fetch" output)
         (y-or-n-p (format "%s\nFetch and retry? " output)))
    (message "Fetching...")
    (jj--run-async (append '("git" "fetch")
                           (when remote (list "--remote" remote)))
                   (lambda (status output)
                     (if (eql 0 status)
                         (progn
                           (jj--refresh-log default-directory)
                           (jj--git-push selector-args remote))
                       (message "%s" output)
                       (display-buffer (current-buffer))))))
   (t
    (message "%s" output)
    (display-buffer (current-buffer)))))

;;;###autoload
(defun jj-git-push (&optional arg)
  "Push with `jj git push', using jj's default set of bookmarks.

Preview the push and ask for confirmation first.  With prefix ARG, ask
for the remote."
  (interactive "P")
  (jj--git-push nil (jj--git-push-read-remote arg)))

(defvar-local jj-diff--revision nil
  "Change id of the revision rendered in the current `jj-diff-mode' buffer.")

(defvar-local jj-diff--from nil
  "Revision the current `jj-diff-mode' buffer diffs from, or nil for its parent.")

(defun jj-diff--fill (directory change-id &optional from)
  "Render the diff of CHANGE-ID into the current buffer.

DIRECTORY is the working directory for the jj repo or nil to use
`default-directory'.

Show `jj diff --git -r CHANGE-ID', or `jj diff --git --from FROM --to
CHANGE-ID' when FROM is non-nil.  Return the process exit status.  The
output is uncolored so that `diff-mode' can font-lock it."
  (let* ((root (or (jj--root directory)
                   (user-error "Not inside a Jujutsu repository")))
         (root (file-name-as-directory root))
         (inhibit-read-only t))
    (dolist (overlay (jj--revision-overlays))
      (delete-overlay overlay))
    (erase-buffer)
    ;; Set the buffer-local outside any `let' binding of `default-directory',
    ;; otherwise `setq-local' would write the dynamic binding instead.
    (setq-local default-directory root)
    (setq-local jj-diff--revision change-id)
    (setq-local jj-diff--from from)
    (setq-local header-line-format
                (when from (format "Diff from %s to %s" from change-id)))
    (let ((status (apply #'jj--call root (list (current-buffer) t)
                         "diff" "--git"
                         ;; jj rejects `-r' combined with `--from'.
                         (append (if from
                                     (list "--from" from "--to" change-id)
                                   (list "-r" change-id))
                                 ;; `--color=never' overrides any `--color' in
                                 ;; `jj-arguments'; `diff-mode' supplies the colors.
                                 '("--color=never")))))
      (when (eql 0 status)
        (overlay-put (make-overlay (point-min) (point-max))
                     'jj--revision
                     (jj--make-revision :change-id change-id)))
      (font-lock-ensure)
      (goto-char (point-min))
      status)))

(defun jj-diff--refresh ()
  "Re-render the diff buffer for `jj-diff--revision'."
  (interactive)
  (jj-diff--fill nil jj-diff--revision jj-diff--from))

(defvar jj-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'jj-diff--refresh)
    (define-key map (kbd "s") #'jj-diff-squash)
    map)
  "Keymap for `jj-diff-mode'.")

;;;###autoload
(define-derived-mode jj-diff-mode diff-mode "jj-diff"
  "Major mode for displaying `jj diff --git' output.

\\{jj-diff-mode-map}"
  (setq-local buffer-read-only t)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm) (jj-diff--refresh))))

(defun jj-diff--show (change-id &optional from)
  "Show the diff of CHANGE-ID, optionally from FROM, in the diff buffer.

See `jj-diff--fill'."
  (let ((dir    default-directory)
        (buffer (get-buffer-create jj-diff-buffer-name)))
    (with-current-buffer buffer
      (jj-diff-mode)
      (let ((status (jj-diff--fill dir change-id from)))
        (unless (eql 0 status)
          (message "jj diff exited with status %s" status))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun jj-diff ()
  "Show `jj diff --git -r REVISION' for the revision at point."
  (interactive)
  (let ((revision (or (jj--revision-at-point)
                      (user-error "No revision at point"))))
    (jj-diff--show (jj--revision-change-id revision))))

(defun jj--read-revision (prompt)
  "Read a revision with PROMPT, completing over bookmarks and common revsets.

Any revset may be entered.  The default is \"@-\"."
  (let ((revision (completing-read
                   (format-prompt prompt "@-")
                   (append '("@" "@-" "trunk()") (jj--bookmark-names))
                   nil nil nil nil "@-")))
    (when (string-empty-p revision)
      (user-error "No revision given"))
    revision))

;;;###autoload
(defun jj-diff-from (from)
  "Show `jj diff --git --from FROM --to REVISION' for the revision at point.

Interactively, read FROM with completion over bookmarks and common
revsets."
  (interactive
   (progn
     (unless (jj--revision-at-point)
       (user-error "No revision at point"))
     (list (jj--read-revision "Diff from"))))
  (let ((revision (or (jj--revision-at-point)
                      (user-error "No revision at point"))))
    (jj-diff--show (jj--revision-change-id revision) from)))

(defun jj-diff-squash ()
  "Squash the revision shown in this `jj-diff-mode' buffer into its parent.

Ask for confirmation first.  On success, kill the diff buffer."
  (interactive)
  (unless jj-diff--revision
    (user-error "No revision in this buffer"))
  (when jj-diff--from
    (user-error "Squash is only available for single-revision diffs"))
  (when (y-or-n-p (format "Squash %s into its parent? " jj-diff--revision))
    (let ((buffer (current-buffer)))
      ;; Keep the parent's description so jj does not open an editor.
      (when (jj--run-on-change-id jj-diff--revision
                                  "squash" "--use-destination-message" "-r")
        (kill-buffer buffer)))))

(defun jj--output (&rest args)
  "Return the uncolored standard output of `jj ARGS' in the repository root.

Signal a `user-error' if the command fails."
  (let ((root (or (jj--root)
                  (user-error "Not inside a Jujutsu repository"))))
    (with-temp-buffer
      (let ((status (apply #'jj--call root (list t nil)
                           (append args '("--color=never")))))
        (unless (eql 0 status)
          (user-error "jj %s exited with status %s"
                      (string-join args " ") status))
        (buffer-string)))))

(defun jj--description (change-id)
  "Return the full description of CHANGE-ID."
  (jj--output "log" "-r" change-id "--no-graph" "-T" "description"))

(defun jj--diff-stat (change-id)
  "Return the lines of `jj diff --stat' for CHANGE-ID."
  (split-string (jj--output "diff" "--stat" "-r" change-id) "\n" t))

(defvar-local jj-describe--revision nil
  "Change id of the revision described in the current `jj-describe-mode' buffer.")

(defvar jj-describe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'jj-describe-commit)
    (define-key map (kbd "C-c C-k") #'jj-describe-abort)
    map)
  "Keymap for `jj-describe-mode'.")

;;;###autoload
(define-derived-mode jj-describe-mode markdown-ts-mode "jj-describe"
  "Major mode for editing a revision description.
Lines starting with \"JJ:\" are removed when committing.

\\{jj-describe-mode-map}"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<jj-describe-mode-map>\\[jj-describe-commit]: commit, \\[jj-describe-abort]: abort")))

(defun jj-describe--insert-trailer (change-id)
  "Insert the \"JJ:\" comment trailer for CHANGE-ID at point."
  (let ((start (point)))
    (insert "JJ: Change ID: " change-id "\n"
            "JJ: This commit contains the following changes:\n")
    (dolist (line (jj--diff-stat change-id))
      (insert "JJ:     " line "\n"))
    (insert "JJ:\n"
            "JJ: Lines starting with \"JJ:\" (like this one) will be removed.\n")
    ;; An overlay, unlike a `face' property, survives tree-sitter font-lock.
    (overlay-put (make-overlay start (point)) 'face 'font-lock-comment-face)))

;;;###autoload
(defun jj-describe ()
  "Edit the description of the revision at point.
The description opens in a `jj-describe-mode' buffer; commit it with
\\<jj-describe-mode-map>\\[jj-describe-commit] or abort with \\[jj-describe-abort]."
  (interactive)
  (let* ((revision (or (jj--revision-at-point)
                       (user-error "No revision at point")))
         (change-id (jj--revision-change-id revision))
         (root (file-name-as-directory (jj--root)))
         (description (string-trim-right (jj--description change-id)))
         (buffer (get-buffer-create jj-describe-buffer-name)))
    (with-current-buffer buffer
      (jj-describe-mode)
      (erase-buffer)
      (setq-local default-directory root)
      (setq-local jj-describe--revision change-id)
      (insert description "\n\n")
      (jj-describe--insert-trailer change-id)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun jj-describe--message ()
  "Return the description in the current buffer, without \"JJ:\" lines."
  (let ((message (string-trim
                  (replace-regexp-in-string
                   "^JJ:.*\n?" ""
                   (buffer-substring-no-properties (point-min) (point-max))))))
    (if (string-empty-p message) "" (concat message "\n"))))

(defun jj-describe-commit ()
  "Set the description of the revision to the contents of this buffer.

On success, kill the buffer."
  (interactive)
  (unless jj-describe--revision
    (user-error "No revision in this buffer"))
  (let ((buffer (current-buffer)))
    (when (jj--run-on-change-id jj-describe--revision
                                "describe" "-m" (jj-describe--message) "-r")
      (kill-buffer buffer))))

(defun jj-describe-abort ()
  "Kill this description buffer without changing the revision."
  (interactive)
  (quit-window t))

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

(defvar jj-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'jj-log)
    (define-key map (kbd "e") #'jj-edit)
    (define-key map (kbd "b") #'jj-bookmark)
    (define-key map (kbd "d") #'jj-diff)
    (define-key map (kbd "D") #'jj-diff-from)
    (define-key map (kbd "m") #'jj-describe)
    (define-key map (kbd "N") #'jj-new)
    (define-key map (kbd "n") #'jj-log-next-revision)
    (define-key map (kbd "p") #'jj-log-prev-revision)
    (define-key map (kbd "P") #'jj-git-push)
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
    (jj-log--fill root buffer)
    (pop-to-buffer buffer)))

(defun jj-log--capture-position ()
  "Return the current position in this `jj-log-mode' buffer as a plist.

The plist holds the change id of the revision at point, the numeric
position of point, and the window start of each window showing the
current buffer.  See `jj-log--restore-position'."
  (list :change-id (when-let* ((overlay (jj--revision-overlay-at-point)))
                     (jj--revision-change-id
                      (overlay-get overlay 'jj--revision)))
        :point (point)
        :windows (mapcar (lambda (window)
                           (cons window (window-start window)))
                         (get-buffer-window-list nil nil 0))))

(defun jj-log--restore-position (position)
  "Restore the position in this `jj-log-mode' buffer from POSITION.

POSITION is a plist captured by `jj-log--capture-position' before the
buffer was re-rendered.  Point goes to the revision with the same change
id as before, falling back to the previous numeric position when that
revision is gone, and to `point-min' for a fresh buffer.  Windows that
showed the buffer keep their point and, when the revision is gone, their
start."
  (let ((overlay
         (when-let* ((change-id (plist-get position :change-id)))
           (cl-find-if (lambda (overlay)
                         (string= change-id
                                  (jj--revision-change-id
                                   (overlay-get overlay 'jj--revision))))
                       (jj--revision-overlays)))))
    (cond
     (overlay (goto-char (overlay-start overlay)))
     ((plist-get position :point)
      (goto-char (min (plist-get position :point) (point-max))))
     (t (goto-char (point-min))))
    (dolist (pair (plist-get position :windows))
      (when-let* ((window (car pair)))
        (when (window-live-p window)
          (set-window-point window (point))
          (if overlay
              ;; Keep the saved scroll unless the revision left the view.
              (unless (and (>= (point) (window-start window))
                           (< (point) (window-end window t)))
                (with-selected-window window (recenter)))
            (when (and (>= (cdr pair) (point-min))
                       (<= (cdr pair) (point-max)))
              (set-window-start window (cdr pair)))))))))

(defun jj-log--fill (root buffer)
  "Render `jj log' for the repository at ROOT into BUFFER.

When BUFFER already showed revisions, point is restored to the same
revision; see `jj-log--capture-position'."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (position (jj-log--capture-position)))
      (erase-buffer)
      ;; `erase-buffer' leaves the old revision overlays behind as empty
      ;; overlays at the start of the buffer; get rid of them so they do not
      ;; pile up across refreshes.
      (dolist (overlay (jj--revision-overlays))
        (delete-overlay overlay))
      (jj-log-mode)
      (setq-local default-directory root)
      (let* ((coding-system-for-read 'utf-8)
             (status (jj--call root (list buffer nil)
                               "log" "-T" (jj--log-template))))
        (ansi-color-apply-on-region (point-min) (point-max))
        (if (eql 0 status)
            (progn
              (jj--log-finalize)
              (jj-log--restore-position position))
          (message "jj log exited with status %s" status)
          (goto-char (point-min)))))))

(defun jj--refresh-log (root)
  "Refresh the log buffer without displaying it, if it shows ROOT."
  (let ((buffer (get-buffer jj-log-buffer-name)))
    (when (and buffer
               (equal (file-name-as-directory root)
                      (file-name-as-directory
                       (buffer-local-value 'default-directory buffer))))
      (jj-log--fill root buffer))))

;;;###autoload
(defalias 'jj #'jj-log
  "Show the output of `jj log'.  Alias for `jj-log'.")

(provide 'jj)
;;; jj.el ends here
