;;; gent-tools.el --- Tools for the gent coding agent -*- lexical-binding: t; -*-

;;; Commentary:

;; A small tool registry plus the tools gent exposes to the model:
;; `read_file', `edit_file', `write_file' and `run_shell'.
;;
;; Every tool has a name, a description, a JSON schema for its
;; parameters and a handler.  Handlers are called as (HANDLER ARGS DONE)
;; with `default-directory' bound to the project root; they must call
;; DONE exactly once with the result string.  Handlers may signal an
;; error instead, which is reported back to the model as the result.
;;
;; Tools that change things declare a `:preview' function.  Before such
;; a tool runs, the caller's CONFIRM function (see `gent-tools-call') is
;; given the preview (a diff or the command) and a RESPOND callback.
;; Nothing runs until RESPOND is called, so the user can take as long as
;; they like.  Tools listed in `gent-tools-trusted' skip this step.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'gent-llm)

(defgroup gent-tools nil
  "Tools for the gent coding agent."
  :group 'gent)

(defcustom gent-tools-trusted nil
  "Names of tools that may run without asking for confirmation."
  :type '(repeat string)
  :group 'gent-tools)

(defcustom gent-tools-output-max-chars 20000
  "Maximum number of characters of tool output sent to the model."
  :type 'integer
  :group 'gent-tools)

(defcustom gent-tools-read-default-limit 2000
  "Number of lines `read_file' returns when no limit is given."
  :type 'integer
  :group 'gent-tools)

(defcustom gent-tools-shell-timeout 120
  "Default timeout, in seconds, for `run_shell' commands."
  :type 'number
  :group 'gent-tools)

(defvar-local gent-tools-session-trusted nil
  "Tool names trusted for the current session only.")

(cl-defstruct (gent-tool (:constructor gent-tool--create))
  "A tool the model can call."
  name description parameters handler preview)

(defvar gent-tools--registry nil
  "Alist of (NAME . `gent-tool'), in definition order.")

(defvar gent-tools--processes nil
  "Live processes started by tools, killed by `gent-tools-kill-processes'.")

(cl-defun gent-define-tool (name &key description parameters handler preview)
  "Register the tool NAME.
DESCRIPTION and PARAMETERS (a JSON-schema alist) are sent to the
model.  HANDLER is called with (ARGS DONE) and must call DONE with a
result string.  PREVIEW, when non-nil, is called with ARGS and returns
\(KIND . TEXT) describing the change so the user can confirm it."
  (setf (alist-get name gent-tools--registry nil nil #'equal)
        (gent-tool--create :name name
                           :description description
                           :parameters parameters
                           :handler handler
                           :preview preview))
  name)

(defun gent-tools-get (name)
  "Return the tool named NAME, or nil."
  (alist-get name gent-tools--registry nil nil #'equal))

(defun gent-tools-definitions ()
  "Return the registered tools as `gent-llm-make-tool' definitions."
  (mapcar (lambda (entry)
            (let ((tool (cdr entry)))
              (gent-llm-make-tool
               :name (gent-tool-name tool)
               :description (gent-tool-description tool)
               :parameters (gent-tool-parameters tool))))
          gent-tools--registry))

(defun gent-tools-names ()
  "Return the names of the registered tools."
  (mapcar #'car gent-tools--registry))

(defun gent-tools-truncate (text &optional max)
  "Return TEXT cut to at most MAX characters, keeping its head and tail.
MAX defaults to `gent-tools-output-max-chars'."
  (let ((max (or max gent-tools-output-max-chars)))
    (if (<= (length text) max)
        text
      (let ((half (/ max 2)))
        (concat (substring text 0 half)
                (format "\n\n[... %d characters omitted ...]\n\n"
                        (- (length text) (* half 2)))
                (substring text (- (length text) half)))))))

;;; Dispatch

(defun gent-tools--parse-arguments (arguments)
  "Parse ARGUMENTS, a JSON string or alist, into an alist.
Signal an error when it is not a JSON object."
  (cond ((null arguments) nil)
        ((listp arguments) arguments)
        ((string-empty-p (string-trim arguments)) nil)
        (t (let ((parsed (gent-llm--json-parse-string arguments)))
             (unless (and parsed (listp parsed))
               (error "Arguments are not a JSON object: %s" arguments))
             parsed))))

(defun gent-tools--trusted-p (name)
  "Return non-nil if tool NAME may run without confirmation."
  (or (member name gent-tools-trusted)
      (member name gent-tools-session-trusted)))

(defun gent-tools--rejection (answer)
  "Return the message for the model when ANSWER rejects a tool call.
Return nil when ANSWER approves it."
  (pcase answer
    ('nil "The user rejected this tool call.")
    (`(reject . ,note)
     (if (and note (not (string-empty-p note)))
         (format "The user rejected this tool call with the note: %s" note)
       "The user rejected this tool call."))
    (_ nil)))

(defun gent-tools-call (name arguments root done &optional confirm)
  "Run tool NAME with ARGUMENTS inside ROOT and call DONE with the result.
ARGUMENTS is the raw JSON string from the model.  Errors, unknown
tools and rejections are reported through DONE as strings, so the
model always gets an answer.  The result is truncated to
`gent-tools-output-max-chars'.

When the tool changes something and is not trusted, CONFIRM is
called as (CONFIRM NAME KIND TEXT RESPOND), where KIND is `diff' or
`command' and TEXT shows what will happen.  The tool waits until
RESPOND is called with the answer: t to approve, nil to reject,
\(reject . NOTE) to reject with a note for the model, or `always'
to approve and trust NAME for the rest of the session.  On approval
the preview is computed again; if it changed, CONFIRM is called
again with the new one.  Without CONFIRM, such calls are rejected."
  (let* ((called nil)
         (finish (lambda (result)
                   (unless called
                     (setq called t)
                     (funcall done (gent-tools-truncate
                                    (if (stringp result)
                                        result
                                      (format "%s" result)))))))
         (tool (gent-tools-get name))
         (session (current-buffer))
         (default-directory (file-name-as-directory root)))
    (cl-labels
        ((run (args)
           (let ((preview (and (gent-tool-preview tool)
                               (not (gent-tools--trusted-p name))
                               (funcall (gent-tool-preview tool) args))))
             (cond ((not preview)
                    (funcall (gent-tool-handler tool) args finish))
                   ((not confirm)
                    (funcall finish "Error: this tool needs approval and nobody can approve it."))
                   (t (ask args preview)))))
         (ask (args preview)
           (let ((answered nil))
             (funcall confirm name (car preview) (cdr preview)
                      (lambda (answer)
                        (unless answered
                          (setq answered t)
                          (let ((default-directory (file-name-as-directory root)))
                            (condition-case err
                                (respond args preview answer)
                              (error (funcall finish (format "Error: %s"
                                                             (error-message-string err)))))))))))
         (respond (args preview answer)
           (let ((rejection (gent-tools--rejection answer)))
             (if rejection
                 (funcall finish rejection)
               (when (and (eq answer 'always) (buffer-live-p session))
                 (with-current-buffer session
                   (cl-pushnew name gent-tools-session-trusted :test #'equal)))
               (let ((fresh (funcall (gent-tool-preview tool) args)))
                 (if (equal fresh preview)
                     (funcall (gent-tool-handler tool) args finish)
                   (ask args fresh)))))))
      (condition-case err
          (if (not tool)
              (funcall finish (format "Error: unknown tool `%s'.  Available tools: %s"
                                      name (string-join (gent-tools-names) ", ")))
            (run (gent-tools--parse-arguments arguments)))
        (error (funcall finish (format "Error: %s" (error-message-string err))))))))

(defun gent-tools-kill-processes ()
  "Kill every process started by a tool."
  (dolist (process gent-tools--processes)
    (when (process-live-p process)
      (delete-process process)))
  (setq gent-tools--processes nil))

;;; File helpers

(defun gent-tools--resolve (path)
  "Return PATH expanded against `default-directory'.
Signal an error if it falls outside `default-directory'."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "Missing `path'"))
  (let* ((root (file-name-as-directory (expand-file-name default-directory)))
         (file (expand-file-name path root)))
    (unless (or (file-in-directory-p file root)
                (string= (file-name-as-directory file) root))
      (error "Path %s is outside the project root %s" path root))
    file))

(defun gent-tools--relative (file)
  "Return FILE relative to `default-directory'."
  (file-relative-name file default-directory))

(defun gent-tools--file-string (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun gent-tools--check-buffer (file)
  "Signal an error if a buffer visiting FILE has unsaved changes."
  (let ((buffer (find-buffer-visiting file)))
    (when (and buffer (buffer-modified-p buffer))
      (error "%s has unsaved changes in buffer %s; ask the user to save it first"
             (gent-tools--relative file) (buffer-name buffer)))))

(defun gent-tools--write (file content)
  "Write CONTENT to FILE and refresh any buffer visiting it."
  (gent-tools--check-buffer file)
  (let ((coding (and (file-exists-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       last-coding-system-used))))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write (or coding 'utf-8-unix)))
      (write-region content nil file nil 'silent)))
  (let ((buffer (find-buffer-visiting file)))
    (when buffer
      (with-current-buffer buffer
        (revert-buffer :ignore-auto :noconfirm :preserve-modes)))))

(defun gent-tools--diff (file old new)
  "Return a unified diff for FILE going from OLD to NEW."
  (let ((old-file (make-temp-file "gent-old"))
        (new-file (make-temp-file "gent-new"))
        (name (gent-tools--relative file)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (or old "") nil old-file nil 'silent)
            (write-region new nil new-file nil 'silent))
          (with-temp-buffer
            (call-process "diff" nil t nil "-u"
                          "--label" (if old (concat "a/" name) "/dev/null")
                          "--label" (concat "b/" name)
                          old-file new-file)
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))

(defun gent-tools--replace (content old new)
  "Return CONTENT with the single occurrence of OLD replaced by NEW.
Signal an error unless OLD occurs exactly once."
  (when (or (null old) (string-empty-p old))
    (error "`old_string' must not be empty; use write_file to create files"))
  (let ((case-fold-search nil)
        (start (string-search old content)))
    (unless start
      (error "`old_string' was not found; read the file again and copy the text exactly"))
    (when (string-search old content (1+ start))
      (error "`old_string' occurs more than once; include more surrounding context"))
    (concat (substring content 0 start)
            (or new "")
            (substring content (+ start (length old))))))

;;; read_file

(defun gent-tools--read-file (args done)
  "Call DONE with the lines of the file named in ARGS."
  (let* ((file (gent-tools--resolve (map-elt args 'path)))
         (offset (max 1 (or (map-elt args 'offset) 1)))
         (limit (or (map-elt args 'limit) gent-tools-read-default-limit)))
    (cond
     ((file-directory-p file)
      (funcall done (string-join
                     (mapcar (lambda (name)
                               (if (file-directory-p (expand-file-name name file))
                                   (concat name "/")
                                 name))
                             (directory-files file nil
                                              directory-files-no-dot-files-regexp))
                     "\n")))
     ((not (file-exists-p file))
      (error "%s does not exist" (gent-tools--relative file)))
     (t
      (with-temp-buffer
        (insert-file-contents file)
        (let ((total (count-lines (point-min) (point-max)))
              (lines nil)
              (line offset))
          (goto-char (point-min))
          (forward-line (1- offset))
          (while (and (not (eobp)) (< (- line offset) limit))
            (push (format "%6d\t%s" line
                          (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                  lines)
            (setq line (1+ line))
            (forward-line 1))
          (funcall done
                   (concat (string-join (nreverse lines) "\n")
                           (if (< (1- line) total)
                               (format "\n[%d more lines; use offset %d to continue]"
                                       (- total (1- line)) line)
                             "")))))))))

(gent-define-tool
 "read_file"
 :description "Read a text file in the project.  Each line is prefixed with its \
line number and a tab; the prefix is not part of the file.  Use `offset' and \
`limit' to read part of a large file.  A directory path lists its entries."
 :parameters '((type . "object")
               (properties
                . ((path . ((type . "string")
                            (description . "Path relative to the project root")))
                   (offset . ((type . "integer")
                              (description . "1-based line to start at")))
                   (limit . ((type . "integer")
                             (description . "Maximum number of lines to return")))))
               (required . ["path"]))
 :handler #'gent-tools--read-file)

;;; edit_file

(defun gent-tools--edit-new-content (args)
  "Return (FILE OLD-CONTENT NEW-CONTENT) for the edit described by ARGS."
  (let* ((file (gent-tools--resolve (map-elt args 'path))))
    (unless (file-regular-p file)
      (error "%s does not exist; use write_file to create it"
             (gent-tools--relative file)))
    (gent-tools--check-buffer file)
    (let ((old (gent-tools--file-string file)))
      (list file old (gent-tools--replace old (map-elt args 'old_string)
                                          (map-elt args 'new_string))))))

(gent-define-tool
 "edit_file"
 :description "Replace one exact occurrence of `old_string' with `new_string' \
in a file.  `old_string' must match the file exactly, including whitespace, and \
must be unique; include surrounding lines to make it unique.  Read the file \
before editing it."
 :parameters '((type . "object")
               (properties
                . ((path . ((type . "string")
                            (description . "Path relative to the project root")))
                   (old_string . ((type . "string")
                                  (description . "Exact text to replace")))
                   (new_string . ((type . "string")
                                  (description . "Replacement text")))))
               (required . ["path" "old_string" "new_string"]))
 :preview (lambda (args)
            (pcase-let ((`(,file ,old ,new) (gent-tools--edit-new-content args)))
              (cons 'diff (gent-tools--diff file old new))))
 :handler (lambda (args done)
            (pcase-let ((`(,file ,_old ,new) (gent-tools--edit-new-content args)))
              (gent-tools--write file new)
              (funcall done (format "Edited %s" (gent-tools--relative file))))))

;;; write_file

(gent-define-tool
 "write_file"
 :description "Create a file, or overwrite an existing one, with `content'.  \
Prefer edit_file for changes to existing files."
 :parameters '((type . "object")
               (properties
                . ((path . ((type . "string")
                            (description . "Path relative to the project root")))
                   (content . ((type . "string")
                               (description . "The complete new file contents")))))
               (required . ["path" "content"]))
 :preview (lambda (args)
            (let ((file (gent-tools--resolve (map-elt args 'path))))
              (gent-tools--check-buffer file)
              (cons 'diff (gent-tools--diff
                           file
                           (and (file-regular-p file) (gent-tools--file-string file))
                           (or (map-elt args 'content) "")))))
 :handler (lambda (args done)
            (let ((file (gent-tools--resolve (map-elt args 'path)))
                  (content (or (map-elt args 'content) "")))
              (when (file-directory-p file)
                (error "%s is a directory" (gent-tools--relative file)))
              (gent-tools--write file content)
              (funcall done (format "Wrote %d characters to %s"
                                    (length content) (gent-tools--relative file))))))

;;; run_shell

(defun gent-tools--run-shell (args done)
  "Run the command in ARGS asynchronously and call DONE with its output."
  (let* ((command (map-elt args 'command))
         (timeout (or (map-elt args 'timeout) gent-tools-shell-timeout))
         (buffer (generate-new-buffer " *gent-shell*"))
         (timed-out nil)
         (timer nil)
         (process nil))
    (unless (and (stringp command) (not (string-empty-p (string-trim command))))
      (error "Missing `command'"))
    (setq process
          (make-process
           :name "gent-shell"
           :buffer buffer
           :command (list shell-file-name shell-command-switch command)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (proc _event)
             (unless (process-live-p proc)
               (when timer (cancel-timer timer))
               (setq gent-tools--processes (delq proc gent-tools--processes))
               (let ((output (if (buffer-live-p buffer)
                                 (with-current-buffer buffer (buffer-string))
                               ""))
                     (status (process-exit-status proc)))
                 (when (buffer-live-p buffer)
                   (kill-buffer buffer))
                 (funcall done
                          (concat (gent-tools-truncate output)
                                  (unless (or (string-empty-p output)
                                              (string-suffix-p "\n" output))
                                    "\n")
                                  (if timed-out
                                      (format "[timed out after %s seconds]" timeout)
                                    (format "[exit code %d]" status)))))))))
    (push process gent-tools--processes)
    (process-send-eof process)
    (setq timer (run-at-time timeout nil
                             (lambda ()
                               (when (process-live-p process)
                                 (setq timed-out t)
                                 (delete-process process)))))))

(gent-define-tool
 "run_shell"
 :description "Run a shell command in the project root and return its combined \
stdout and stderr plus the exit code.  Use it to list files (ls, find), search \
(rg, grep), build and run tests.  Commands get no stdin and must not be \
interactive."
 :parameters '((type . "object")
               (properties
                . ((command . ((type . "string")
                               (description . "The shell command to run")))
                   (timeout . ((type . "integer")
                               (description . "Timeout in seconds")))))
               (required . ["command"]))
 :preview (lambda (args)
            (cons 'command (format "$ %s\n\n(in %s)"
                                   (map-elt args 'command) default-directory)))
 :handler #'gent-tools--run-shell)

(provide 'gent-tools)
;;; gent-tools.el ends here
