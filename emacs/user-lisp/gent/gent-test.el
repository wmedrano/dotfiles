;;; gent-test.el --- Tests for gent -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from this directory with:
;;
;;   emacs -Q --batch -L . -L ../../elpa/shell-maker-* \
;;     -l gent-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'gent)

;;; Helpers

(defmacro gent-test--with-project (&rest body)
  "Run BODY with `default-directory' bound to a fresh temporary project."
  (declare (indent 0))
  `(let ((default-directory (file-name-as-directory (make-temp-file "gent-test" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory default-directory t))))

(defvar gent-test--confirm nil
  "Confirm function `gent-test--call' passes to `gent-tools-call'.")

(defun gent-test--answering (answer)
  "Return a confirm function that answers every request with ANSWER."
  (lambda (_name _kind _text respond) (funcall respond answer)))

(defun gent-test--call (name args &optional root)
  "Call tool NAME with ARGS (an alist) in ROOT and wait for its result.
Approval requests go to `gent-test--confirm'."
  (let ((result nil))
    (gent-tools-call name (json-encode args) (or root default-directory)
                     (lambda (r) (setq result r))
                     gent-test--confirm)
    (while (not result)
      (accept-process-output nil 0.05))
    result))

(defun gent-test--write-file (name content)
  "Write CONTENT to NAME in `default-directory'."
  (write-region content nil (expand-file-name name) nil 'silent))

(defun gent-test--read-file (name)
  "Return the contents of NAME in `default-directory'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name))
    (buffer-string)))

(defun gent-test--sse (&rest deltas)
  "Return an SSE stream with one chunk per DELTAS alist."
  (concat (mapconcat (lambda (delta)
                       (format "data: %s\n\n"
                               (json-encode `((choices . [((delta . ,delta))])))))
                     deltas "")
          "data: [DONE]\n\n"))

;;; SSE parsing

(ert-deftest gent-test-filter-text ()
  (let ((result (gent-llm--filter
                 `((:pending . ,(gent-test--sse '((content . "Hel"))
                                                '((content . "lo"))))))))
    (should (equal (map-elt result :filtered) "Hello"))
    (should-not (map-elt result :pending))))

(ert-deftest gent-test-filter-partial-line ()
  (let ((result (gent-llm--filter
                 '((:pending . "data: {\"choices\":[{\"delta\":{\"content\":\"a\"}}]}\n\ndata: {\"cho")))))
    (should (equal (map-elt result :filtered) "a"))
    (should (equal (map-elt result :pending) "data: {\"cho"))))

(ert-deftest gent-test-filter-tool-calls ()
  (let* ((result (gent-llm--filter
                  `((:pending
                     . ,(gent-test--sse
                         '((tool_calls . [((index . 0) (id . "c1")
                                           (function . ((name . "read_file")
                                                        (arguments . "{\"pa"))))]))
                         '((tool_calls . [((index . 0)
                                           (function . ((arguments . "th\":\"x\"}"))))])))))))
         (calls (gent-llm--tool-calls (map-elt result :tool-calls))))
    (should (= (length calls) 1))
    (should (equal (map-elt (car calls) 'id) "c1"))
    (should (equal (map-elt (map-elt (car calls) 'function) 'arguments)
                   "{\"path\":\"x\"}"))))

;; Reasoning arrives separately from the answer.
(ert-deftest gent-test-filter-reasoning ()
  (let ((result (gent-llm--filter
                 `((:pending . ,(gent-test--sse '((reasoning_content . "Hmm, "))
                                                '((reasoning_content . "ok."))
                                                '((content . "42"))))))))
    (should (equal (map-elt result :reasoning) "Hmm, ok."))
    (should (equal (map-elt result :filtered) "42"))))

(ert-deftest gent-test-payload-reasoning-effort ()
  (let ((backend (gent-llm-make-backend :model "m" :reasoning-effort "low")))
    (should (equal (map-elt (gent-llm--payload backend nil "hi" nil nil) 'reasoning_effort)
                   "low"))
    (should (equal (map-elt (gent-llm--payload backend nil "hi" nil nil) 'model)
                   "m")))
  (let ((backend (gent-llm-make-backend :model "m" :reasoning-effort nil)))
    (should-not (assq 'reasoning_effort (gent-llm--payload backend nil "hi" nil nil)))))

(ert-deftest gent-test-backend-timeout ()
  (let ((provider (gent-llm-make-provider :default-timeout 30)))
    (should (= (gent-llm--timeout (gent-llm-make-backend :provider provider)) 30))
    (should (= (gent-llm--timeout (gent-llm-make-backend :provider provider :timeout 5))
               5))))

(ert-deftest gent-test-provider-headers ()
  (should (member "Authorization: Bearer k"
                  (gent-llm--headers (gent-llm-make-provider :api-key (lambda () "k")))))
  (should-not (seq-find (lambda (h) (string-prefix-p "Authorization" h))
                        (gent-llm--headers (gent-llm-make-provider))))
  (should (member "X-A: 1" (gent-llm--headers
                            (gent-llm-make-provider :headers '("X-A: 1")))))
  (should (member "X-B: 2" (gent-llm--headers
                            (gent-llm-make-provider :headers (lambda () '("X-B: 2")))))))

;;; Message building

(ert-deftest gent-test-normalize-tool-messages ()
  (let ((messages (gent-llm--normalize-messages
                   '(((role . "assistant") (content . "")
                      (tool_calls . (((id . "c1")))) (extra . 1))
                     ((role . "tool") (tool_call_id . "c1") (content . "ok")))
                   "next")))
    (should (= (length messages) 3))
    (should-not (map-elt (aref messages 0) 'extra))
    (should (vectorp (map-elt (aref messages 0) 'tool_calls)))
    (should (equal (map-elt (aref messages 2) 'content) "next"))))

(ert-deftest gent-test-system-message ()
  (gent-test--with-project
    (gent-test--write-file "AGENTS.md" "Use tabs.")
    (let ((content (map-elt (gent--system-message default-directory) 'content)))
      (should (string-search default-directory content))
      (should (string-search "edit_file" content))
      (should (string-search "Use tabs." content)))))

(ert-deftest gent-test-tool-definitions ()
  (should (equal (sort (gent-tools-names) #'string<)
                 '("edit_file" "read_file" "run_shell" "write_file")))
  (dolist (definition (gent-tools-definitions))
    (should (map-elt (map-elt definition 'function) 'parameters))))

(ert-deftest gent-test-repair ()
  (let ((repaired (gent--repair
                   '(((role . "user") (content . "hi"))
                     ((role . "assistant") (content . "")
                      (tool_calls . [((id . "a")) ((id . "b"))]))
                     ((role . "tool") (tool_call_id . "a") (content . "done"))))))
    (should (= (length repaired) 4))
    (should (equal (mapcar (lambda (m) (map-elt m 'tool_call_id)) (cddr repaired))
                   '("b" "a")))))

(ert-deftest gent-test-compact ()
  (let* ((gent-context-max-tokens 1000)
         (gent-context-compact-ratio 1.0)
         (gent-context-keep-tool-results 1)
         (big (make-string 2000 ?x))
         (messages (list `((role . "tool") (tool_call_id . "1") (content . ,big))
                         `((role . "tool") (tool_call_id . "2") (content . ,big))
                         `((role . "tool") (tool_call_id . "3") (content . ,big))))
         (compacted (gent--compact messages)))
    (should (equal (map-elt (nth 0 compacted) 'content) gent--elided))
    (should (equal (map-elt (nth 1 compacted) 'content) gent--elided))
    (should (equal (map-elt (nth 2 compacted) 'content) big))
    (should (equal (map-elt (nth 0 compacted) 'tool_call_id) "1"))
    (let ((small (list '((role . "tool") (content . "x")))))
      (should (eq (gent--compact small) small)))))

;;; Tools

(ert-deftest gent-test-truncate ()
  (should (equal (gent-tools-truncate "short" 10) "short"))
  (let ((cut (gent-tools-truncate (make-string 100 ?a) 20)))
    (should (string-search "omitted" cut))
    (should (< (length cut) 100))))

(ert-deftest gent-test-read-file ()
  (gent-test--with-project
    (gent-test--write-file "a.txt" "one\ntwo\nthree\n")
    (should (equal (gent-test--call "read_file" '((path . "a.txt")))
                   "     1\tone\n     2\ttwo\n     3\tthree"))
    (let ((part (gent-test--call "read_file" '((path . "a.txt") (offset . 2) (limit . 1)))))
      (should (string-prefix-p "     2\ttwo" part))
      (should (string-search "use offset 3" part)))
    (should (string-search "a.txt" (gent-test--call "read_file" '((path . "."))))) ))

(ert-deftest gent-test-path-outside-root ()
  (gent-test--with-project
    (should (string-prefix-p "Error: Path"
                             (gent-test--call "read_file" '((path . "../etc/passwd")))))
    (should (string-prefix-p "Error: Path"
                             (gent-test--call "read_file" '((path . "/etc/passwd")))))))

(ert-deftest gent-test-unknown-tool-and-bad-json ()
  (gent-test--with-project
    (let ((result nil))
      (gent-tools-call "nope" "{}" default-directory (lambda (r) (setq result r)))
      (should (string-search "unknown tool" result))
      (gent-tools-call "read_file" "{not json" default-directory (lambda (r) (setq result r)))
      (should (string-prefix-p "Error:" result)))))

(ert-deftest gent-test-edit-file ()
  (gent-test--with-project
    (let ((gent-test--confirm (gent-test--answering t)))
      (gent-test--write-file "a.txt" "foo bar foo\nbaz\n")
      (should (string-search "more than once"
                             (gent-test--call "edit_file" '((path . "a.txt")
                                                            (old_string . "foo")
                                                            (new_string . "x")))))
      (should (string-search "not found"
                             (gent-test--call "edit_file" '((path . "a.txt")
                                                            (old_string . "qux")
                                                            (new_string . "x")))))
      (should (string-prefix-p "Edited"
                               (gent-test--call "edit_file" '((path . "a.txt")
                                                              (old_string . "baz")
                                                              (new_string . "qux")))))
      (should (equal (gent-test--read-file "a.txt") "foo bar foo\nqux\n")))))

(ert-deftest gent-test-confirmation ()
  (gent-test--with-project
    (gent-test--write-file "a.txt" "old\n")
    (let* ((seen nil)
           (gent-test--confirm
            (lambda (name kind text respond)
              (setq seen (list name kind text))
              (funcall respond '(reject . "use new2")))))
      (should (string-search "use new2"
                             (gent-test--call "edit_file" '((path . "a.txt")
                                                            (old_string . "old")
                                                            (new_string . "new")))))
      (should (equal (gent-test--read-file "a.txt") "old\n"))
      (should (equal (car seen) "edit_file"))
      (should (eq (nth 1 seen) 'diff))
      (should (string-search "+new" (nth 2 seen))))
    (let ((gent-test--confirm (gent-test--answering nil)))
      (should (string-search "rejected"
                             (gent-test--call "run_shell" '((command . "touch x")))))
      (should-not (file-exists-p "x")))
    (with-temp-buffer
      (let* ((asked 0)
             (gent-test--confirm (lambda (_name _kind _text respond)
                                   (cl-incf asked)
                                   (funcall respond 'always))))
        (gent-test--call "run_shell" '((command . "true")))
        (gent-test--call "run_shell" '((command . "true")))
        (should (= asked 1))))
    (let ((gent-tools-trusted '("run_shell"))
          (gent-test--confirm (lambda (&rest _) (error "Should not ask"))))
      (should (string-search "[exit code 0]"
                             (gent-test--call "run_shell" '((command . "true"))))))
    (let ((gent-test--confirm nil))
      (should (string-search "nobody can approve"
                             (gent-test--call "run_shell" '((command . "true"))))))))

(ert-deftest gent-test-confirmation-waits ()
  (gent-test--with-project
    (gent-test--write-file "a.txt" "old\n")
    (let* ((requests nil)
           (result nil)
           (confirm (lambda (_name _kind text respond)
                      (push (cons text respond) requests))))
      (gent-tools-call "edit_file"
                       (json-encode '((path . "a.txt") (old_string . "old") (new_string . "new")))
                       default-directory (lambda (r) (setq result r)) confirm)
      ;; Nothing happens until the user answers.
      (should (= (length requests) 1))
      (should-not result)
      (should (equal (gent-test--read-file "a.txt") "old\n"))
      ;; The file changes before approval, so the diff is shown again.
      (gent-test--write-file "a.txt" "old\nmore\n")
      (funcall (cdr (car requests)) t)
      (should (= (length requests) 2))
      (should (string-search "more" (car (car requests))))
      (should-not result)
      ;; Answering the stale request again does nothing.
      (funcall (cdr (cadr requests)) t)
      (should (= (length requests) 2))
      (funcall (cdr (car requests)) t)
      (should (equal result "Edited a.txt"))
      (should (equal (gent-test--read-file "a.txt") "new\nmore\n")))))

(ert-deftest gent-test-write-file-and-buffer-sync ()
  (gent-test--with-project
    (let ((gent-test--confirm (gent-test--answering t))
          (file (expand-file-name "sub/b.txt")))
      (should (string-prefix-p "Wrote"
                               (gent-test--call "write_file" '((path . "sub/b.txt")
                                                               (content . "hello\n")))))
      (should (equal (gent-test--read-file "sub/b.txt") "hello\n"))
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (progn
              (gent-test--call "edit_file" '((path . "sub/b.txt")
                                             (old_string . "hello")
                                             (new_string . "bye")))
              (should (equal (with-current-buffer buffer (buffer-string)) "bye\n"))
              (with-current-buffer buffer (insert "unsaved"))
              (should (string-search "unsaved changes"
                                     (gent-test--call "write_file"
                                                      '((path . "sub/b.txt")
                                                        (content . "x"))))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest gent-test-run-shell ()
  (gent-test--with-project
    (let ((gent-tools-trusted '("run_shell")))
      (should (equal (gent-test--call "run_shell" '((command . "echo hi; echo err >&2; exit 3")))
                     "hi\nerr\n[exit code 3]"))
      (should (string-search "timed out"
                             (gent-test--call "run_shell" '((command . "sleep 5")
                                                            (timeout . 0.2))))))))

;;; Agent loop

(defun gent-test--fake-chat (responses requests)
  "Return a fake `gent-llm-chat' that replays RESPONSES.
RESPONSES is a cons whose car is a list of (TEXT . CALLS).  The
messages of each request are pushed onto the car of the cons
REQUESTS."
  (lambda (&rest args)
    (push (plist-get args :messages) (car requests))
    (let ((response (pop (car responses))))
      (run-at-time 0 nil
                   (lambda ()
                     (when (car response)
                       (funcall (plist-get args :on-token) (car response)))
                     (when (cdr response)
                       (funcall (plist-get args :on-tool-calls) (cdr response) "tool_calls"))
                     (funcall (plist-get args :on-done) (or (car response) "")))))
    nil))

(defun gent-test--run-loop (responses prompt)
  "Run PROMPT through the loop with a fake model replaying RESPONSES.
Return (SUCCESS OUTPUT MESSAGES REQUESTS)."
  (let* ((requests (list nil))
         (queue (list responses))
         (done nil) (success nil) (output ""))
    (cl-letf (((symbol-function 'gent-llm-chat)
               (gent-test--fake-chat queue requests)))
      (with-temp-buffer
        (setq gent--root default-directory)
        (gent--start-turn
         (current-buffer) prompt
         (list (cons :write-output (lambda (text &optional _) (setq output (concat output text))))
               (cons :finish-output (lambda (ok) (setq success ok done t)))))
        (while (not done)
          (accept-process-output nil 0.05))
        (list success output gent--messages (reverse (car requests)))))))

(defun gent-test--tool-call (id name args)
  "Return a tool call with ID to NAME with ARGS."
  `((id . ,id) (type . "function")
    (function . ((name . ,name) (arguments . ,(json-encode args))))))

(ert-deftest gent-test-loop-plain-answer ()
  (gent-test--with-project
    (pcase-let ((`(,success ,output ,messages ,requests)
                 (gent-test--run-loop '(("Hi there")) "hello")))
      (should success)
      (should (equal output "Hi there"))
      (should (= (length requests) 1))
      (should (equal (map-elt (car (car requests)) 'role) "system"))
      (should (equal (mapcar (lambda (m) (map-elt m 'role)) messages)
                     '("user" "assistant"))))))

(ert-deftest gent-test-loop-with-tools ()
  (gent-test--with-project
    (gent-test--write-file "a.txt" "secret\n")
    (pcase-let ((`(,success ,output ,messages ,requests)
                 (gent-test--run-loop
                  `((nil ,(gent-test--tool-call "c1" "read_file" '((path . "a.txt")))
                         ,(gent-test--tool-call "c2" "missing" nil))
                    ("It says secret."))
                  "what is in a.txt?")))
      (should success)
      (should (string-search "read_file" output))
      (should (string-search "It says secret." output))
      (should (= (length requests) 2))
      (should (equal (mapcar (lambda (m) (map-elt m 'role)) messages)
                     '("user" "assistant" "tool" "tool" "assistant")))
      (should (string-search "secret" (map-elt (nth 2 messages) 'content)))
      (should (string-search "unknown tool" (map-elt (nth 3 messages) 'content)))
      (should (equal (map-elt (nth 2 messages) 'tool_call_id) "c1")))))

(defmacro gent-test--with-paused-loop (responses prompt &rest body)
  "Run PROMPT against a fake model replaying RESPONSES, then BODY.
BODY runs in the gent buffer once a change awaits approval, with
`output', `done' and `success' bound.  Afterwards the loop is run to
completion."
  (declare (indent 2))
  `(let* ((requests (list nil))
          (done nil) (success nil) (output ""))
     (cl-letf (((symbol-function 'gent-llm-chat)
                (gent-test--fake-chat (list ,responses) requests)))
       (with-temp-buffer
         (setq gent--root default-directory)
         (gent--start-turn
          (current-buffer) ,prompt
          (list (cons :write-output (lambda (text &optional _) (setq output (concat output text))))
                (cons :finish-output (lambda (ok) (setq success ok done t)))))
         (with-timeout (5 (error "Timed out waiting for approval"))
           (while (and (not done) (not gent--pending))
             (accept-process-output nil 0.05)))
         ,@body
         (with-timeout (5 (error "Timed out waiting for the turn"))
           (while (not done)
             (accept-process-output nil 0.05)))
         (ignore output success)))))

(ert-deftest gent-test-loop-waits-for-approval ()
  (gent-test--with-project
    (gent-test--write-file "a.txt" "old\n")
    (gent-test--with-paused-loop
        `((nil ,(gent-test--tool-call "c1" "edit_file" '((path . "a.txt")
                                                         (old_string . "old")
                                                         (new_string . "new"))))
          ("Done."))
        "edit"
      (let ((gent-buffer (current-buffer)))
        (should (equal (plist-get gent--pending :name) "edit_file"))
        (should (string-search "+new" output))
        (should (string-search "Awaiting approval" output))
        (should (string-search "approve?" (format "%s" mode-line-process)))
        ;; Waiting does not advance the turn.
        (accept-process-output nil 0.2)
        (should-not done)
        (should (equal (gent-test--read-file "a.txt") "old\n"))
        ;; Approve from some other buffer.
        (with-temp-buffer
          (gent-approve))
        (should-not (buffer-local-value 'gent--pending gent-buffer)))
      (with-timeout (5 (error "Timed out"))
        (while (not done) (accept-process-output nil 0.05)))
      (should success)
      (should (equal (gent-test--read-file "a.txt") "new\n"))
      (should (string-search "Done." output))
      (should (equal (map-elt (nth 2 gent--messages) 'content) "Edited a.txt")))))

(ert-deftest gent-test-loop-reject-with-note ()
  (gent-test--with-project
    (gent-test--with-paused-loop
        `((nil ,(gent-test--tool-call "c1" "run_shell" '((command . "touch x"))))
          ("Ok."))
        "touch"
      (should (string-search "$ touch x" output))
      (gent-reject "use y instead"))
    (should-not (file-exists-p "x"))))

(ert-deftest gent-test-reject-records-note ()
  (gent-test--with-project
    (let (tool-message)
      (gent-test--with-paused-loop
          `((nil ,(gent-test--tool-call "c1" "run_shell" '((command . "touch x"))))
            ("Ok."))
          "touch"
        (gent-reject "use y instead")
        (with-timeout (5 (error "Timed out"))
          (while (not done) (accept-process-output nil 0.05)))
        (setq tool-message (nth 2 gent--messages)))
      (should (string-search "use y instead" (map-elt tool-message 'content))))))

(ert-deftest gent-test-interrupt-clears-pending ()
  (gent-test--with-project
    (cl-letf (((symbol-function 'gent--ensure-buffer) #'ignore)
              ((symbol-function 'shell-maker-interrupt) #'ignore))
      (gent-test--with-paused-loop
          `((nil ,(gent-test--tool-call "c1" "run_shell" '((command . "touch x")))))
          "touch"
        (gent-interrupt)
        (should-not gent--pending)
        (should (equal (map-elt (car (last gent--messages)) 'content)
                       "Cancelled by the user."))
        (should-error (gent-approve) :type 'user-error)
        ;; The interrupted turn never finishes on its own.
        (setq done t))
      (should-not (file-exists-p "x")))))

(ert-deftest gent-test-loop-reasoning ()
  (gent-test--with-project
    (cl-letf (((symbol-function 'gent-llm-chat)
               (lambda (&rest args)
                 (run-at-time 0 nil
                              (lambda ()
                                (funcall (plist-get args :on-reasoning) "Let me ")
                                (funcall (plist-get args :on-reasoning) "think.")
                                (funcall (plist-get args :on-token) "Answer.")
                                (funcall (plist-get args :on-done) "Answer.")))
                 nil)))
      (with-temp-buffer
        (let ((done nil) (output ""))
          (gent--start-turn
           (current-buffer) "hi"
           (list (cons :write-output (lambda (text &optional _) (setq output (concat output text))))
                 (cons :finish-output (lambda (_) (setq done t)))))
          (while (not done) (accept-process-output nil 0.05))
          ;; One visible header, then hidden reasoning, then the answer.
          (should (string-match "Reasoning.*\n\\(Let me think\\.\n\n\\)Answer\\.\\'" output))
          (should (eq (get-text-property (match-beginning 1) 'invisible output) 'gent-details))
          (should-not (get-text-property (match-beginning 0) 'invisible output))
          (should-not (get-text-property (match-end 1) 'invisible output))
          ;; Reasoning is not kept in the conversation.
          (should (equal (map-elt (car (last gent--messages)) 'content) "Answer.")))))))

(ert-deftest gent-test-loop-iteration-limit ()
  (gent-test--with-project
    (let ((gent-max-iterations 2))
      (pcase-let ((`(,success ,output ,messages ,requests)
                   (gent-test--run-loop
                    (mapcar (lambda (id)
                              (list nil (gent-test--tool-call id "read_file" '((path . ".")))))
                            '("c1" "c2" "c3"))
                    "loop")))
        (should-not success)
        (should (string-search "stopped after 2" output))
        (should (= (length requests) 2))
        ;; The unanswered call from the last response gets a placeholder.
        (should (equal (map-elt (car (last messages)) 'role) "tool"))))))

(ert-deftest gent-test-loop-error ()
  (gent-test--with-project
    (cl-letf (((symbol-function 'gent-llm-chat)
               (lambda (&rest args)
                 (run-at-time 0 nil (plist-get args :on-error) "boom")
                 nil)))
      (with-temp-buffer
        (let ((done nil) (success t) (output ""))
          (gent--start-turn
           (current-buffer) "hi"
           (list (cons :write-output (lambda (text &optional _) (setq output (concat output text))))
                 (cons :finish-output (lambda (ok) (setq success ok done t)))))
          (while (not done) (accept-process-output nil 0.05))
          (should-not success)
          (should (string-search "boom" output)))))))

(ert-deftest gent-test-loop-cancel ()
  (gent-test--with-project
    (let ((finished nil))
      (cl-letf (((symbol-function 'gent-llm-chat)
                 (gent-test--fake-chat (list '(("late"))) (list nil))))
        (with-temp-buffer
          (gent--start-turn
           (current-buffer) "hi"
           (list (cons :write-output #'ignore)
                 (cons :finish-output (lambda (_) (setq finished t)))))
          ;; Cancel before the fake response arrives.
          (setq gent--turn (1+ gent--turn))
          (accept-process-output nil 0.1)
          (should-not finished)
          (should (= (length gent--messages) 1)))))))

(ert-deftest gent-test-session-roundtrip ()
  (gent-test--with-project
    (let ((file (expand-file-name "s.eld"))
          (messages '(((role . "user") (content . "hi"))
                      ((role . "assistant") (content . "")
                       (tool_calls . [((id . "c") (function . ((name . "x"))))])))))
      (with-temp-buffer
        (setq gent--messages messages)
        (gent-save-session file))
      (cl-letf (((symbol-function 'gent--ensure-buffer) #'ignore)
                ((symbol-function 'shell-maker-write-output) #'ignore)
                ((symbol-function 'shell-maker-finish-output) #'ignore))
        (with-temp-buffer
          (gent-load-session file)
          (should (equal gent--messages messages)))))))

(provide 'gent-test)
;;; gent-test.el ends here
