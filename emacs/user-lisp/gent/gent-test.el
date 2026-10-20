;;; gent-test.el --- Tests for gent -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from this directory by evaluating:
;;
;;   (shell-command
;;    "emacs -Q --batch -L . -l gent-test.el -f ert-run-tests-batch-and-exit")

;;; Code:

(require 'ert)
(require 'gent)
(require 'gent-providers)

;;; Helpers

(cl-defstruct (gent-test-backend (:include gent-llm-generic-backend))
  "A backend with no dependency on the OpenAI implementation."
  (model-id "original")
  (effort "low")
  request process)

(cl-defmethod gent-llm-backend-chat ((backend gent-test-backend) &rest args)
  "Record request ARGS on BACKEND and return its configured process."
  (setf (gent-test-backend-request backend) args)
  (gent-test-backend-process backend))

(cl-defmethod gent-llm-copy-backend ((backend gent-test-backend))
  "Return a copy of BACKEND whose settings can be changed independently."
  (copy-gent-test-backend backend))

(cl-defmethod gent-llm-backend-model ((backend gent-test-backend))
  "Return the model ID used by BACKEND."
  (gent-test-backend-model-id backend))

(cl-defmethod (setf gent-llm-backend-model) (model (backend gent-test-backend))
  "Set BACKEND's model ID to MODEL."
  (setf (gent-test-backend-model-id backend) model))

(cl-defmethod gent-llm-backend-reasoning-effort ((backend gent-test-backend))
  "Return BACKEND's reasoning effort, or nil to omit it."
  (gent-test-backend-effort backend))

(cl-defmethod (setf gent-llm-backend-reasoning-effort) (effort (backend gent-test-backend))
  "Set BACKEND's reasoning effort to EFFORT, or nil to omit it."
  (setf (gent-test-backend-effort backend) effort))

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

;;; Generic dispatch

(ert-deftest gent-test-generic-chat-dispatch ()
  (let* ((process (make-pipe-process :name "gent-test-dispatch" :noquery t))
         (explicit (make-gent-test-backend :process process))
         (gent-llm-default-backend (make-gent-test-backend :process process))
         (args (list :session-id "conversation-1"
                     :messages '(((role . "system") (content . "system")))
                     :prompt "hello"
                     :tools (list (gent-llm-make-tool :name "f"))
                     :tool-choice "auto"
                     :on-token (lambda (_chunk))
                     :on-reasoning (lambda (_chunk))
                     :on-tool-calls (lambda (_calls _reason))
                     :on-done (lambda (_text))
                     :on-error (lambda (_message)))))
    (unwind-protect
        (progn
          (should (gent-llm-generic-backend-p explicit))
          (should-not (gent-openai-backend-p explicit))
          (should (eq (apply #'gent-llm-chat :backend explicit args) process))
          (should (equal (gent-test-backend-request explicit) args))
          (should-not (gent-test-backend-request gent-llm-default-backend))
          (should (eq (apply #'gent-llm-chat args) process))
          (should (equal (gent-test-backend-request gent-llm-default-backend) args))
          (setf (gent-test-backend-request gent-llm-default-backend) nil)
          (should (eq (apply #'gent-llm-chat :backend nil args) process))
          (should (equal (gent-test-backend-request gent-llm-default-backend) args)))
      (delete-process process))))

(ert-deftest gent-test-chat-no-backend ()
  (let ((gent-llm-default-backend nil))
    (should-error (gent-llm-chat :prompt "hello"))))

(ert-deftest gent-test-chat-session-id ()
  (let ((backend (make-gent-test-backend)))
    (gent-llm-chat :backend backend :prompt "hello")
    (should-not (plist-get (gent-test-backend-request backend) :session-id))
    (setf (gent-test-backend-request backend) nil)
    (dolist (id '("" 123 "session\nextra" "session\rextra"))
      (should-error (gent-llm-chat :backend backend :session-id id)))
    (should-not (gent-test-backend-request backend))))

;;; SSE parsing

(ert-deftest gent-test-filter-text ()
  (let ((result (gent-openai--filter
                 `((:pending . ,(gent-test--sse '((content . "Hel"))
                                                '((content . "lo"))))))))
    (should (equal (map-elt result :filtered) "Hello"))
    (should-not (map-elt result :pending))))

(ert-deftest gent-test-filter-partial-line ()
  (let ((result (gent-openai--filter
                 '((:pending . "data: {\"choices\":[{\"delta\":{\"content\":\"a\"}}]}\n\ndata: {\"cho")))))
    (should (equal (map-elt result :filtered) "a"))
    (should (equal (map-elt result :pending) "data: {\"cho"))))

(ert-deftest gent-test-filter-tool-calls ()
  (let* ((result (gent-openai--filter
                  `((:pending
                     . ,(gent-test--sse
                         '((tool_calls . [((index . 0) (id . "c1")
                                           (function . ((name . "read_file")
                                                        (arguments . "{\"pa"))))]))
                         '((tool_calls . [((index . 0)
                                           (function . ((arguments . "th\":\"x\"}"))))])))))))
         (calls (gent-openai--tool-calls (map-elt result :tool-calls))))
    (should (= (length calls) 1))
    (should (equal (map-elt (car calls) 'id) "c1"))
    (should (equal (map-elt (map-elt (car calls) 'function) 'arguments)
                   "{\"path\":\"x\"}"))))

;; Reasoning arrives separately from the answer.
(ert-deftest gent-test-filter-reasoning ()
  (let ((result (gent-openai--filter
                 `((:pending . ,(gent-test--sse '((reasoning_content . "Hmm, "))
                                                '((reasoning_content . "ok."))
                                                '((content . "42"))))))))
    (should (equal (map-elt result :reasoning) "Hmm, ok."))
    (should (equal (map-elt result :filtered) "42"))))

(ert-deftest gent-test-payload-reasoning-effort ()
  (let ((backend (gent-make-openai-backend :model "m" :reasoning-effort "low")))
    (should (equal (map-elt (gent-openai--payload backend nil "hi" nil nil) 'reasoning_effort)
                   "low"))
    (should (equal (map-elt (gent-openai--payload backend nil "hi" nil nil) 'model)
                   "m")))
  (let ((backend (gent-make-openai-backend :model "m" :reasoning-effort nil)))
    (should-not (assq 'reasoning_effort (gent-openai--payload backend nil "hi" nil nil)))))

(ert-deftest gent-test-backend-timeout ()
  (should (= (gent-openai-backend-timeout (gent-make-openai-backend)) 600))
  (should (= (gent-openai-backend-timeout (gent-make-openai-backend :timeout 5)) 5)))

(ert-deftest gent-test-backend-headers ()
  (should (member "Authorization: Bearer k"
                  (gent-openai--headers (gent-make-openai-backend :api-key (lambda () "k")))))
  (should-not (seq-find (lambda (h) (string-prefix-p "Authorization" h))
                        (gent-openai--headers (gent-make-openai-backend))))
  (should (member "X-A: 1" (gent-openai--headers
                            (gent-make-openai-backend :headers '("X-A: 1")))))
  (should (member "X-B: 2" (gent-openai--headers
                            (gent-make-openai-backend :headers (lambda () '("X-B: 2"))))))
  (should (equal (gent-openai--headers
                  (gent-make-openai-backend
                   :session-id-header "X-Session"
                   :headers (lambda () '("X-B: 2")))
                  "conversation")
                 '("Content-Type: application/json"
                   "X-Session: conversation" "X-B: 2"))))

(ert-deftest gent-test-backend-url ()
  (should (equal (gent-openai--url (gent-make-openai-backend :url-base "https://x/v1/"))
                 "https://x/v1/chat/completions"))
  (should (equal (gent-openai--url (gent-make-openai-backend
                                :url-base "https://x" :api-path "/custom"))
                 "https://x/custom")))

(ert-deftest gent-test-service-backends ()
  (dolist (spec '((gent-llm-make-dumile-backend "dumile"
                   "https://dumile.local/v1/chat/completions")
                  (gent-llm-make-opencode-go-backend "opencodego"
                   "https://opencode.ai/inference/go/v1/chat/completions")))
    (let* ((key "first")
           (reads 0)
           (backend (funcall (car spec) :model "m"
                             :api-key (lambda () (cl-incf reads) key))))
      (should (gent-llm-generic-backend-p backend))
      (should (gent-openai-backend-p backend))
      (should (= reads 0))
      (should (equal (gent-openai-backend-name backend) (nth 1 spec)))
      (should (equal (gent-openai--url backend) (nth 2 spec)))
      (should (= (gent-openai-backend-timeout backend) 600))
      (let ((payload (gent-openai--payload backend nil "hi" nil nil)))
        (should (equal (map-elt payload 'model) "m"))
        (should (= (map-elt payload 'max_tokens) 4096))
        (should (equal (map-elt payload 'reasoning_effort) "low"))
        (should-not (assq 'temperature payload)))
      (should (member "Authorization: Bearer first" (gent-openai--headers backend "session")))
      (setq key "second")
      (should (member "Authorization: Bearer second" (gent-openai--headers backend "session")))
      (should (= reads 2)))
    (let* ((backend (funcall (car spec) :model "custom" :api-key "k"
                             :timeout 5 :max-tokens nil
                             :temperature 0.5 :reasoning-effort nil))
           (payload (gent-openai--payload backend nil "hi" nil nil)))
      (should (= (gent-openai-backend-timeout backend) 5))
      (should (equal (map-elt payload 'model) "custom"))
      (should-not (assq 'max_tokens payload))
      (should-not (assq 'reasoning_effort payload))
      (should (= (map-elt payload 'temperature) 0.5))
      (should (member "Authorization: Bearer k" (gent-openai--headers backend "session"))))))

(ert-deftest gent-test-opencode-go-session ()
  (let ((backend (gent-llm-make-opencode-go-backend :headers '("X-Extra: yes"))))
    (should (equal (gent-openai-backend-headers backend) '("X-Extra: yes")))
    (should (equal (gent-openai--headers backend "first")
                   '("Content-Type: application/json"
                     "x-opencode-session: first" "X-Extra: yes")))
    (should (equal (gent-openai--headers backend "second")
                   '("Content-Type: application/json"
                     "x-opencode-session: second" "X-Extra: yes")))
    (should (equal (gent-openai--headers (gent-llm-copy-backend backend) "first")
                   (gent-openai--headers backend "first"))))
  (let ((backend (gent-llm-make-dumile-backend)))
    (should (equal (gent-openai--headers backend "session")
                   (gent-openai--headers backend)))))

(ert-deftest gent-test-opencode-go-requires-session ()
  (let ((backend (gent-llm-make-opencode-go-backend))
        (writes 0) (processes 0))
    (cl-letf (((symbol-function 'gent-openai--write-payload)
               (lambda (&rest _) (cl-incf writes)))
              ((symbol-function 'make-process)
               (lambda (&rest _) (cl-incf processes))))
      (dolist (id '(nil "" "id\nextra"))
        (should-error (gent-llm-backend-chat backend :session-id id :prompt "hello")))
      (should (string-match-p
               "requires :session-id"
               (error-message-string
                (should-error (gent-llm-chat :backend backend :prompt "hello")))))
      (should (= writes 0))
      (should (= processes 0)))))

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
    (cl-letf (((symbol-function 'gent--ensure-buffer) #'ignore))
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
          (session-id nil)
          (messages '(((role . "user") (content . "hi"))
                      ((role . "assistant") (content . "")
                       (tool_calls . [((id . "c") (function . ((name . "x"))))])))))
      (with-temp-buffer
        (setq gent--messages messages)
        (gent-save-session file)
        (setq session-id gent--session-id)
        (should (stringp session-id)))
      (with-temp-buffer
        (gent-mode)
        (gent--draw nil)
        (gent-load-session file)
        (should (equal gent--session-id session-id))
        (should (equal gent--messages messages))
        ;; The loaded conversation is shown above the prompt.
        (should (string-search "hi" (buffer-substring (point-min) gent--output-marker)))
        (should (string-search "⚙ x" (buffer-string)))))))

(ert-deftest gent-test-session-load-legacy ()
  (gent-test--with-project
    (let ((file (expand-file-name "legacy.eld"))
          (messages '(((role . "user") (content . "hello")))))
      (with-temp-file file
        (prin1 `((version . 1) (messages . ,messages)) (current-buffer)))
      (with-temp-buffer
        (gent-mode)
        (gent--draw nil)
        (setq gent--session-id "previous-conversation")
        (gent-load-session file)
        (should (equal gent--messages messages))
        (should (stringp gent--session-id))
        (should-not (string-empty-p gent--session-id))
        (should-not (equal gent--session-id "previous-conversation"))))))

;;; Buffer

(defmacro gent-test--with-gent-buffer (&rest body)
  "Run BODY in a fresh `gent-mode' buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (gent-mode)
     (setq gent--root default-directory)
     (gent--draw nil)
     ,@body))

(defun gent-test--type (text)
  "Type TEXT into the input area and send it."
  (goto-char (point-max))
  (insert text)
  (gent-send))

(defun gent-test--wait-idle ()
  "Wait until the current gent buffer is not busy."
  (with-timeout (5 (error "Timed out waiting for the turn"))
    (while gent--busy
      (accept-process-output nil 0.05))))

(ert-deftest gent-test-conversation-session-lifecycle ()
  (gent-test--with-project
    (gent-test--write-file "a.txt" "hello")
    (let* ((gent-llm-default-backend (make-gent-test-backend))
           (gent-backends (list (cons "other" (make-gent-test-backend :model-id "other"))))
           (ids nil)
           (fake (gent-test--fake-chat
                  (list `((nil ,(gent-test--tool-call "c1" "read_file" '((path . "a.txt"))))
                          ("first") ("settings changed") ("late")
                          ("continued") ("reset") ("other buffer")))
                  (list nil))))
      (cl-letf (((symbol-function 'gent-llm-chat)
                 (lambda (&rest args)
                   (push (plist-get args :session-id) ids)
                   (apply fake args))))
        (gent-test--with-gent-buffer
          (should-not gent--session-id)
          (gent-test--type "read a.txt")
          (gent-test--wait-idle)
          (should (= (length ids) 2))
          (should (stringp gent--session-id))
          (should-not (string-empty-p gent--session-id))
          (should (equal (car ids) (cadr ids)))
          (gent-set-backend "other")
          (gent-set-reasoning-effort "high")
          (gent-test--type "after settings")
          (gent-test--wait-idle)
          ;; An interrupted request and its replacement keep the conversation ID.
          (gent-test--type "interrupt me")
          (gent-interrupt)
          (gent-test--type "continue")
          (gent-test--wait-idle)
          (should (= (length ids) 5))
          (should (cl-every (lambda (id) (equal id gent--session-id)) ids))
          ;; Sharing the same backend does not share conversation IDs.
          (setq gent--backend gent-llm-default-backend)
          (gent-reset)
          (should-not gent--session-id)
          (gent-test--type "new conversation")
          (gent-test--wait-idle)
          (should-not (equal (car ids) (cadr ids))))
        (gent-test--with-gent-buffer
          (gent-test--type "separate buffer")
          (gent-test--wait-idle))
        (should (= (length ids) 7))
        (should (= (length (delete-dups (copy-sequence ids))) 3))))))

(ert-deftest gent-test-batch-session-id ()
  (gent-test--with-project
    (let* ((ids nil)
           (fake (gent-test--fake-chat (list '(("one") ("two"))) (list nil))))
      (cl-letf (((symbol-function 'gent-llm-chat)
                 (lambda (&rest args)
                   (push (plist-get args :session-id) ids)
                   (apply fake args))))
        (with-temp-buffer
          (let ((standard-output (current-buffer)))
            (gent-batch-run "first")
            (gent-batch-run "second"))))
      (should (= (length ids) 2))
      (should (cl-every (lambda (id) (and (stringp id) (not (string-empty-p id)))) ids))
      (should-not (equal (car ids) (cadr ids))))))

(ert-deftest gent-test-set-backend-copies-configuration ()
  (let* ((go (gent-llm-make-opencode-go-backend :model "go-model" :api-key "go-key"))
         (dumile (gent-llm-make-dumile-backend :model "dumile-model" :api-key "dumile-key"))
         (gent-backends (list (cons "go" go) (cons "dumile" dumile)))
         (gent-llm-default-backend go))
    (gent-test--with-gent-buffer
      (gent-set-backend "go")
      (should-not (eq gent--backend go))
      (let ((previous gent--backend))
        (gent-set-backend "dumile")
        (should-not (eq gent--backend dumile))
        (should (equal gent--backend-name "dumile"))
        (should (equal (gent-llm-backend-model gent--backend) "dumile-model"))
        (should (equal (gent-openai--url gent--backend)
                       "https://dumile.local/v1/chat/completions"))
        (should (equal (gent-openai--headers gent--backend "session")
                       '("Content-Type: application/json" "Authorization: Bearer dumile-key")))
        (should (equal (gent-llm-backend-model previous) "go-model"))
        (should (member "x-opencode-session: session"
                        (gent-openai--headers previous "session"))))
      (gent-set-reasoning-effort "high")
      (should (equal (gent-llm-backend-reasoning-effort dumile) "low")))
    (should (eq gent-llm-default-backend go))
    (should (equal (gent-llm-backend-model go) "go-model"))
    (gent-test--with-gent-buffer
      (should-not gent--backend)
      (should-not gent--backend-name)
      (should (eq (gent--current-backend) go)))))

(ert-deftest gent-test-set-backend-interactive ()
  (let* ((gent-backends (list (cons "local" (make-gent-test-backend :model-id "m"))))
         (gent-llm-default-backend nil)
         (current-prefix-arg '(4))
         (reads 0))
    (gent-test--with-gent-buffer
      (should (eq (keymap-lookup gent-mode-map "C-c C-m") #'gent-set-backend))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &optional _predicate require-match &rest _)
                   (cl-incf reads)
                   (should (equal prompt "Backend: "))
                   (should (eq collection gent-backends))
                   (should require-match)
                   "local")))
        (call-interactively #'gent-set-backend))
      (should (= reads 1))
      (should (equal (gent-llm-backend-model gent--backend) "m")))))

(ert-deftest gent-test-set-backend-invalid-selection ()
  (let ((gent-backends (list (cons "local" (make-gent-test-backend)))))
    (gent-test--with-gent-buffer
      (gent-set-backend "local")
      (let ((previous gent--backend))
        (should-error (gent-set-backend "missing") :type 'user-error)
        (should (eq gent--backend previous))
        (should (equal gent--backend-name "local")))
      (let ((gent-backends nil))
        (should-error (call-interactively #'gent-set-backend) :type 'user-error)))))

(ert-deftest gent-test-generic-backend-settings ()
  (dolist (backend (list (make-gent-test-backend)
                         (gent-make-openai-backend :model "original"
                                                   :url-base "https://test/v1")))
    (let ((gent-llm-default-backend nil)
          (gent-backends (list (cons "local" backend))))
      (gent-test--with-gent-buffer
        (gent-set-backend "local")
        (should-not (eq gent--backend backend))
        (should (equal (gent-llm-backend-model gent--backend) "original"))
        (let ((previous gent--backend))
          (gent-set-reasoning-effort "high")
          (should-not (eq gent--backend previous))
          (should (equal (gent-llm-backend-reasoning-effort gent--backend) "high"))
          (should (equal (gent-llm-backend-reasoning-effort previous) "low")))
        (should (equal (gent-llm-backend-model gent--backend) "original"))
        (should (equal (gent-llm-backend-model backend) "original"))
        (should (equal (gent-llm-backend-reasoning-effort backend) "low"))))))

(ert-deftest gent-test-generic-backend-session ()
  (gent-test--with-project
    (let ((gent-llm-default-backend (gent-make-openai-backend :model "original"))
          (gent-backends (list (cons "saved" (make-gent-test-backend :model-id "saved"))))
          (file (expand-file-name "session.eld")))
      (gent-test--with-gent-buffer
        (gent-set-backend "saved")
        (setq gent--messages '(((role . "user") (content . "hello"))))
        (gent-save-session file))
      (gent-test--with-gent-buffer
        (gent-load-session file)
        (should (gent-test-backend-p gent--backend))
        (should-not (eq gent--backend gent-llm-default-backend))
        (should-not (eq gent--backend (alist-get "saved" gent-backends nil nil #'equal)))
        (should (equal gent--backend-name "saved"))
        (should (equal (gent-llm-backend-model gent--backend) "saved"))
        (should (equal (gent-llm-backend-model gent-llm-default-backend) "original"))
        (should (equal gent--messages '(((role . "user") (content . "hello")))))))))

(ert-deftest gent-test-session-load-legacy-model ()
  (gent-test--with-project
    (let ((gent-llm-default-backend (make-gent-test-backend))
          (file (expand-file-name "legacy-model.eld")))
      (with-temp-file file
        (prin1 '((version . 1) (model . "saved")) (current-buffer)))
      (gent-test--with-gent-buffer
        (gent-load-session file)
        (should (equal (gent-llm-backend-model gent--backend) "saved"))
        (should-not gent--backend-name)
        (should (equal (gent-llm-backend-model gent-llm-default-backend) "original"))))))

(ert-deftest gent-test-session-load-missing-backend ()
  (gent-test--with-project
    (let ((gent-backends nil)
          (file (expand-file-name "missing-backend.eld")))
      (with-temp-file file
        (prin1 '((version . 1) (backend-name . "missing") (model . "saved"))
               (current-buffer)))
      (gent-test--with-gent-buffer
        (setq gent--backend (make-gent-test-backend)
              gent--messages '(((role . "user") (content . "keep")))
              gent--session-id "current")
        (let ((previous gent--backend))
          (should-error (gent-load-session file) :type 'user-error)
          (should (eq gent--backend previous))
          (should (equal gent--session-id "current"))
          (should (equal gent--messages '(((role . "user") (content . "keep"))))))))))

(ert-deftest gent-test-buffer-send ()
  (gent-test--with-project
    (cl-letf (((symbol-function 'gent-llm-chat)
               (gent-test--fake-chat (list '(("Hi there"))) (list nil))))
      (gent-test--with-gent-buffer
        (gent-test--type "hello")
        (should gent--busy)
        ;; Input stays editable while the model answers.
        (insert "next")
        (gent-test--wait-idle)
        (let ((transcript (buffer-substring-no-properties (point-min) gent--output-marker)))
          (should (string-match-p "gent> hello\n\nHi there\n\\'" transcript)))
        (should (equal (gent--input) "next"))
        (should (get-text-property (point-min) 'read-only))
        (should-error (progn (goto-char (point-min)) (insert "x")) :type 'text-read-only)
        (should (equal (mapcar (lambda (m) (map-elt m 'role)) gent--messages)
                       '("user" "assistant")))))))

(ert-deftest gent-test-buffer-queue ()
  (gent-test--with-project
    (let ((requests (list nil)))
      (cl-letf (((symbol-function 'gent-llm-chat)
                 (gent-test--fake-chat (list '(("one") ("two"))) requests)))
        (gent-test--with-gent-buffer
          (gent-test--type "first")
          (gent-test--type "second")
          (should (equal gent--queue '("second")))
          (gent-test--wait-idle)
          (should-not gent--queue)
          (should (= (length (car requests)) 2))
          (should (string-match-p "first\n\none\n\ngent> second\n\ntwo"
                                  (buffer-substring-no-properties (point-min) gent--output-marker))))))))

(ert-deftest gent-test-buffer-history ()
  (gent-test--with-gent-buffer
    (ring-insert gent--history "a")
    (ring-insert gent--history "b")
    (gent-previous-input 1)
    (should (equal (gent--input) "b"))
    (gent-previous-input 1)
    (should (equal (gent--input) "a"))
    (gent-previous-input 1)
    (should (equal (gent--input) "a"))
    (gent-next-input 1)
    (should (equal (gent--input) "b"))
    (gent-next-input 1)
    (should (equal (gent--input) ""))))

;;; HTTP

(defmacro gent-test--with-fake-curl (script &rest body)
  "Run BODY with `gent-llm-curl-executable' running shell SCRIPT.
The script sees curl's arguments and gets the config on stdin."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "gent-curl" t))
          (gent-llm-curl-executable (expand-file-name "curl" dir)))
     (unwind-protect
         (progn
           (with-temp-file gent-llm-curl-executable
             (insert "#!/bin/sh\n" ,script))
           (set-file-modes gent-llm-curl-executable #o755)
           ,@body)
       (delete-directory dir t))))

(defun gent-test--chat (&optional backend session-id)
  "Run `gent-llm-chat' and return (TOKENS CALLS DONE ERROR).
Use BACKEND when non-nil, otherwise create a test OpenAI backend.
Pass SESSION-ID as the conversation ID."
  (let ((tokens nil) (calls nil) (done nil) (error nil) (finished nil))
    (gent-llm-chat
     :backend (or backend (gent-make-openai-backend
                          :model "m"
                          :url-base "http://x" :api-key "k"))
     :session-id session-id
     :prompt "hi"
     :on-token (lambda (token) (push token tokens))
     :on-tool-calls (lambda (c _) (setq calls c))
     :on-done (lambda (text) (setq done text finished t))
     :on-error (lambda (message) (setq error message finished t)))
    (with-timeout (5 (error "Timed out waiting for curl"))
      (while (not finished)
        (accept-process-output nil 0.05)))
    (list (nreverse tokens) calls done error)))

(ert-deftest gent-test-curl-stream ()
  (gent-test--with-fake-curl
      (format "cat > /dev/null\nprintf '%%s' '%s'\n"
              (gent-test--sse '((content . "Hel"))
                              '((content . "lo"))
                              '((tool_calls . [((index . 0) (id . "c1")
                                                (function . ((name . "f")
                                                             (arguments . "{}"))))]))))
    (pcase-let ((`(,tokens ,calls ,done ,error) (gent-test--chat)))
      (should (equal (string-join tokens) "Hello"))
      (should (equal done "Hello"))
      (should (equal (map-elt (car calls) 'id) "c1"))
      (should-not error))))

(ert-deftest gent-test-curl-config ()
  ;; The key goes through stdin, not the command line.
  (gent-test--with-fake-curl
      "case \"$*\" in *Bearer*) exit 9;; esac
grep -q 'header = \"Authorization: Bearer k\"' || exit 7
"
    (should-not (nth 3 (gent-test--chat)))))

(ert-deftest gent-test-curl-session-header ()
  (gent-test--with-fake-curl
      (concat "case \"$*\" in *conversation*) exit 9;; esac\n"
              "grep -q 'header = \"x-opencode-session: conversation\"' || exit 7\n")
    (should-not (nth 3 (gent-test--chat
                       (gent-llm-make-opencode-go-backend :model "m")
                       "conversation")))))

(ert-deftest gent-test-curl-error ()
  (gent-test--with-fake-curl "cat > /dev/null; echo 'bad key'; exit 22\n"
    (pcase-let ((`(,_ ,_ ,done ,error) (gent-test--chat)))
      (should-not done)
      (should (string-search "exit 22" error))
      (should (string-search "bad key" error)))))

(provide 'gent-test)
;;; gent-test.el ends here
