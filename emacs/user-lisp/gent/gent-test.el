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

(defvar gent-test--trusted nil
  "Trust predicate `gent-test--call' passes to `gent-tools-call'.")

(defun gent-test--answering (answer)
  "Return a confirm function that answers every request with ANSWER."
  (lambda (_name _kind _text respond) (funcall respond answer)))

(defun gent-test--call (name args &optional root)
  "Call tool NAME with ARGS (an alist) in ROOT and wait for its result.
Approval requests go to `gent-test--confirm'."
  (let ((result nil))
    (gent-tools-call name (json-encode args) (or root default-directory)
                     (lambda (r) (setq result r))
                     gent-test--confirm gent-test--trusted)
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

(ert-deftest gent-test-filter-null-fields ()
  ;; Explicit nulls should behave like absent fields.
  (let ((result
         (gent-openai--filter
          `((:pending
             . ,(concat
                 "data: {\"choices\":null}\n\n"
                 "data: {\"choices\":[{\"delta\":null,\"finish_reason\":null}]}\n\n"
                 "data: {\"choices\":[{\"delta\":{\"content\":null,"
                 "\"reasoning_content\":null,\"tool_calls\":null},"
                 "\"finish_reason\":null}]}\n\n"
                 "data: {\"choices\":[{\"delta\":{\"content\":\"Hello\"},"
                 "\"finish_reason\":null}]}\n\n"
                 "data: [DONE]\n\n"))))))
    (should (equal (map-elt result :filtered) "Hello"))
    (should (equal (map-elt result :reasoning) ""))
    (should (equal (map-elt result :tool-calls) []))
    (should-not (map-elt result :finish-reason))
    (should-not (map-elt result :pending))))

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

;; Some servers report failures inside a 200 stream.
(ert-deftest gent-test-filter-error ()
  (let ((result (gent-openai--filter
                 `((:pending
                    . ,(concat
                        (gent-test--sse '((content . "a")))
                        "data: {\"error\":{\"message\":\"overloaded\"}}\n\n"
                        "data: {\"error\":\"second\"}\n\n"
                        "data: {\"error\":null}\n\n"))))))
    (should (equal (map-elt result :filtered) "a"))
    (should (equal (map-elt result :error) "overloaded")))
  (should-not (map-elt (gent-openai--filter
                        `((:pending . ,(gent-test--sse '((content . "a"))))))
                       :error)))

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

(ert-deftest gent-test-normalize-reasoning ()
  (dolist (reasoning (list (propertize "Think.\nThen act." 'face 'bold) "" nil))
    (let* ((normalized (gent-llm--normalize-message
                        `((role . "assistant") (content . "")
                          (reasoning_content . ,reasoning))))
           (value (map-elt normalized 'reasoning_content)))
      (should (equal value reasoning))
      (if reasoning
          (should-not (text-properties-at 0 value))
        (should-not (assq 'reasoning_content normalized)))))
  (should (= (gent--estimate-tokens
              '(((role . "assistant") (content . "1234")
                 (reasoning_content . "12345678"))))
             3)))

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

(ert-deftest gent-test-tool-args-schema ()
  (let ((gent-tools--registry nil))
    (should (equal
             (gent-define-tool
              "example"
              :args '((text :type string :description "Input text")
                      (count :type integer :optional t :default 2)
                      (enabled :type boolean :optional t :default :false))
              :handler #'ignore)
             "example"))
    (let* ((tool (gent-tools-get "example"))
           (schema (gent-tool-parameters tool))
           (properties (map-elt schema 'properties))
           (wire (gent-tools--parse-arguments (json-encode schema))))
      (should (equal (map-elt schema 'type) "object"))
      (should (equal (map-elt schema 'required) ["text"]))
      (should (equal (map-elt (map-elt properties 'text) 'description) "Input text"))
      (should (equal (map-elt (map-elt properties 'count) 'type) "integer"))
      (should (= (map-elt (map-elt properties 'count) 'default) 2))
      (should (eq (map-elt wire 'additionalProperties) :false))
      (should (eq (map-elt (map-elt (map-elt wire 'properties) 'enabled) 'default)
                  :false)))
    ;; Re-registering replaces the definition, including its declarations.
    (gent-define-tool "example" :args nil :handler #'ignore)
    (should (equal (gent-tools-names) '("example")))
    (let ((schema (gent-tool-parameters (gent-tools-get "example"))))
      (should (equal (map-elt schema 'required) []))
      (should (hash-table-p (map-elt schema 'properties)))
      (should (string-match-p "\"properties\":{}" (json-encode schema))))))

(ert-deftest gent-test-tool-args-invalid-declarations ()
  (let ((gent-tools--registry nil))
    (gent-define-tool "example" :handler #'ignore)
    (let ((original (gent-tools-get "example")))
      (dolist (args '(bad [text] (("text" :type string)) ((:text :type string))
                      ((nil :type string)) ((t :type string))
                      ((text)) ((text :type object)) ((text :type array))
                      ((text :type "string")) ((text :type string :description 1))
                      ((text :type string :optional yes))
                      ((text :type string :default "required"))
                      ((text :type integer :optional t :default "wrong"))
                      ((text :type boolean :optional t :default nil))
                      ((text :type string :enum ("one" "two")))
                      ((text :type string :type integer))
                      ((text :type string :optional))
                      ((text :type string) (text :type integer))
                      ((text :type string) . bad)))
        (should-error (gent-define-tool "example" :args args :handler #'ignore))
        (should (eq (gent-tools-get "example") original))))
    (should-error (gent-define-tool "legacy" :parameters '((type . "object"))
                                   :handler #'ignore))))

(ert-deftest gent-test-tool-args-defaults-and-preview ()
  (let ((gent-tools--registry nil)
        preview-args handler-args result)
    (gent-define-tool
     "example"
     :args '((text :type string)
             (count :type integer :optional t :default 2)
             (enabled :type boolean :optional t :default :false)
             (other :type string :optional t))
     :preview (lambda (args)
                (setq preview-args args)
                (cons 'command "example"))
     :handler (lambda (args done)
                (setq handler-args args)
                (funcall done "ok")))
    (let ((input '((text . "") (count . 0) (enabled . t))))
      (gent-tools-call "example" input default-directory
                       (lambda (value) (setq result value))
                       (gent-test--answering t))
      (should (equal result "ok"))
      (should (eq preview-args handler-args))
      (should (equal handler-args input))
      (should-not (eq handler-args input)))
    (gent-tools-call "example" "{\"text\":\"hello\"}" default-directory
                     (lambda (value) (setq result value))
                     (gent-test--answering t))
    (should (equal handler-args '((text . "hello") (count . 2) (enabled . :false))))
    (should-not (assq 'other handler-args))))

(ert-deftest gent-test-tool-args-types-and-validation ()
  (let ((gent-tools--registry nil)
        (effects 0)
        received result)
    (dolist (case '((string "\"hello\"" "hello")
                    (integer "0" 0)
                    (number "1.5" 1.5)
                    (number "2" 2)
                    (boolean "true" t)
                    (boolean "false" :false)))
      (gent-define-tool
       "example" :args `((value :type ,(car case)))
       :preview (lambda (_args) (cl-incf effects) nil)
       :handler (lambda (args done)
                  (cl-incf effects)
                  (setq received (map-elt args 'value))
                  (funcall done "ok")))
      (gent-tools-call "example" (format "{\"value\":%s}" (cadr case))
                       default-directory (lambda (value) (setq result value)))
      (should (equal result "ok"))
      (should (equal received (nth 2 case)))
      (setq effects 0)
      (dolist (input '("{}" "{\"value\":null}" "{\"value\":[]}"
                       "{\"value\":{}}" "{\"value\":true,\"extra\":1}"))
        (gent-tools-call "example" input default-directory
                         (lambda (value) (setq result value)))
        (should (string-prefix-p "Error:" result))
        (should (zerop effects))))
    ;; A non-integer number must not pass integer validation.
    (gent-define-tool "example" :args '((value :type integer))
                      :handler (lambda (_args _done) (cl-incf effects)))
    (gent-tools-call "example" "{\"value\":1.5}" default-directory
                     (lambda (value) (setq result value)))
    (should (string-prefix-p "Error:" result))
    (should (zerop effects))))

(ert-deftest gent-test-tool-args-json-readers ()
  (let ((gent-tools--registry nil)
        received result)
    (gent-define-tool
     "example"
     :args '((enabled :type boolean :optional t :default t))
     :handler (lambda (args done)
                (setq received args)
                (funcall done "ok")))
    (gent-define-tool "empty" :handler (lambda (args done)
                                        (should-not args)
                                        (funcall done "empty")))
    (dolist (input '("{}" " \n{\"enabled\":false}\t"))
      (gent-tools-call "example" input default-directory
                       (lambda (value) (setq result value)))
      (should (equal result "ok"))
      (should (equal received (if (equal input "{}")
                                 '((enabled . t)) '((enabled . :false))))))
    (gent-tools-call "empty" "{}" default-directory
                     (lambda (value) (setq result value)))
    (should (equal result "empty"))
    (dolist (input '("null" "false" "[]" "[{}]" "\"text\"" "1" ""
                     " " "{bad}" "{} trailing" "{} {}"))
      (gent-tools-call "example" input default-directory
                       (lambda (value) (setq result value)))
      (should (string-prefix-p "Error:" result)))))

(ert-deftest gent-test-tool-args-async-completion ()
  (let ((gent-tools--registry nil)
        complete results)
    (gent-define-tool
     "example" :args '((text :type string))
     :handler (lambda (args done)
                ;; Retain completion just as a process sentinel would.
                (setq complete (lambda () (funcall done (map-elt args 'text))))
                "ignored return value"))
    (gent-tools-call "example" "{\"text\":\"later\"}" default-directory
                     (lambda (value) (push value results)))
    (should-not results)
    (funcall complete)
    (funcall complete)
    (should (equal results '("later")))))

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
  ;; The temporary project lives under /tmp, which is itself an allowed root,
  ;; so only paths that escape both the project and /tmp are refused.
  (gent-test--with-project
    (should (string-prefix-p "Error: Path"
                             (gent-test--call "read_file" '((path . "/etc/passwd")))))
    (should (string-prefix-p "Error: Path"
                             (gent-test--call "read_file" '((path . "../../etc/passwd")))))
    ;; Absolute paths under /tmp are in scope even when they are outside the
    ;; project, as when the model writes a scratch file there.
    (let ((outside (make-temp-file "gent-test-outside")))
      (unwind-protect
          (progn
            (with-temp-file outside (insert "outside sibling"))
            (should (string-search "outside sibling"
                                   (gent-test--call
                                    "read_file" `((path . ,outside))))))
        (delete-file outside)))))

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
    (let* ((asked 0)
           (trusted nil)
           (gent-test--trusted (lambda (name) (member name trusted)))
           (gent-test--confirm (lambda (name _kind _text respond)
                                 (cl-incf asked)
                                 (push name trusted)
                                 (funcall respond 'always))))
      (gent-test--call "run_shell" '((command . "true")))
      (gent-test--call "run_shell" '((command . "true")))
      (should (= asked 1)))
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

(defun gent-test--shell-processes (command)
  "Return the live processes running shell COMMAND."
  (seq-filter (lambda (process)
                (and (process-live-p process)
                     (equal (car (last (process-command process))) command)))
              (process-list)))

(ert-deftest gent-test-cancel-running-call ()
  (gent-test--with-project
    (let ((gent-tools-trusted '("run_shell"))
          slow-result fast-result)
      (let ((cancel (gent-tools-call "run_shell" "{\"command\": \"sleep 5\"}"
                                     default-directory
                                     (lambda (r) (setq slow-result r)))))
        (gent-tools-call "run_shell" "{\"command\": \"sleep 0.2; echo fast\"}"
                         default-directory (lambda (r) (setq fast-result r)))
        (should (gent-test--shell-processes "sleep 5"))
        (funcall cancel)
        (should-not (gent-test--shell-processes "sleep 5"))
        ;; Other calls keep running, and the cancelled call never answers.
        (with-timeout (5 (error "Timed out"))
          (while (not fast-result) (accept-process-output nil 0.05)))
        (should (equal fast-result "fast\n[exit code 0]"))
        (should-not slow-result)
        ;; Cancelling again, or after the call finished, does nothing.
        (funcall cancel)))))

(ert-deftest gent-test-cancel-pending-call ()
  (gent-test--with-project
    (let* ((respond nil)
           (result nil)
           (cancel (gent-tools-call "run_shell" "{\"command\": \"touch x\"}"
                                    default-directory
                                    (lambda (r) (setq result r))
                                    (lambda (_name _kind _text r) (setq respond r)))))
      (should respond)
      (funcall cancel)
      (funcall respond t)
      (accept-process-output nil 0.2)
      (should-not result)
      (should-not (file-exists-p "x")))))

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
        (setq gent--current-session (make-gent--session :root default-directory))
        (gent--start-turn
         (current-buffer) prompt
         (list (cons :write-output (lambda (text &optional _) (setq output (concat output text))))
               (cons :finish-output (lambda (ok)
                                      (should (eq (gent--state) 'idle))
                                      (setq success ok done t)))))
        (while (not done)
          (accept-process-output nil 0.05))
        (list success output (gent--session-messages gent--current-session) (reverse (car requests)))))))

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
         (setq gent--current-session (make-gent--session :root default-directory))
         (gent--start-turn
          (current-buffer) ,prompt
          (list (cons :write-output (lambda (text &optional _) (setq output (concat output text))))
                (cons :finish-output (lambda (ok) (setq success ok done t)))))
         (with-timeout (5 (error "Timed out waiting for approval"))
           (while (and (not done) (not (gent--session-pending gent--current-session)))
             (accept-process-output nil 0.05)))
         (should (eq (gent--state) 'awaiting-approval))
         ,@body
         (with-timeout (5 (error "Timed out waiting for the turn"))
           (while (not done)
             (accept-process-output nil 0.05)))
         (should (eq (gent--state) 'idle))
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
        (should (equal (plist-get (gent--session-pending gent--current-session) :name) "edit_file"))
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
        (should (eq (gent--state) 'running))
        (should-not (gent--pending-in-buffer gent-buffer)))
      (with-timeout (5 (error "Timed out"))
        (while (not done) (accept-process-output nil 0.05)))
      (should success)
      (should (equal (gent-test--read-file "a.txt") "new\n"))
      (should (string-search "Done." output))
      (should (equal (map-elt (nth 2 (gent--session-messages gent--current-session)) 'content)
                     "Edited a.txt")))))

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
        (setq tool-message (nth 2 (gent--session-messages gent--current-session))))
      (should (string-search "use y instead" (map-elt tool-message 'content))))))

(ert-deftest gent-test-interrupt-clears-pending ()
  (gent-test--with-project
    (cl-letf (((symbol-function 'gent--ensure-buffer) #'ignore))
      (gent-test--with-paused-loop
          `((nil ,(gent-test--tool-call "c1" "run_shell" '((command . "touch x")))))
          "touch"
        (gent-interrupt)
        (should-not (gent--session-pending gent--current-session))
        (should (equal (map-elt (car (last (gent--session-messages gent--current-session))) 'content)
                       "Cancelled by the user."))
        (should-error (gent-approve) :type 'user-error)
        ;; The interrupted turn never finishes on its own.
        (setq done t))
      (should-not (file-exists-p "x")))))

(ert-deftest gent-test-interrupt-cancels-running-tool ()
  (gent-test--with-project
    (cl-letf (((symbol-function 'gent--ensure-buffer) #'ignore))
      (gent-test--with-paused-loop
          `((nil ,(gent-test--tool-call "c1" "run_shell" '((command . "sleep 5")))))
          "sleep"
        (gent-approve)
        (should (gent-test--shell-processes "sleep 5"))
        (should (gent--session-tool-cancel gent--current-session))
        (gent-interrupt)
        (should-not (gent-test--shell-processes "sleep 5"))
        (should-not (gent--session-tool-cancel gent--current-session))
        (should (equal (map-elt (car (last (gent--session-messages gent--current-session))) 'content)
                       "Cancelled by the user."))
        ;; The interrupted turn never finishes on its own.
        (setq done t)))))

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
        (setq gent--current-session (make-gent--session))
        (let ((done nil) (output ""))
          (gent--start-turn
           (current-buffer) "hi"
           (list (cons :write-output (lambda (text &optional _) (setq output (concat output text))))
                 (cons :finish-output (lambda (_) (setq done t)))))
          (while (not done) (accept-process-output nil 0.05))
          ;; One visible header, then hidden reasoning, then the answer.
          (should (string-match "Reasoning.*\n\\(Let me think\\.\n\n\\)Answer\\.\\'" output))
          (should (get-text-property (match-beginning 1) 'gent-details output))
          (should-not (get-text-property (match-beginning 0) 'gent-details output))
          (should-not (get-text-property (match-end 1) 'gent-details output))
          ;; Reasoning is stored separately from the answer for continuation.
          (should (equal (map-elt (car (last (gent--session-messages gent--current-session))) 'content)
                         "Answer."))
          (should (equal (map-elt (car (last (gent--session-messages gent--current-session)))
                                 'reasoning_content)
                         "Let me think.")))))))

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
        (setq gent--current-session (make-gent--session))
        (let ((done nil) (success t) (output ""))
          (gent--start-turn
           (current-buffer) "hi"
           (list (cons :write-output (lambda (text &optional _) (setq output (concat output text))))
                 (cons :finish-output (lambda (ok) (setq success ok done t)))))
          (while (not done) (accept-process-output nil 0.05))
          (should (eq (gent--state) 'idle))
          (should-not success)
          (should (string-search "boom" output)))))))

(ert-deftest gent-test-loop-cancel ()
  (gent-test--with-project
    (let ((finished nil))
      (cl-letf (((symbol-function 'gent-llm-chat)
                 (gent-test--fake-chat (list '(("late"))) (list nil))))
        (with-temp-buffer
          (setq gent--current-session (make-gent--session))
          (gent--start-turn
           (current-buffer) "hi"
           (list (cons :write-output #'ignore)
                 (cons :finish-output (lambda (_) (setq finished t)))))
          ;; Cancel before the fake response arrives.
          (cl-incf (gent--session-turn gent--current-session))
          (accept-process-output nil 0.1)
          (should-not finished)
          (should (= (length (gent--session-messages gent--current-session)) 1)))))))

;;; Buffer

(defmacro gent-test--with-gent-buffer (&rest body)
  "Run BODY in a fresh `gent-mode' buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (gent-mode)
     (setf (gent--session-root gent--current-session) default-directory)
     (gent--draw nil)
     ,@body))

(defun gent-test--type (text)
  "Type TEXT into the input area and send it."
  (goto-char (point-max))
  (insert text)
  (gent-send))

(defun gent-test--wait-idle ()
  "Wait until the current gent session is idle."
  (with-timeout (5 (error "Timed out waiting for the turn"))
    (while (not (eq (gent--state) 'idle))
      (accept-process-output nil 0.05))))

(ert-deftest gent-test-toggle-details-survives-fontification ()
  (gent-test--with-gent-buffer
    (let ((source (current-buffer)))
      (gent--insert-output source (gent--format-reasoning-header))
      (gent--insert-output source (gent--format-reasoning "Thinking "))
      (gent--insert-output source (gent--format-reasoning "more.\n"))
      (gent--insert-output source "Answer.\n")
      (gent--insert-output source (gent--format-tool-result "Tool output."))
      (insert "draft input")
      (let ((reasoning (save-excursion (goto-char (point-min)) (search-forward "Thinking") (1- (point))))
            (answer (save-excursion (goto-char (point-min)) (search-forward "Answer") (1- (point))))
            (tool (save-excursion (goto-char (point-min)) (search-forward "Tool output") (1- (point))))
            (input gent--input-marker)
            (toggle (key-binding (kbd "C-c C-t"))))
        (should (eq toggle #'gent-toggle-details))
        ;; Exercise Markdown's removal of managed text properties explicitly,
        ;; even when batch mode has not enabled font-lock automatically.
        (let ((inhibit-read-only t))
          (font-lock-unfontify-region (point-min) (point-max))
          (font-lock-ensure))
        (dotimes (_ 2)
          (should (invisible-p reasoning))
          (should (invisible-p tool))
          (should-not (invisible-p answer))
          (should-not (invisible-p input))
          (call-interactively toggle)
          (should-not (invisible-p reasoning))
          (should-not (invisible-p tool))
          (call-interactively toggle))
        ;; New output obeys the visibility setting, and adjacent visible text
        ;; never becomes part of a reasoning overlay.
        (call-interactively toggle)
        (gent--insert-output source (gent--format-reasoning "Later reasoning.\n"))
        (gent--insert-output source "Later answer.\n")
        (goto-char (point-min))
        (search-forward "Later reasoning")
        (should-not (invisible-p (1- (point))))
        (call-interactively toggle)
        (should (invisible-p (1- (point))))
        (search-forward "Later answer")
        (should-not (invisible-p (1- (point))))
        (should (equal (gent--input) "draft input"))))))

(ert-deftest gent-test-dry-run-matches-send-without-side-effects ()
  (gent-test--with-project
    (gent-test--write-file "AGENTS.md" "Include these project instructions.")
    (let* ((gent-llm-default-backend
            (gent-make-openai-backend
             :model "preview-model" :url-base "https://example.test/v1"
             :reasoning-effort "high" :max-tokens 123 :temperature 0.5
             :session-id-header "x-session"
             :api-key (lambda () (ert-fail "Preview resolved API credentials"))
             :headers (lambda () (ert-fail "Preview resolved HTTP headers"))))
           (gent-context-keep-tool-results 0)
           (preview nil)
           (sent nil))
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer))
                    ((symbol-function 'gent-llm-chat)
                     (lambda (&rest args) (setq sent args) nil))
                    ((symbol-function 'gent-tools-call)
                     (lambda (&rest _) (ert-fail "Preview executed a tool"))))
            (gent-test--with-gent-buffer
              (setq-local gent-context-max-tokens 100)
              (let* ((session gent--current-session)
                     (messages
                      `(((role . "assistant") (content . "")
                         (tool_calls . [((id . "old-call")
                                         (function . ((name . "read_file")
                                                      (arguments . "{}"))))]))
                        ((role . "tool") (tool_call_id . "old-call")
                         (content . ,(make-string 1000 ?x)))
                        ((role . "assistant") (content . "")
                         (tool_calls . [((id . "missing-call")
                                         (function . ((name . "read_file")
                                                      (arguments . "{}"))))]))))
                     (snapshot (copy-tree messages t)))
                (setf (gent--session-messages session) messages)
                (insert "  proposed prompt \n")
                (ring-insert gent--history "previous prompt")
                (setq gent--history-index 0)
                (let ((text (buffer-string))
                      (point-before (point))
                      (modified (buffer-modified-p))
                      (undo buffer-undo-list)
                      (history (ring-elements gent--history)))
                  (setq preview (gent-send-dry-run))
                  (should-not sent)
                  (should (eq messages (gent--session-messages session)))
                  (should (equal snapshot messages))
                  (should (equal (buffer-string) text))
                  (should (= (point) point-before))
                  (should (eq modified (buffer-modified-p)))
                  (should (eq undo buffer-undo-list))
                  (should (equal history (ring-elements gent--history)))
                  (should (= gent--history-index 0))
                  (should (eq (gent--state) 'idle))
                  (should-not (gent--session-queue session))
                  (should-not (gent--session-id session))
                  (should-not (gent--session-request-process session))
                  (should (= (gent--session-turn session) 0)))
                (let ((payload
                       (with-current-buffer preview
                         (should (derived-mode-p 'gent-dry-run-mode))
                         (should buffer-read-only)
                         (should-not (buffer-modified-p))
                         (should (string-search "old-call" (buffer-string)))
                         (should (string-search "Estimated context:" (buffer-string)))
                         (should (string-search "tokens / 100 budget" (buffer-string)))
                         (should (string-search "generated when sent" (buffer-string)))
                         (goto-char (point-min))
                         (search-forward "\nRequest body\n\n")
                         (gent-llm--json-parse-string
                          (buffer-substring-no-properties (point) (point-max))))))
                  (gent-send)
                  (should sent)
                  (should (equal (map-elt payload 'messages)
                                 (gent-llm--normalize-messages
                                  (plist-get sent :messages) nil)))
                  (should (equal (map-elt payload 'tools)
                                 (gent-llm--json-parse-string
                                  (json-encode (vconcat (plist-get sent :tools))))))
                  (should (equal (map-elt payload 'model) "preview-model"))
                  (should (= (map-elt payload 'max_tokens) 123))
                  (should (= (map-elt payload 'temperature) 0.5))
                  (should (equal (map-elt payload 'reasoning_effort) "high"))
                  (should (string-search "Include these project instructions."
                                         (map-elt (aref (map-elt payload 'messages) 0)
                                                  'content)))
                  (should (equal (map-elt (car (last (plist-get sent :messages))) 'content)
                                 "proposed prompt"))
                  (should (cl-find gent--elided (plist-get sent :messages)
                                   :key (lambda (m) (map-elt m 'content)) :test #'equal))
                  (should (cl-find "missing-call" (plist-get sent :messages)
                                   :key (lambda (m) (map-elt m 'tool_call_id))
                                   :test #'equal))))))
        (when (buffer-live-p preview) (kill-buffer preview))))))

(ert-deftest gent-test-dry-run-refresh-and-generic-backend ()
  (gent-test--with-project
    (let ((gent-llm-default-backend (make-gent-test-backend))
          (preview nil))
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
            (gent-test--with-gent-buffer
              (let ((source (current-buffer)))
                (setf (gent--session-id gent--current-session) "existing-id")
                (insert "first preview")
                (setq preview (gent-send-dry-run))
                (should (eq (keymap-lookup gent-mode-map "C-c C-d") #'gent-send-dry-run))
                (gent--set-input "updated preview")
                (with-current-buffer preview
                  (should (eq gent--dry-run-source source))
                  (should (eq (keymap-lookup (current-local-map) "g") #'gent-dry-run-refresh))
                  (should (eq (keymap-lookup (current-local-map) "q") #'quit-window))
                  (should (string-search "existing-id" (buffer-string)))
                  (should (string-search "does not provide a wire preview" (buffer-string)))
                  (gent-dry-run-refresh)
                  (should (string-search "updated preview" (buffer-string)))
                  (should-not (string-search "first preview" (buffer-string))))))
            ;; Refresh also handles a source buffer that has been killed.
            (with-current-buffer preview
              (should-error (gent-dry-run-refresh) :type 'user-error)))
        (when (buffer-live-p preview) (kill-buffer preview))))))

(ert-deftest gent-test-dry-run-rejects-unavailable-input ()
  (with-temp-buffer
    (should-error (gent-send-dry-run) :type 'user-error))
  (gent-test--with-gent-buffer
    (insert " \n ")
    (should-error (gent-send-dry-run) :type 'user-error)
    (gent--set-input "preview me")
    (gent--transition 'start)
    (should-error (gent-send-dry-run) :type 'user-error)
    (gent--transition 'await-approval '(:name "edit_file"))
    (should-error (gent-send-dry-run) :type 'user-error)
    (gent--transition 'interrupt)
    (setf (gent--session-queue gent--current-session) '("queued prompt"))
    (should-error (gent-send-dry-run) :type 'user-error)
    (should (equal (gent--input) "preview me"))))

(ert-deftest gent-test-backend-preview-excludes-authentication ()
  (let* ((backend (gent-make-openai-backend
                   :url-base "https://example.test/v1" :model "model"
                   :api-key "private-key" :headers '("X-Custom-Auth: private-header")))
         (details (gent-llm-backend-preview
                   backend '(((role . "user") (content . "hello"))) nil))
         (text (prin1-to-string details)))
    (should (equal (map-elt details 'endpoint) "https://example.test/v1/chat/completions"))
    (should (equal (map-elt (map-elt details 'payload) 'model) "model"))
    (should-not (string-search "private-key" text))
    (should-not (string-search "private-header" text))))

(ert-deftest gent-test-session-buffer-isolation ()
  (with-temp-buffer
    (should-not gent--current-session)
    (should-not (gent--live-p (current-buffer) 0))
    (should-not (memq (current-buffer) (gent--pending-buffers)))
    (should-not gent--current-session)
    (gent-mode)
    (let ((first gent--current-session))
      (setf (gent--session-root first) default-directory
            (gent--session-messages first) '(((role . "user") (content . "first"))))
      (with-temp-buffer
        (gent-mode)
        (should-not (eq gent--current-session first))
        (should-not (gent--session-messages gent--current-session))
        (gent--append '((role . "user") (content . "second"))))
      (should (eq gent--current-session first))
      (should (equal (gent--session-messages first)
                     '(((role . "user") (content . "first"))))))))

(ert-deftest gent-test-session-reset-rejects-stale-callbacks ()
  (let ((gent-backends (list (cons "local" (make-gent-test-backend))))
        (requests nil))
    (cl-letf (((symbol-function 'gent-llm-chat)
               (lambda (&rest args) (push args requests) nil)))
      (gent-test--with-gent-buffer
        (gent-set-backend "local")
        (gent-test--type "old")
        (let* ((session gent--current-session)
               (backend (gent--session-backend session))
               (root (gent--session-root session))
               (turn (gent--session-turn session))
               (session-id (gent--session-id session))
               (old (car requests))
               (process (make-pipe-process :name "gent-test-reset" :noquery t)))
          (setf (gent--session-request-process session) process)
          (gent-reset t)
          (should-not (process-live-p process))
          (should (eq gent--current-session session))
          (should (> (gent--session-turn session) turn))
          (should-not (gent--live-p (current-buffer) turn))
          (should-not (gent--session-messages session))
          (should-not (gent--session-id session))
          (should (eq (gent--session-backend session) backend))
          (should (equal (gent--session-backend-name session) "local"))
          (should (equal (gent--session-root session) root))
          (gent-test--type "new")
          (should-not (equal (gent--session-id session) session-id))
          (funcall (plist-get old :on-token) "stale-token")
          (funcall (plist-get old :on-done) "stale-answer")
          (funcall (plist-get old :on-error) "stale-error")
          (should (eq (gent--state) 'running))
          (should (equal (gent--session-messages session)
                         '(((role . "user") (content . "new")))))
          (should-not (string-search "stale-" (buffer-string)))
          (funcall (plist-get (car requests) :on-done) "current")
          (should (eq (gent--state) 'idle))
          (should (equal (map-elt (car (last (gent--session-messages session))) 'content)
                         "current")))))))

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
          (should-not (gent--session-id gent--current-session))
          (gent-test--type "read a.txt")
          (gent-test--wait-idle)
          (should (= (length ids) 2))
          (should (stringp (gent--session-id gent--current-session)))
          (should-not (string-empty-p (gent--session-id gent--current-session)))
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
          (should (cl-every (lambda (id)
                              (equal id (gent--session-id gent--current-session)))
                            ids))
          ;; Sharing the same backend does not share conversation IDs.
          (setf (gent--session-backend gent--current-session) gent-llm-default-backend)
          (gent-reset t)
          (should-not (gent--session-id gent--current-session))
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
           (sessions nil)
           (fake (gent-test--fake-chat (list '(("one") ("two"))) (list nil))))
      (cl-letf (((symbol-function 'gent-llm-chat)
                 (lambda (&rest args)
                   (push (plist-get args :session-id) ids)
                   (push gent--current-session sessions)
                   (apply fake args))))
        (with-temp-buffer
          (let ((standard-output (current-buffer)))
            (gent-batch-run "first")
            (gent-batch-run "second"))))
      (should (= (length ids) 2))
      (should (= (length sessions) 2))
      (should-not (eq (car sessions) (cadr sessions)))
      (should (cl-every (lambda (id) (and (stringp id) (not (string-empty-p id)))) ids))
      (should-not (equal (car ids) (cadr ids))))))

(ert-deftest gent-test-set-backend-copies-configuration ()
  (let* ((go (gent-llm-make-opencode-go-backend :model "go-model" :api-key "go-key"))
         (dumile (gent-llm-make-dumile-backend :model "dumile-model" :api-key "dumile-key"))
         (gent-backends (list (cons "go" go) (cons "dumile" dumile)))
         (gent-llm-default-backend go))
    (gent-test--with-gent-buffer
      (gent-set-backend "go")
      (should-not (eq (gent--session-backend gent--current-session) go))
      (let ((previous (gent--session-backend gent--current-session)))
        (gent-set-backend "dumile")
        (should-not (eq (gent--session-backend gent--current-session) dumile))
        (should (equal (gent--session-backend-name gent--current-session) "dumile"))
        (should (equal (gent-llm-backend-model (gent--session-backend gent--current-session))
                       "dumile-model"))
        (should (equal (gent-openai--url (gent--session-backend gent--current-session))
                       "https://dumile.local/v1/chat/completions"))
        (should (equal (gent-openai--headers (gent--session-backend gent--current-session) "session")
                       '("Content-Type: application/json" "Authorization: Bearer dumile-key")))
        (should (equal (gent-llm-backend-model previous) "go-model"))
        (should (member "x-opencode-session: session"
                        (gent-openai--headers previous "session"))))
      (gent-set-reasoning-effort "high")
      (should (equal (gent-llm-backend-reasoning-effort dumile) "low")))
    (should (eq gent-llm-default-backend go))
    (should (equal (gent-llm-backend-model go) "go-model"))
    (gent-test--with-gent-buffer
      (should-not (gent--session-backend gent--current-session))
      (should-not (gent--session-backend-name gent--current-session))
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
      (should (equal (gent-llm-backend-model (gent--session-backend gent--current-session)) "m")))))

(ert-deftest gent-test-set-backend-invalid-selection ()
  (let ((gent-backends (list (cons "local" (make-gent-test-backend)))))
    (gent-test--with-gent-buffer
      (gent-set-backend "local")
      (let ((previous (gent--session-backend gent--current-session)))
        (should-error (gent-set-backend "missing") :type 'user-error)
        (should (eq (gent--session-backend gent--current-session) previous))
        (should (equal (gent--session-backend-name gent--current-session) "local")))
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
        (should-not (eq (gent--session-backend gent--current-session) backend))
        (should (equal (gent-llm-backend-model (gent--session-backend gent--current-session)) "original"))
        (let ((previous (gent--session-backend gent--current-session)))
          (gent-set-reasoning-effort "high")
          (should-not (eq (gent--session-backend gent--current-session) previous))
          (should (equal (gent-llm-backend-reasoning-effort
                          (gent--session-backend gent--current-session))
                         "high"))
          (should (equal (gent-llm-backend-reasoning-effort previous) "low")))
        (should (equal (gent-llm-backend-model (gent--session-backend gent--current-session)) "original"))
        (should (equal (gent-llm-backend-model backend) "original"))
        (should (equal (gent-llm-backend-reasoning-effort backend) "low"))))))

(ert-deftest gent-test-header-hat-status ()
  (let ((gent-llm-default-backend nil))
    (gent-test--with-gent-buffer
      (dolist (case '((idle shadow "Ready")
                      (running gent-prompt-face "Working")
                      ((awaiting-approval :name "edit_file")
                       gent-approval-face "Approval needed")))
        (pcase-let ((`(,state ,face ,label) case))
          (setf (gent--session-state gent--current-session) state
                (gent--session-queue gent--current-session) '("next"))
          (let ((header (gent--header-line))
                (details (gent--header-details)))
            (should (string-prefix-p "   ████   gent · " header))
            (should (string-prefix-p " ▀▀▀▀▀▀▀▀ Model: " details))
            (dolist (row (list header details))
              (dotimes (i 8)
                (should (eq (get-text-property (1+ i) 'face row) face)))
              (should-not (get-text-property 9 'face row))
              (should-not (string-search "\n" row)))
            (should (string-search label header))
            (should (string-search "1 queued" header))))))))

(ert-deftest gent-test-header-two-rows ()
  (let ((gent-llm-default-backend nil))
    (gent-test--with-gent-buffer
      (should (equal tab-line-format '(:eval (gent--header-line))))
      (should (equal header-line-format '(:eval (gent--header-details))))
      (should (local-variable-p 'face-remapping-alist))
      (should (equal (alist-get 'tab-line face-remapping-alist) '(header-line)))
      (setf (gent--session-root gent--current-session) "/tmp/gent-project/")
      (let ((header (gent--header-line))
            (details (gent--header-details)))
        (should (string-search "gent · gent-project" header))
        (should (string-search "Model: No backend" details))
        (should (string-search "Reasoning: default" details)))
      (setf (gent--session-backend gent--current-session)
            (make-gent-test-backend :model-id "test-model" :effort "high"))
      (let* ((details (gent--header-details))
             (model-pos (string-search "test-model" details))
             (effort-pos (string-search "high" details)))
        (should (string-search "Model: test-model" details))
        (should (string-search "Reasoning: high" details))
        (dolist (pos (list model-pos effort-pos))
          (should (get-text-property pos 'help-echo details))
          (should (eq (get-text-property pos 'mouse-face details) 'highlight))
          (should (functionp
                   (lookup-key (get-text-property pos 'local-map details)
                               [header-line mouse-1]))))))))

(ert-deftest gent-test-buffer-send ()
  (gent-test--with-project
    (cl-letf (((symbol-function 'gent-llm-chat)
               (gent-test--fake-chat (list '(("Hi there"))) (list nil))))
      (gent-test--with-gent-buffer
        (gent-test--type "hello")
        (should (eq (gent--state) 'running))
        ;; Input stays editable while the model answers.
        (insert "next")
        (gent-test--wait-idle)
        (let ((transcript (buffer-substring-no-properties (point-min) gent--output-marker)))
          (should (string-match-p "gent> hello\n\nHi there\n\\'" transcript)))
        (should (equal (gent--input) "next"))
        (should (get-text-property (point-min) 'read-only))
        (should-error (progn (goto-char (point-min)) (insert "x")) :type 'text-read-only)
        (should (equal (mapcar (lambda (m) (map-elt m 'role))
                              (gent--session-messages gent--current-session))
                       '("user" "assistant")))))))

(ert-deftest gent-test-buffer-queue ()
  (gent-test--with-project
    (let ((requests (list nil)))
      (cl-letf (((symbol-function 'gent-llm-chat)
                 (gent-test--fake-chat (list '(("one") ("two"))) requests)))
        (gent-test--with-gent-buffer
          (gent-test--type "first")
          (gent-test--type "second")
          (should (equal (gent--session-queue gent--current-session) '("second")))
          (gent-test--wait-idle)
          (should-not (gent--session-queue gent--current-session))
          (should (= (length (car requests)) 2))
          (should (string-match-p "first\n\none\n\ngent> second\n\ntwo"
                                  (buffer-substring-no-properties (point-min) gent--output-marker))))))))

(ert-deftest gent-test-buffer-queues-during-approval ()
  (dolist (answer '(t (reject . "skip this edit")))
    (gent-test--with-project
      (gent-test--write-file "a.txt" "old")
      (let ((requests (list nil)))
        (cl-letf (((symbol-function 'gent-llm-chat)
                   (gent-test--fake-chat
                    (list `((nil ,(gent-test--tool-call
                                   "c1" "edit_file"
                                   '((path . "a.txt") (old_string . "old")
                                     (new_string . "new"))))
                            ("Finished edit turn.") ("Finished queued turn.")))
                    requests)))
          (gent-test--with-gent-buffer
            (should (eq (gent--state) 'idle))
            (gent-test--type "edit")
            (with-timeout (5 (error "Timed out waiting for approval"))
              (while (eq (gent--state) 'running)
                (accept-process-output nil 0.05)))
            (should (eq (gent--state) 'awaiting-approval))
            (should (string-search "Approval needed" (gent--header-line)))
            (gent-test--type "next")
            (should (equal (gent--session-queue gent--current-session) '("next")))
            (should (= (length (car requests)) 1))
            (should (equal (gent-test--read-file "a.txt") "old"))
            (gent--answer (current-buffer) answer)
            (should (eq (gent--state) 'running))
            (should-not (gent--session-pending gent--current-session))
            (should (string-search "Working" (gent--header-line)))
            (gent-test--wait-idle)
            (should-not (gent--session-queue gent--current-session))
            (should (= (length (car requests)) 3))
            (should (string-search "Ready" (gent--header-line)))
            (should (equal (gent-test--read-file "a.txt")
                           (if (eq answer t) "new" "old")))
            (should (equal (mapcar (lambda (message) (map-elt message 'content))
                                  (seq-filter (lambda (message)
                                                (equal (map-elt message 'role) "user"))
                                              (gent--session-messages gent--current-session)))
                           '("edit" "next")))))))))

(ert-deftest gent-test-buffer-error-starts-queued-turn ()
  (let (requests)
    (cl-letf (((symbol-function 'gent-llm-chat)
               (lambda (&rest args) (push args requests) nil)))
      (gent-test--with-gent-buffer
        (gent-test--type "first")
        (gent-test--type "second")
        (let ((first (car requests)))
          (funcall (plist-get first :on-error) "request failed")
          (should (eq (gent--state) 'running))
          (should-not (gent--session-queue gent--current-session))
          (should (= (length requests) 2))
          ;; A late callback from the failed request cannot finish its successor.
          (funcall (plist-get first :on-done) "stale answer")
          (should (eq (gent--state) 'running))
          (funcall (plist-get (car requests) :on-done) "second answer")
          (should (eq (gent--state) 'idle))
          ;; Completed turns also ignore late callbacks from the same generation.
          (let ((transcript (buffer-string)))
            (funcall (plist-get (car requests) :on-error) "late error")
            (funcall (plist-get (car requests) :on-done) "duplicate answer")
            (should (equal (buffer-string) transcript))))))))

(ert-deftest gent-test-buffer-always-trusts-tool-until-reset ()
  (cl-letf (((symbol-function 'gent-llm-chat) (lambda (&rest _) nil)))
    (gent-test--with-gent-buffer
      (gent-test--type "first")
      (gent--await-approval (current-buffer) #'ignore "run_shell" 'command "true" #'ignore)
      (gent--answer (current-buffer) 'always)
      (should (equal (gent--session-trusted gent--current-session) '("run_shell")))
      (gent-reset t)
      (should-not (gent--session-trusted gent--current-session)))))

(ert-deftest gent-test-buffer-reset-confirms-before-discarding ()
  (gent-test--with-gent-buffer
    (let ((asked nil))
      ;; Nothing to lose yet, so no question.
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked t) t)))
        (gent-reset)
        (should-not asked))
      (gent--append '((role . "user") (content . "keep")))
      ;; Declining keeps the conversation.
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked t) nil)))
        (gent-reset)
        (should asked)
        (should (equal (gent--session-messages gent--current-session)
                       '(((role . "user") (content . "keep"))))))
      ;; Accepting discards it.
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked t) t)))
        (setq asked nil)
        (gent-reset)
        (should asked)
        (should-not (gent--session-messages gent--current-session)))
      ;; A prefix argument skips the question.
      (gent--append '((role . "user") (content . "again")))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (error "Should not ask"))))
        (gent-reset t)
        (should-not (gent--session-messages gent--current-session))))))

(ert-deftest gent-test-buffer-interrupt-discards-approval-and-queue ()
  (let (request)
    (cl-letf (((symbol-function 'gent-llm-chat)
               (lambda (&rest args) (setq request args) nil)))
      (gent-test--with-gent-buffer
        (gent-test--type "first")
        ;; Starting another turn must preserve the active conversation on error.
        (should-error (gent--start-turn (current-buffer) "overlap"
                                       (gent--shell (current-buffer))))
        (should (equal (map-elt (car (gent--session-messages gent--current-session)) 'content)
                       "first"))
        (gent-test--type "second")
        (gent--await-approval (current-buffer) #'ignore "edit_file" 'diff "diff" #'ignore)
        (should-error (gent--transition 'finish))
        (should (eq (gent--state) 'awaiting-approval))
        (gent-interrupt)
        (should (eq (gent--state) 'idle))
        (should-not (gent--session-queue gent--current-session))
        (should-not (gent--session-pending gent--current-session))
        (should-error (gent-approve) :type 'user-error)
        (funcall (plist-get request :on-done) "stale answer")
        (should (eq (gent--state) 'idle))
        (should-not (string-search "stale answer" (buffer-string)))))))

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

(ert-deftest gent-test-curl-stream-error ()
  (gent-test--with-fake-curl
      (format "cat > /dev/null\nprintf '%%s' '%s'\n"
              (concat (gent-test--sse '((content . "Hel")))
                      "data: {\"error\":{\"message\":\"rate limited\"}}\n\n"))
    (pcase-let ((`(,_tokens ,_calls ,done ,error) (gent-test--chat)))
      (should-not done)
      (should (equal error "gent-llm: server error: rate limited")))))

(ert-deftest gent-test-curl-tool-continuation-preserves-reasoning ()
  (gent-test--with-project
    (gent-test--write-file "a.txt" "hello")
    (gent-test--with-fake-curl
        (format
         (concat "for arg do case \"$arg\" in @*) payload=${arg#@};; esac; done\n"
                 "cat > /dev/null\n"
                 "if grep -q '\"tool_call_id\":\"c1\"' \"$payload\"; then\n"
                 "printf '%%s' '%s'\nelse\nprintf '%%s' '%s'\nfi\n")
         (gent-test--sse '((content . "It says hello.")))
         (gent-test--sse
          '((reasoning_content . "Need to read "))
          '((reasoning_content . "a.txt.\n"))
          '((tool_calls . [((index . 0) (id . "c1") (type . "function")
                            (function . ((name . "read_file")
                                         (arguments . "{\"path\":\"a.txt\"}"))))]))))
      (let* ((gent-llm-default-backend
              (gent-make-openai-backend :model "m" :url-base "http://x"))
             (write-payload (symbol-function 'gent-openai--write-payload))
             (payloads nil))
        (cl-letf (((symbol-function 'gent-openai--write-payload)
                   (lambda (payload)
                     ;; Inspect the JSON actually written for curl.
                     (let ((file (funcall write-payload payload)))
                       (with-temp-buffer
                         (insert-file-contents file)
                         (push (gent-llm--json-parse-string (buffer-string)) payloads))
                       file))))
          (gent-test--with-gent-buffer
            (gent-test--type "read a.txt")
            (gent-test--wait-idle)
            (should (= (length payloads) 2))
            (should-not (assq 'reasoning_content
                              (aref (map-elt (cadr payloads) 'messages) 1)))
            (let* ((messages (map-elt (car payloads) 'messages))
                   (assistant (aref messages 2))
                   (tool (aref messages 3)))
              (should (equal (map-elt assistant 'role) "assistant"))
              (should (equal (map-elt assistant 'content) ""))
              (should (equal (map-elt assistant 'reasoning_content) "Need to read a.txt.\n"))
              (should (equal (map-elt (aref (map-elt assistant 'tool_calls) 0) 'id) "c1"))
              (should (equal (map-elt tool 'tool_call_id) "c1"))
              (should (string-search "hello" (map-elt tool 'content))))
            ;; Each response has its own reasoning accumulator.
            (let ((answer (car (last (gent--session-messages gent--current-session)))))
              (should (equal (map-elt answer 'content) "It says hello."))
              (should-not (assq 'reasoning_content answer)))))))))

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
