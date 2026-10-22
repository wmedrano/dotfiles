;;; cargo-minor-mode.el --- Cargo commands for Rust and Cargo.toml buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode providing Cargo commands, run from the Cargo workspace root.

;;; Code:

(require 'cl-lib)

(defun cargo-minor-mode--manifest-path (&rest args)
  "Return the path reported by `cargo locate-project ARGS', or nil."
  (with-temp-buffer
    (when (zerop (apply #'process-file "cargo" nil (list t nil) nil
                        "locate-project" "--message-format" "plain" args))
      (let ((path (string-trim (buffer-string))))
        (unless (string-empty-p path) path)))))

(defun cargo-minor-mode--package-name (manifest)
  "Return the package name declared in MANIFEST, or nil if it has none."
  (with-temp-buffer
    (insert-file-contents manifest)
    (goto-char (point-min))
    (when (re-search-forward "^\\[package\\]" nil t)
      (let ((end (save-excursion
                   (if (re-search-forward "^\\[" nil t) (point) (point-max)))))
        (when (re-search-forward "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" end t)
          (match-string 1))))))

(cl-defun cargo-minor-mode--run (subcommand &key all-packages test-filter)
  "Run cargo SUBCOMMAND from the Cargo root, for the current package if any.

When ALL-PACKAGES is non-nil, omit the package selector.
When TEST-FILTER is non-nil, pass it as a test name filter."
  (let* ((root-manifest (or (cargo-minor-mode--manifest-path "--workspace")
                            (user-error "Not inside a Cargo project")))
         (manifest (unless all-packages (cargo-minor-mode--manifest-path)))
         (package (and manifest (cargo-minor-mode--package-name manifest)))
         (default-directory (file-name-directory root-manifest)))
    (compile (concat (if package
                 (format "cargo %s --color=always -p %s" subcommand
                         (shell-quote-argument package))
               (format "cargo %s --color=always" subcommand))
                     (when test-filter
                       (concat " " (shell-quote-argument test-filter)))))))

(defun cargo-minor-mode--test-context ()
  "Return a test or test module filter enclosing point, if any."
  (let ((position (point))
        modules test-module test-name)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\_<\\(fn\\|mod\\)\\_>[ \t\n]+\\([[:alnum:]_]+\\)" position t)
        (let* ((start (match-beginning 0))
               (kind (match-string-no-properties 1))
               (name (match-string-no-properties 2))
               (state (save-excursion (syntax-ppss start)))
               (attributes
                (save-excursion
                  (goto-char start)
                  (beginning-of-line)
                  (let ((end (point)))
                    (while (and (zerop (forward-line -1))
                                (looking-at-p "[ \t]*#\\[")))
                    (unless (looking-at-p "[ \t]*#\\[")
                      (forward-line 1))
                    (buffer-substring-no-properties (point) end)))))
          (unless (or (nth 3 state) (nth 4 state))
            (save-excursion
              (when (and (re-search-forward "[{;]" position t)
                         (eq (char-before) ?{)
                         (let ((end (ignore-errors
                                      (scan-sexps (1- (point)) 1))))
                           (or (null end) (< position end))))
                (if (equal kind "mod")
                    (progn
                      (setq modules (append modules (list name)))
                      (when (or (equal name "tests")
                                (string-match-p "#\\[cfg[ \t]*(test)\\]" attributes))
                        (setq test-module (string-join modules "::"))))
                  (when (string-match-p "#\\[test\\]" attributes)
                    (setq test-name (string-join (append modules (list name)) "::")))))))))
      (or test-name test-module))))

;;;###autoload
(defun cargo-dwim (&optional all-packages)
  "Run the test at point, tests in the enclosing test module, or Clippy.

With ALL-PACKAGES (interactively, a prefix argument), run all tests
without a filter in test contexts, or Clippy across the workspace."
  (interactive "P")
  (let ((test-filter (cargo-minor-mode--test-context)))
    (cargo-minor-mode--run (cond (test-filter "test")
                                 (all-packages "clippy --workspace")
                                 (t "clippy"))
                          :all-packages all-packages
                          :test-filter (unless all-packages test-filter))))

;;;###autoload
(defun cargo-check (&optional all-packages)
  "Run `cargo check' from the Cargo root, for the current package if any.

When ALL-PACKAGES is non-nil (interactively, with a prefix argument),
run without a package selector."
  (interactive "P")
  (cargo-minor-mode--run "check" :all-packages all-packages))

;;;###autoload
(defun cargo-clippy (&optional all-packages)
  "Run `cargo clippy' from the Cargo root, for the current package if any.

When ALL-PACKAGES is non-nil (interactively, with a prefix argument),
run without a package selector."
  (interactive "P")
  (cargo-minor-mode--run "clippy" :all-packages all-packages))

;;;###autoload
(defun cargo-test (&optional all-packages)
  "Run `cargo test' from the Cargo root, for the current package if any.

When ALL-PACKAGES is non-nil (interactively, with a prefix argument),
run without a package selector."
  (interactive "P")
  (cargo-minor-mode--run "test" :all-packages all-packages))

;;;###autoload
(define-minor-mode cargo-minor-mode
  "Minor mode with Cargo commands."
  :lighter " Cargo"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'cargo-dwim)
            (define-key map (kbd "C-c C-k") #'cargo-check)
            (define-key map (kbd "C-c C-l") #'cargo-clippy)
            (define-key map (kbd "C-c C-t") #'cargo-test)
            map))

(defcustom cargo-minor-mode-inhibit-functions nil
  "Functions called with no arguments before enabling `cargo-minor-mode'.
If any returns non-nil, `cargo-minor-mode-maybe-enable' does nothing."
  :type 'hook
  :group 'tools)

;;;###autoload
(defun cargo-minor-mode-maybe-enable ()
  "Enable `cargo-minor-mode' in Rust buffers and Cargo.toml buffers.
Do nothing if any of `cargo-minor-mode-inhibit-functions' returns non-nil."
  (when (and (or (derived-mode-p 'rust-ts-mode 'rust-mode)
                 (and buffer-file-name
                      (string= (file-name-nondirectory buffer-file-name)
                               "Cargo.toml")))
             (not (run-hook-with-args-until-success
                   'cargo-minor-mode-inhibit-functions)))
    (cargo-minor-mode)))

(provide 'cargo-minor-mode)
;;; cargo-minor-mode.el ends here
