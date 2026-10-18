;;; cargo-minor-mode.el --- Cargo commands for Rust and Cargo.toml buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode providing Cargo commands, run from the Cargo workspace root.

;;; Code:

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

(defun cargo-minor-mode--run (subcommand)
  "Run cargo SUBCOMMAND from the Cargo root, for the current package if any."
  (let* ((root-manifest (or (cargo-minor-mode--manifest-path "--workspace")
                            (user-error "Not inside a Cargo project")))
         (manifest (cargo-minor-mode--manifest-path))
         (package (and manifest (cargo-minor-mode--package-name manifest)))
         (default-directory (file-name-directory root-manifest)))
    (compile (if package
                 (format "cargo %s --color=always -p %s" subcommand
                         (shell-quote-argument package))
               (format "cargo %s --color=always" subcommand)))))

;;;###autoload
(defun cargo-check ()
  "Run `cargo check' from the Cargo root, for the current package if any."
  (interactive)
  (cargo-minor-mode--run "check"))

;;;###autoload
(defun cargo-clippy ()
  "Run `cargo clippy' from the Cargo root, for the current package if any."
  (interactive)
  (cargo-minor-mode--run "clippy"))

;;;###autoload
(defun cargo-test ()
  "Run `cargo test' from the Cargo root, for the current package if any."
  (interactive)
  (cargo-minor-mode--run "test"))

;;;###autoload
(define-minor-mode cargo-minor-mode
  "Minor mode with Cargo commands."
  :lighter " Cargo"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-k") #'cargo-check)
            (define-key map (kbd "C-c C-l") #'cargo-clippy)
            (define-key map (kbd "C-c C-t") #'cargo-test)
            map))

;;;###autoload
(defun cargo-minor-mode-maybe-enable ()
  "Enable `cargo-minor-mode' if the current buffer visits a Cargo.toml."
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) "Cargo.toml"))
    (cargo-minor-mode)))

(provide 'cargo-minor-mode)
;;; cargo-minor-mode.el ends here
