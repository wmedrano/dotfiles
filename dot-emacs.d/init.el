;;; package --- Emacs configuration.
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("16198c5c7319d07ded977d2414a96fff95f468af313cff6f684fd02f9dfff9b2" default))
 '(package-selected-packages
   '(llm nerd-icons-ivy-rich ivy-rich counsel swiper ivy-posframe ivy evil-commentary elysium gptel chatgpt-shell zig-ts-mode spacemacs-theme elpy evil gdscript-mode org-preview-html geiser-guile htmlize clojure-ts-mode async caddyfile-mode expand-region company-posframe vterm smartparens undo-tree anzu nord-theme monokai-pro-theme edit-indirect eat dracula-theme filladapt doom-modeline go-mode ace-window zig-mode modus-themes diff-hl dired-posframe which-key-posframe which-key transient-posframe markdown-mode diminish yaml-mode eglot-booster rust-mode magit company))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun open-emacs-config ()
  "Open the Emacs config."
  (interactive)
  (find-file user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'jekyll)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default make-backup-files  nil
              auto-save-default  nil
              gc-cons-percentage 1.0
	      redisplay-dont-pause t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(evil-mode)
(define-key evil-motion-state-map (kbd "h") #'evil-backward-char)
(define-key evil-motion-state-map (kbd "n") #'evil-next-line)
(define-key evil-motion-state-map (kbd "e") #'evil-previous-line)
(define-key evil-motion-state-map (kbd "l") #'evil-forward-char)
(define-key evil-motion-state-map (kbd "j") #'evil-search-next)
(define-key evil-motion-state-map (kbd "<home>") #'evil-goto-first-line)
(define-key evil-motion-state-map (kbd "<end>") #'evil-goto-line)
(define-key evil-motion-state-map (kbd "<return>") nil)
(define-key evil-normal-state-map (kbd "<return>") #'ignore)
(define-key evil-motion-state-map (kbd "RET") nil)
(add-to-list 'evil-motions 'flymake-goto-next-error)
(add-to-list 'evil-motions 'flymake-goto-prev-error)

(require 'evil-commentary)
(evil-commentary-mode t)

(require 'cua-base)
(cua-mode)
(global-set-key (kbd "<home>") #'beginning-of-buffer)
(global-set-key (kbd "<end>")  #'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ace-window)
(setq-default aw-dispatch-always t
              mouse-autoselect-window t)
(global-set-key (kbd "C-w") #'ace-window)
(define-key evil-motion-state-map (kbd "C-w") nil)
(define-key evil-insert-state-map (kbd "C-w") nil)

(require 'winner)
(winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default enable-recursive-minibuffers t)

(require 'ivy)
(ivy-mode t)

(require 'ivy-posframe)
(add-hook 'after-init-hook #'ivy-posframe-mode)

(require 'counsel)
(define-key counsel-mode-map (kbd "C-x b") #'counsel-switch-buffer)
(define-key counsel-mode-map (kbd "C-x B") #'counsel-switch-buffer-other-window)
(counsel-mode t)

(require 'ivy-rich)
(require 'nerd-icons-ivy-rich)
(ivy-rich-mode t)
(nerd-icons-ivy-rich-mode t)

(require 'swiper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP - Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eglot)
(require 'eglot-booster)
(setq-default
 eglot-extend-to-xref t)

(defun eglot-format-on-save ()
  "Run `eglot-format-buffer` on save."
  (add-hook 'before-save-hook #'eglot-format-buffer 0 t))

(defun eglot-enable-inlay-hints ()
  "Enable `inlay-hints-mode`."
  (eglot-inlay-hints-mode 1))
(defun eglot-disable-inlay-hints ()
  "Disable `inlay-hints-mode`."
  (eglot-inlay-hints-mode -1))
(add-hook 'eglot-managed-mode-hook #'eglot-disable-inlay-hints)

;; Requires https://github.com/jdtsmith/eglot-booster to be installed
;; somewhere in $PATH.
(eglot-booster-mode)

(define-key eglot-mode-map (kbd "<f2>") #'eglot-rename)
(define-key eglot-mode-map (kbd "<f3>") #'eglot-code-actions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hover & References
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eldoc)
(setq-default eldoc-idle-delay 0.6
	      eldoc-echo-area-use-multiline-p nil)
(add-to-list 'evil-emacs-state-modes #'xref--xref-buffer-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
(setq-default company-tooltip-width-grow-only t
              company-idle-delay 0.1)
(global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax & Compile Errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
(define-key flymake-mode-map (kbd "<f8>") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "S-<f8>") #'flymake-show-buffer-diagnostics)

(setq-default
 compile-command                ""
 compilation-scroll-output      nil
 next-error-highlight-no-select t
 next-error-highlight           t
 next-error-message-highlight   'keep)
(global-set-key (kbd "<f5>") #'recompile)
(define-key compilation-mode-map (kbd "<f8>") #'compilation-next-error)
(define-key compilation-mode-map (kbd "<s-f8>") #'compilation-previous-error)

(defun project-current-root ()
  "Get the root of the current project."
  (and-let* ((project (project-current)))
    (project-root project)))

(defun add-project-root-to-compilation-search-path ()
  "Add the project root to the compilation search path."
  (and-let* ((root    (project-current-root)))
    (add-to-list 'compilation-search-path root)))
(add-hook 'prog-mode-hook #'add-project-root-to-compilation-search-path)


(require 'treesit)
(setq-default treesit-font-lock-level 4)

(defun treesit-node-rust-function-p (node)
  "Return `t` if NODE is a function definition node."
  (string-equal (treesit-node-type node) "function_item"))

;; Colorize compilation buffer.
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Use eat for terminal. It adds color.
(require 'eat)
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Formatting
;;   - Code formatting functions are defined here.
;;   - The specific triggering is left in the language specific sections.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)
(defun delete-trailing-whitespace-on-save ()
  "Add `delete-trailing-whitespace` before save."
  (add-hook 'before-save-hook #'delete-trailing-whitespace 0 t))

(defun set-fill-column-80 ()
  "Run `set-fill-column` with 80."
  (set-fill-column 80))

(defun set-fill-column-100 ()
  "Run `set-fill-column` with 100."
  (set-fill-column 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(setq-default
 magit-auto-revert-immediately t)
(global-auto-revert-mode)
(define-key magit-status-mode-map (kbd "C-w") #'ace-window)
(define-key magit-status-mode-map (kbd "e")   #'magit-section-backward)
(add-hook 'git-commit-mode-hook #'evil-insert-state)

(require 'diff-hl)
(require 'diff-hl-flydiff)
(global-diff-hl-mode)
(add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
(add-hook 'after-revert-hook #'diff-hl-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'elisp-mode)

(defun elisp-eval-on-save ()
  "Run `eval-buffer` on save."
  (interactive)
  (eval-buffer)
  (add-hook 'after-save-hook #'eval-buffer 0 t))

(defun elisp-init-eval-on-save ()
  "Run `eval-buffer` on save only if the buffer is the user config."
  (when (string-equal (buffer-file-name) user-init-file)
    (elisp-eval-on-save)))

(add-hook 'emacs-lisp-mode-hook #'delete-trailing-whitespace-on-save)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(add-hook 'emacs-lisp-mode-hook #'elisp-init-eval-on-save)
(seq-doseq (p load-path)
  (add-to-list 'elisp-flymake-byte-compile-load-path p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rust-mode)
(setenv "RUST_BACKTRACE" "0") ;; Set this to 1 for more stack trace info.
(setenv "CARGO_TERM_COLOR" "always")
(setenv "NEXTEST_HIDE_PROGRESS_BAR" "1")

(defun rustfmt-on-save ()
  "Run rustfmt on save."
  (add-hook 'before-save-hook #'rust-format-buffer 0 t))

(defun rust-function-at-point ()
  "Get the name of the current function."
  (and-let* ((available (treesit-available-p))
             (node (treesit-node-at (point)))
             (function-node (treesit-parent-until
                             node
                             #'treesit-node-rust-function-p))
             (function-name-node (treesit-node-child-by-field-name
                                  function-node
                                  "name")))
    (treesit-node-text function-name-node t)))

(defun cargo-clippy ()
  "Run cargo clippy."
  (interactive)
  (compile "cargo clippy"))

(defun cargo-clippy-fix ()
  "Use cargo clippy to fix all files within the workspace."
  (interactive)
  (compile "cargo clippy --fix --allow-dirty --allow-staged"))

(defun cargo-test ()
  "Run cargo test."
  (interactive)
  (compile "cargo nextest run --no-fail-fast"))

(defun cargo-run ()
  "Run cargo run."
  (interactive)
  (compile "cargo run"))

(defun cargo-build ()
  "Run cargo build."
  (interactive)
  (compile "cargo build"))

(defun cargo-test-function-at-point ()
  "Run cargo test for the current function.

If there is no function at the point, then all tests are run."
  (interactive)
  (if-let ((fap (rust-function-at-point)))
      (compile (concat "cargo test " fap))
    (cargo-test)))

(add-hook 'rust-mode-hook #'rust-ts-mode)
(add-hook 'rust-ts-mode-hook #'rustfmt-on-save)
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook #'set-fill-column-100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GDScript - Godot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'gdscript-mode)
(add-hook 'gdscript-mode-hook #'eglot-ensure)
(define-key gdscript-comint--mode-map (kbd "<f5>") #'gdscript-godot-run-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zig
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'zig-mode)
(require 'filladapt)
(setq-default zig-format-on-save t)

(add-hook 'zig-mode-hook #'eglot-ensure)
(add-hook 'zig-mode-hook #'set-fill-column-100)
(add-hook 'zig-mode-hook #'filladapt-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'geiser-guile)
(setq-default geiser-guile-binary "guile3.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(add-hook 'markdown-mode-hook #'delete-trailing-whitespace-on-save)
(add-hook 'markdown-mode-hook #'set-fill-column-80)
(add-hook 'markdown-mode-hook #'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default org-support-shift-select t
              org-src-fontify-natively t)
(org-babel-do-load-languages
 ;; Requires installing geiser.
 'org-babel-load-languages '((scheme . t)
                             (python . t)))
(add-hook 'org-mode-hook #'set-fill-column-80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Browsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-posframe)
(setq-default dired-posframe-min-width 120)
(define-key dired-mode-map (kbd "SPC") #'dired-posframe-mode)
(add-hook 'dired-posframe-mode-hook #'dired-posframe-show)

(require 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LLM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'chatgpt-shell)
(setq-default
 chatgpt-shell-google-key (secrets-get-secret "kdewallet" "gemini"))

(require 'gptel)
(require 'gptel-gemini)
(setq-default
 gptel-model 'gemini-2.0-flash-thinking-exp
 gptel-backend (gptel-make-gemini "Gemini"
                 :key (secrets-get-secret "kdewallet" "gemini")
                 :stream t))


(require 'elysium)

;; Defined under lisp/codename-goose.el
(require 'codename-goose)
(defun goose-fix-unit-tests ()
  "Tell goose to fix each unit test."
  (interactive)
  (goose-do "Fix all unit tests."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun revert-all-buffers ()
  "Revert all buffers that are visiting files.
This function iterates through all buffers in the current Emacs session
and reverts those buffers which are associated with a file on disk.
This is useful to reload files that might have been changed externally."
  (interactive)
  (seq-doseq (buffer (buffer-list))
         (with-current-buffer buffer
           (when buffer-file-name
             (revert-buffer-quick)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look & Feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 inhibit-startup-screen                   t
 ring-bell-function                       #'ignore
 scroll-conservatively                    101
 display-line-numbers-grow-only           t
 display-line-numbers-width               3
 native-comp-async-report-warnings-errors nil)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(column-number-mode)
(set-frame-font "JetBrains Mono 10")

(require 'monokai-pro-theme)
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'monokai-pro t)

(require 'transient-posframe)
(transient-posframe-mode)

(require 'which-key)
(require 'which-key-posframe)
(which-key-mode)
(which-key-posframe-mode)

(require 'doom-modeline)
(setq-default doom-modeline-hud t)
(doom-modeline-mode)

(require 'diminish)
(diminish 'company-mode)
(diminish 'eldoc-mode)
(diminish 'which-key-mode)
(diminish 'auto-revert-mode)
(diminish 'auto-fill-function)
(diminish 'which-key-mode)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init)
;;; init.el ends here
