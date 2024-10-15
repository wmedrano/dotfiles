;;; package --- Emacs configuration.
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(expand-region company-posframe vterm consult-project-extra consult-eglot consult nerd-icons-completion marginalia orderless vertico-posframe vertico smartparens undo-tree anzu nord-theme monokai-pro-theme edit-indirect eat dracula-theme filladapt doom-modeline go-mode ace-window zig-mode modus-themes diff-hl dired-posframe which-key-posframe which-key transient-posframe markdown-mode diminish yaml-mode eglot-booster rust-mode magit company))
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

(require 'winner)
(winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'vertico)
(setq-default enable-recursive-minibuffers t)
(vertico-mode t)

(require 'orderless)
(setq-default completion-styles '(basic orderless)
              completion-category-overrides '((file (styles basic partial-completion))))

(require 'vertico-posframe)
(setq-default vertico-posframe-border-width 2)
(vertico-posframe-mode t)

(require 'nerd-icons-completion)
(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

(require 'marginalia)
(marginalia-mode t)

(require 'consult)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-x p b") #'consult-project-buffer)
(require 'consult-register)

(require 'consult-project-extra)
(global-set-key (kbd "C-x p p") #'consult-project-extra-find)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP - Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eglot)
(require 'eglot-booster)
(setq-default eglot-events-buffer-size 0
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

(setq-default compile-command "")
(setq-default
 next-error-highlight-no-select t
 next-error-highlight           t
 next-error-message-highlight   'keep)
(global-set-key (kbd "<f5>") #'recompile)
(define-key compilation-mode-map (kbd "<f8>") #'compilation-next-error)

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

(defun rust-function-at-point ()
  "Get the name of the current function."
  (and-let* ((_ (treesit-available-p))
             (node (treesit-node-at (point)))
             (function-node (treesit-parent-until
                             node
                             #'treesit-node-rust-function-p))
             (function-name-node (treesit-node-child-by-field-name
                                  function-node
                                  "name")))
    (treesit-node-text function-name-node t)))

(defun consult-compile--rust ()
  "Get the compile commands for Rust."
  `("cargo nextest run"
    "cargo build"
    "cargo clippy"
    ,(concat "cargo nextest run " (or (rust-function-at-point) "no-rust-function-at-point"))))

(defun consult-compile ()
  "Get the default compile directory for the current buffer."
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (cmds              (consult-compile--rust))
         (cmd               (completing-read "Command: " cmds)))
    (compile cmd)))

(global-set-key (kbd "C-c g") #'consult-ripgrep)
(global-set-key (kbd "C-c l") #'consult-line)
(global-set-key (kbd "C-c p") #'consult-project-extra-find)
(global-set-key (kbd "C-c s") #'sort-lines)
(global-set-key (kbd "C-c t") #'consult-compile)
(global-set-key (kbd "C-c x") #'consult-complex-command)

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
(setq-default magit-auto-revert-immediately t)
(global-auto-revert-mode)
(define-key magit-status-mode-map (kbd "C-w") #'ace-window)
(define-key magit-status-mode-map (kbd "e")   #'magit-section-backward)

(require 'diff-hl)
(require 'diff-hl-flydiff)
(global-diff-hl-mode)
(add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)

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

(add-hook 'rust-mode-hook #'rust-ts-mode)
(add-hook 'rust-ts-mode-hook #'rustfmt-on-save)
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook #'set-fill-column-100)

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
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(add-hook 'markdown-mode-hook #'delete-trailing-whitespace-on-save)
(add-hook 'markdown-mode-hook #'set-fill-column-80)
(add-hook 'markdown-mode-hook #'auto-fill-mode)

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
;; Look & Feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default inhibit-startup-screen t
              ring-bell-function #'ignore
              scroll-conservatively 101
              display-line-numbers-grow-only t
              display-line-numbers-width 3)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(column-number-mode)
(if (string-equal (system-name) "quest")
    (set-frame-font "JetBrains Mono 10")
  (set-frame-font "JetBrains Mono 14"))

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

(provide 'init)
;;; init.el ends here
