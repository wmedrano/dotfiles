;;; package --- Emacs configuration.
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(edit-indirect eat dracula-theme filladapt doom-modeline go-mode evil-commentary ace-window zig-mode evil catppuccin-theme modus-themes diff-hl dired-posframe which-key-posframe which-key transient-posframe markdown-mode diminish yaml-mode eglot-booster rust-mode nerd-icons-ivy-rich magit counsel swiper ivy-rich ivy-posframe ivy company))
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
(define-key evil-motion-state-map (kbd "n") #'evil-next-line)
(define-key evil-motion-state-map (kbd "e") #'evil-previous-line)
(define-key evil-motion-state-map (kbd "j") #'evil-search-next)
(define-key evil-motion-state-map (kbd "J") #'evil-search-backward)
(global-set-key (kbd "<home>") #'beginning-of-buffer)
(global-set-key (kbd "<end>")  #'end-of-buffer)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'ace-window)
(setq-default aw-dispatch-always t
              mouse-autoselect-window t)
(global-set-key (kbd "C-w") #'ace-window)
(define-key evil-motion-state-map (kbd "C-w") nil t)
(define-key evil-insert-state-map (kbd "C-w") nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ivy)
(setq-default ivy-height (if (display-graphic-p) 32 10))
(define-key ivy-minibuffer-map (kbd "S-<return>") #'ivy-immediate-done)
(ivy-mode)

(require 'counsel)
(define-key counsel-mode-map (kbd "C-x b") #'counsel-switch-buffer)
(define-key counsel-mode-map (kbd "C-x C-b") #'counsel-switch-buffer-other-window)
(counsel-mode)

(require 'ivy-posframe)
(setq-default ivy-posframe-min-width 220)
(ivy-posframe-mode)

(require 'ivy-rich)
(require 'nerd-icons-ivy-rich)
(ivy-rich-mode)
(nerd-icons-ivy-rich-mode)

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

(require 'xref)
(define-key xref--xref-buffer-mode-map (kbd "e") #'xref-prev-line)
(add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)

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
(delete 'compilation-mode evil-motion-state-modes)
(add-to-list 'evil-emacs-state-modes 'compilation-mode)
(add-to-list 'evil-emacs-state-modes 'special-mode)

;; Colorize compilation buffer.
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Use eat for terminal. It adds color.
(require 'eat)
(add-hook 'eshell-load-hook #'eat-eshell-mode)

(defun wm-compile-cmds ()
  "Get the valid compile commands for the current buffer."
  (let ((cmds '((conf-toml-mode . ("cargo test"
                                   "cargo build"))
		(markdown-mode . ("bundle exec jekyll serve --livereload"))
		(ruby-mode . ("bundle exec jekyll serve --livereload"
			      "bundle update"))
                (rust-mode . ("cargo test"
                              "cargo build"))
		(zig-mode . ("zig build test --summary all"
			     "zig build run --summary all"
			     "zig build check --summary all" "zig build install --summary all"))
		)))
    (alist-get major-mode cmds)))

(defvar wm-compile--hist '())

(defun wm-compile ()
  "Run a common compile command."
  (interactive)
  (let* ((name->cmd (wm-compile-cmds))
         (cmd       (ivy-completing-read "Command: "
                                         name->cmd
                                         nil
                                         t
                                         nil
                                         'wm-compile--hist)))
    (compile (format "time %s" cmd))))

(global-set-key (kbd "C-c C-t") #'wm-compile)

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
(add-hook 'git-commit-mode-hook #'evil-insert-state)

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
(setenv "RUST_BACKTRACE" "1")
(setenv "CARGO_TERM_COLOR" "always")
(setenv "NEXTEST_HIDE_PROGRESS_BAR" "1")

(defun rustfmt-on-save ()
  "Run rustfmt on save."
  (add-hook 'before-save-hook #'rust-format-buffer 0 t))

(add-hook 'rust-mode-hook #'rustfmt-on-save)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'set-fill-column-100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zig
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'zig-mode)
(require 'filladapt)
(setq-default zig-format-on-save t)

(add-hook 'zig-mode-hook #'eglot-ensure)
(add-hook 'zig-mode-hook #'set-fill-column-100)
(add-hook 'zig-mode-hook #'filladapt-mode)
(define-key zig-mode-map (kbd "C-c C-t") #'wm-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(add-hook 'markdown-mode-hook #'delete-trailing-whitespace-on-save)
(add-hook 'markdown-mode-hook #'set-fill-column-80)
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(define-key markdown-mode-map (kbd "C-c C-t") #'wm-compile)

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
              display-line-numbers-width 3
              initial-frame-alist '((fullscreen . maximized)))
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(column-number-mode)
(if (string-equal (system-name) "quest")
    (set-frame-font "JetBrains Mono 10")
  (set-frame-font "JetBrains Mono 14"))

;; (require 'dracula-theme)
;; (mapc #'disable-theme custom-enabled-themes)
;; (load-theme 'dracula t)

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
(diminish 'counsel-mode)
(diminish 'eldoc-mode)
(diminish 'ivy-posframe-mode)
(diminish 'ivy-mode)
(diminish 'which-key-mode)
(diminish 'auto-revert-mode)
(diminish 'auto-fill-function)
(diminish 'which-key-mode)
(diminish 'evil-commentary-mode)

(provide 'init)
;;; init.el ends here
