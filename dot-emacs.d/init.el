;;; package --- Emacs configuration.
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(diff-hl dired-posframe which-key-posframe which-key transient-posframe markdown-mode diminish yaml-mode eglot-booster rust-mode nerd-icons-ivy-rich magit counsel swiper ivy-rich ivy-posframe ivy company))
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
(setq-default make-backup-files nil
	      auto-save-default nil
	      gc-cons-percentage 1.0
	      gc-cons-threshold (* 512 1024 1024))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cua-base)
(cua-mode)
(define-key cua-global-keymap (kbd "<home>") #'beginning-of-buffer)
(define-key cua-global-keymap (kbd "<end>")  #'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ivy)
(setq-default ivy-height 32)
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

(defun eglot-format-on-save ()
  "Run `eglot-format-buffer` on save."
  (add-hook 'before-save-hook #'eglot-format-buffer 0 t))

(defun eglot-disable-inlay-hints-mode ()
  "Disable `inlay-hints-mode`."
  (eglot-inlay-hints-mode))
(add-hook 'eglot-managed-mode-hook #'eglot-disable-inlay-hints-mode)

;; Requires https://github.com/jdtsmith/eglot-booster to be installed
;; somewhere in $PATH.
(eglot-booster-mode)

(define-key eglot-mode-map (kbd "<f2>") #'eglot-rename)
(define-key eglot-mode-map (kbd "<f3>") #'eglot-code-actions)

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
(global-set-key (kbd "<f5>") #'recompile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Formatting
;;   - Code formatting functions are defined here.
;;   - The specific triggering is left in the language specific sections.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'elisp-mode)

(defun elisp-eval-on-save ()
  "Run `eval-buffer` on save."
  (interactive)
  (add-hook 'after-save-hook #'eval-buffer 0 t))

(add-hook 'emacs-lisp-mode-hook #'delete-trailing-whitespace-on-save)
(add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(seq-doseq (p load-path)
  (add-to-list 'elisp-flymake-byte-compile-load-path p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rust-mode)
(add-to-list 'exec-path "~/.cargo/bin")

(defun rustfmt-on-save ()
  "Run rustfmt on save."
  (add-hook 'before-save-hook #'rust-format-buffer 0 t))

(add-hook 'rust-mode-hook #'rustfmt-on-save)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'set-fill-column-100)
(add-hook 'rust-mode-hook #'flyspell-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(add-hook 'markdown-mode #'flyspell-mode)
(add-hook 'markdown-mode #'delete-trailing-whitespace-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Browsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-posframe)
(setq-default dired-posframe-min-width 120)
(define-key dired-mode-map (kbd "SPC") #'dired-posframe-mode)
(add-hook 'dired-posframe-mode-hook #'dired-posframe-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look & Feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default inhibit-startup-screen t
	      ring-bell-function #'ignore
	      scroll-conservatively 101
	      display-line-numbers-grow-only t
	      initial-frame-alist '((fullscreen . maximized)))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(column-number-mode)
(set-frame-font "JetBrains Mono 11")

(load-theme 'modus-vivendi t)

(require 'diminish)
(diminish 'company-mode)
(diminish 'counsel-mode)
(diminish 'eldoc-mode)
(diminish 'ivy-posframe-mode)
(diminish 'ivy-mode)
(diminish 'which-key-mode)
(diminish 'auto-revert-mode)

(require 'transient-posframe)
(transient-posframe-mode)

(require 'which-key)
(require 'which-key-posframe)
(which-key-mode)
(which-key-posframe-mode)

(provide 'init)
;;; init.el ends here
