;;; package --- Config for will@wmedrano.dev -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window company consult diff-hl doom-modeline doom-themes gptel htmlize
                magit marginalia markdown-mode nerd-icons-completion orderless
                rg rust-mode vertico yaml-mode zig-ts-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reduce GC related stutters by increasing the amount of wasted memory before a
;; garbage collection run. Also run the GC when idle to reduce.
(setq-default gc-cons-percentage 1.0)
(run-with-idle-timer 3 t #'garbage-collect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-hl-line-mode t)

(require 'doom-modeline)
(when (not (custom-theme-enabled-p 'doom-dracula))
  (load-theme 'doom-dracula t))
(doom-modeline-mode t)

;; Despite being `next-error' which is usually reserved for compilation mode,
;; the highlight times also apply to search buffers like `occur-mode' and
;; `xref-mode'.
(setq-default next-error-highlight           3
              next-error-highlight-no-select 3)

(add-hook 'after-init-hook #'toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cua-base)
(cua-mode t)
(global-set-key (kbd "<home>") #'beginning-of-buffer)
(global-set-key (kbd "<end>")  #'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable backup and auto-save files.
(setq-default auto-save-interval 0
              auto-save-default  nil
              create-lockfiles   nil
              make-backup-files  nil)

(setq-default revert-without-query '(".*"))
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default project-switch-commands #'project-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default enable-recursive-minibuffers t)
(savehist-mode t)

(require 'vertico)
(vertico-mode t)

(setq-default completion-styles '(orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion))))

(require 'consult)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-x C-b") #'consult-buffer-other-window)

(require 'nerd-icons-completion)
(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

(require 'marginalia)
(marginalia-mode t)

(require 'which-key)
(which-key-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default scroll-conservatively 101
              scroll-margin         5)

(require 'ace-window)
(setq-default aw-dispatch-always t)
(global-set-key (kbd "C-c w") #'ace-window)

(require 'winner)
(winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diff-hl)
(require 'diff-hl-flydiff)
(setq-default diff-hl-flydiff-delay 1.0)
(global-diff-hl-mode)
(add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
(add-hook 'after-revert-hook #'diff-hl-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Sitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 treesit-language-source-alist
 '((rust . ("https://github.com/tree-sitter/tree-sitter-rust.git" "v0.24.0"))
   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
   (zig  . ("https://github.com/maxxnino/tree-sitter-zig.git"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eglot)
(setq-default eglot-extend-to-xref)
(add-hook 'rust-mode-hook #'eglot-ensure)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode t)

(defun fill-column-100 ()
  "Set `fill-column' to 100."
  (setq-local fill-column 100))

(setq-default
 indent-tabs-mode nil
 tab-width        4
 fill-column      80)

(add-hook 'rust-mode-hook #'fill-column-100)

(defun eglot-format-buffer-before-save ()
  "Format the buffer if it is managed by eglot."
  (when (eglot-managed-p) (eglot-format-buffer)))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'before-save-hook #'eglot-format-buffer-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Auto Complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company mode is a frontend for code completion. Completions are done through
;; the completion at point functions. This comes with many useful completion
;; backends, including Eglot.
(require 'company)
(setq-default company-tooltip-minimum-width 64
              company-idle-delay            0.2)
(global-company-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default compile-command "")
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


;; Flymake is usually enabled automatically with Eglot mode. However, some modes
;; do not use LSP/Eglot.
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(defvar flymake-prefix-map nil)
(setq-local flymake-prefix-map
            (define-keymap
              "!" #'flymake-show-buffer-diagnostics))
(define-key flymake-mode-map (kbd "C-c !") flymake-prefix-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flyspell)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(cl-loop for key in '("C-," "C-;" "C-." "C-c $" "C-M-i")
         do (define-key flyspell-mode-map (kbd key) nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make flymake elisp backend of known packages.
(cl-loop for path in load-path
         do  (add-to-list 'elisp-flymake-byte-compile-load-path path))

(define-minor-mode emacs-lisp-eval-after-save-mode
  "Evaluate the current Emacs Lisp buffer after a successful save.")

(defun emacs-lisp-maybe-eval-after-save ()
  "Evaluate the current buffer if `emacs-lisp-eval-after-save-mode' is enabled."
  (when emacs-lisp-eval-after-save-mode
    (eval-buffer)))
(add-hook 'after-save-hook #'emacs-lisp-maybe-eval-after-save)

(defun emacs-lisp-config-eval-after-save ()
  "Enable `emacs-lisp-eval-after-save-mode' for the main configuration."
  (when (string-match-p "/init.el$" (buffer-file-name))
    (emacs-lisp-eval-after-save-mode)))
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-config-eval-after-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun electric-indent-mode-local-off ()
  "Turn off electric indent mode on the local buffer."
  (electric-indent-local-mode -1))

(add-hook 'org-mode-hook #'electric-indent-mode-local-off)

(setq-default org-support-shift-select t
              org-src-fontify-natively t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zig
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.zig$" . zig-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
;;; init.el ends here
