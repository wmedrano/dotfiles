;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window consult corfu diff-hl marginalia nerd-icons
                nerd-icons-completion orderless posframe vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.5)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package vertico
  :ensure t
  :commands (vertico-mode)
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :defines marginalia-annotators
  :commands (marginalia-mode)
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators
        (assq-delete-all 'project-file marginalia-annotators)))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :commands (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :defer 1
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x p b" . consult-project-buffer)
   ("C-x r b" . consult-bookmark)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-s l" . consult-line)
   ("M-s r" . consult-ripgrep)))

(use-package corfu
  :ensure t
  :commands (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :init
  (global-corfu-mode))

(use-package emacs
  :custom
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (use-short-answers t)
  (initial-frame-alist '((fullscreen . maximized)))
  (default-frame-alist '((fullscreen . maximized)))
  (inhibit-startup-screen t)
  (auto-save-default nil)
  (make-backup-files nil)
  (scroll-conservatively 101)
  (indent-tabs-mode nil)
  :config
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 140))

(use-package custom
  :custom
  (custom-theme-directory (locate-user-emacs-file "user-lisp"))
  :config
  (load-theme 'wezterm t))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . eglot-ensure))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . eglot-ensure))

(use-package eglot-extras
  :load-path "user-lisp"
  :commands (eglot-format-on-save-mode)
  :hook ((rust-ts-mode typescript-ts-mode) . eglot-format-on-save-mode))

(use-package toml-ts-mode
  :mode "\\.toml\\'")

(use-package cargo-minor-mode
  :load-path "user-lisp"
  :hook ((rust-ts-mode . cargo-minor-mode)
         (toml-ts-mode . cargo-minor-mode-maybe-enable)))

(use-package autorevert
  :init
  (global-auto-revert-mode))

(use-package idle-gc-mode
  :defer 1
  :load-path "user-lisp"
  :commands (idle-gc-mode)
  :init
  (idle-gc-mode))

(use-package project)

(use-package monorepo
  :load-path "user-lisp"
  :after project
  :commands (monorepo-add-directory monorepo-remove-directory monorepo-find-file)
  :functions (monorepo-project-try)
  :init
  (autoload 'monorepo-project-try "monorepo")
  (add-to-list 'project-find-functions #'monorepo-project-try))

(use-package jj
  :load-path "user-lisp/jj"
  :commands (jj jj-log))

(use-package which-key
  :defer 2
  :custom
  (which-key-lighter "")
  (which-key-idle-delay 0.2)
  :init
  (which-key-mode))

(use-package flymake
  :hook (emacs-lisp-mode . flymake-mode))

(use-package diff-hl
  :ensure t
  :hook (dired-mode . diff-hl-dired-mode)
  :commands (global-diff-hl-mode diff-hl-flydiff-mode)
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package window
  :init
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(xref\\|Help\\|eldoc.*\\|Flymake diagnostics.*\\|jj-log\\)\\*\\'"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.2)))
  (add-to-list 'display-buffer-alist
               '((derived-mode . compilation-mode)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 1)
                 (window-height . 0.3))))

(use-package posframe
  :ensure t
  :functions (posframe-workable-p))

(use-package ace-window
  :ensure t
  :custom
  (aw-dispatch-always t)
  :custom-face
  (aw-leading-char-face
   ((t (:foreground "#ff3030"
        :background "#151515"
        :height 6.0
        :weight ultra-bold
        :family "JetBrains Mono"
        :box (:line-width 3 :color "white")))))
  :bind ("M-o" . ace-window))

(use-package ace-window-posframe
  :after ace-window
  :functions (ace-window-posframe-mode)
  :config
  (when (posframe-workable-p)
    (ace-window-posframe-mode 1)))

(defun display-line-numbers-off ()
  "Disable `display-line-numbers' in the current buffer."
  (display-line-numbers-mode -1))

(use-package display-line-numbers
  :custom
  (display-line-numbers-width 3)
  :hook (diff-mode . display-line-numbers-off)
  :init
  (global-display-line-numbers-mode))

(use-package compile
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-context-lines 3)
  :hook
  (compilation-mode . display-line-numbers-off)
  :bind
  (("<f5>" . recompile)
   ("M-g n" . next-error)
   ("M-g p" . previous-error)))

(use-package ansi-color
  :after compile
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package ansi-osc
  :after compile
  :custom
  (ansi-osc-for-compilation-buffer t)
  :hook
  (compilation-filter . ansi-osc-compilation-filter))

;;; init.el ends here
