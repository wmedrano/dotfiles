;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ace-window consult corfu marginalia orderless vertico)))
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
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

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
  :config
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

(use-package cargo-minor-mode
  :load-path "user-lisp"
  :hook rust-ts-mode)

(use-package autorevert
  :init
  (global-auto-revert-mode))

(use-package cargo-toml-ts-mode
  :load-path "user-lisp"
  :mode "/Cargo\\.toml\\'"
  :hook cargo-minor-mode)

(use-package idle-gc-mode
  :defer 1
  :load-path "user-lisp"
  :init
  (idle-gc-mode))

(use-package which-key
  :defer 2
  :custom
  (which-key-lighter "")
  :init
  (which-key-mode))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package display-line-numbers
  :custom
  (display-line-numbers-width 3)
  :init
  (global-display-line-numbers-mode))

;;; init.el ends here
