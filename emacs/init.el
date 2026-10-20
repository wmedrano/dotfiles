;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; ----------------------------------------------------------------------------
;;; 1. Early Startup & Performance
;;; ----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window consult corfu diff-hl dracula-theme marginalia
                markdown-ts-mode nerd-icons nerd-icons-completion
                orderless posframe vertico))
 '(safe-local-variable-values
   '((compilation-scroll-output . first-error) (vc-handled-backends))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.5)

;; Load a .el file instead of its stale .elc, e.g. in user-lisp/.
(setq load-prefer-newer t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package idle-gc-mode
  :defer 1
  :load-path "user-lisp"
  :commands (idle-gc-mode)
  :init
  (idle-gc-mode))

;;; ----------------------------------------------------------------------------
;;; 2. Core Editor & Defaults
;;; ----------------------------------------------------------------------------

(use-package emacs
  :custom
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (blink-cursor-mode nil)
  (use-short-answers t)
  (initial-frame-alist '((fullscreen . maximized)))
  (default-frame-alist '((fullscreen . maximized)))
  (inhibit-startup-screen t)
  (auto-save-default nil)
  (make-backup-files nil)
  (scroll-conservatively 101)
  (scroll-margin 8)
  (indent-tabs-mode nil)
  (fill-column 80)
  (ring-bell-function #'ignore)
  :config
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 120))

(use-package autorevert
  :init
  (global-auto-revert-mode))

(use-package which-key
  :defer 2
  :custom
  (which-key-lighter "")
  (which-key-idle-delay 0.2)
  :init
  (which-key-mode))

(use-package isearch
  :custom
  (isearch-allow-scroll t))

;;; ----------------------------------------------------------------------------
;;; 3. Visuals & UI
;;; ----------------------------------------------------------------------------

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)
  (set-face-attribute 'line-number-current-line nil
                      :inherit 'line-number
                      :weight 'bold
                      :slant 'normal
                      :foreground "#f1fa8c"
                      :background "#44475a"))

(use-package nerd-icons
  :ensure t)

(defun display-line-numbers-off ()
  "Disable `display-line-numbers' in the current buffer."
  (display-line-numbers-mode -1))

(use-package display-line-numbers
  :custom
  (display-line-numbers-width 3)
  :hook ((diff-mode . display-line-numbers-off)
         (jj-log-mode . display-line-numbers-off)
         (help-mode . display-line-numbers-off)
         (gent-mode . display-line-numbers-off)
         (dired-mode . display-line-numbers-off)
         (xref--xref-buffer-mode . display-line-numbers-off)
         (flymake-diagnostics-buffer-mode . display-line-numbers-off)
         (flymake-project-diagnostics-mode . display-line-numbers-off)
         (messages-buffer-mode . display-line-numbers-off)
         (profiler-report-mode . display-line-numbers-off))
  :init
  (global-display-line-numbers-mode))

;;; ----------------------------------------------------------------------------
;;; 4. Window Management & Navigation
;;; ----------------------------------------------------------------------------

(use-package window
  :bind ("M-o" . other-window)
  :init
  (add-to-list 'display-buffer-alist
               '((derived-mode . (compilation-mode
                                  flymake-diagnostics-buffer-mode
                                  flymake-project-diagnostics-mode))
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.2)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(xref\\|Help\\|eldoc.*\\|jj-log\\)\\*\\'"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 1)
                 (window-height . 0.2))))

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
  :bind ("C-c w" . ace-window))

(use-package ace-window-posframe
  :after ace-window
  :functions (ace-window-posframe-mode)
  :config
  (when (posframe-workable-p)
    (ace-window-posframe-mode 1)))

;;; ----------------------------------------------------------------------------
;;; 5. Completion Ecosystem
;;; ----------------------------------------------------------------------------

(use-package savehist
  :init
  (savehist-mode))

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

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :commands (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

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

;;; ----------------------------------------------------------------------------
;;; 6. Project Management & Version Control
;;; ----------------------------------------------------------------------------

(use-package project)

(use-package file-extra
  :load-path "user-lisp"
  :commands (copy-filename copy-filename-absolute))

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
  :commands (jj jj-log)
  :bind
  (("C-c j" . jj)))

(use-package diff-hl
  :ensure t
  :hook (dired-mode . diff-hl-dired-mode)
  :commands (global-diff-hl-mode diff-hl-flydiff-mode)
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package diff-mode
  :bind
  (:map diff-mode-map
        ("RET" . diff-goto-source)
        ("M-o" . nil)))

;;; ----------------------------------------------------------------------------
;;; 7. Development & IDE Tooling
;;; ----------------------------------------------------------------------------

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

(use-package flymake
  :hook (emacs-lisp-mode . flymake-mode)
  :bind
  (("M-g d" . flymake-show-buffer-diagnostics)
   ("M-g D" . flymake-show-project-diagnostics)))

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-minor-mode-string nil))

(use-package eldoc-extra
  :load-path "user-lisp"
  :bind ("<f1>" . eldoc-extra-toggle-buffer))

(use-package eglot
  :preface
  (defun eglot-inlay-hints-off ()
    "Disable Eglot inlay hints in the current buffer."
    (eglot-inlay-hints-mode -1))
  :hook (eglot-managed-mode . eglot-inlay-hints-off)
  :commands (eglot-inlay-hints-mode)
  :bind
  (:prefix "C-c e"
   :prefix-map eglot-prefix-map
   ("r" . eglot-rename)
   ("t" . eglot-momentary-inlay-hints)
   ("f" . flymake-show-buffer-diagnostics)
   ("F" . flymake-show-project-diagnostics)
   ("a" . eglot-code-actions)
   ("d" . xref-find-definitions)
   ("x" . xref-find-references)))

(use-package eglot-extras
  :load-path "user-lisp"
  :commands (eglot-format-on-save-mode)
  :hook ((rust-ts-mode typescript-ts-mode) . eglot-format-on-save-mode))

;;; ----------------------------------------------------------------------------
;;; 8. Major Modes & Tools
;;; ----------------------------------------------------------------------------

(defun set-fill-column-100 ()
  "Set `fill-column' to 100 in the current buffer."
  (interactive)
  (setq fill-column 100))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook ((rust-ts-mode . eglot-ensure)
         ((rust-ts-mode rust-mode) . set-fill-column-100)))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . eglot-ensure))

(use-package toml-ts-mode
  :mode "\\.toml\\'")

(use-package cargo-minor-mode
  :load-path "user-lisp"
  :hook ((rust-ts-mode . cargo-minor-mode)
         (toml-ts-mode . cargo-minor-mode-maybe-enable)))

(use-package markdown-ts-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package gent
  :load-path "user-lisp/gent"
  :commands (gent)
  :functions (gent-llm-make-dumile-backend gent-llm-make-opencode-go-backend
              gent-tools-names)
  :defines (gent-backends gent-llm-default-backend gent-tools-trusted)
  :preface
  (defun pi-api-key (provider)
    "Return the API key for PROVIDER, a symbol, from the pi agent's auth file."
    (with-temp-buffer
      (insert-file-contents "~/.pi/agent/auth.json")
      (alist-get 'key (alist-get provider (json-parse-buffer :object-type 'alist)))))
  (defun set-gent-backend (name)
    "Set `gent-llm-default-backend' to the backend NAME in `gent-backends'."
    (interactive
     (progn
       (require 'gent)
       (list (completing-read "Backend: " gent-backends nil t))))
    (setq gent-llm-default-backend (alist-get name gent-backends nil nil #'equal))
    (message "gent: using %s" name))
  :config
  (require 'gent-providers)
  (setq gent-tools-trusted (gent-tools-names))
  (setq gent-backends
        `(("dumile/qwen-flash-next"
           . ,(gent-llm-make-dumile-backend
               :api-key (lambda () (pi-api-key 'dumile))
               :model "qwen-flash-next"))
          ("opencodego/deepseek-v4.1-flash"
           . ,(gent-llm-make-opencode-go-backend
               :api-key (lambda () (pi-api-key 'opencode-go))
               :model "deepseek-v4.1-flash"))))
  (set-gent-backend "opencodego/deepseek-v4.1-flash"))

;;; init.el ends here
