;;; wezterm-theme.el --- Theme based on WezTerm's default colors -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom theme built from WezTerm's built-in default palette (see
;; `ColorPalette::compute_default' in wezterm's term/src/color.rs).

;;; Code:

(deftheme wezterm
  "Theme based on WezTerm's default color palette.")

(let ((bg            "#000000")
      (fg            "#b2b2b2")
      (black         "#000000")
      (red           "#cc5555")
      (green         "#55cc55")
      (yellow        "#cdcd55")
      (blue          "#5455cb")
      (magenta       "#cc55cc")
      (cyan          "#7acaca")
      (white         "#cccccc")
      (bright-black  "#555555")
      (bright-red    "#ff5555")
      (bright-green  "#55ff55")
      (bright-yellow "#ffff55")
      (bright-blue   "#5555ff")
      (bright-magenta "#ff55ff")
      (bright-cyan   "#55ffff")
      (bright-white  "#ffffff")
      (grey          "#808080")
      (cursor        "#52ad70")
      ;; WezTerm's selection is #806699 at 50% alpha, blended over black.
      (selection     "#40334c")
      (scrollbar     "#222222")
      (split         "#444444"))
  (custom-theme-set-faces
   'wezterm
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,cursor))))
   `(region ((t (:background ,selection :extend t))))
   `(highlight ((t (:background ,split))))
   `(hl-line ((t (:background ,scrollbar :extend t))))
   `(fringe ((t (:background ,bg))))
   `(vertical-border ((t (:foreground ,split))))
   `(window-divider ((t (:foreground ,split))))
   `(shadow ((t (:foreground ,grey))))
   `(minibuffer-prompt ((t (:foreground ,cyan :weight bold))))
   `(link ((t (:foreground ,bright-blue :underline t))))
   `(link-visited ((t (:foreground ,magenta :underline t))))
   `(error ((t (:foreground ,bright-red :weight bold))))
   `(warning ((t (:foreground ,bright-yellow :weight bold))))
   `(success ((t (:foreground ,bright-green :weight bold))))
   `(match ((t (:foreground ,black :background ,yellow))))
   `(isearch ((t (:foreground ,black :background ,bright-yellow))))
   `(lazy-highlight ((t (:foreground ,bright-white :background ,blue))))
   `(isearch-fail ((t (:foreground ,bright-white :background ,red))))
   `(show-paren-match ((t (:background ,split :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,bright-white :background ,red))))
   `(trailing-whitespace ((t (:background ,red))))

   ;; Mode line
   `(mode-line ((t (:foreground ,white :background ,split))))
   `(mode-line-inactive ((t (:foreground ,grey :background ,scrollbar))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(header-line ((t (:foreground ,white :background ,scrollbar))))

   ;; Line numbers
   `(line-number ((t (:foreground ,bright-black :background ,bg))))
   `(line-number-current-line ((t (:foreground ,white :background ,bg))))

   ;; Font lock
   `(font-lock-builtin-face ((t (:foreground ,bright-magenta))))
   `(font-lock-comment-face ((t (:foreground ,grey))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,grey))))
   `(font-lock-constant-face ((t (:foreground ,red))))
   `(font-lock-doc-face ((t (:foreground ,green))))
   `(font-lock-function-name-face ((t (:foreground ,cyan))))
   `(font-lock-keyword-face ((t (:foreground ,magenta))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,bright-red))))
   `(font-lock-preprocessor-face ((t (:foreground ,bright-cyan))))
   `(font-lock-negation-char-face ((t (:foreground ,bright-red))))

   ;; Terminal colors (term, vterm, eshell, compilation output)
   `(ansi-color-black ((t (:foreground ,black :background ,black))))
   `(ansi-color-red ((t (:foreground ,red :background ,red))))
   `(ansi-color-green ((t (:foreground ,green :background ,green))))
   `(ansi-color-yellow ((t (:foreground ,yellow :background ,yellow))))
   `(ansi-color-blue ((t (:foreground ,blue :background ,blue))))
   `(ansi-color-magenta ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-cyan ((t (:foreground ,cyan :background ,cyan))))
   `(ansi-color-white ((t (:foreground ,white :background ,white))))
   `(ansi-color-bright-black ((t (:foreground ,bright-black :background ,bright-black))))
   `(ansi-color-bright-red ((t (:foreground ,bright-red :background ,bright-red))))
   `(ansi-color-bright-green ((t (:foreground ,bright-green :background ,bright-green))))
   `(ansi-color-bright-yellow ((t (:foreground ,bright-yellow :background ,bright-yellow))))
   `(ansi-color-bright-blue ((t (:foreground ,bright-blue :background ,bright-blue))))
   `(ansi-color-bright-magenta ((t (:foreground ,bright-magenta :background ,bright-magenta))))
   `(ansi-color-bright-cyan ((t (:foreground ,bright-cyan :background ,bright-cyan))))
   `(ansi-color-bright-white ((t (:foreground ,bright-white :background ,bright-white))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'wezterm)
;;; wezterm-theme.el ends here
