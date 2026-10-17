;;; cargo-toml-ts-mode.el --- Major mode for Cargo.toml files -*- lexical-binding: t; -*-

;;; Commentary:

;; Major mode for Cargo manifests, derived from `toml-ts-mode'.

;;; Code:

(require 'toml-ts-mode)

;;;###autoload
(define-derived-mode cargo-toml-ts-mode toml-ts-mode "Cargo-TOML"
  "Major mode for editing Cargo.toml files.")

(provide 'cargo-toml-ts-mode)
;;; cargo-toml-ts-mode.el ends here
