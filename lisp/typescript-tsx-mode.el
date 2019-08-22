;;; typescript-tsx-mode.el --- Major mode for TSX files  -*- lexical-binding: t; -*-

;;; Commentary:

;; Major mode for editing TypeScript TSX files.

;;; Code:

(require 'web-mode)

(define-derived-mode typescript-tsx-mode web-mode
  "TypeScript-TSX"
  "Major mode for the TypeScript TSX language.")

;; Integration with Flycheck
(declare-function flycheck-add-mode "ext:flycheck")

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))
(with-eval-after-load 'tide
  (flycheck-add-mode 'tsx-tide 'typescript-tsx-mode))

(provide 'typescript-tsx-mode)
;;; typescript-tsx-mode.el ends here
