;;; c-snippets.el --- C snippets  -*- lexical-binding: t -*-

;;; Commentary:

;; Collection of snippets for C.

;;; Code:

(eval-when-compile
  (require 'skeleton))

;; Header file template
(define-skeleton c-snippets-header-template
  "Insert header file template for C."
  "Guard: "
  "#ifndef " str | - "
#define " str "

" _ "

#endif
")

(provide 'c-snippets)
;;; c-snippets.el ends here
