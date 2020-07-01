;;; emacs-lisp-snippets.el --- Emacs Lisp snippets  -*- lexical-binding: t -*-

;;; Commentary:

;; Collection of snippets for Emacs Lisp.

;;; Code:

(eval-when-compile
  (require 'skeleton))

;; File template
(define-skeleton emacs-lisp-snippets-file-template
  "Insert file template for Emacs Lisp."
  "Name: "
  ";;; " str | - ".el --- " _ "  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:



(provide '" str ")
;;; " str ".el ends here
")

(provide 'emacs-lisp-snippets)
;;; emacs-lisp-snippets.el ends here
