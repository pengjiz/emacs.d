;;; html-snippets.el --- HTML snippets  -*- lexical-binding: t -*-

;;; Commentary:

;; Collection of snippets for HTML.

;;; Code:

(eval-when-compile
  (require 'skeleton))

;; File template
(define-skeleton html-snippets-file-template
  "Insert file template for HTML."
  "Title: "
  "<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <title>" str | "Untitled" "</title>" _ "
  </head>
  <body>

  </body>
</html>
")

(provide 'html-snippets)
;;; html-snippets.el ends here
