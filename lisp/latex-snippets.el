;;; latex-snippets.el --- LaTeX snippets  -*- lexical-binding: t -*-

;;; Commentary:

;; Collection of snippets for LaTeX.

;;; Code:

(eval-when-compile
  (require 'skeleton))

;; File template
(define-skeleton latex-snippets-file-template
  "Insert file template for LaTeX."
  "Class: "
  "\\documentclass[11pt]{" str | "scrartcl" "}

\\title{" _ "}
\\author{}
\\date{}

\\begin{document}

\\maketitle

\\end{document}
")

(provide 'latex-snippets)
;;; latex-snippets.el ends here
