;;; pest-mode.el --- Major mode for pest PEG  -*- lexical-binding: t -*-

;;; Commentary:

;; Major mode for editing pest PEG files of the Rust parser pest.

;;; Code:

(eval-when-compile
  (require 'rx))

;;; Option

(defgroup pest nil
  "Major mode for pest PEG."
  :group 'languages)

(defcustom pest-indent-offset
  4
  "Number of spaces for each indentation level in `pest-mode'."
  :type 'integer
  :safe #'integerp)

;;; Keyword

(defconst pest-special-rule-keywords
  '("WHITESPACE" "COMMENT" "ANY" "SOI" "EOI" "POP" "POP_ALL" "PEEK" "PEEK_ALL"
    "DROP")
  "Keywords for special rules.")

(defconst pest-builtin-rule-keywords
  '("ASCII_DIGIT" "ASCII_NONZERO_DIGIT" "ASCII_BIN_DIGIT" "ASCII_OCT_DIGIT"
    "ASCII_HEX_DIGIT" "ASCII_ALPHA_LOWER" "ASCII_ALPHA_UPPER" "ASCII_ALPHA"
    "ASCII_ALPHANUMERIC" "ASCII" "NEWLINE")
  "Keywords for builtin rules.")

(defconst pest-forbidden-keywords
  '("abstract" "alignof" "as" "become" "box" "break" "const" "continue" "crate"
    "do" "else" "enum" "extern" "false" "final" "fn" "for" "if" "impl" "in"
    "let" "loop" "macro" "match" "mod" "move" "mut" "offsetof" "override" "priv"
    "proc" "pure" "pub" "ref" "return" "Self" "self" "sizeof" "static" "struct"
    "super" "trait" "true" "type" "typeof" "unsafe" "unsized" "use" "virtual"
    "where" "while" "yield")
  "Keywords that are not allowed as rule names.")

;;; Font-lock

(defvar pest-font-lock-keywords
  `((,(rx "=" (0+ space)
          (group (in "_@$!"))
          (0+ space) "{")
     (1 font-lock-keyword-face))
    (,(regexp-opt pest-forbidden-keywords 'symbols)
     (1 font-lock-warning-face))
    (,(regexp-opt pest-builtin-rule-keywords 'symbols)
     (1 font-lock-constant-face))
    (,(regexp-opt pest-special-rule-keywords 'symbols)
     (1 font-lock-builtin-face))
    (,(rx (group (1+ (or word (syntax symbol))))
          (0+ space) "=")
     (1 font-lock-variable-name-face)))
  "Font-lock keywords for `pest-mode'.")

;;; Syntax

(defvar pest-mode-syntax-table
  (let ((table (make-syntax-table)))
    (dolist (char '(?+ ?- ?* ?| ?& ?$ ?=))
      (modify-syntax-entry char "." table))
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `pest-mode'.")

(defvar pest-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (group "'")
        (or (and "\\" (or (and "u{" (** 1 6 hex) "}")
                          (and "x" (= 2 hex))
                          (in "nrt0\'\"\\")))
            (not (in "'\\")))
        (group "'"))
    (1 "\"")
    (2 "\"")))
  "Function to apply syntax properties for `pest-mode'.")

;;; Indentation

(defun pest--get-indent-column ()
  "Get indentation column for the current line."
  (let ((depth (car (syntax-ppss (line-beginning-position)))))
    (* pest-indent-offset
       (if (looking-at "\\s-*\\s)")
           (1- depth)
         depth))))

(defun pest-indent-line ()
  "Indent line for `pest-mode'."
  (let ((target (save-excursion
                  (indent-line-to (pest--get-indent-column))
                  (point))))
    (when (< (point) target)
      (goto-char target))))

;;; Completion

(defun pest--annotate-candidate (candidate)
  "Annotate CANDIDATE by its category."
  (if (member candidate pest-special-rule-keywords)
      " <s>"
    " <c>"))

(defun pest-completion-at-point ()
  "Complete symbol at point for `pest-mode'."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point))))
    (list start end
          (completion-table-dynamic
           (lambda (_) (append pest-special-rule-keywords
                               pest-builtin-rule-keywords)))
          :annotation-function #'pest--annotate-candidate)))

;;; Imenu

(defvar pest-imenu-generic-expression
  `((nil
     ,(rx bol (0+ space)
          (group (1+ (or word (syntax symbol))))
          (0+ space) "=")
     1))
  "Imenu generic expression for `pest-mode'.")

;;; Mode

(define-derived-mode pest-mode prog-mode "pest"
  "Major mode for pest PEG."
  (make-local-variable 'indent-line-function)
  (make-local-variable 'electric-indent-chars)
  (setf indent-line-function #'pest-indent-line)
  (push ?\} electric-indent-chars)

  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (setf font-lock-defaults '(pest-font-lock-keywords)
        syntax-propertize-function pest-syntax-propertize-function
        comment-start "//"
        comment-start-skip "//+\\s-*")

  (add-hook 'completion-at-point-functions #'pest-completion-at-point nil t)
  (setf imenu-generic-expression pest-imenu-generic-expression))

(provide 'pest-mode)
;;; pest-mode.el ends here
