;;; pest-mode.el --- Major mode for pest PEG -*- lexical-binding: t -*-

;;; Commentary:

;; Major mode for editing pest PEG files of the Rust parser pest.

;;; Code:

(eval-when-compile
  (require 'rx))

;;; Options

(defgroup pest nil
  "Major mode for pest PEG."
  :group 'languages)

(defcustom pest-indent-offset
  2
  "Number of spaces for each indentation level in `pest-mode'."
  :type 'integer
  :safe #'integerp
  :group 'pest)

;;; Keywords

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
  `((,(regexp-opt pest-special-rule-keywords 'symbols)
     (1 font-lock-keyword-face))
    (,(regexp-opt pest-builtin-rule-keywords 'symbols)
     (1 font-lock-constant-face))
    (,(regexp-opt pest-forbidden-keywords 'symbols)
     (1 font-lock-warning-face))
    (,(rx (group (in "_@$!"))
          (* (syntax whitespace))
          "{")
     (1 font-lock-builtin-face)))
  "Font-lock keywords for `pest-mode'.")

;;; Syntax

(defvar pest-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (dolist (char '(?= ?~ ?| ?* ?+ ?? ?& ?!))
      (modify-syntax-entry char "." table))
    (modify-syntax-entry ?\" "\"" table)
    ;; Single quotes are for character literals, but there seems to be no such
    ;; syntax class. So we use string quotes as what rust-mode does.
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `pest-mode'.")

;;; Indentation

(defun pest-indent-line ()
  "Indent line for `pest-mode'."
  (let (indent-point)
    (save-excursion
      (let ((level (car (syntax-ppss (line-beginning-position)))))
        (when (looking-at "\\s-*\\s)")
          (setf level (1- level)))
        (indent-line-to (* pest-indent-offset level))
        (setf indent-point (point))))
    (unless (> (point) indent-point)
      (goto-char indent-point))))

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

;;; Mode

(define-derived-mode pest-mode prog-mode "pest"
  "Major mode for pest PEG."
  (make-local-variable 'indent-line-function)
  (setf indent-line-function #'pest-indent-line)

  (setf font-lock-defaults '(pest-font-lock-keywords))

  (make-local-variable 'comment-start)
  (setf comment-start "//")
  (make-local-variable 'comment-start-skip)
  (setf comment-start-skip (rx "/"
                               (1+ "/")
                               (0+ (syntax whitespace))))

  (add-hook 'completion-at-point-functions
            #'pest-completion-at-point nil t))

(provide 'pest-mode)
;;; pest-mode.el ends here
