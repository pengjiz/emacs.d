;;; gnuplot.el --- Major mode for gnuplot  -*- lexical-binding: t -*-

;;; Commentary:

;; Major mode for editing gnuplot scripts.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

;;; Option

(defgroup gnuplot nil
  "Major mode for gnuplot."
  :group 'languages)

(defcustom gnuplot-indent-offset
  4
  "Number of spaces for each indentation level in `gnuplot-mode'."
  :type 'integer
  :safe #'integerp)

(defcustom gnuplot-program
  "gnuplot"
  "Name of the gnuplot program."
  :type 'string)

(defcustom gnuplot-extra-options
  '("--persist")
  "Extra options passed to gnuplot."
  :type '(repeat string))

;;; Keyword

(defconst gnuplot-command-keywords
  '("bind" "cd" "clear" "exit" "fit" "help" "history" "load" "pause" "print"
    "pwd" "quit" "save" "set" "show" "unset" "update" "undefine" "test" "system"
    "raise" "lower" "eval" "shell" "reset" "reread" "refresh" "call" "plot"
    "splot" "replot")
  "Keywords for commands.")

(defconst gnuplot-builtin-function-keywords
  '("abs" "acosh" "acos" "arg" "asinh" "asin" "atan" "atanh" "atan2" "besj1"
    "besj0" "besy1" "besy0" "ceil" "column" "cosh" "cos" "erfc" "erf" "exp"
    "floor" "gamma" "ibeta" "igamma" "imag" "int" "inverf" "invnorm" "lgamma"
    "log" "log10" "norm" "rand" "real" "sgn" "sinh" "sin" "sqrt" "tanh" "tan"
    "tm_hour" "tm_mday" "tm_min" "tm_mon" "tm_sec" "tm_wday" "tm_yday" "tm_year"
    "valid" "EllipticPi" "EllipticE" "EllipticK" "words" "word" "value"
    "timecolumn" "substr" "strstrt" "strptime" "strlen" "stringcolumn"
    "strftime" "sprintf" "lambertw" "gprintf" "exists" "defined" "columnhead")
  "Keywords for builtin functions.")

(defconst gnuplot-plot-option-keywords
  '("axes" "every" "index" "lw" "lt" "ls" "linestyle" "linetype" "linewidth"
    "notitle" "pt" "ps" "pointsize" "pointtype" "smooth" "title" "using" "with"
    "noautoscale" "volatile" "matrix" "nonuniform" "binary" "fillstyle"
    "linecolor" "pointinterval" "nosurface" "nocontours" "nohidden3d")
  "Keywords for plot options.")

(defconst gnuplot-plot-style-keywords
  '("boxerrorbars" "boxes" "boxxyerrorbars" "candlesticks" "dots" "errorbars"
    "financebars" "fsteps" "histeps" "impulses" "lines" "linespoints" "points"
    "steps" "vector" "xerrorbars" "xyerrorbars" "yerrorbars" "vectors"
    "filledcurves" "labels" "rgbalpha" "rgbimage" "image" "circles" "pm3d"
    "histograms" "xyerrorlines" "xerrorlines" "errorlines" "yerrorlines")
  "Keywords for plot styles.")

;;; Font-lock

(defvar gnuplot-font-lock-keywords
  `(("!" . font-lock-negation-char-face)
    (,(rx "@" (1+ (or word (syntax symbol)))) . font-lock-preprocessor-face)
    (,(regexp-opt '("do" "for" "in" "while" "if" "else" "array") 'symbols)
     (1 font-lock-keyword-face))
    (,(regexp-opt gnuplot-command-keywords 'symbols)
     (1 font-lock-keyword-face))
    (,(regexp-opt gnuplot-builtin-function-keywords 'symbols)
     (1 font-lock-builtin-face))
    (,(regexp-opt gnuplot-plot-option-keywords 'symbols)
     (1 font-lock-keyword-face))
    (,(regexp-opt gnuplot-plot-style-keywords 'symbols)
     (1 font-lock-constant-face))
    (,(rx symbol-start "array" (1+ space)
          (group (1+ (or word (syntax symbol)))))
     (1 font-lock-variable-name-face))
    (,(rx (group (1+ (or word (syntax symbol))))
          (0+ space) "=")
     (1 font-lock-variable-name-face))
    (,(rx "[" (0+ space)
          (group (1+ (or word (syntax symbol))))
          (1+ space) "in" (1+ space))
     (1 font-lock-variable-name-face))
    (,(rx (group (1+ (or word (syntax symbol))))
          "(" (1+ (or word (syntax symbol) "," space)) ")"
          (0+ space) "=")
     (1 font-lock-function-name-face)))
  "Font-lock keywords for `gnuplot-mode'.")

;;; Syntax

(defvar gnuplot-mode-syntax-table
  (let ((table (make-syntax-table)))
    (dolist (char '(?+ ?- ?* ?/ ?% ?| ?& ?< ?> ?=))
      (modify-syntax-entry char "." table))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `gnuplot-mode'.")

;;; Indentation

(defun gnuplot--get-indent-column ()
  "Get indentation column for the current line."
  (let ((depth (car (syntax-ppss (line-beginning-position))))
        (closing (looking-at "\\s-*\\s)"))
        (continued (eq ?\\ (char-before (1- (point))))))
    (* gnuplot-indent-offset
       (+ depth
          (or (and closing -1) 0)
          (or (and continued 1) 0)))))

(defun gnuplot-indent-line ()
  "Indent line for `gnuplot-mode'."
  (let ((target (save-excursion
                  (indent-line-to (gnuplot--get-indent-column))
                  (point))))
    (when (< (point) target)
      (goto-char target))))

;;; Completion

(defvar gnuplot--keyword-category-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    (dolist (keyword gnuplot-command-keywords)
      (puthash keyword 'command table))
    (dolist (keyword gnuplot-builtin-function-keywords)
      (puthash keyword 'function table))
    (dolist (keyword gnuplot-plot-option-keywords)
      (puthash keyword 'option table))
    (dolist (keyword gnuplot-plot-style-keywords)
      (puthash keyword 'value table))
    table)
  "Categories for keywords.")

(defun gnuplot--annotate-candidate (candidate)
  "Annotate CANDIDATE by its category."
  (cl-case (gethash candidate gnuplot--keyword-category-table)
    ((command) " <c>")
    ((function) " <f>")
    ((option) " <o>")
    ((value) " <v>")))

(defun gnuplot-completion-at-point ()
  "Complete symbol at point for `gnuplot-mode'."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point))))
    (list start end gnuplot--keyword-category-table
          :annotation-function #'gnuplot--annotate-candidate)))

;;; Imenu

(defvar gnuplot-imenu-generic-expression
  `((nil
     ,(rx bol (0+ space)
          "array" (1+ space)
          (group (1+ (or word (syntax symbol)))))
     1)
    (nil
     ,(rx bol (0+ space)
          (group (1+ (or word (syntax symbol))))
          (0+ space) "=")
     1)
    ("Functions"
     ,(rx bol (0+ space)
          (group (1+ (or word (syntax symbol))))
          "(" (1+ (or word (syntax symbol) "," space)) ")"
          (0+ space) "=")
     1))
  "Imenu generic expression for `gnuplot-mode'.")

;;; Run

(defconst gnuplot--output-buffer-name "*gnuplot*"
  "Name of buffer for gnuplot output.")

(defun gnuplot-run-region (start end)
  "Run the region between START and END."
  (interactive "r")
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (when (get-buffer gnuplot--output-buffer-name)
    (kill-buffer gnuplot--output-buffer-name))
  (let* ((prefix (message "Running %s..." gnuplot-program))
         (args `(,@gnuplot-extra-options "-"))
         (status (apply #'call-process-region start end gnuplot-program
                        nil gnuplot--output-buffer-name nil args)))
    (message "%s%s" prefix (or (and (eq 0 status) "done")
                               "process exited abnormally")))
  (deactivate-mark))

(defun gnuplot-run-buffer ()
  "Run the current buffer."
  (interactive)
  (gnuplot-run-region (point-min) (point-max)))

;; Integration with Org Plot
(with-eval-after-load 'org-plot
  (setf (symbol-function 'gnuplot-send-buffer-to-gnuplot)
        #'gnuplot-run-buffer))

;;; Keymap

(defvar gnuplot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'gnuplot-run-region)
    (define-key map (kbd "C-c C-b") #'gnuplot-run-buffer)
    map)
  "Keymap for `gnuplot-mode'.")

;;; Mode

(define-derived-mode gnuplot-mode prog-mode "Gnuplot"
  "Major mode for gnuplot."
  (make-local-variable 'indent-line-function)
  (make-local-variable 'electric-indent-chars)
  (setf indent-line-function #'gnuplot-indent-line)
  (dolist (char '(?\; ?\}))
    (push char electric-indent-chars))

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (setf font-lock-defaults '(gnuplot-font-lock-keywords)
        comment-start "#"
        comment-start-skip "#+\\s-*")

  (add-hook 'completion-at-point-functions #'gnuplot-completion-at-point nil t)
  (setf imenu-generic-expression gnuplot-imenu-generic-expression))

(provide 'gnuplot)
;;; gnuplot.el ends here
