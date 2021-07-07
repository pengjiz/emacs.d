;;; graphviz-dot-mode.el --- Major mode for DOT  -*- lexical-binding: t -*-

;;; Commentary:

;; Major mode for editing Graphviz DOT files.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

;;; Option

(defgroup graphviz-dot nil
  "Major mode for the Graphviz DOT language."
  :group 'languages)

(defcustom graphviz-dot-indent-offset
  2
  "Number of spaces for each indentation level in `graphviz-dot-mode'."
  :type 'integer
  :safe #'integerp)

;;; Keyword

(defconst graphviz-dot-terminal-keywords
  '("graph" "digraph" "subgraph" "node" "edge")
  "Keywords for terminals.")

(defconst graphviz-dot-attribute-keywords
  '("Damping" "K" "URL" "_background" "area" "arrowhead" "arrowsize" "arrowtail"
    "bb" "bgcolor" "center" "charset" "clusterrank" "color" "colorscheme"
    "comment" "compound" "concentrate" "constraint" "decorate" "defaultdist"
    "dim" "dimen" "dir" "diredgeconstraints" "distortion" "dpi" "edgeURL"
    "edgehref" "edgetarget" "edgetooltip" "epsilon" "esep" "fillcolor"
    "fixedsize" "fontcolor" "fontname" "fontnames" "fontpath" "fontsize"
    "forcelabels" "gradientangle" "group" "headURL" "head_lp" "headclip"
    "headhref" "headlabel" "headport" "headtarget" "headtooltip" "height" "href"
    "id" "image" "imagepath" "imagepos" "imagescale" "inputscale" "label"
    "labelURL" "label_scheme" "labelangle" "labeldistance" "labelfloat"
    "labelfontcolor" "labelfontname" "labelfontsize" "labelhref" "labeljust"
    "labelloc" "labeltarget" "labeltooltip" "landscape" "layer" "layerlistsep"
    "layers" "layerselect" "layersep" "layout" "len" "levels" "levelsgap"
    "lhead" "lheight" "lp" "ltail" "lwidth" "margin" "maxiter" "mclimit"
    "mindist" "minlen" "mode" "model" "mosek" "newrank" "nodesep" "nojustify"
    "normalize" "notranslate" "nslimit" "nslimit1" "ordering" "orientation"
    "orientation" "outputorder" "overlap" "overlap_scaling" "overlap_shrink"
    "pack" "packmode" "pad" "page" "pagedir" "pencolor" "penwidth" "peripheries"
    "pin" "pos" "quadtree" "quantum" "rank" "rankdir" "ranksep" "ratio" "rects"
    "regular" "remincross" "repulsiveforce" "resolution" "root" "rotate"
    "rotation" "samehead" "sametail" "samplepoints" "scale" "searchsize" "sep"
    "shape" "shapefile" "showboxes" "sides" "size" "skew" "smoothing" "sortv"
    "splines" "start" "style" "stylesheet" "tailURL" "tail_lp" "tailclip"
    "tailhref" "taillabel" "tailport" "tailtarget" "tailtooltip" "target"
    "tooltip" "truecolor" "vertices" "viewport" "voro_margin" "weight" "width"
    "xdotversion" "xlabel" "xlp" "z")
  "Keywords for attributes of terminals.")

;;; Font-lock

(defvar graphviz-dot-font-lock-keywords
  `((,(regexp-opt '("--" "->")) . font-lock-keyword-face)
    (,(regexp-opt graphviz-dot-terminal-keywords 'symbols)
     (1 font-lock-keyword-face))
    (,(rx symbol-start
          (group "strict")
          (1+ space) (or "graph" "digraph")
          symbol-end)
     (1 font-lock-warning-face))
    (,(regexp-opt graphviz-dot-attribute-keywords 'symbols)
     (1 font-lock-variable-name-face))
    (,(rx symbol-start (or "graph" "digraph" "subgraph") (1+ space)
          (group (1+ (or word (syntax symbol)))))
     (1 font-lock-function-name-face)))
  "Font-lock keywords for `graphviz-dot-mode'.")

;;; Syntax

(defvar graphviz-dot-mode-syntax-table
  (let ((table (make-syntax-table)))
    (dolist (char '(?- ?=))
      (modify-syntax-entry char "." table))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `graphviz-dot-mode'.")

(defvar graphviz-dot-syntax-propertize-function
  (syntax-propertize-rules
   ("^#" (0 "<"))
   ("-\\(>\\)" (1 ".")))
  "Function to apply syntax properties for `graphviz-dot-mode'.")

;;; Indentation

(defun graphviz-dot--get-indent-column ()
  "Get indentation column for the current line."
  (let ((depth (car (syntax-ppss (line-beginning-position)))))
    (* graphviz-dot-indent-offset
       (if (looking-at "\\s-*\\s)")
           (1- depth)
         depth))))

(defun graphviz-dot-indent-line ()
  "Indent line for `graphviz-dot-mode'."
  (let ((target (save-excursion
                  (indent-line-to (graphviz-dot--get-indent-column))
                  (point))))
    (when (< (point) target)
      (goto-char target))))

;;; Completion

(defun graphviz-dot--annotate-candidate (candidate)
  "Annotate CANDIDATE by its category."
  (if (member candidate graphviz-dot-terminal-keywords)
      " <t>"
    " <a>"))

(defun graphviz-dot-completion-at-point ()
  "Completion symbol at point for `graphviz-dot-mode'."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point))))
    (list start end
          (completion-table-dynamic
           (lambda (_) (append graphviz-dot-terminal-keywords
                               graphviz-dot-attribute-keywords)))
          :annotation-function #'graphviz-dot--annotate-candidate)))

;;; Imenu

(defvar graphviz-dot-imenu-generic-expression
  `((nil
     ,(rx symbol-start (or "graph" "digraph" "subgraph") (1+ space)
          (group (1+ (or word (syntax symbol)))))
     1))
  "Imenu generic expression for `graphviz-dot-mode'.")

;;; Compilation

(defvar compilation-error-regexp-alist-alist)
(defvar compilation-error-regexp-alist)

(with-eval-after-load 'compile
  (cl-pushnew `(graphviz
                ,(rx bol
                     "Error: "
                     (group (1+ nonl))
                     ": "
                     (1+ nonl)
                     "line "
                     (group (1+ digit)))
                1 2)
              compilation-error-regexp-alist-alist
              :test #'eq :key #'car)
  (cl-pushnew 'graphviz compilation-error-regexp-alist
              :test #'eq))

;;; Mode

(define-derived-mode graphviz-dot-mode prog-mode "DOT"
  "Major mode for the Graphviz DOT language."
  (make-local-variable 'indent-line-function)
  (make-local-variable 'electric-indent-chars)
  (setf indent-line-function #'graphviz-dot-indent-line)
  (dolist (char '(?\; ?\}))
    (push char electric-indent-chars))

  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (setf font-lock-defaults '(graphviz-dot-font-lock-keywords)
        syntax-propertize-function graphviz-dot-syntax-propertize-function
        comment-start "//"
        comment-start-skip (rx "/" (or (1+ "*") (1+ "/"))
                               (0+ space)))

  (add-hook 'completion-at-point-functions
            #'graphviz-dot-completion-at-point nil t)
  (setf imenu-generic-expression graphviz-dot-imenu-generic-expression))

(provide 'graphviz-dot-mode)
;;; graphviz-dot-mode.el ends here
