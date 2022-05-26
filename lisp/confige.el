;;; confige.el --- Simple configuration macro  -*- lexical-binding: t -*-

;;; Commentary:

;; A simple macro for organizing user configuration.

;;; Code:

(require 'package)
(eval-when-compile
  (require 'subr-x))

;;; Option

(defgroup confige nil
  "Organize configuration easily."
  :group 'initialization)

(defcustom confige-select-package
  nil
  "Whether to mark a package as selected when installing it.
Non-nil means adding the package to `package-selected-packages'
at installation."
  :type 'boolean)

(defcustom confige-keyword-default-alist
  nil
  "Default values for keywords.
Those values are only used when the corresponding keywords are
not given to the macro."
  :type '(alist :key-type symbol
                :value-type sexp))

;;; Helper

(defmacro confige-preload (feature)
  "Require FEATURE only at compile time."
  (declare (debug (form)))
  (when (bound-and-true-p byte-compile-current-file)
    `(eval-when-compile
       (require ,feature))))

(defun confige-ensure-package (package)
  "Ensure that PACKAGE is installed."
  (unless (package-installed-p package)
    (package--archives-initialize)
    (unless (assq package package-archive-contents)
      (package-refresh-contents))
    (package-install package (not confige-select-package))))

;;; Core

(defvar confige-keyword-registry nil
  "Registry for keywords.
Use `confige-register' to add new keywords.")

(defun confige-register (keyword handler order &optional compact)
  "Register KEYWORD with HANDLER function.
ORDER specifies the order of processing. If COMPACT is non-nil,
the keyword and value should be given directly. Otherwise they
should be grouped as a list."
  (declare (indent 1))
  (unless (keywordp keyword)
    (error "Invalid keyword %S" keyword))
  (unless (functionp handler)
    (error "Invalid handler %S" handler))
  (unless (numberp order)
    (error "Invalid order %S" order))
  (push (list keyword :order order :handler handler :compact compact)
        confige-keyword-registry))

(defun confige--keyword-compact-p (keyword)
  "Return non-nil if KEYWORD should be compact."
  (if-let* ((spec (cdr (assq keyword confige-keyword-registry))))
      (plist-get spec :compact)
    (error "Unregistered keyword %S" keyword)))

(defun confige--normalize (args)
  "Normalize ARGS into regular blocks."
  (let ((keyword nil)
        (blocks nil))
    (dolist (arg args)
      (cond (keyword
             (push (cons keyword arg) blocks)
             (setf keyword nil))
            ((keywordp arg)
             (unless (confige--keyword-compact-p arg)
               (error "Invalid format for extended keyword %S" arg))
             (setf keyword arg))
            ((listp arg)
             (let ((key (car arg)))
               (when (confige--keyword-compact-p key)
                 (error "Invalid format for compact keyword %S" key)))
             (push arg blocks))
            (t
             (error "Invalid argument %S" arg))))
    (when keyword
      (error "Dangling keyword %S" keyword))
    (nreverse blocks)))

(defun confige--get-block-order (block)
  "Get the order for BLOCK."
  (when-let* ((keyword (car block))
              (spec (cdr (assq keyword confige-keyword-registry))))
    (plist-get spec :order)))

(defun confige--prepare (args)
  "Prepare ARGS into blocks for processing."
  (let ((blocks (confige--normalize args))
        (defaults nil))
    (dolist (default confige-keyword-default-alist)
      (unless (assq (car default) blocks)
        (push default defaults)))
    (sort (append blocks (nreverse defaults))
          (lambda (first second)
            (< (confige--get-block-order first)
               (confige--get-block-order second))))))

(defun confige--handle-block (block)
  "Handle BLOCK using the appropriate keyword handler."
  (when-let* ((keyword (car block))
              (spec (cdr (assq keyword confige-keyword-registry)))
              (handler (plist-get spec :handler)))
    (funcall handler block)))

(defun confige--process (blocks)
  "Process BLOCKS with registered keywords."
  (mapcan #'confige--handle-block blocks))

(defvar confige--unit-spec nil "Spec of a unit.")

(defun confige-set (key value)
  "Set VALUE for KEY in the unit spec.
This function should be mainly used in a keyword handler."
  (push (cons key value) confige--unit-spec))

(defun confige-get (key)
  "Get value for KEY in the unit spec.
This function should be mainly used in a keyword handler."
  (cdr (assq key confige--unit-spec)))

(defmacro confige (name &rest args)
  "Configure a unit of NAME according to ARGS.
NAME is usually but not necessarily the name of a package or a
feature. ARGS will be processed based on registered keywords in
`confige-keyword-registry'."
  (declare (indent 1)
           (debug (symbolp &rest &or (symbolp &rest form) [symbolp sexp])))
  (let ((confige--unit-spec nil))
    (confige-set 'name name)
    (macroexp-progn (confige--process (confige--prepare args)))))

;;; Keyword

(defun confige--handle-feature (block)
  "Handle BLOCK for setting feature name."
  (if-let* ((value (cdr block))
            (feature (and (symbolp value) value)))
      (ignore (confige-set 'feature feature))
    (error "Invalid feature name %S" value)))

(defun confige--handle-ensure (block)
  "Handle BLOCK for package installation."
  (when-let* ((value (cdr block))
              (packages (or (and (listp value) value)
                            (and value (list (confige-get 'name))))))
    (mapcar (lambda (package)
              `(confige-ensure-package ',package))
            packages)))

(defun confige--handle-preload (block)
  "Handle BLOCK for loading feature at compile time."
  (when-let* ((feature (and (cdr block)
                            (or (confige-get 'feature)
                                (confige-get 'name)))))
    `((confige-preload ',feature))))

(defun confige--handle-load (block)
  "Handle BLOCK for loading feature."
  (when-let* ((feature (and (cdr block)
                            (or (confige-get 'feature)
                                (confige-get 'name)))))
    (confige-set 'loaded t)
    `((require ',feature))))

(defun confige--handle-after (block)
  "Handle BLOCK for forms after loading feature."
  (let* ((forms (cdr block)))
    (if (confige-get 'loaded)
        forms
      `((with-eval-after-load ',(or (confige-get 'feature)
                                    (confige-get 'name))
          ,@forms)))))

(defun confige--plain-handle (block)
  "Handle BLOCK for plain forms."
  (cdr block))

(confige-register :feature #'confige--handle-feature 10 t)
(confige-register :ensure #'confige--handle-ensure 20 t)
(confige-register :preload #'confige--handle-preload 30 t)
(confige-register :preface #'confige--plain-handle 40)
(confige-register :before #'confige--plain-handle 50)
(confige-register :load #'confige--handle-load 60 t)
(confige-register :after #'confige--handle-after 70)
(confige-register :postface #'confige--plain-handle 80)

(provide 'confige)
;;; confige.el ends here
