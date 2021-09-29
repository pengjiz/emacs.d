;;; liteline.el --- Lightweight mode line  -*- lexical-binding: t; -*-

;;; Commentary:

;; A lightweight mode line configuration.

;;; Code:

(require 'compile)
(require 'vc-hooks)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'let-alist))

;;; Face and option

(defgroup liteline nil
  "A lightweight mode line."
  :group 'mode-line)

(defcustom liteline-minor-mode-indicator-alist
  '((typo-mode . "t")
    (abbrev-mode . "r")
    (auto-fill-function . "q")
    (visual-line-mode . "v")
    (visible-mode . "p")
    (whitespace-mode . "w")
    (flyspell-mode . "s")
    (auto-revert-mode . "g"))
  "Indicators for minor modes that are shown when enabled."
  :type '(alist :key-type symbol
                :value-type string))

(defface liteline-transient
  '((t :inherit mode-line-highlight))
  "Face used by transient information.")

(defface liteline-buffer-name
  '((t :inherit mode-line-buffer-id))
  "Face used by normal buffer names.")

(defface liteline-buffer-file-missing
  '((t :inherit (bold error)))
  "Face used for buffers backed by missing files.")

(defface liteline-git-branch
  '((t :inherit vc-up-to-date-state))
  "Face used by Git branch names.")

(defface liteline-git-new
  '((t :inherit success))
  "Face used for new files in Git.")

(defface liteline-git-edited
  '((t :inherit warning))
  "Face used for edited files in Git.")

(defface liteline-git-warning
  '((t :inherit (bold warning)))
  "Face used for warnings in Git.")

(defface liteline-git-error
  '((t :inherit (bold error)))
  "Face used for errors in Git.")

(defface liteline-flycheck-general
  '((t :inherit compilation-mode-line-run))
  "Face used to show general Flycheck messages.")

(defface liteline-flycheck-urgent
  '((t :inherit compilation-mode-line-fail))
  "Face used to show urgent Flycheck messages.")

(defface liteline-flycheck-clean
  '((t :inherit compilation-mode-line-exit))
  "Face used to show there are no Flycheck issues.")

(defface liteline-flycheck-info
  '((t :inherit compilation-info))
  "Face used by Flycheck information count.")

(defface liteline-flycheck-warning
  '((t :inherit compilation-warning))
  "Face used by Flycheck warning count.")

(defface liteline-flycheck-error
  '((t :inherit compilation-error))
  "Face used by Flycheck error count.")

;;; Mode line helper

;; Core
(eval-and-compile
  (defvar liteline--segment-fn-alist nil "Functions of segments.")

  (defun liteline--prepare-segments (segments)
    "Prepare mode line forms of SEGMENTS.
Return forms that can be passed directly to `format-mode-line'."
    (let (forms)
      (dolist (segment segments)
        (if (stringp segment)
            (push segment forms)
          (if-let* ((fn (cdr (assq segment liteline--segment-fn-alist))))
              (push `(:eval (,fn)) forms)
            (error "%s is not a valid segment" segment))))
      (cons "" (nreverse forms)))))

(defmacro liteline-define-segment (name doc &rest body)
  "Define a mode line segment function for NAME with DOC and BODY."
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name stringp def-body)))
  (let ((symbol (intern (format "liteline--segment-%s" name))))
    (cl-pushnew (cons name symbol) liteline--segment-fn-alist
                :test #'eq :key #'car)
    `(progn
       (defun ,symbol ()
         ,(or doc (format "Show the %s information." name))
         ,@body)
       (cl-pushnew (cons ',name #',symbol) liteline--segment-fn-alist
                   :test #'eq :key #'car))))

(defvar liteline--mode-line-fn-alist nil "Functions of mode lines.")

(defvar-local liteline--mode-line-right-string nil
  "Formatted string for the right part of mode line.")
(put 'liteline--mode-line-right-string 'risky-local-variable t)

(defun liteline--prepare-mode-line (name)
  "Prepare the mode line of NAME."
  (if-let* ((fn (cdr (assq name liteline--mode-line-fn-alist))))
      `(:eval (,fn))
    (error "%s is not a valid mode line" name)))

(defun liteline--wrap-mode-line (left right)
  "Wrap LEFT and RIGHT segment forms into a full mode line."
  (setf liteline--mode-line-right-string (format-mode-line right))
  (remove-list-of-text-properties 0 (length liteline--mode-line-right-string)
                                  '(mouse-face help-echo keymap local-map)
                                  liteline--mode-line-right-string)
  (let* ((width (string-width liteline--mode-line-right-string))
         (space `(space :align-to (- (+ right right-fringe right-margin)
                                     ,width)))
         (padding (propertize " " 'display space)))
    `(,left ,padding liteline--mode-line-right-string)))

(defmacro liteline-define-mode-line (name left &optional right)
  "Define a mode line function for NAME with LEFT and RIGHT segments."
  (declare (indent 1)
           (debug (&define name listp [&optional listp])))
  (let ((symbol (intern (format "liteline--mode-line-%s" name))))
    `(progn
       (defun ,symbol ()
         ,(format "Show the %s mode line." name)
         (liteline--wrap-mode-line ',(liteline--prepare-segments left)
                                   ',(liteline--prepare-segments right)))
       (cl-pushnew (cons ',name #',symbol) liteline--mode-line-fn-alist
                   :test #'eq :key #'car))))

(defun liteline-set-mode-line (name &optional default)
  "Set `mode-line-format' to the mode line of NAME.
If DEFAULT is non-nil, set the default value."
  (setf (if default
            (default-value 'mode-line-format)
          mode-line-format)
        (list "%e" (liteline--prepare-mode-line name))))

;; Active window
(defvar liteline--active-window nil "Current active window.")

(defun liteline--set-active-window (&rest _)
  "Set the current active window."
  (let ((window (selected-window)))
    (setf liteline--active-window
          (if (minibuffer-window-active-p window)
              (minibuffer-selected-window)
            window))))

(defun liteline--window-active-p ()
  "Return t if the selected window is active."
  (eq (selected-window) liteline--active-window))

(defun liteline--setup-active-window ()
  "Setup active window detection."
  (liteline--set-active-window)
  (add-hook 'pre-redisplay-functions #'liteline--set-active-window))

;;; Segment

;; Transient information
(defun liteline--get-macro-indicator ()
  "Return an indicator when defining keyboard macros."
  (and defining-kbd-macro "M>"))

(defun liteline--get-recursive-editing-depth ()
  "Return recursion depth when in recursive editing."
  (let ((depth (- (recursion-depth) (minibuffer-depth))))
    (when (> depth 0)
      (format "@%d" depth))))

(liteline-define-segment transient
  "Show transient information."
  (when (liteline--window-active-p)
    (let (indicators)
      (dolist (fn '(liteline--get-macro-indicator
                    liteline--get-recursive-editing-depth))
        (when-let* ((indicator (funcall fn)))
          (push " " indicators)
          (push indicator indicators)))
      (when indicators
        (propertize (apply #'concat " " indicators)
                    'face 'liteline-transient)))))

;; Basic buffer information
(defun liteline--get-buffer-name ()
  "Return buffer name."
  (let ((face (cond ((not buffer-file-name) 'liteline-buffer-name)
                    ((and (file-remote-p buffer-file-name)
                          (not (file-remote-p buffer-file-name nil t)))
                     'liteline-buffer-file-missing)
                    ((not (file-exists-p buffer-file-name))
                     'liteline-buffer-file-missing)
                    (t 'liteline-buffer-name))))
    (propertize "%b" 'face face)))

(liteline-define-segment basic
  "Show basic buffer information."
  (let ((narrow (or (and (buffer-narrowed-p) "#") "-"))
        (name (liteline--get-buffer-name)))
    `(" %Z%*%+" ,narrow "%@ " ,name " ")))

;; Position
(declare-function image-get-display-property "image-mode")
(declare-function image-display-size "image-mode")
(declare-function image-mode-window-get "image-mode")
(declare-function doc-view-last-page-number "doc-view")

(defvar-local liteline--image-size nil "Current image size.")
(put 'liteline--image-size 'risky-local-variable t)

(liteline-define-segment position
  "Show position information."
  (cl-case major-mode
    ((image-mode)
     ;; NOTE: It may fail to get the desired display property when ace-window is
     ;; active because ace-window adds additional overlays to windows.
     (when-let* ((spec (and (not (bound-and-true-p ace-window-mode))
                            (image-get-display-property))))
       (setf liteline--image-size (image-display-size spec t)))
     (concat (and size-indication-mode " %I")
             (and liteline--image-size
                  (format " %dx%d "
                          (car liteline--image-size)
                          (cdr liteline--image-size)))))
    ((doc-view-mode)
     (concat (and size-indication-mode " %I")
             (format " %d/%d "
                     (image-mode-window-get 'page)
                     (doc-view-last-page-number))))
    (otherwise
     (let (position)
       (when mode-line-percent-position
         (push 'mode-line-percent-position position)
         (push " " position))
       (let ((column (or (and column-number-indicator-zero-based "%c") "%C")))
         (cond ((and line-number-mode column-number-mode)
                (push (concat " %l:" column) position))
               (line-number-mode
                (push " L%l" position))
               (column-number-mode
                (push (concat " C" column) position))))
       (when size-indication-mode
         (push " %I" position))
       (and position `("" ,@position " "))))))

;; Git
(defvar-local liteline--git nil "Current Git status.")
(put 'liteline--git 'risky-local-variable t)

(defun liteline--get-git ()
  "Return current Git status."
  (if vc-display-status
      (concat (propertize (substring-no-properties vc-mode 5)
                          'face 'liteline-git-branch)
              ":"
              (cl-case (vc-state buffer-file-name 'Git)
                ((up-to-date) "-")
                ((edited) (propertize "*" 'face 'liteline-git-edited))
                ((added) (propertize "+" 'face 'liteline-git-new))
                ((conflict) (propertize "=" 'face 'liteline-git-error))
                ((removed) (propertize "!" 'face 'liteline-git-warning))
                ((needs-update) (propertize "^" 'face 'liteline-git-warning))
                ((needs-merge) (propertize "&" 'face 'liteline-git-warning))
                ((ignored) "~")
                (otherwise (propertize "?" 'face 'liteline-git-error))))
    "Git"))

(defun liteline--update-git (&rest _)
  "Update `liteline--git'."
  (if (and buffer-file-name
           vc-mode
           (eq (vc-backend buffer-file-name) 'Git))
      (setf liteline--git (liteline--get-git))
    (setf liteline--git nil))
  (force-mode-line-update))

(defun liteline--setup-git ()
  "Setup Git integration."
  (advice-add #'vc-mode-line :after #'liteline--update-git))

(liteline-define-segment git
  "Show Git status."
  (and (liteline--window-active-p)
       liteline--git
       '(" " liteline--git " ")))

;; Modes
(defvar gud-minor-mode)
(defvar reftex-index-restriction-indicator)
(defvar reftex-toc-include-labels-indicator)
(defvar reftex-toc-include-index-indicator)
(defvar reftex-toc-max-level-indicator)
(declare-function calc-set-mode-line "calc")
(declare-function reftex-offer-label-menu "reftex-ref")

(defun liteline--get-minor-modes ()
  "Return indicators for certain enabled minor modes."
  (let (indicators)
    (dolist (mode liteline-minor-mode-indicator-alist)
      (let ((symbol (car mode)))
        (when (and (boundp symbol) (symbol-value symbol))
          (push (cdr mode) indicators))))
    (when indicators
      (push " " indicators)
      (cons "" (nreverse indicators)))))

(defvar-local liteline--calc-extra nil "Extra information for Calc.")
(put 'liteline--calc-extra 'risky-local-variable t)

(defun liteline--update-calc-extra (&rest _)
  "Update `liteline--calc-extra'."
  (when (eq major-mode 'calc-mode)
    (let* ((name mode-line-buffer-identification)
           (index (string-match-p ":" name))
           (extra (string-trim (substring-no-properties name (1+ index)))))
      (if (string-empty-p extra)
          (setf liteline--calc-extra nil)
        (setf liteline--calc-extra extra)))))

(defun liteline--clear-local-mode-line ()
  "Unset the local `mode-line-format'."
  (kill-local-variable 'mode-line-format))

(defun liteline--get-buffer-size (&optional _)
  "Keep the original `buffer-size' function."
  nil)
(setf (symbol-function 'liteline--get-buffer-size)
      (symbol-function 'buffer-size))

;; HACK: Here we modify a function that happens to be called right after setting
;; the RefTeX mode line.
(defun liteline--avoid-reftex-mode-line (fn &rest args)
  "Apply FN on ARGS with protection on the mode line."
  (cl-letf (((symbol-function 'buffer-size)
             (lambda (&optional buffer)
               (liteline--clear-local-mode-line)
               (liteline--get-buffer-size buffer))))
    (apply fn args)))

(defun liteline--get-major-mode-extra ()
  "Return extra information for major mode."
  (cl-case (or (and (eq major-mode 'gud-mode)
                    (eq gud-minor-mode 'pdb)
                    'gud-pdb-mode)
               major-mode)
    ;; Python environment
    ((python-mode inferior-python-mode gud-pdb-mode)
     (when-let* ((env (and (fboundp 'conda-get-current-environment)
                           (conda-get-current-environment))))
       (format "[%s]" env)))
    ;; RefTeX
    ((reftex-index-mode)
     (when-let* ((indicator reftex-index-restriction-indicator))
       (substring-no-properties (format "[%s]" indicator))))
    ((reftex-select-label-mode)
     (when (bound-and-true-p reftex-refstyle)
       (substring-no-properties (format "[%s]" reftex-refstyle))))
    ((reftex-toc-mode)
     (let ((label (or reftex-toc-include-labels-indicator "-"))
           (index (or reftex-toc-include-index-indicator "-"))
           (level (or reftex-toc-max-level-indicator "-")))
       (substring-no-properties (format " L:%s I:%s T:%s" label index level))))
    ;; Calc
    ((calc-mode)
     (when liteline--calc-extra
       (concat " " liteline--calc-extra)))))

(defun liteline--setup-calc ()
  "Setup Calc."
  (with-eval-after-load 'calc
    (advice-add #'calc-set-mode-line :after #'liteline--update-calc-extra)))

(defun liteline--setup-reftex ()
  "Setup RefTeX."
  (with-eval-after-load 'reftex-index
    (add-hook 'reftex-index-mode-hook #'liteline--clear-local-mode-line))
  (with-eval-after-load 'reftex-toc
    (add-hook 'reftex-toc-mode-hook #'liteline--clear-local-mode-line))
  (with-eval-after-load 'reftex-ref
    (advice-add #'reftex-offer-label-menu :around
                #'liteline--avoid-reftex-mode-line)))

(defun liteline--setup-conda ()
  "Setup conda."
  (with-eval-after-load 'conda
    (add-hook 'conda-post-activate-hook #'force-mode-line-update)
    (add-hook 'conda-post-deactivate-hook #'force-mode-line-update)))

(defvar-local liteline--major-mode-extra nil
  "Extra information for major mode.")
(put 'liteline--major-mode-extra 'risky-local-variable t)

(liteline-define-segment modes
  "Show modes related information."
  (setf liteline--major-mode-extra (liteline--get-major-mode-extra))
  `(" "
    (current-input-method ("" current-input-method-title " "))
    ,(liteline--get-minor-modes)
    mode-name
    liteline--major-mode-extra
    mode-line-process
    " "))

;; Flycheck
(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)
(declare-function flycheck-count-errors "ext:flycheck")

(defvar-local liteline--flycheck nil "Current Flycheck status.")
(put 'liteline--flycheck 'risky-local-variable t)

(defun liteline--update-flycheck (&optional status)
  "Update `liteline--flycheck' according to STATUS."
  (setf liteline--flycheck
        (cl-case status
          ((no-checker)
           (propertize "No checker" 'face 'liteline-flycheck-general))
          ((running)
           (propertize "Running" 'face 'liteline-flycheck-general))
          ((errored)
           (propertize "Errored" 'face 'liteline-flycheck-urgent))
          ((interrupted)
           (propertize "Interrupted" 'face 'liteline-flycheck-general))
          ((suspicious)
           (propertize "Unknown" 'face 'liteline-flycheck-urgent))
          ((finished)
           (let-alist (flycheck-count-errors flycheck-current-errors)
             (if (or .error .warning .info)
                 (concat (propertize (format "%dE" (or .error 0))
                                     'face 'liteline-flycheck-error)
                         " "
                         (propertize (format "%dW" (or .warning 0))
                                     'face 'liteline-flycheck-warning)
                         " "
                         (propertize (format "%dI" (or .info 0))
                                     'face 'liteline-flycheck-info))
               (propertize "No issues"
                           'face 'liteline-flycheck-clean)))))))

(defun liteline--setup-flycheck ()
  "Setup Flycheck."
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-status-changed-functions #'liteline--update-flycheck)
    ;; Reset status
    (add-hook 'flycheck-mode-hook #'liteline--update-flycheck)))

(liteline-define-segment flycheck
  "Show Flycheck status."
  (and (liteline--window-active-p)
       liteline--flycheck
       '(" " liteline--flycheck " ")))

;; Misc information
(liteline-define-segment misc
  "Show misc information."
  (when (liteline--window-active-p)
    (cl-case major-mode
      ((Man-mode)
       '(" " Man-page-mode-string " "))
      ((Info-mode)
       `("" ,(cadr mode-line-buffer-identification) " ")))))

;;; Mode line

;; Main
(liteline-define-mode-line main
  (transient basic position)
  (git modes flycheck misc))

;;; Entry point

(defun liteline-setup ()
  "Setup mode line."
  (liteline--setup-active-window)
  (liteline--setup-git)
  (liteline--setup-calc)
  (liteline--setup-reftex)
  (liteline--setup-conda)
  (liteline--setup-flycheck)

  (liteline-set-mode-line 'main t))

(provide 'liteline)
;;; liteline.el ends here
