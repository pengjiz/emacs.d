;;; liteline.el --- Lightweight mode line  -*- lexical-binding: t; -*-

;;; Commentary:

;; A lightweight mode line configuration.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'let-alist))

;;; Face and option

(defgroup liteline nil
  "A light-weight mode line."
  :group 'mode-line)

(defcustom liteline-word-count-modes
  '(text-mode)
  "Major modes in which word count is shown."
  :type '(repeat symbol))

(defcustom liteline-important-minor-modes-alist
  '((typo-mode . "t")
    (abbrev-mode . "r")
    (auto-fill-function . "q")
    (visual-line-mode . "v")
    (whitespace-mode . "w")
    (flyspell-mode . "s"))
  "Minor modes that should be shown in the mode line."
  :type '(alist :key-type symbol
                :value-type string))

(defface liteline-buffer-modified
  '((t :inherit (bold warning)))
  "Face used for indicating buffer is modified.")

(defface liteline-buffer-read-only
  '((t :inherit (bold error)))
  "Face used for indicating buffer is read-only.")

(defface liteline-buffer-narrowed
  '((t :inherit warning))
  "Face used for indicating buffer is narrowed.")

(defface liteline-buffer-name
  '((t :inherit mode-line-buffer-id))
  "Face used by normal buffer name.")

(defface liteline-buffer-file-non-existent
  '((t :inherit (bold error)))
  "Face used for name of a buffer backed by a non-existent file.")

(defface liteline-action
  '((t :inherit match))
  "Face used by general action information.")

(defface liteline-flycheck-message
  '((t :inherit warning))
  "Face used to show general Flycheck messages.")

(defface liteline-flycheck-message-urgent
  '((t :inherit (bold error)))
  "Face used to show urgent Flycheck messages.")

(defface liteline-flycheck-error-info
  '((t :inherit flycheck-fringe-info))
  "Face used by Flycheck errors of the information level.")

(defface liteline-flycheck-error-warning
  '((t :inherit flycheck-fringe-warning))
  "Face used by Flycheck errors of the warning level.")

(defface liteline-flycheck-error-error
  '((t :inherit flycheck-fringe-error))
  "Face used by Flycheck errors of the error level.")

(defface liteline-flycheck-error-clean
  '((t :inherit success))
  "Face used to show there are no Flycheck errors.")

(defface liteline-git-branch
  '((t :inherit mode-line-buffer-id))
  "Face used for Git branch.")

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

;;; Mode line helper

(eval-and-compile
  (defvar liteline--segment-fns-alist nil "Functions of segments.")

  (defun liteline--prepare-segments (segments)
    "Prepare mode line forms of SEGMENTS.
Return forms that can be passed directly to `format-mode-line'."
    (let (forms it)
      (dolist (segment segments)
        (cond ((stringp segment)
               (push segment forms))
              ((consp segment)
               (push segment forms))
              ((symbolp segment)
               (if (setf it (cdr (assq segment liteline--segment-fns-alist)))
                   (push `(:eval (,it)) forms)
                 (push it forms)))
              (t
               (error "%s is not a valid segment" segment))))
      (cons "" (nreverse forms)))))

(defmacro liteline-define-segment (name doc &rest body)
  "Define a mode line segment function for NAME with DOC and BODY."
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name stringp def-body)))
  (let ((symbol (intern (format "liteline--segment-%s" name))))
    (cl-pushnew (cons name symbol) liteline--segment-fns-alist
                :test #'eq :key #'car)
    `(progn
       (defun ,symbol ()
         ,(or doc (format "Show the %s information." name))
         ,@body)
       (cl-pushnew (cons ',name #',symbol) liteline--segment-fns-alist
                   :test #'eq :key #'car))))

(defun liteline--window-active-p ()
  "Return t if the selected window is active."
  (or (eq (selected-window) (old-selected-window))
      (and (not (zerop (minibuffer-depth)))
           (eq (selected-window)
               (with-selected-window (minibuffer-window)
                 (minibuffer-selected-window))))))

(defvar liteline--mode-line-fns-alist nil "Functions of mode lines.")

(defun liteline--prepare-mode-line (name)
  "Prepare the mode line of NAME."
  (if-let* ((fn (cdr (assq name liteline--mode-line-fns-alist))))
      `(:eval (,fn))
    (error "%s is not a valid mode line" name)))

(defsubst liteline--escape (string)
  "Escape special characters in STRING for mode line."
  (replace-regexp-in-string "%" "%%" string))

(defsubst liteline--clean-text-properties (string)
  "Remove text properties for mouse in STRING."
  (remove-list-of-text-properties 0 (length string)
                                  '(mouse-face help-echo keymap local-map)
                                  string))

(defun liteline--wrap-mode-line (left right)
  "Wrap LEFT and RIGHT segment forms into a full mode line."
  (let* ((right-string (format-mode-line right))
         (right-width (string-width right-string)))
    (liteline--clean-text-properties right-string)
    (list left
          (propertize
           " "
           'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,right-width))))
          (liteline--escape right-string))))

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
       (cl-pushnew (cons ',name #',symbol) liteline--mode-line-fns-alist
                   :test #'eq :key #'car))))

(defun liteline-set-mode-line (name &optional default)
  "Set `mode-line-format' to the mode line of NAME.
If DEFAULT is non-nil, set the default value."
  (setf (if default
            (default-value 'mode-line-format)
          mode-line-format)
        (list "%e" (liteline--prepare-mode-line name))))

;;; Basic segment

;; Buffer information
(defun liteline--get-buffer-modification ()
  "Return buffer modification status."
  (cond (buffer-read-only
         (propertize "%%"
                     'face 'liteline-buffer-read-only))
        ((buffer-modified-p)
         (propertize "*"
                     'face 'liteline-buffer-modified))
        (t "-")))

(defun liteline--get-buffer-name ()
  "Return buffer name."
  (let* ((face (if (and buffer-file-name
                        (not (file-exists-p buffer-file-name)))
                   'liteline-buffer-file-non-existent
                 'liteline-buffer-name))
         (name (propertize (buffer-name)
                           'face face))
         (narrowed (buffer-narrowed-p)))
    (concat
     (and narrowed
          (propertize "~[" 'face 'liteline-buffer-narrowed))
     name
     (and narrowed
          (propertize "]" 'face 'liteline-buffer-narrowed)))))

(defun liteline--get-buffer-hostname ()
  "Return the hostname for remote buffers."
  (when-let* ((root default-directory)
              (host (file-remote-p root 'host)))
    (concat "@" host)))

(liteline-define-segment buffer-info
  "Show the basic buffer information."
  (concat
   " "
   (liteline--get-buffer-modification)
   " "
   (liteline--get-buffer-name)
   (liteline--get-buffer-hostname)
   " "))

;; Position
(declare-function image-get-display-property "image-mode")
(declare-function image-mode-window-get "image-mode")
(declare-function doc-view-last-page-number "doc-view")

(liteline-define-segment buffer-position
  "Show the position information."
  (cl-case major-mode
    ((image-mode)
     (let ((size (image-size (image-get-display-property) :pixels)))
       (format " %dx%d " (car size) (cdr size))))
    ((doc-view-mode)
     (format " %d/%d "
             (image-mode-window-get 'page)
             (doc-view-last-page-number)))
    (otherwise
     (concat
      " "
      (and size-indication-mode "%I ")
      (let ((column-number
             (if column-number-indicator-zero-based
                 "%c"
               "%C")))
        (cond ((and line-number-mode column-number-mode)
               (concat "%l:" column-number " "))
              ((line-number-mode
                "L%l "))
              ((column-number-mode
                (concat "C" column-number " ")))))
      "%p "))))

;; Encoding
(liteline-define-segment buffer-encoding
  "Show the encoding information."
  (and (liteline--window-active-p)
       (concat
        " "
        (cl-case (coding-system-eol-type buffer-file-coding-system)
          ((0) "LF ")
          ((1) "CRLF ")
          ((2) "CR "))
        (let ((sys (coding-system-plist buffer-file-coding-system)))
          (if (memq (plist-get sys :category)
                    '(coding-category-undecided coding-category-utf-8))
              "UTF-8"
            (upcase (symbol-name (plist-get sys :name)))))
        " ")))

;; Input method
(liteline-define-segment input-method
  "Show the input method name."
  (and (liteline--window-active-p)
       current-input-method
       (concat " " current-input-method-title " ")))

;; Major mode
(declare-function reftex-offer-label-menu "reftex-ref")

(defun liteline--setup-conda ()
  "Setup conda."
  (with-eval-after-load 'conda
    (add-hook 'conda-post-activate-hook #'force-mode-line-update)
    (add-hook 'conda-post-deactivate-hook #'force-mode-line-update)))

(defun liteline--clear-local-mode-line ()
  "Unset the local `mode-line-format'."
  (kill-local-variable 'mode-line-format))

(defun liteline--get-buffer-size (&optional _)
  "Keep the original `buffer-size' function."
  nil)
(setf (symbol-function 'liteline--get-buffer-size)
      (symbol-function 'buffer-size))

;; HACK: buffer-size happens to be called right after modifying
;; mode-line-format. So here we manipulate buffer-size to clear the mode line.
(defun liteline--avoid-reftex-mode-line (fn &rest args)
  "Apply FN on ARGS with protection on the mode line."
  (cl-letf (((symbol-function 'buffer-size)
             (lambda (&optional buffer)
               (liteline--clear-local-mode-line)
               (liteline--get-buffer-size buffer))))
    (apply fn args)))

(defun liteline--setup-reftex ()
  "Setup RefTeX."
  (with-eval-after-load 'reftex-index
    (add-hook 'reftex-index-mode-hook #'liteline--clear-local-mode-line))
  (with-eval-after-load 'reftex-toc
    (add-hook 'reftex-toc-mode-hook #'liteline--clear-local-mode-line))
  (with-eval-after-load 'reftex-ref
    (advice-add #'reftex-offer-label-menu :around
                #'liteline--avoid-reftex-mode-line)))

(defun liteline--get-major-mode-extra-info ()
  "Return the extra information for the current major mode."
  (cl-case major-mode
    ;; Python environment
    ((python-mode inferior-python-mode)
     (when (bound-and-true-p conda-current-environment)
       (format "[%s]" conda-current-environment)))
    ;; RefTeX index
    ((reftex-index-mode)
     (when (bound-and-true-p reftex-index-restriction-indicator)
       (format "[%s]" reftex-index-restriction-indicator)))
    ;; RefTeX reference style
    ((reftex-select-label-mode)
     (when (bound-and-true-p reftex-refstyle)
       (format "[%s]" reftex-refstyle)))
    ;; RefTeX TOC
    ((reftex-toc-mode)
     (format
      " L:%s I:%s T:%s"
      (or (bound-and-true-p reftex-toc-include-labels-indicator) "-")
      (or (bound-and-true-p reftex-toc-include-index-indicator) "-")
      (or (bound-and-true-p reftex-toc-max-level-indicator) "-")))
    ;; Calc modes
    ((calc-mode)
     (let ((string (substring-no-properties
                    (string-trim (replace-regexp-in-string
                                  "\\`Calc.*: " ""
                                  mode-line-buffer-identification)))))
       (unless (string-empty-p string)
         (concat " " string))))))

(liteline-define-segment major-mode
  "Show the major mode name and other related information."
  '(" "
    mode-name
    (:eval (liteline--get-major-mode-extra-info))
    mode-line-process
    " "))

;;; Additional segment & third-party integration

;; Keyboard macro
(defun liteline--get-macro-indicator ()
  "Return an indicator when recording keyboard macros."
  (when defining-kbd-macro
    "M>"))

;; Recursive editing
(defun liteline--get-recursive-editing-depth ()
  "Return recursion depth when in recursive editing."
  (let ((depth (- (recursion-depth) (minibuffer-depth))))
    (when (> depth 0)
      (format "@%d" depth))))

;; Selection
(defsubst liteline--get-column (pos)
  "Return the column number of POS."
  (save-excursion (goto-char pos)
                  (current-column)))

(defun liteline--get-selection-info ()
  "Return the selection information."
  (when mark-active
    (let* ((start (region-beginning))
           (end (region-end))
           (lines (count-lines start end)))
      (propertize
       (concat (cond ((bound-and-true-p rectangle-mark-mode)
                      (format "%dx%dB"
                              lines
                              (abs (- (liteline--get-column end)
                                      (liteline--get-column start)))))
                     ((> lines 1)
                      (format "%dC %dL" (- end start) lines))
                     (t
                      (format "%dC" (- end start))))
               (when (apply #'derived-mode-p liteline-word-count-modes)
                 (format " %dW" (count-words start end))))))))

(liteline-define-segment action
  "Show action status."
  (when (liteline--window-active-p)
    (let (action)
      (dolist (fn '(liteline--get-selection-info
                    liteline--get-recursive-editing-depth
                    liteline--get-macro-indicator))
        (when-let* ((string (funcall fn)))
          (push " " action)
          (push string action)))
      (when action
        (propertize (apply #'concat " " action) 'face 'liteline-action)))))

;; Flycheck
(defvar-local liteline--flycheck nil "Current Flycheck status.")
(put 'liteline--flycheck 'risky-local-variable t)

(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)
(declare-function flycheck-count-errors "ext:flycheck")

(defun liteline--update-flycheck (&optional status)
  "Update `liteline--flycheck' according to STATUS."
  (setf liteline--flycheck
        (cl-case status
          ((no-checker) (propertize "No checker"
                                    'face 'liteline-flycheck-message))
          ((running) (propertize "Running"
                                 'face 'liteline-flycheck-message))
          ((errored) (propertize "Errored"
                                 'face 'liteline-flycheck-message-urgent))
          ((interrupted) (propertize "Interrupted"
                                     'face 'liteline-flycheck-message))
          ((suspicious) (propertize "Unknown"
                                    'face 'liteline-flycheck-message-urgent))
          ((finished)
           (let-alist (flycheck-count-errors flycheck-current-errors)
             (if (or .error .warning .info)
                 (concat
                  (propertize (format "%dE "
                                      (or .error 0))
                              'face 'liteline-flycheck-error-error)
                  (propertize (format "%dW "
                                      (or .warning 0))
                              'face 'liteline-flycheck-error-warning)
                  (propertize (format "%dI"
                                      (or .info 0))
                              'face 'liteline-flycheck-error-info))
               (propertize "No issues"
                           'face 'liteline-flycheck-error-clean)))))))

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
       (concat " " liteline--flycheck " ")))

;; Git
(defvar-local liteline--git nil "Current Git status.")
(put 'liteline--git 'risky-local-variable t)

(defun liteline--update-git (&rest _)
  "Update `liteline--git'."
  (setf liteline--git
        (when (and vc-mode vc-display-status buffer-file-name)
          (concat
           (propertize (substring-no-properties vc-mode 5)
                       'face 'liteline-git-branch)
           ":"
           (cl-case (vc-state buffer-file-name)
             ((up-to-date) "-")
             ((edited) (propertize "*" 'face 'liteline-git-edited))
             ((added) (propertize "+" 'face 'liteline-git-new))
             ((conflict) (propertize "=" 'face 'liteline-git-error))
             ((removed) (propertize "!" 'face 'liteline-git-warning))
             ((needs-update) (propertize "^" 'face 'liteline-git-warning))
             ((needs-merge) (propertize "&" 'face 'liteline-git-warning))
             ((ignored) "~")
             (otherwise (propertize "?" 'face 'liteline-git-error))))))
  (force-mode-line-update))

(defun liteline--setup-git ()
  "Setup Git integration."
  (with-eval-after-load 'vc-hooks
    (advice-add #'vc-mode-line :after #'liteline--update-git)))

(liteline-define-segment git
  "Show Git branch and state."
  (and (liteline--window-active-p)
       liteline--git
       (concat " " liteline--git " ")))

;; Minor mode
(liteline-define-segment minor-modes
  "Show some important minor modes."
  (when (liteline--window-active-p)
    (let (modes)
      (dolist (mode liteline-important-minor-modes-alist)
        (let ((symbol (car mode)))
          (when (and (boundp symbol) (symbol-value symbol))
            (push (cdr mode) modes))))
      (when modes
        (push " " modes)
        (apply #'concat " " (nreverse modes))))))

;; Misc information
(defun liteline--get-org-timer ()
  "Return Org timer information."
  (when (bound-and-true-p org-timer-mode-line-string)
    (substring-no-properties org-timer-mode-line-string 2 -1)))

(liteline-define-segment misc
  "Show some misc but important information."
  (when (liteline--window-active-p)
    (concat " " (liteline--get-org-timer) " ")))

;; Two-column
(defvar 2C-mode-line-format)

(defun litelite--setup-two-column ()
  "Setup two-column."
  (with-eval-after-load 'two-column
    (setf 2C-mode-line-format (default-value 'mode-line-format))))

;;; Mode line

;; Main
(liteline-define-mode-line main
  (action
   buffer-info
   buffer-position)
  (misc
   git
   buffer-encoding
   input-method
   minor-modes
   major-mode
   flycheck))

;;; Entry point

(defun liteline-setup ()
  "Setup mode line."
  (liteline--setup-conda)
  (liteline--setup-reftex)
  (liteline--setup-flycheck)
  (liteline--setup-git)
  (litelite--setup-two-column)

  (liteline-set-mode-line 'main t))

(provide 'liteline)
;;; liteline.el ends here
