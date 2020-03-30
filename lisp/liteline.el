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
  "Major modes in which showing word count is meanningful."
  :type '(repeat symbol))

(defcustom liteline-input-method-names-alist
  '(("chinese-b5-tsangchi" . "TWc"))
  "Alternative names of input methods."
  :type '(alist :key-type string
                :value-type string))

(defcustom liteline-important-minor-modes-alist
  '((typo-mode . "t")
    (auto-fill-function . "q")
    (visual-line-mode . "v")
    (whitespace-mode . "w")
    (flyspell-mode . "s"))
  "Minor modes that should be shown in the mode line."
  :type '(alist :key-type symbol
                :value-type string))

(defface liteline-buffer-modified
  '((t (:inherit (bold warning))))
  "Face used for indicating buffer is modified.")

(defface liteline-buffer-read-only
  '((t (:inherit (bold error))))
  "Face used for indicating buffer is read-only.")

(defface liteline-buffer-narrowed
  '((t (:inherit warning)))
  "Face used for indicating buffer is narrowed.")

(defface liteline-buffer-name
  '((t (:inherit mode-line-buffer-id)))
  "Face used by normal buffer name.")

(defface liteline-buffer-file-non-existent
  '((t (:inherit (bold error))))
  "Face used for name of a buffer backed by a non-existent file.")

(defface liteline-action
  '((t (:inherit match)))
  "Face used by general action information.")

(defface liteline-flycheck-message
  '((t (:inherit warning)))
  "Face used to show general Flycheck messages.")

(defface liteline-flycheck-message-urgent
  '((t (:inherit (bold error))))
  "Face used to show urgent Flycheck messages.")

(defface liteline-flycheck-error-info
  '((t (:inherit flycheck-fringe-info)))
  "Face used by Flycheck errors of the information level.")

(defface liteline-flycheck-error-warning
  '((t (:inherit flycheck-fringe-warning)))
  "Face used by Flycheck errors of the warning level.")

(defface liteline-flycheck-error-error
  '((t (:inherit flycheck-fringe-error)))
  "Face used by Flycheck errors of the error level.")

(defface liteline-flycheck-error-clean
  '((t (:inherit success)))
  "Face used to show there are no Flycheck errors.")

(defface liteline-git-branch
  '((t (:inherit mode-line-buffer-id)))
  "Face used for Git branch.")

(defface liteline-git-new
  '((t (:inherit success)))
  "Face used for new files in Git.")

(defface liteline-git-edited
  '((t (:inherit warning)))
  "Face used for edited files in Git.")

(defface liteline-git-warning
  '((t (:inherit (bold warning))))
  "Face used for warnings in Git.")

(defface liteline-git-error
  '((t (:inherit (bold error))))
  "Face used for errors in Git.")

;;; Mode line helper (mostly copied from doom-emacs)

(eval-and-compile
  (defvar liteline--segment-fns-alist nil "Functions for segments."))

;; NOTE: In general each segment should have a trailing whitespace as
;; the separator. There might be a better way to handle this.
(defmacro liteline-def-segment (name &rest body)
  "Define a mode line segment with NAME and BODY and `byte-compile' it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "liteline--segment-%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "Liteline segment %s." name))))
    (cl-pushnew (cons name sym) liteline--segment-fns-alist
                :test #'equal)
    `(progn
       (setf (symbol-function ',sym) (lambda () ,docstring ,@body))
       (cl-pushnew (cons ',name #',sym) liteline--segment-fns-alist
                   :test #'equal)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defsubst liteline--escape (string)
  "Escape special characters in STRING for mode line."
  (replace-regexp-in-string "%" "%%" string))

(defsubst liteline--clean-text-properties (string)
  "Remove text properties for mouse in STRING."
  (remove-list-of-text-properties 0 (length string)
                                  '(mouse-face help-echo keymap local-map)
                                  string))

(defun liteline--prepare-segments (segments)
  "Prepare SEGMENTS for defining a mode line."
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
    (cons "" (nreverse forms))))

(defun liteline-def-mode-line (name lhs &optional rhs)
  "Define a function of NAME with LHS and RHS for mode line."
  (let ((sym (intern (format "liteline--mode-line-%s" name)))
        (lhs-forms (liteline--prepare-segments lhs))
        (rhs-forms (liteline--prepare-segments rhs)))
    (setf (symbol-function sym)
          (lambda ()
            (let* ((rhs-string (format-mode-line (cons "" rhs-forms)))
                   (rhs-width (string-width rhs-string)))
              ;; NOTE: Generally those segments with mouse interaction are in
              ;; the right half, so here we only clean it. Also note that this
              ;; function modify the object in place.
              (liteline--clean-text-properties rhs-string)
              (list lhs-forms
                    (propertize
                     " "
                     'display
                     `((space :align-to (- (+ right right-fringe right-margin)
                                           ,rhs-width))))
                    ;; NOTE: The % character in the string will cause problems
                    ;; so here we remove them.
                    (liteline--escape rhs-string)))))))

(defun liteline--get-mode-line (name)
  "Return the `mode-line-format' of NAME."
  (let ((fn (intern-soft (format "liteline--mode-line-%s" name))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun liteline-set-mode-line (name &optional default)
  "Set `mode-line-format' to the format of NAME.
If DEFAULT is non-nil, set the default value."
  (when-let* ((ml-format (liteline--get-mode-line name)))
    (setf (if default
              (default-value 'mode-line-format)
            mode-line-format)
          (list "%e" ml-format))))

;;; Active window

(defvar liteline--active-window nil "Current active window.")

(declare-function lv-message "ext:lv")
(declare-function transient--show "ext:transient")

(defun liteline--set-active-window (&rest _)
  "Set `liteline--active-window'."
  (when-let* ((window (selected-window)))
    (unless (or (eq window liteline--active-window)
                (window-minibuffer-p window))
      (setf liteline--active-window window)
      (force-mode-line-update))))

(defun liteline--set-active-window-after-select (_ &optional norecord)
  "Set active window if NORECORD is nil."
  (unless norecord
    (liteline--set-active-window)))

(defun liteline--clear-active-window (&rest _)
  "Clear `liteline--active-window'."
  (setf liteline--active-window nil)
  (force-mode-line-update))

(defun liteline--active-p ()
  "Return t if the current window is active."
  (eq (selected-window) liteline--active-window))

(defun liteline--ignore-active-window (fn &rest args)
  "Ignore the active window system when calling FN on ARGS."
  (cl-letf (((symbol-function 'liteline--set-active-window)
             (lambda (&rest _) nil)))
    (apply fn args)))

(defun liteline--setup-active-window ()
  "Setup the active window system."
  ;; Start
  (liteline--set-active-window)

  ;; After change window configuration
  (add-hook 'window-configuration-change-hook #'liteline--set-active-window)
  ;; After switching window
  (advice-add #'select-window :after #'liteline--set-active-window-after-select)
  ;; After switching frame
  (advice-add #'handle-switch-frame :after #'liteline--set-active-window)

  ;; Some functions do not play well with this
  (with-eval-after-load 'lv
    (advice-add #'lv-message :around #'liteline--ignore-active-window))
  (with-eval-after-load 'transient
    (advice-add #'transient--show :around #'liteline--ignore-active-window)))

;;; Basic segment

;; Buffer information
(defun liteline--get-buffer-modification ()
  "Return a string showing buffer modification status."
  (cond (buffer-read-only
         (propertize "%%"
                     'face 'liteline-buffer-read-only))
        ((buffer-modified-p)
         (propertize "*"
                     'face 'liteline-buffer-modified))
        (t "-")))

(defun liteline--get-buffer-name ()
  "Return a string showing buffer name."
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

(liteline-def-segment buffer-info
  "Show the basic buffer information."
  (concat
   " "
   (liteline--get-buffer-modification)
   " "
   (liteline--get-buffer-name)
   (liteline--get-buffer-hostname)
   " "))

;; Buffer size and point position
(declare-function image-get-display-property "image-mode")
(declare-function image-mode-window-get "image-mode")
(declare-function doc-view-last-page-number "doc-view")

(liteline-def-segment buffer-position
  "Show the position of point in the buffer."
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

;; Buffer encoding
(liteline-def-segment buffer-encoding
  "Show the eol style and encoding of the buffer."
  (and (liteline--active-p)
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
(liteline-def-segment input-method
  "Show the input method of the buffer."
  (and (liteline--active-p)
       current-input-method
       (concat
        " "
        (or (cdr (assoc current-input-method
                        liteline-input-method-names-alist))
            current-input-method-title)
        " ")))

;; Major mode
(declare-function reftex-offer-label-menu "reftex-ref")

(defun liteline--setup-conda ()
  "Setup conda."
  (with-eval-after-load 'conda
    (add-hook 'conda-post-activate-hook #'force-mode-line-update)
    (add-hook 'conda-post-deactivate-hook #'force-mode-line-update)))

(defun liteline--clear-local-mode-line ()
  "Unset the local mode line."
  (kill-local-variable 'mode-line-format))

;; NOTE: Suppress a compiler warning.
(defun liteline--get-buffer-size (&optional _)
  "Keep the original `buffer-size' function."
  nil)
(setf (symbol-function 'liteline--get-buffer-size)
      (symbol-function 'buffer-size))

;; HACK: A big hack on preventing the mode line from being modified.
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
  "Show the extra information for the current major mode."
  (cl-case major-mode
    ;; Conda environment
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

(liteline-def-segment major-mode
  "Show the major mode name together with process string."
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
    "Macro>"))

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

(liteline-def-segment action
  "Show action status."
  (when (liteline--active-p)
    (let (action)
      (dolist (fn '(liteline--get-selection-info
                    liteline--get-macro-indicator))
        (when-let* ((string (funcall fn)))
          (push " " action)
          (push string action)))
      (when action
        (propertize (apply #'concat " " action) 'face 'liteline-action)))))

;; Flycheck
(defvar-local liteline--flycheck nil "Current Flycheck status string.")
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
    (add-hook 'flycheck-status-changed-functions
              #'liteline--update-flycheck)
    ;; Reset status
    (add-hook 'flycheck-mode-hook
              #'liteline--update-flycheck)
    (liteline--update-flycheck flycheck-last-status-change)))

(liteline-def-segment flycheck
  "Show Flycheck status."
  (and (liteline--active-p)
       liteline--flycheck
       (concat " " liteline--flycheck " ")))

;; Git
(defvar-local liteline--git nil "Current Git status string.")
(put 'liteline--git 'risky-local-variable t)

(defun liteline--update-git ()
  "Update `liteline--git'."
  (setf liteline--git
        (when (and vc-mode buffer-file-name)
          (concat
           (propertize (substring-no-properties vc-mode 5)
                       'face 'liteline-git-branch)
           ":"
           (cl-case (vc-state buffer-file-name)
             ((added) (propertize "A" 'face 'liteline-git-new))
             ((edited) (propertize "E" 'face 'liteline-git-edited))
             ((removed) (propertize "D" 'face 'liteline-git-warning))
             ((need-merge) (propertize "M" 'face 'liteline-git-warning))
             ((need-update) (propertize "O" 'face 'liteline-git-warning))
             ((conflict) (propertize "C" 'face 'liteline-git-error))
             ((ignored) "#")
             ((up-to-date) "-")
             (otherwise (propertize "!" 'face 'liteline-git-error)))))))

(defun liteline--setup-git ()
  "Setup Git."
  (add-hook 'after-revert-hook #'liteline--update-git)
  (add-hook 'after-save-hook #'liteline--update-git)
  (add-hook 'find-file-hook #'liteline--update-git t)
  (with-eval-after-load 'vc-hooks
    (advice-add #'vc-refresh-state :after #'liteline--update-git)))

(liteline-def-segment git
  "Show Git branch and state."
  (and (liteline--active-p)
       liteline--git
       (concat " " liteline--git " ")))

;; Minor mode
(liteline-def-segment minor-modes
  "Show some important minor modes."
  (when (liteline--active-p)
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
  "Return a string of the Org timer."
  (when (bound-and-true-p org-timer-mode-line-string)
    (substring-no-properties org-timer-mode-line-string 2 -1)))

(liteline-def-segment misc
  "Show some misc but important information."
  (when (liteline--active-p)
    (concat " " (liteline--get-org-timer) " ")))

;; Two-column
(defvar 2C-mode-line-format)

(defun litelite--setup-two-column ()
  "Setup two-column."
  (with-eval-after-load 'two-column
    (setf 2C-mode-line-format (default-value 'mode-line-format))))

;;; Mode line

;; Main
(liteline-def-mode-line
 'main
 '(action buffer-info buffer-position)
 '(misc git buffer-encoding input-method minor-modes major-mode flycheck))

;;; Entry point

(defun liteline-setup ()
  "Setup mode line."
  ;; Preparation for segments
  (liteline--setup-active-window)
  (liteline--setup-conda)
  (liteline--setup-reftex)
  (liteline--setup-flycheck)
  (liteline--setup-git)
  (litelite--setup-two-column)

  ;; Default mode line
  (liteline-set-mode-line 'main t))

(provide 'liteline)
;;; liteline.el ends here
