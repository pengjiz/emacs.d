;;; page-turner.el --- Improve reading experience  -*- lexical-binding: t -*-

;;; Commentary:

;; Tweaks and hacks to improve reading experience.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Option

(defgroup page-turner nil
  "Improve reading experience."
  :group 'text)

(defcustom page-turner-text-width
  nil
  "Width of text.
If nil use value of `fill-column'."
  :type '(choice integer
                 (const :tag "Fill column" nil)))

(defcustom page-turner-prose-family
  nil
  "Variable width font family for prose."
  :type '(choice string
                 (const :tag "Default" nil)))

(defcustom page-turner-prose-height
  nil
  "Font size for prose."
  :type '(choice string
                 (const :tag "Default" nil)))

(defcustom page-turner-prose-line-spacing
  0.2
  "Additional spacing between lines."
  :type '(choice number
                 (const :tag "None" nil)))

(defcustom page-turner-setup-elfeed-eww
  t
  "Whether to setup EWW for Elfeed."
  :type 'boolean)

;;; Prose style

(defvar visual-fill-column-width)
(defvar visual-fill-column-center-text)
(declare-function face-remap-remove-relative "face-remap")

(defvar-local page-turner--font-remapping-cookies nil
  "Face remapping cookies added for prose font.")

(defun page-turner--reset-font ()
  "Reset font to default."
  (dolist (cookie page-turner--font-remapping-cookies)
    (face-remap-remove-relative cookie))
  (setf page-turner--font-remapping-cookies nil))

(defun page-turner--set-prose-font ()
  "Set font for prose."
  (page-turner--reset-font)
  (when page-turner-prose-family
    (push (face-remap-add-relative 'variable-pitch :family
                                   page-turner-prose-family)
          page-turner--font-remapping-cookies))
  (when page-turner-prose-height
    (push (face-remap-add-relative 'variable-pitch :height
                                   page-turner-prose-height)
          page-turner--font-remapping-cookies)))

(defun page-turner--set-prose-styles (&rest _)
  "Set styles for prose."
  (setf line-spacing page-turner-prose-line-spacing)
  (page-turner--set-prose-font)
  ;; NOTE: For some unknown reason it is necessary to enable the mode before
  ;; setting the options.
  (unless (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-mode))
  (setf visual-fill-column-width page-turner-text-width
        visual-fill-column-center-text t)
  (unless (bound-and-true-p visual-line-mode)
    (visual-line-mode)))

;;; EWW

(defvar shr-width)
(defvar shr-max-width)
(defvar shr-use-fonts)
(defvar shr-internal-width)
(defvar shr-table-separator-pixel-width)

(defun page-turner--tag-shr-hr (_)
  "Keep the original `shr-tag-hr' function."
  nil)
(with-eval-after-load 'shr
  (setf (symbol-function 'page-turner--tag-shr-hr)
        (symbol-function 'shr-tag-hr)))

(defun page-turner--tag-shr-table (_)
  "Keep the original `shr-tag-table' function."
  nil)
(with-eval-after-load 'shr
  (setf (symbol-function 'page-turner--tag-shr-table)
        (symbol-function 'shr-tag-table)))

(defun page-turner--adjust-shr-rendering (fn &rest args)
  "Apply FN on ARGS but adjust HTML rendering."
  (cl-letf ((shr-width nil)
            (shr-max-width (or page-turner-text-width fill-column))
            ;; Disable line filling
            ((symbol-function 'shr-fill-line) #'ignore)
            ;; Adjust length of the horizontal rule
            ((symbol-function 'shr-tag-hr)
             (lambda (dom)
               (let* ((extra (and shr-use-fonts
                                  shr-table-separator-pixel-width))
                      (shr-internal-width (- shr-internal-width
                                             (or extra 0))))
                 (page-turner--tag-shr-hr dom))))
            ;; Disable line truncating for tables
            ((symbol-function 'shr-tag-table)
             (lambda (dom)
               (let ((truncate-lines nil))
                 (page-turner--tag-shr-table dom)))))
    (apply fn args)))

(defvar page-turner--use-eww-prose-styles nil
  "Whether to use prose styles for EWW.")

(defun page-turner--set-eww-styles (&rest args)
  "Set styles appropriately for EWW buffer in ARGS."
  (let ((buffer (nth 4 args)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if page-turner--use-eww-prose-styles
            (page-turner--set-prose-styles)
          (kill-local-variable 'line-spacing)
          (page-turner--reset-font)
          ;; NOTE: For some unknown reason it is necessary to disable the mode
          ;; before resetting the options.
          (when (bound-and-true-p visual-fill-column-mode)
            (visual-fill-column-mode 0))
          (kill-local-variable 'visual-fill-column-width)
          (kill-local-variable 'visual-fill-column-center-text)
          (when (bound-and-true-p visual-line-mode)
            (visual-line-mode 0)))))))

(defun page-turner--add-eww-prose-hint (fn &rest args)
  "Apply FN on ARGS but add a hint of prose styles for EWW."
  (let ((page-turner--use-eww-prose-styles t))
    (apply fn args)))

(defun page-turner--setup-eww ()
  "Setup EWW integration."
  (with-eval-after-load 'eww
    (advice-add 'eww-readable :around #'page-turner--adjust-shr-rendering)
    (advice-add 'eww-readable :around #'page-turner--add-eww-prose-hint)
    (advice-add 'eww-display-html :before #'page-turner--set-eww-styles)))

;;; Elfeed

(defvar elfeed-show-entry-switch)
(defvar elfeed-show-refresh-function)
(declare-function elfeed-show-refresh "ext:elfeed-show")
(declare-function eww-readable "eww")

(defun page-turner--adjust-elfeed-link-length (fn &rest args)
  "Apply FN on ARGS but adjust link text length for Elfeed."
  (let ((shr-width (or page-turner-text-width fill-column)))
    (apply fn args)))

;; Some images are scaled based on the window size, so here we display the
;; buffer first before rendering.
(defun page-turner--adjust-elfeed-image-size (fn &rest args)
  "Apply FN on ARGS but adjust image size for Elfeed."
  (let ((buffer (let ((elfeed-show-entry-switch #'identity)
                      (elfeed-show-refresh-function #'ignore))
                  (apply fn args))))
    (prog1 (funcall elfeed-show-entry-switch buffer)
      (with-current-buffer buffer
        (elfeed-show-refresh)))))

(defun page-turner--eww-readable-once ()
  "View EWW readable parts only once."
  (unwind-protect
      (eww-readable)
    (remove-hook 'eww-after-render-hook #'page-turner--eww-readable-once t)))

(defun page-turner--elfeed-eww (url)
  "Browse URL with EWW for Elfeed."
  (let ((buffer (generate-new-buffer "*elfeed-eww*")))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (eww-mode)
      (make-local-variable 'eww-auto-rename-buffer)
      (setf eww-auto-rename-buffer nil)
      (add-hook 'eww-after-render-hook #'page-turner--eww-readable-once nil t)
      (eww url))))

(defun page-turner--use-elfeed-eww (fn &rest args)
  "Apply FN on ARGS but use EWW for Elfeed when appropriate."
  (cl-letf (((symbol-function 'browse-url-generic) #'page-turner--elfeed-eww))
    (apply fn args)))

(defun page-turner--setup-elfeed ()
  "Setup Elfeed integration."
  (with-eval-after-load 'elfeed-search
    (when page-turner-setup-elfeed-eww
      (advice-add 'elfeed-search-browse-url :around
                  #'page-turner--use-elfeed-eww)))
  (with-eval-after-load 'elfeed-show
    (advice-add 'elfeed-insert-html :around #'page-turner--adjust-shr-rendering)
    (add-hook 'elfeed-show-mode-hook #'page-turner--set-prose-styles)
    (advice-add 'elfeed-insert-link :around
                #'page-turner--adjust-elfeed-link-length)
    (advice-add 'elfeed-show-entry :around
                #'page-turner--adjust-elfeed-image-size)
    (when page-turner-setup-elfeed-eww
      (advice-add 'elfeed-show-visit :around #'page-turner--use-elfeed-eww))))

;;; Markdown mode

(defvar markdown-live-preview-window-function)
(declare-function markdown-live-preview-window-eww "ext:markdown-mode")

(defun page-turner--get-markdown-live-preview-buffer (file)
  "Get a buffer showing FILE with EWW."
  (let* ((shr-width nil)
         (shr-max-width (or page-turner-text-width fill-column))
         (buffer (markdown-live-preview-window-eww file)))
    (with-current-buffer buffer
      (page-turner--set-prose-font))
    buffer))

(defun page-turner--setup-markdown ()
  "Setup Markdown mode integration."
  (with-eval-after-load 'markdown-mode
    (setf markdown-live-preview-window-function
          #'page-turner--get-markdown-live-preview-buffer)))

;;; Nov mode

(defvar nov-text-width)
(defvar nov-variable-pitch)

(defun page-turner--setup-nov ()
  "Setup nov mode integration."
  (with-eval-after-load 'nov
    (setf nov-text-width t
          nov-variable-pitch t)
    (add-hook 'nov-mode-hook #'page-turner--set-prose-styles)))

;;; Man

(defvar Man-width)
(defvar Man-width-max)

(defun page-turner--setup-man ()
  "Setup man integration."
  (with-eval-after-load 'man
    (setf Man-width nil
          Man-width-max (or page-turner-text-width fill-column))))

;;; Notmuch

(defvar notmuch-wash-wrap-lines-length)

(defun page-turner--setup-notmuch ()
  "Setup notmuch integration."
  (with-eval-after-load 'notmuch-wash
    (setf notmuch-wash-wrap-lines-length
          (or page-turner-text-width fill-column))))

;;; Entry point

(defun page-turner-setup ()
  "Setup tweaks and hacks for improving reading experience."
  (page-turner--setup-eww)
  (page-turner--setup-elfeed)
  (page-turner--setup-markdown)
  (page-turner--setup-nov)
  (page-turner--setup-man)
  (page-turner--setup-notmuch))

(provide 'page-turner)
;;; page-turner.el ends here
