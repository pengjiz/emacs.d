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

;;; Prose style

(declare-function shr-fill-line "shr")
(declare-function face-remap-remove-relative "face-remap")
(defvar shr-width)
(defvar shr-use-fonts)
(defvar shr-internal-width)
(defvar shr-table-separator-pixel-width)
(defvar visual-fill-column-width)
(defvar visual-fill-column-center-text)

(defvar-local page-turner--face-remapping-cookies nil
  "Face remapping cookies added.")
;; NOTE: Here we do not use minor mode because we do not want an interactive
;; command but merely a function for hooks or advice.
(defvar-local page-turner--in-prose-styles nil
  "Whether the current buffer is in prose styles.")

(defun page-turner--disable-shr-filling (fn &rest args)
  "Apply FN on ARGS, but disable `shr-fill-line'."
  (cl-letf (((symbol-function 'shr-fill-line) #'ignore))
    (apply fn args)))

;; NOTE: Even if text filling is disabled this variable is still used when
;; rendering some elements. So here we set it properly.
(defun page-turner--set-shr-width (fn &rest args)
  "Apply FN on ARGS, but force using `page-turner-text-width'."
  (let ((shr-width (or page-turner-text-width fill-column)))
    (apply fn args)))

;; HACK: For some reason the hr line will be one character longer than expected,
;; so here we use a hack to adjust the width.
(defun page-turner--tag-shr-hr (_)
  "Keep the original `shr-tag-hr' function."
  nil)
(with-eval-after-load 'shr
  (setf (symbol-function 'page-turner--tag-shr-hr)
        (symbol-function 'shr-tag-hr)))

(defun page-turner--adjust-shr-hr-width (fn &rest args)
  "Apply FN on ARGS, but adjust hr line width."
  (cl-letf (((symbol-function 'shr-tag-hr)
             (lambda (dom)
               (let ((shr-internal-width (if shr-use-fonts
                                             (- shr-internal-width
                                                shr-table-separator-pixel-width)
                                           shr-internal-width)))
                 (page-turner--tag-shr-hr dom)))))
    (apply fn args)))

;; HACK: When rendering tables sometimes line truncating will be turned on,
;; which is undesirable in prose styles. So here we use a hack to avoid that.
(defun page-turner--tag-shr-table (_)
  "Keep the original `shr-tag-table' function."
  nil)
(with-eval-after-load 'shr
  (setf (symbol-function 'page-turner--tag-shr-table)
        (symbol-function 'shr-tag-table)))

(defun page-turner--avoid-shr-truncating (fn &rest args)
  "Apply FN on ARGS, but avoid line truncating."
  (cl-letf (((symbol-function 'shr-tag-table)
             (lambda (dom)
               (let (truncate-lines)
                 (page-turner--tag-shr-table dom)))))
    (apply fn args)))

(defun page-turner--reset-font ()
  "Reset font to default."
  (dolist (cookie page-turner--face-remapping-cookies)
    (face-remap-remove-relative cookie))
  (setf page-turner--face-remapping-cookies nil))

(defun page-turner--set-prose-font ()
  "Set font for prose."
  (page-turner--reset-font)
  (when page-turner-prose-family
    (push (face-remap-add-relative 'variable-pitch
                                   :family page-turner-prose-family)
          page-turner--face-remapping-cookies))
  (when page-turner-prose-height
    (push (face-remap-add-relative 'variable-pitch
                                   :height page-turner-prose-height)
          page-turner--face-remapping-cookies)))

(defun page-turner--set-prose-styles (&rest _)
  "Set styles for prose."
  (unless page-turner--in-prose-styles
    ;; Set spacing
    (setf line-spacing page-turner-prose-line-spacing)
    ;; Set font
    (page-turner--set-prose-font)
    ;; Center text
    (setf visual-fill-column-width page-turner-text-width
          visual-fill-column-center-text t)
    (unless (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-mode))
    (unless (bound-and-true-p visual-line-mode)
      (visual-line-mode))
    (setf page-turner--in-prose-styles t)))

;;; EWW

(declare-function eww-readable "eww")
(declare-function eww-display-html "eww")

;; NOTE: eww-readable does not create new buffer, instead it reuses the current
;; buffer but replaces contents. Then the buffer will always be in prose styles
;; afterwards. So here we clear styles before displaying documents.
(defun page-turner--reset-eww-styles (&rest args)
  "Reset styles in buffer from ARGS."
  (let ((buffer (nth 4 args)))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'page-turner--in-prose-styles buffer))
      (with-current-buffer buffer
        (kill-local-variable 'line-spacing)
        (page-turner--reset-font)
        (kill-local-variable 'visual-fill-column-width)
        (kill-local-variable 'visual-fill-column-center-text)
        (when (bound-and-true-p visual-fill-column-mode)
          (visual-fill-column-mode 0))
        (when (bound-and-true-p visual-line-mode)
          (visual-line-mode 0))
        (setf page-turner--in-prose-styles nil)))))

(defun page-turner--setup-eww ()
  "Setup EWW integration."
  (with-eval-after-load 'eww
    (advice-add #'eww-readable :around #'page-turner--disable-shr-filling)
    (advice-add #'eww-readable :around #'page-turner--set-shr-width)
    (advice-add #'eww-readable :after #'page-turner--set-prose-styles)
    (advice-add #'eww-readable :around #'page-turner--adjust-shr-hr-width)
    (advice-add #'eww-readable :around #'page-turner--avoid-shr-truncating)
    (advice-add #'eww-display-html :before #'page-turner--reset-eww-styles)))

;;; Nov mode

(defvar nov-text-width)
(defvar nov-variable-pitch)

(defun page-turner--setup-nov ()
  "Setup nov mode integration."
  (with-eval-after-load 'nov
    (setf nov-text-width t
          nov-variable-pitch t)
    (add-hook 'nov-mode-hook #'page-turner--set-prose-styles)))

;;; Elfeed

(declare-function elfeed-insert-html "ext:elfeed-show")
(declare-function elfeed-insert-link "ext:elfeed-show")

(defun page-turner--setup-elfeed ()
  "Setup Elfeed integration."
  (with-eval-after-load 'elfeed-show
    (advice-add #'elfeed-insert-html :around #'page-turner--disable-shr-filling)
    (advice-add #'elfeed-insert-html :around #'page-turner--set-shr-width)
    (advice-add #'elfeed-insert-link :around #'page-turner--set-shr-width)
    (advice-add #'elfeed-insert-html :around #'page-turner--adjust-shr-hr-width)
    (advice-add #'elfeed-insert-html :around #'page-turner--avoid-shr-truncating)
    (add-hook 'elfeed-show-mode-hook #'page-turner--set-prose-styles)))

;;; Markdown mode

(declare-function markdown-live-preview-window-eww "ext:markdown-mode")
(defvar markdown-live-preview-window-function)

;; NOTE: Visual fill column and visual line do not play well with live
;; previewing for some unknown reason, so here we only set font and text width.
(defun page-turner--get-markdown-live-preview-buffer (file)
  "Get a buffer showing FILE with EWW."
  (let* ((shr-width (or page-turner-text-width fill-column))
         (buffer (markdown-live-preview-window-eww file)))
    (with-current-buffer buffer
      (page-turner--set-prose-font))
    buffer))

(defun page-turner--setup-markdown ()
  "Setup Markdown mode integration."
  (with-eval-after-load 'markdown-mode
    (setf markdown-live-preview-window-function
          #'page-turner--get-markdown-live-preview-buffer)))

;;; Man

(defvar Man-width)
(defvar Man-width-max)

(defun page-turner--setup-man ()
  "Setup man integration."
  (with-eval-after-load 'man
    (setf Man-width nil
          Man-width-max (or page-turner-text-width fill-column))))

;;; Entry point

(defun page-turner-setup ()
  "Setup tweaks and hacks for improving reading experience."
  (page-turner--setup-eww)
  (page-turner--setup-nov)
  (page-turner--setup-elfeed)
  (page-turner--setup-markdown)
  (page-turner--setup-man))

(provide 'page-turner)
;;; page-turner.el ends here
