;;; page-turner.el --- Improve reading experience  -*- lexical-binding: t -*-

;;; Commentary:

;; Tweaks and hacks to improve reading experience.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Options

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

;;; Prose

(declare-function shr-fill-line "shr")
(declare-function face-remap-remove-relative "face-remap")
(defvar visual-fill-column-width)
(defvar visual-fill-column-center-text)

(defvar-local page-turner--face-remapping-cookies nil
  "Face remapping cookies added.")
;; NOTE: This is a marker for whether prose styles have been applied. Here we do
;; not use minor mode because we do not want an interactive command and instead
;; we want a flexible function that can be used for advising or hooks. Also,
;; with this marker we can avoid some unnecessary works.
(defvar-local page-turner--in-prose-styles nil
  "Whether the current buffer is in prose styles.")

(defun page-turner--disable-shr-filling (fn &rest args)
  "Apply FN on ARGS, but disable `shr-fill-line'."
  (cl-letf (((symbol-function 'shr-fill-line) #'ignore))
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

;; EWW
(declare-function eww-readable "eww")
(declare-function eww-display-html "eww")

;; NOTE: EWW readable do not create new buffer, instead it reuses the current
;; buffer but replaces contents. That would cause problems when we want to leave
;; readable mode. So here we clear styles before displaying documents. This
;; applies for EWW readable as well because otherwise running readable in
;; readable mode will not use visual line for some reason.
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
    (advice-add #'eww-readable :after #'page-turner--set-prose-styles)
    (advice-add #'eww-display-html :before #'page-turner--reset-eww-styles)))

;; Nov mode
(defvar nov-text-width)
(defvar nov-variable-pitch)

(defun page-turner--setup-nov ()
  "Setup nov mode integration."
  (with-eval-after-load 'nov
    (setf nov-text-width t
          nov-variable-pitch t)
    (add-hook 'nov-mode-hook #'page-turner--set-prose-styles)))

;; Elfeed
(declare-function elfeed-insert-html "ext:elfeed")

(defun page-turner--setup-elfeed ()
  "Setup Elfeed integration."
  (with-eval-after-load 'elfeed-show
    (advice-add #'elfeed-insert-html :around #'page-turner--disable-shr-filling)
    (add-hook 'elfeed-show-mode-hook #'page-turner--set-prose-styles)))

;; Markdown mode
;;
;; NOTE: For some reason visual fill column and visual line does not work well
;; with live previewing, so here we only set font and text width.
;;
;; TODO: Find a way to enable all prose styles.
(declare-function markdown-live-preview-window-eww "ext:markdown-mode")
(defvar markdown-live-preview-window-function)
(defvar shr-width)

(defun page-turner--get-preview-buffer (file)
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
          #'page-turner--get-preview-buffer)))

;;; Documentation

(defun page-turner--set-shr-width (fn &rest args)
  "Apply FN on ARGS, but force using `page-turner-text-width'."
  (let ((shr-width (or page-turner-text-width fill-column)))
    (apply fn args)))

;; Racket mode
(declare-function racket--do-describe "ext:racket-complete")

(defun page-turner--setup-racket ()
  "Setup Racket mode integration."
  (with-eval-after-load 'racket-complete
    (advice-add #'racket--do-describe :around #'page-turner--set-shr-width)))

;; Man
(defvar Man-width)

(defun page-turner--setup-man ()
  "Setup man integration."
  (with-eval-after-load 'man
    (setf Man-width (or page-turner-text-width fill-column))))

;;; Entry point

(defun page-turner-setup ()
  "Setup tweaks and hacks for improving reading experience."
  (page-turner--setup-eww)
  (page-turner--setup-nov)
  (page-turner--setup-elfeed)
  (page-turner--setup-markdown)
  (page-turner--setup-racket)
  (page-turner--setup-man))

(provide 'page-turner)
;;; page-turner.el ends here
