;;; doc-view-extras.el --- Extra DocView extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra extensions for DocView.

;;; Code:

(require 'doc-view)

;;; Scale without ImageMagick

(defun doc-view-extras--enlarge (factor)
  "Enlarge the document by FACTOR."
  (interactive (list doc-view-shrink-factor))
  (if doc-view-scale-internally
      (let ((width (ceiling (* factor doc-view-image-width))))
        (unless (= width doc-view-image-width)
          (make-local-variable 'doc-view-image-width)
          (setf doc-view-image-width width)
          (doc-view-insert-image (plist-get (cdr (doc-view-current-image))
                                            :file)
                                 :width width)))
    (let ((resolution (ceiling (* factor doc-view-resolution))))
      (unless (= resolution doc-view-resolution)
        (make-local-variable 'doc-view-resolution)
        (setf doc-view-resolution resolution)
        (doc-view-reconvert-doc)))))

(defun doc-view-extras--reset-scale ()
  "Reset the document scale."
  (interactive)
  (if doc-view-scale-internally
      (progn
        (kill-local-variable 'doc-view-image-width)
        (doc-view-insert-image (plist-get (cdr (doc-view-current-image))
                                          :file)
                               :width doc-view-image-width))
    (kill-local-variable 'doc-view-resolution)
    (doc-view-reconvert-doc)))

(defun doc-view-extras--tweak-image-width (args)
  "Add the width property to ARGS when appropriate."
  (if (or (not doc-view-scale-internally)
          (memq :width (cdr args)))
      args
    `(,@args :width ,doc-view-image-width)))

;;; Entry point

(defun doc-view-extras-setup ()
  "Setup DocView extensions."
  (setf (symbol-function 'doc-view-enlarge) #'doc-view-extras--enlarge
        (symbol-function 'doc-view-scale-reset) #'doc-view-extras--reset-scale)
  (advice-add #'doc-view-insert-image :filter-args
              #'doc-view-extras--tweak-image-width))

(provide 'doc-view-extras)
;;; doc-view-extras.el ends here
