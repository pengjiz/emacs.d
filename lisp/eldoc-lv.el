;;; eldoc-lv.el --- Show ElDoc in LV  -*- lexical-binding: t; -*-

;;; Commentary:

;; Show ElDoc message in an LV buffer when the minibuffer is in use.

;;; Code:

(require 'eldoc)
(eval-when-compile
  (require 'cl-lib))

(defvar eldoc-lv--buffer nil "Buffer for ElDoc.")
(defvar eldoc-lv--window nil "Window for ElDoc.")

(defun eldoc-lv--get-buffer ()
  "Ensure the ElDoc buffer is live and return it."
  (unless (buffer-live-p eldoc-lv--buffer)
    (setf eldoc-lv--buffer (get-buffer-create " *ElDoc*"))
    (with-current-buffer eldoc-lv--buffer
      (setf window-size-fixed t
            mode-line-format nil
            cursor-type nil)))
  eldoc-lv--buffer)

(defun eldoc-lv--get-window ()
  "Ensure the ElDoc window is live and return it."
  (unless (window-live-p eldoc-lv--window)
    (setf eldoc-lv--window
          (let ((ignore-window-parameters t))
            (split-window (frame-root-window) -1 'below))))
  eldoc-lv--window)

(defun eldoc-lv--message (format-string &rest args)
  "Set the contents of the ElDoc buffer to (`format' FORMAT-STRING ARGS)."
  (let ((string (apply #'format format-string args))
        (window (eldoc-lv--get-window))
        (buffer (eldoc-lv--get-buffer))
        deactivate-mark)
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert string)
      (goto-char (point-min))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-other-window t)
      (let (window-size-fixed)
        (fit-window-to-buffer window nil 1)))))

(defun eldoc-lv--cleanup ()
  "Clean up after exiting minibuffer."
  (when (window-live-p eldoc-lv--window)
    (delete-window eldoc-lv--window))
  (setf eldoc-last-message nil))

(defun eldoc-lv-message (format-string &rest args)
  "Display message in an LV buffer when in minibuffer.
Otherwise work like `message'.

FORMAT-STRING and ARGS are passed to `eldoc-lv--message' or `message'."
  (if (minibufferp)
      (progn
        (add-hook 'minibuffer-exit-hook #'eldoc-lv--cleanup nil t)
        (if format-string
            (apply #'eldoc-lv--message format-string args)
          (eldoc-lv--cleanup)))
    (apply #'message format-string args)))

(defvar aw-ignored-buffers)
(with-eval-after-load 'ace-window
  (cl-pushnew " *ElDoc*" aw-ignored-buffers :test #'equal))

;;; Setup

(defun eldoc-lv-setup ()
  "Setup ElDoc to use `eldoc-lv-message'."
  (setf eldoc-message-function #'eldoc-lv-message))

(provide 'eldoc-lv)
;;; eldoc-lv.el ends here
