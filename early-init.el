;;; early-init.el --- Early initialization  -*- lexical-binding: t -*-

;;; Commentary:

;; Early initialization file.

;;; Code:

(defconst early-init--original-gc-cons-threshold gc-cons-threshold
  "Original value of `gc-cons-threshold'.")
(setf gc-cons-threshold 100000000)
(defun early-init--restore-gc-cons-threshold ()
  "Restore `gc-cons-threshold' to the original value."
  (setf gc-cons-threshold early-init--original-gc-cons-threshold))
(add-hook 'emacs-startup-hook #'early-init--restore-gc-cons-threshold)

(setf load-prefer-newer t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(menu-bar-mode 0)
(setf (default-value 'mode-line-format) nil)

;;; early-init.el ends here
