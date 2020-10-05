;;; early-init.el --- Early initialization  -*- lexical-binding: t -*-

;;; Commentary:

;; Early initialization file.

;;; Code:

(setf gc-cons-threshold 100000000)
(defun my-set-gc-cons-threshold ()
  "Set `gc-cons-threshold' to a normal value."
  (setf gc-cons-threshold 10000000))
(add-hook 'emacs-startup-hook #'my-set-gc-cons-threshold)

(setf load-prefer-newer t)

(when (fboundp #'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode 0))
(menu-bar-mode 0)

;;; early-init.el ends here
