;;; window-extras.el --- Extra window utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra window utilities.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;; Buffer display hack

;; Minibuffer
(defun window-extras--avoid-fitting-to-buffer (fn &rest args)
  "Apply FN on ARGS but avoid fitting window to buffer."
  (cl-letf (((symbol-function 'fit-window-to-buffer) #'ignore))
    (apply fn args)))

(defun window-extras--setup-minibuffer ()
  "Setup minibuffer integration."
  (with-eval-after-load 'minibuffer
    (advice-add #'minibuffer-completion-help :around
                #'window-extras--avoid-fitting-to-buffer)))

;; Org mode
(defun window-extras--setup-org ()
  "Setup Org mode integration."
  (with-eval-after-load 'org
    (setf (symbol-function 'org-switch-to-buffer-other-window)
          #'switch-to-buffer-other-window)))

;; Calc
(declare-function calc "calc")

(defun window-extras--show-calc-at-bottom (fn &rest args)
  "Apply FN on ARGS but force using the root window to split."
  (cl-letf (((symbol-function 'get-largest-window)
             (lambda (&rest _) (frame-root-window))))
    (apply fn args)))

(defun window-extras--setup-calc ()
  "Setup Calc integration."
  (with-eval-after-load 'calc
    (advice-add #'calc :around #'window-extras--show-calc-at-bottom)))

;; IELM
(declare-function ielm "ielm")

(defun window-extras--pop-to-ielm-buffer (fn &rest args)
  "Apply FN on ARGS but force using `pop-to-buffer'."
  (cl-letf (((symbol-function 'pop-to-buffer-same-window)
             (lambda (buffer &rest norecord)
               (pop-to-buffer buffer nil norecord))))
    (apply fn args)))

(defun window-extras--setup-ielm ()
  "Setup IELM integration."
  (with-eval-after-load 'ielm
    (advice-add #'ielm :around #'window-extras--pop-to-ielm-buffer)))

;; Ediff
(defvar window-extras--pre-ediff-configuration nil
  "Window configuration before entering Ediff.")

(defun window-extras--save-pre-ediff-configuration ()
  "Save window configuration before entering Ediff."
  (setf window-extras--pre-ediff-configuration (current-window-configuration)))

(defun window-extras--restore-pre-ediff-configuration ()
  "Restore window configuration after leaving Ediff."
  (when (window-configuration-p window-extras--pre-ediff-configuration)
    (set-window-configuration window-extras--pre-ediff-configuration)))

(defun window-extras--setup-ediff ()
  "Setup Ediff integration."
  (add-hook 'ediff-before-setup-hook
            #'window-extras--save-pre-ediff-configuration)
  (dolist (hook '(ediff-quit-hook ediff-suspend-hook))
    (add-hook hook #'window-extras--restore-pre-ediff-configuration t)))

;; Idris mode
(defvar idris-hole-list-buffer-name)
(declare-function idris-hole-list-quit "ext:idris-hole-list")

(defun window-extras--quit-idris-hole-list (&rest _)
  "Quit Idris hole list window when appropriate."
  (when-let* ((window (get-buffer-window idris-hole-list-buffer-name t)))
    (quit-window nil window)))

(defun window-extras--setup-idris ()
  "Setup Idris mode integration."
  (with-eval-after-load 'idris-hole-list
    (advice-add #'idris-hole-list-quit :before
                #'window-extras--quit-idris-hole-list)))

;;; Entry point

(defun window-extras-setup ()
  "Setup window extensions."
  (window-extras--setup-minibuffer)
  (window-extras--setup-org)
  (window-extras--setup-calc)
  (window-extras--setup-ielm)
  (window-extras--setup-ediff)
  (window-extras--setup-idris))

(provide 'window-extras)
;;; window-extras.el ends here
