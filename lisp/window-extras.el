;;; window-extras.el --- Extra window utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra window utilities.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;; Window display management integration

;; Org mode
(defvar org-src-window-setup)
(declare-function org-add-log-note "org")
(declare-function org-agenda-get-restriction-and-command "org-agenda")
(declare-function org-capture-place-template "org-capture")
(declare-function org-src-switch-to-buffer "org-src")
(declare-function org-export--dispatch-ui "ox")

(defun window-extras--avoid-deleting-other (fn &rest args)
  "Avoid deleting other windows and apply FN on ARGS."
  (cl-letf (((symbol-function 'delete-other-windows) #'ignore))
    (apply fn args)))

(defun window-extras--quit-org-src (fn &rest args)
  "Apply FN on ARGS but quit window appropriately."
  (let ((window (selected-window)))
    (apply fn args)
    (when (and (eq org-src-window-setup 'other-window)
               (eq (cadr args) 'exit))
      (quit-restore-window window))))

(defun window-extras--setup-org ()
  "Setup Org mode integration."
  (with-eval-after-load 'org
    (setf (symbol-function 'org-switch-to-buffer-other-window)
          #'switch-to-buffer-other-window)
    (advice-add #'org-add-log-note :around
                #'window-extras--avoid-deleting-other))

  (with-eval-after-load 'org-agenda
    (advice-add #'org-agenda-get-restriction-and-command :around
                #'window-extras--avoid-deleting-other))

  (with-eval-after-load 'org-capture
    (advice-add #'org-capture-place-template :around
                #'window-extras--avoid-deleting-other))

  (with-eval-after-load 'ox
    (advice-add #'org-export--dispatch-ui :around
                #'window-extras--avoid-deleting-other))

  (with-eval-after-load 'org-src
    (advice-add #'org-src-switch-to-buffer :around
                #'window-extras--quit-org-src)))

;;; Buffer display hack

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
  (window-extras--setup-org)
  (window-extras--setup-calc)
  (window-extras--setup-ielm)
  (window-extras--setup-ediff)
  (window-extras--setup-idris))

(provide 'window-extras)
;;; window-extras.el ends here
