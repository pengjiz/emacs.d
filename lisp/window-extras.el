;;; window-extras.el --- Extra window utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra window utilities.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;; Integration with window display management

;; Org mode
(defvar org-src-window-setup)
(declare-function org-add-log-note "org")
(declare-function org-agenda-get-restriction-and-command "org-agenda")
(declare-function org-capture-place-template "org-capture")
(declare-function org-src-switch-to-buffer "org-src")
(declare-function org-export--dispatch-ui "ox")

(defun window-extras--avoid-deleting-windows (fn &rest args)
  "Avoid deleting other windows and apply FN on ARGS."
  (cl-letf (((symbol-function 'delete-other-windows) #'ignore))
    (apply fn args)))

(defun window-extras--pop-to-org-buffer (buffer-or-name &optional norecord)
  "Apply `pop-to-buffer' with BUFFER-OR-NAME and NORECORD."
  (pop-to-buffer buffer-or-name nil norecord))

(defun window-extras--pop-to-org-src-buffer (fn buffer &rest args)
  "Use `pop-to-buffer' to display BUFFER when appropriate.
Otherwise apply FN on BUFFER and ARGS."
  (if (eq org-src-window-setup 'other-window)
      (pop-to-buffer buffer)
    (apply fn buffer args)))

(defun window-extras--setup-org ()
  "Setup Org mode integration."
  (with-eval-after-load 'org
    (setf (symbol-function 'org-switch-to-buffer-other-window)
          #'window-extras--pop-to-org-buffer)
    (advice-add #'org-add-log-note :around
                #'window-extras--avoid-deleting-windows))

  (with-eval-after-load 'org-agenda
    (advice-add #'org-agenda-get-restriction-and-command :around
                #'window-extras--avoid-deleting-windows))

  (with-eval-after-load 'org-capture
    (advice-add #'org-capture-place-template :around
                #'window-extras--avoid-deleting-windows))

  (with-eval-after-load 'ox
    (advice-add #'org-export--dispatch-ui :around
                #'window-extras--avoid-deleting-windows))

  (with-eval-after-load 'org-src
    (advice-add #'org-src-switch-to-buffer :around
                #'window-extras--pop-to-org-src-buffer)))

;; Racket mode
(defvar racket-logger--buffer-name)
(declare-function racket-logger "ext:racket-logger")

(defun window-extras--pop-to-racket-logger-buffer (fn &rest args)
  "Apply FN on ARGS but force using `pop-to-buffer'."
  (save-window-excursion
    (apply fn args))
  (pop-to-buffer racket-logger--buffer-name))

(defun window-extras--setup-racket ()
  "Setup Racket mode integration."
  (with-eval-after-load 'racket-logger
    (advice-add #'racket-logger :around
                #'window-extras--pop-to-racket-logger-buffer)))

;;; Hacks for changing how buffers are displayed

;; Calc
(declare-function calc "calc")

(defun window-extras--show-calc-at-bottom (fn &rest args)
  "Apply FN on ARGS but force using the root window to split."
  (cl-letf (((symbol-function 'get-largest-window)
             (lambda (&rest _) (frame-root-window))))
    (apply fn args)))

(defun window-extras--setup-calc ()
  "Setup Calc integration."
  (advice-add #'calc :around #'window-extras--show-calc-at-bottom))

;; Elfeed
(declare-function elfeed "ext:elfeed")

(defun window-extras--show-elfeed-fullframe (fn &rest args)
  "Apply FN on ARGS but force using the full frame."
  (cl-letf (((symbol-function 'switch-to-buffer)
             (lambda (buffer &optional norecord &rest _)
               (when-let* ((window (display-buffer buffer)))
                 (select-window window norecord)
                 (delete-other-windows window)))))
    (apply fn args)))

(defun window-extras--setup-elfeed ()
  "Setup Elfeed integration."
  (advice-add #'elfeed :around #'window-extras--show-elfeed-fullframe))

;;; Entry point

(defun window-extras-setup ()
  "Setup window extensions."
  (window-extras--setup-org)
  (window-extras--setup-racket)
  (window-extras--setup-calc)
  (window-extras--setup-elfeed))

(provide 'window-extras)
;;; window-extras.el ends here
