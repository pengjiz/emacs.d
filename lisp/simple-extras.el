;;; simple-extras.el --- Extra basic editing helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Extra basic editing helpers.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Move point

;; Normal line
(defun simple-extras-move-beginning-of-line (arg)
  "Move point back to indentation or beginning of line.

Move point to the first non-whitespace character of the line. If
point is already there move to the beginning of line.

If ARG is not nil or 1, move forward ARG - 1 lines first."
  (interactive "^p")
  (setf arg (or arg 1))

  (when (/= arg 1)
    (let (line-move-visual)
      (line-move (1- arg) t)))

  (let ((point-before (point)))
    (back-to-indentation)
    (when (= point-before (point))
      (move-beginning-of-line 1))))

;; Visual line
(defun simple-extras-back-to-indentation-of-visual-line ()
  "Move point to the first non-whitespace character on this visual line."
  (interactive "^")
  (beginning-of-visual-line 1)
  (skip-syntax-forward " "
                       (save-excursion
                         (end-of-visual-line 1)
                         (point)))
  (backward-prefix-chars))

(defun simple-extras-move-beginning-of-visual-line (arg)
  "Move point back to indentation or beginning of visual line.

Move point to the first non-whitespace character of the visual
line. If point is already there move to the beginning of visual
line.

If ARG is not nil or 1, move forward ARG - 1 visual lines first."
  (interactive "^p")
  (setf arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual t))
      (line-move (1- arg) t)))

  (let ((point-before (point)))
    (simple-extras-back-to-indentation-of-visual-line)
    (when (= point-before (point))
      (beginning-of-visual-line 1))))

;; ESS line
(declare-function ess-roxy-move-beginning-of-line "ext:ess-roxy")
(declare-function ess-roxy-entry-p "ext:ess-roxy")

(defun simple-extras-move-beginning-of-ess-line (arg)
  "Move point to a proper beginning of line for ESS.

If ARG is not nil or 1, move forward ARG - 1 lines or visual
lines first."
  (interactive "^p")
  (cond ((ess-roxy-entry-p)
         (ess-roxy-move-beginning-of-line arg))
        (visual-line-mode
         (simple-extras-move-beginning-of-visual-line arg))
        (t
         (simple-extras-move-beginning-of-line arg))))

;;; Fill

(define-minor-mode simple-extras-auto-fill-comments-mode
  "Minor mode to automatically fill comments only."
  :lighter nil
  :keymap nil
  (if simple-extras-auto-fill-comments-mode
      (progn
        (make-local-variable 'comment-auto-fill-only-comments)
        (setf comment-auto-fill-only-comments t)
        (auto-fill-mode))
    (kill-local-variable 'comment-auto-fill-only-comments)
    (auto-fill-mode 0)))

;;; Eval and replace

(defun simple-extras-eval-last-sexp-and-replace ()
  "Replace the sexp before point with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

;;; Copy email address

(defun simple-extras--copy-mail-address (&optional to &rest _)
  "Copy email address TO if given."
  (interactive)
  (when to
    (kill-new to)
    (message "Email address `%s' saved to kill ring" to)))

;;; Entry point

(defun simple-extras-setup ()
  "Setup simple extensions."
  (setf (symbol-function 'compose-mail) #'simple-extras--copy-mail-address))

(provide 'simple-extras)
;;; simple-extras.el ends here
