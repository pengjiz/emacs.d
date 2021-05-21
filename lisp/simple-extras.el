;;; simple-extras.el --- Extra basic editing helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Extra basic editing helpers.

;;; Code:

;;; Fill and unfill

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

(defun simple-extras-unfill-paragraph (&optional region)
  "Unwrap paragraph at or after point.
REGION is directly passed to `fill-paragraph'."
  (interactive (list t))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;;; Evaluate

(defun simple-extras-eval-and-replace-last-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

;;; Completion

(defun simple-extras-force-completion-at-point ()
  "Force completion on the text around point."
  (interactive)
  (minibuffer-hide-completions)
  (let ((completion-cycle-threshold t))
    (completion-at-point)))

;;; Copy

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
