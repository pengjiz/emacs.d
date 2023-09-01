;;; simple-extras.el --- Extra basic editing helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Extra basic editing helpers.

;;; Code:

;;; Fill and unfill

(define-minor-mode simple-extras-auto-fill-comments-mode
  "Minor mode to automatically fill comments only."
  :group 'comment :lighter nil :keymap nil
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

;;; Mail

(defvar simple-extras--mail-send-hook nil
  "Hook to run before sending a mail.")

(defun simple-extras--compose-mail (&rest args)
  "Start composing a draft mail with ARGS."
  (let ((to (or (nth 0 args) ""))
        (subject (or (nth 1 args) ""))
        (other-headers (nth 2 args))
        (continue (nth 3 args))
        (switch-fn (or (nth 4 args) #'pop-to-buffer-same-window))
        (buffer (get-buffer-create "*draft mail*"))
        (prompt "A draft mail exists; replace it? "))
    (unless (and (> (buffer-size buffer) 0)
                 (or continue (not (y-or-n-p prompt))))
      (with-current-buffer buffer
        (erase-buffer)
        (text-mode)
        (insert "To: " to "\n"
                "Subject: " subject "\n")
        (dolist (header other-headers)
          (let ((name (car header)))
            (unless (equal (downcase name) "body")
              (insert name ": " (cdr header) "\n"))))
        (insert mail-header-separator "\n")))
    (funcall switch-fn buffer)))

(define-mail-user-agent 'simple-extras-mail-user-agent
  #'simple-extras--compose-mail #'ignore
  nil 'simple-extras--mail-send-hook)

(provide 'simple-extras)
;;; simple-extras.el ends here
