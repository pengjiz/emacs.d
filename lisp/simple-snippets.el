;;; simple-snippets.el --- Basic general snippets  -*- lexical-binding: t -*-

;;; Commentary:

;; Collection of basic snippets that are applicable to all languages.

;;; Code:

;; Email
(defun simple-snippets-email ()
  "Insert email address of the current user."
  (interactive)
  (insert user-mail-address))

;; Time
(defun simple-snippets-time ()
  "Insert the current time."
  (interactive)
  (insert (current-time-string)))

(provide 'simple-snippets)
;;; simple-snippets.el ends here
