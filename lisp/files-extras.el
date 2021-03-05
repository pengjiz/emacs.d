;;; files-extras.el --- Extra file utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra file utilities.

;;; Code:

;;; Visit recent files

(defvar recentf-list)

(defvar files-extras--selectrum-filename-syntax-table
  (let ((table (copy-syntax-table minibuffer-local-filename-syntax)))
    (modify-syntax-entry ?\s "_" table)
    table)
  "Syntax table for reading filenames with Selectrum.")

(defun files-extras-find-recent-file ()
  "Visit a recent file with completion."
  (interactive)
  (unless (and (bound-and-true-p recentf-mode)
               recentf-list)
    (user-error "No recent files"))
  (let ((table (if (bound-and-true-p selectrum-mode)
                   files-extras--selectrum-filename-syntax-table
                 minibuffer-local-filename-syntax)))
    (find-file (minibuffer-with-setup-hook
                   (:append (lambda () (set-syntax-table table)))
                 (completing-read "Find recent file: "
                                  (mapcar #'abbreviate-file-name recentf-list)
                                  nil 'confirm nil 'file-name-history)))))

(provide 'files-extras)
;;; files-extras.el ends here
