;;; files-extras.el --- Extra file utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra file utilities.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defvar recentf-list)
(defvar selectrum--minibuffer-local-filename-syntax)

;;; Visit recent file

(defun files-extras--setup-filename-minibuffer ()
  "Setup minibuffer for reading a filename."
  (set-syntax-table (if (and (bound-and-true-p selectrum-mode))
                        selectrum--minibuffer-local-filename-syntax
                      minibuffer-local-filename-syntax)))

(defun files-extras-find-recent-file ()
  "Visit a recent file with completion."
  (interactive)
  (unless (and (bound-and-true-p recentf-mode)
               recentf-list)
    (user-error "No recent files"))
  (find-file (minibuffer-with-setup-hook
                 (:append #'files-extras--setup-filename-minibuffer)
               (completing-read "Find recent file: "
                                (mapcar #'abbreviate-file-name recentf-list)
                                nil 'confirm nil 'file-name-history))))

(provide 'files-extras)
;;; files-extras.el ends here
