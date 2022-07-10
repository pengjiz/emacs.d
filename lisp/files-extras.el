;;; files-extras.el --- Extra file utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra file utilities.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Visit recent files

(defvar recentf-list)

(defun files-extras--read-recent-filename (prompt-prefix)
  "Read a recent filename.
Prompt with a formatted string using PROMPT-PREFIX."
  (unless (and (bound-and-true-p recentf-mode)
               recentf-list)
    (user-error "No recent files"))
  (let* ((table minibuffer-local-filename-syntax)
         (filenames (mapcar #'abbreviate-file-name recentf-list))
         (default (car filenames))
         (prompt (format-prompt prompt-prefix default)))
    (minibuffer-with-setup-hook
        (:append (lambda () (set-syntax-table table)))
      (completing-read prompt
                       (lambda (input predicate action)
                         (if (eq action 'metadata)
                             '(metadata (category . recent-file))
                           (complete-with-action action
                                                 filenames
                                                 input
                                                 predicate)))
                       nil 'confirm nil 'file-name-history default))))

(with-eval-after-load 'minibuffer
  (cl-pushnew '(recent-file (styles substring)) completion-category-defaults
              :test #'eq :key #'car))

(defun files-extras-find-recent-file ()
  "Visit a recent file with completion."
  (interactive)
  (let* ((prefix "Find recent file")
         (filename (files-extras--read-recent-filename prefix)))
    (find-file filename)))

(provide 'files-extras)
;;; files-extras.el ends here
