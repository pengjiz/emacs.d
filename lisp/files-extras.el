;;; files-extras.el --- Extra file utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra file utilities.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defvar recentf-list)
(defvar selectrum--minibuffer-local-filename-syntax)
(declare-function projectile-project-root "ext:projectile")

;;; Copy filename

(defun files-extras-copy-filename (&optional arg)
  "Copy the filename of the current file.
The format depends on ARG.

With a zero argument, copy the abbreviated filename. With one
\\[universal-argument], copy the filename relative to the current
Projectile project root or the default directory if not in a
project or Projectile is not enabled. Otherwise copy the full
filename.

If the buffer is not visiting a file, copy the
`default-directory' instead."
  (interactive "p")
  (when-let* ((filename (or buffer-file-name
                            (expand-file-name default-directory)))
              (filename-to-copy
               (cl-case arg
                 ((0) (abbreviate-file-name filename))
                 ((4) (file-relative-name
                       filename
                       (and (bound-and-true-p projectile-mode)
                            (projectile-project-root))))
                 (otherwise filename))))
    (kill-new filename-to-copy)
    (message "Filename `%s' saved to kill ring" filename-to-copy)))

;;; List files

(defun files-extras-list-files (&optional arg)
  "List files in the Projectile project root or `default-directory'.
With non-nil ARG prompt the user for the directory."
  (interactive "P")
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window display-buffer-at-bottom)
           (window-height . 15)
           (preserve-size . (nil . t))
           (reusable-frames . nil)))
        (directory (if arg
                       (read-directory-name "Directory: " nil nil t)
                     (or (and (bound-and-true-p projectile-mode)
                              (projectile-project-root))
                         default-directory))))
    (dired directory)))

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
