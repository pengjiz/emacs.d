;;; files-extras.el --- Extra file utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra file utilities.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;; Open the file with external programs

(declare-function dired-get-file-for-visit "dired")
(declare-function projectile-project-root "ext:projectile")

(defun files-extras--get-open-program (&optional arg)
  "Get the program name to open files externally.
With non-nil ARG always prompt for the program."
  (let ((program (cl-case system-type
                   ((gnu gnu/linux gnu/kfreebsd) "xdg-open")
                   ((darwin) "open"))))
    (unless (and (not arg)
                 program
                 (executable-find program))
      (setf program
            (read-shell-command "Open current file with: ")))
    program))

(defun files-extras-open-externally (&optional arg)
  "Open the current file externally.
With non-nil ARG always prompt for the program to use."
  (interactive "P")
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (unless (file-exists-p buffer-file-name)
    (error "File does not exist"))
  (call-process
   (files-extras--get-open-program arg)
   nil 0 nil
   buffer-file-name))

(defun files-extras-open-externally-in-dired (&optional arg)
  "Open the file at point externally.
With non-nil ARG always prompt for the program to use."
  (interactive "P")
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (let ((file (dired-get-file-for-visit)))
    (call-process (files-extras--get-open-program arg)
                  nil 0 nil
                  file)))

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
  "List files in the Projectile project root.
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

(provide 'files-extras)
;;; files-extras.el ends here
