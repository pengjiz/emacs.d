;;; flycheck-npm.el --- Flycheck integration for NPM  -*- lexical-binding: t -*-

;;; Commentary:

;; Setup Flycheck to use the binaries installed locally by NPM.

;;; Code:

(require 'flycheck)
(eval-when-compile
  (require 'subr-x))

;;; Core

;; NOTE: For security reasons, we require that node_modules must be in the same
;; directory as package.json, and it must be under the home directory of the
;; current user. Otherwise we just return nil.
(defun flycheck-npm--expand-file-name (file root)
  "Expand name of FILE under ROOT.
Return nil if FILE does not exist."
  (let ((name (expand-file-name (convert-standard-filename file)
                                root)))
    (and (file-exists-p name)
         name)))

(defsubst flycheck-npm--file-under-home-p (file)
  "Return t if FILE is under the home directory."
  (string-prefix-p (expand-file-name "~") (expand-file-name file)))

(defun flycheck-npm-executable-find (executable)
  "Find an EXECUTABLE installed locally by NPM.
Fall back to `flycheck-default-executable-find' if EXECUTABLE
cannot be found locally."
  (or (when-let* ((root (locate-dominating-file
                         (or buffer-file-name default-directory)
                         "package.json"))
                  (npm-bin (flycheck-npm--expand-file-name
                            "node_modules/.bin"
                            root)))
        (and (flycheck-npm--file-under-home-p npm-bin)
             (let ((exec-path (list npm-bin)))
               (executable-find executable))))
      (flycheck-default-executable-find executable)))

;;; Entry point

(defun flycheck-npm-setup ()
  "Setup Flycheck for NPM."
  (make-local-variable 'flycheck-executable-find)
  (setf flycheck-executable-find #'flycheck-npm-executable-find))

(provide 'flycheck-npm)
;;; flycheck-npm.el ends here
