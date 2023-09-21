;;; flycheck-npm.el --- Flycheck integration for NPM  -*- lexical-binding: t -*-

;;; Commentary:

;; Setup Flycheck to use the binaries installed locally by NPM.

;;; Code:

(require 'flycheck)

;;; Core

(defconst flycheck-npm--relative-local-bin
  (convert-standard-filename "node_modules/.bin")
  "Local NPM bin directory name relative to the root.")

(defun flycheck-npm--get-local-bin (root)
  "Return local NPM bin directory under ROOT properly."
  (let ((name (expand-file-name flycheck-npm--relative-local-bin root)))
    (and (file-directory-p name)
         ;; For security reasons
         (file-in-directory-p name "~")
         name)))

(defun flycheck-npm-executable-find (executable)
  "Find an EXECUTABLE installed locally by NPM.
Fall back to `flycheck-default-executable-find' if EXECUTABLE
cannot be found locally."
  (or (when-let* ((start (or buffer-file-name default-directory))
                  (root (locate-dominating-file start "package.json"))
                  (bin (flycheck-npm--get-local-bin root))
                  (exec-path (list bin)))
        (executable-find executable))
      (flycheck-default-executable-find executable)))

;;; Entry point

(defun flycheck-npm-setup ()
  "Setup Flycheck for NPM."
  (unless (file-remote-p default-directory)
    (make-local-variable 'flycheck-executable-find)
    (setf flycheck-executable-find #'flycheck-npm-executable-find)))

(provide 'flycheck-npm)
;;; flycheck-npm.el ends here
