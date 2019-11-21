;;; haskell-extras.el --- Extra extensions for Haskell  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra extensions for editing Haskell files.

;;; Code:

(require 'haskell-customize)

;;; Options

(defcustom haskell-extras-process-types
  '(stack-ghci
    ghci)
  "Process types to consider for a Haskell project.
Each element should be a valid value for the variable
`haskell-process-type', but not all values are supported."
  :group 'haskell
  :type '(repeat symbol)
  :safe #'listp)

;;; Process type detection

(defvar haskell-extras--process-type-indicators-alist
  '((stack-ghci "stack.yaml" "stack")
    (ghci nil "ghc")))

(defun haskell-extras--get-root-directory (filename)
  "Return the root directory of project based on FILENAME."
  (if filename
      (locate-dominating-file default-directory filename)
    default-directory))

(defun haskell-extras-get-process-type ()
  "Return process type for the current Haskell project."
  (let (indicator
        root-directory)
    (or
     (catch 'done
       (dolist (type haskell-extras-process-types)
         (setf indicator
               (cdr (assq type haskell-extras--process-type-indicators-alist)))
         (when (and indicator
                    (setf root-directory
                          (haskell-extras--get-root-directory (car indicator)))
                    (executable-find (cadr indicator)))
           (setf inferior-haskell-root-dir root-directory)
           (throw 'done type))))
     (error "Failed to guess process type"))))

;;; Entry point

(defun haskell-extras-setup ()
  "Setup extensions for Haskell editing."
  (setf (symbol-function 'haskell-process-type)
        #'haskell-extras-get-process-type))

(provide 'haskell-extras)
;;; haskell-extras.el ends here
