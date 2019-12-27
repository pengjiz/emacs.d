;;; counsel-extras.el --- Extra counsel extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra counsel extensions.

;;; Code:

(require 'counsel)
(eval-when-compile
  (require 'subr-x))

;;; Display transformers

;; Show the first line of the docstring for commands

(defun counsel-extras--display-command (command)
  "Return the formatted string of COMMAND for display."
  (let* ((symbol (intern command))
         (doc (and (fboundp symbol)
                   (documentation symbol)))
         (doc-clean (and doc
                         (replace-regexp-in-string
                          ":[a-z]+ advice:.+[\n\r]+" ""
                          doc)))
         (doc-short (if doc-clean
                        (substring doc-clean
                                   0 (string-match "[\n\r]" doc-clean))
                      "")))
    (format "%-30s %s"
            (counsel-M-x-transformer command)
            (propertize doc-short 'face 'counsel-variable-documentation))))

(defun counsel-extras--setup-display-transformers ()
  "Setup extra display transformers for commands."
  (ivy-configure 'counsel-M-x
    :display-transformer-fn #'counsel-extras--display-command))

;;; Extra actions

;; Execute key

;; NOTE: This does not work for calling, but this does work for repeating.
(defun counsel-extras-execute-keys (candidate)
  "Execute the keys of CANDIDATE."
  (when-let* ((keys (cadr candidate))
              (events (mapcar (lambda (key) (cons t key))
                              (read-kbd-macro keys t))))
    (setf prefix-arg current-prefix-arg
          unread-command-events events)))

(defun counsel-extras--setup-actions ()
  "Setup extra actions for commands."
  (ivy-add-actions
   'counsel-descbinds
   '(("x" counsel-extras-execute-keys "execute"))))

;;; Entry point

(defun counsel-extras-setup ()
  "Setup counsel extensions."
  (counsel-extras--setup-display-transformers)
  (counsel-extras--setup-actions))

(provide 'counsel-extras)
;;; counsel-extras.el ends here
