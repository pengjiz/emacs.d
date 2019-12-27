;;; counsel-extras.el --- Extra counsel extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra counsel extensions.

;;; Code:

(require 'counsel)
(eval-when-compile
  (require 'subr-x))

(defgroup counsel-extras nil
  "Extra counsel extensions."
  :group 'counsel)

(defcustom counsel-extras-fd-program
  "fd"
  "The program name for fd."
  :type 'string
  :group 'counsel-extras)

(defcustom counsel-extras-fd-using-display-style
  nil
  "Whether to use `ivy-display-style'.
Non-nil means the value of `ivy-display-style' will be used.
Otherwise `ivy-display-style' will be set to nil temporarily."
  :type 'boolean
  :group 'counsel-extras)

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

;;; Commands

;; Find files with fd
(defvar counsel-extras--fd-command
  (format "%s --type f --color never %%s '%%s'" counsel-extras-fd-program))
(defvar counsel-extras--fd-history nil)

(defun counsel-extras--fd-get-files (input)
  "Return a list of files based on INPUT."
  (or (ivy-more-chars)
      (let* ((pattern (counsel--elisp-to-pcre
                       (setf ivy--old-re
                             (funcall ivy--regex-function input))))
             (case-switch (if (ivy--case-fold-p input)
                              "--ignore-case"
                            "--case-sensitive"))
             (command (format counsel-extras--fd-command
                              case-switch
                              pattern)))
        (counsel--async-command command)
        nil)))

(defun counsel-extras-fd (&optional initial-input directory)
  "Find a file under a directory with fd.
INITIAL-INPUT is used as initial minibuffer input. DIRECTORY is
used as the root directory if given, otherwise the current
directory is used."
  (interactive
   (list nil
         (when current-prefix-arg
           (counsel-read-directory-name "From directory: "))))
  (counsel-require-program counsel-extras-fd-program)
  (let ((default-directory (or directory default-directory))
        (ivy-display-style (and counsel-extras-fd-using-display-style
                                ivy-display-style)))
    (ivy-read "Find file: "
              #'counsel-extras--fd-get-files
              :initial-input initial-input
              :dynamic-collection t
              :action #'find-file
              :require-match t
              :history 'counsel-extras--fd-history
              :caller 'counsel-extras-fd)))

(defun counsel-extras--fd-display-filename (filename)
  "Return the formatted string of FILENAME for display."
  (if-let* ((directory (file-name-directory filename)))
      (concat (propertize directory 'face 'ivy-subdir)
              (file-name-nondirectory filename))
    filename))

(ivy-configure 'counsel-extras-fd
  :unwind-fn #'counsel-delete-process
  :display-transformer-fn #'counsel-extras--fd-display-filename)

(ivy-set-actions
 'counsel-extras-fd
 '(("j" find-file-other-window "other window")
   ("x" counsel-find-file-extern "open externally")))

;;; Entry point

(defun counsel-extras-setup ()
  "Setup counsel extensions."
  (counsel-extras--setup-display-transformers)
  (counsel-extras--setup-actions))

(provide 'counsel-extras)
;;; counsel-extras.el ends here
