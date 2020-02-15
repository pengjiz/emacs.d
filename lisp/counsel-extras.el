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
  :type 'string)

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

;; Execute key when listing key bindings
;;
;; NOTE: This does not work for calling because the key will be executed in the
;; minibuffer. However it works for repeating. There is a similar action by
;; default that works for calling but does not work for repeating. It seems that
;; repeating is more useful here so we override the default.
;;
;; TODO: Find a way to solve the incompatibility with calling.
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

(defun counsel-extras--display-filename (filename)
  "Return the formatted string of FILENAME for display."
  (if-let* ((directory (file-name-directory filename)))
      (concat (propertize directory 'face 'ivy-subdir)
              (file-name-nondirectory filename))
    filename))

;; Find file with fd
(defvar counsel-extras--fd-command-template
  (format "%s --type f --full-path --color never %%s '%%s'"
          counsel-extras-fd-program)
  "Command template for running fd.")
(defvar counsel-extras--fd-history nil "Input history for fd.")

(defun counsel-extras--get-fd-command (input &optional occur)
  "Return a command to get files based on INPUT.
If OCCUR is non-nil return a command for `ivy-occur'."
  (let ((pattern (funcall ivy--regex-function input))
        (case-switch (if (ivy--case-fold-p input)
                         "--ignore-case"
                       "--case-sensitive")))
    (unless occur
      (setf ivy--old-re pattern))
    (format counsel-extras--fd-command-template
            (concat case-switch (and occur " --print0"))
            (counsel--elisp-to-pcre pattern))))

(defun counsel-extras--get-fd-files (input)
  "Get a list of files based on INPUT asynchronously."
  (or (ivy-more-chars)
      (ignore
       (counsel--async-command (counsel-extras--get-fd-command input)))))

(defun counsel-extras-fd (&optional initial-input directory)
  "Find a file under a directory with fd.
INITIAL-INPUT is used as initial minibuffer input. DIRECTORY is
used as the root directory if given, otherwise the current
directory is used."
  (interactive
   (list nil
         (when current-prefix-arg
           (counsel-read-directory-name "From directory: "))))
  (let ((default-directory (or directory default-directory)))
    (when (file-remote-p default-directory)
      (user-error "Remote hosts not supported"))
    (counsel-require-program counsel-extras-fd-program)
    (ivy-read "Find file: "
              #'counsel-extras--get-fd-files
              :initial-input initial-input
              :dynamic-collection t
              :action #'find-file
              :require-match t
              :history 'counsel-extras--fd-history
              :caller 'counsel-extras-fd)))

(defun counsel-extras--occur-fd-files (&optional _)
  "Generate occur buffer for `counsel-extras-fd'."
  (setf default-directory (ivy-state-directory ivy-last))
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (counsel-cmd-to-dired
   (counsel--expand-ls
    (concat (counsel-extras--get-fd-command ivy-text t)
            " | xargs --null ls"))))

(ivy-configure 'counsel-extras-fd
  :occur #'counsel-extras--occur-fd-files
  :unwind-fn #'counsel-delete-process
  :display-transformer-fn #'counsel-extras--display-filename)

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
