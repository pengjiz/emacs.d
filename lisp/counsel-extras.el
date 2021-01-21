;;; counsel-extras.el --- Extra counsel extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra counsel extensions.

;;; Code:

(require 'counsel)
(eval-when-compile
  (require 'subr-x))

;;; Display transformer

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

;; Format directory part for filenames
(defun counsel-extras--display-filename (filename)
  "Return the formatted string of FILENAME for display."
  (if-let* ((directory (file-name-directory filename)))
      (concat (propertize directory 'face 'ivy-subdir)
              (file-name-nondirectory filename))
    filename))

(defun counsel-extras--setup-display-transformers ()
  "Setup extra display transformers for commands."
  (ivy-configure 'counsel-M-x
    :display-transformer-fn #'counsel-extras--display-command)
  (ivy-configure 'counsel-file-jump
    :display-transformer-fn #'counsel-extras--display-filename))

;;; Extra action

;; Execute key when listing key bindings
;;
;; NOTE: This does not work for calling because the key will be executed in the
;; minibuffer. However it works for repeating. There is a similar action builtin
;; that works for calling but does not work for repeating. It seems that
;; repeating is more useful here.
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

;;; Command

;; Search with ripgrep
(defvar projectile-globally-ignored-file-suffixes)
(declare-function projectile-ignored-files-rel "ext:projectile")
(declare-function projectile-ignored-directories-rel "ext:projectile")
(declare-function projectile-acquire-root "ext:projectile")
(declare-function projectile-prepend-project-name "ext:projectile")

;; NOTE: The builtin command has some mechanisms to choose the root directory,
;; which I do not like. This command instead will always fall back to the
;; current directory if the root directory is not provided.
(defun counsel-extras-rg (&optional initial-input directory)
  "Search in files under a directory with ripgrep.
INITIAL-INPUT is used as the initial minibuffer input. DIRECTORY
is used as the root directory if given, otherwise the current
directory is used. With \\[universal-argument] prompt the user
for the root directory.

Extra arguments to ripgrep can be specified interactively in the
input using the same way as `counsel-rg'."
  (interactive
   (list nil
         (and current-prefix-arg
              (counsel-read-directory-name "rg in directory: "))))
  (let (current-prefix-arg)
    (counsel-rg initial-input
                (or directory default-directory)
                nil
                "rg: ")))

(defun counsel-extras--get-project-ignored-globs ()
  "Return a list of ignored globs in Projectile projects."
  (let (globs)
    (dolist (suffix projectile-globally-ignored-file-suffixes)
      (push (concat "*" suffix) globs))
    (append globs
            (projectile-ignored-files-rel)
            (projectile-ignored-directories-rel))))

(defun counsel-extras--get-rg-project-command ()
  "Return a command for running ripgrep in a Projectile project."
  (let* ((counsel-ag-command counsel-rg-base-command)
         (filter (mapconcat (lambda (glob)
                              (concat "--glob !" (shell-quote-argument glob)))
                            (counsel-extras--get-project-ignored-globs)
                            " ")))
    (counsel--format-ag-command filter "%s")))

(defun counsel-extras-rg-project (&optional initial-input directory)
  "Search in files of a Projectile project with ripgrep.
INITIAL-INPUT is used as the initial minibuffer input. DIRECTORY
is used to start the project search if given, otherwise the
current directory is used.

Extra arguments to ripgrep can be specified interactively in the
input using the same way as `counsel-rg'."
  (interactive)
  (unless (bound-and-true-p projectile-mode)
    (user-error "Projectile not enabled"))
  (let* ((default-directory (projectile-acquire-root directory))
         (counsel-rg-base-command (counsel-extras--get-rg-project-command))
         current-prefix-arg)
    (counsel-rg initial-input
                default-directory
                nil
                (projectile-prepend-project-name "rg: "))))

;;; Entry point

(defun counsel-extras-setup ()
  "Setup counsel extensions."
  (counsel-extras--setup-display-transformers)
  (counsel-extras--setup-actions))

(provide 'counsel-extras)
;;; counsel-extras.el ends here
