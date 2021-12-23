;;; eshell-extras.el --- Extra Eshell extensions  -*- lexical-binding: t; -*-

;;; Commentary:

;; Extra extensions for Eshell.

;;; Code:

(require 'esh-mode)
(require 'em-ls)
(require 'em-unix)
(require 'em-dirs)
(require 'em-prompt)
(require 'vc-git)
(eval-when-compile
  (require 'subr-x))

;;; Face and option

(defcustom eshell-extras-prompt-git-state-indicator-alist
  '((clean . nil)
    (dirty . "*"))
  "Indicators for Git working tree states."
  :type '(alist :key-type (choice (const clean)
                                  (const dirty))
                :value-type (choice string
                                    (const :tag "None" nil)))
  :group 'eshell-prompt)

(defface eshell-extras-prompt-plain
  '((t :inherit eshell-prompt))
  "Face used by plain parts in the prompt."
  :group 'eshell-prompt)

(defface eshell-extras-prompt-directory
  '((t :inherit (bold eshell-ls-directory)))
  "Face used by the current directory in the prompt."
  :group 'eshell-prompt)

(defface eshell-extras-prompt-git-branch
  '((t :inherit (bold eshell-ls-special)))
  "Face used by the current Git branch name in the prompt."
  :group 'eshell-prompt)

(defface eshell-extras-prompt-git-status
  '((t :inherit (bold warning)))
  "Face used by the Git working tree status."
  :group 'eshell-prompt)

(defface eshell-extras-prompt-exit-success
  '((t :inherit success))
  "Face used by the prompt when the last command succeeds."
  :group 'eshell-prompt)

(defface eshell-extras-prompt-exit-error
  '((t :inherit (bold error)))
  "Face used by the prompt when the last command fails."
  :group 'eshell-prompt)

;;; Command

;; Common
(defun eshell/mkcd (directory)
  "Create and change to DIRECTORY."
  (eshell/mkdir "-p" directory)
  (eshell/cd directory))

;; Show command status
(defun eshell-extras--set-process (&rest _)
  "Set `mode-line-process' for a running command."
  (setf mode-line-process ":run"))

(defun eshell-extras--clear-process (&rest _)
  "Clear `mode-line-process'."
  (setf mode-line-process nil))

(defun eshell-extras--setup-command ()
  "Setup command related extensions."
  (advice-add #'eshell-command-started :before #'eshell-extras--set-process)
  (advice-add #'eshell-command-finished :after #'eshell-extras--clear-process))

;;; Prompt

(defun eshell-extras--get-directory ()
  "Get the current directory."
  (let* ((directory (abbreviate-file-name (eshell/pwd)))
         (basename (file-name-nondirectory directory)))
    (if (string-empty-p basename)
        directory
      basename)))

(defun eshell-extras--call-git-string (command &rest args)
  "Execute COMMAND and ARGS with Git.
Return the first line of output if any. Otherwise return nil."
  (with-temp-buffer
    (apply #'vc-git--call '(t nil) command args)
    (unless (bobp)
      (goto-char (point-min))
      (buffer-substring-no-properties (point)
                                      (line-end-position)))))

(defun eshell-extras--get-git-branch ()
  "Get the current Git branch name or short SHA."
  (or (eshell-extras--call-git-string "symbolic-ref" "--short" "HEAD")
      (eshell-extras--call-git-string "rev-parse" "--short" "HEAD")))

(defun eshell-extras--get-git-status ()
  "Get the status of the current Git working tree."
  (let ((state (if (eshell-extras--call-git-string "status" "--porcelain")
                   'dirty
                 'clean)))
    (cdr (assq state eshell-extras-prompt-git-state-indicator-alist))))

(defun eshell-extras--get-prompt ()
  "Return a prompt for Eshell."
  (let* ((directory (eshell-extras--get-directory))
         (env (and (fboundp 'conda-get-current-environment)
                   (conda-get-current-environment)))
         (branch (eshell-extras--get-git-branch))
         (status (and branch (eshell-extras--get-git-status)))
         (separator (if (file-remote-p default-directory) "Λ" "λ"))
         (exit (if (zerop eshell-last-command-status)
                   'eshell-extras-prompt-exit-success
                 'eshell-extras-prompt-exit-error)))
    (concat
     ;; Python environment
     (when env
       (propertize (format "(%s)" env) 'face 'eshell-extras-prompt-plain))
     (and env " ")
     ;; Current working directory
     (propertize directory 'face 'eshell-extras-prompt-directory)
     ;; Git branch name or short SHA
     (when branch
       (propertize (concat "@" branch) 'face 'eshell-extras-prompt-git-branch))
     ;; Git working tree status
     (when status
       (propertize status 'face 'eshell-extras-prompt-git-status))
     " "
     ;; Separator with additional information
     (propertize separator 'face exit)
     " ")))

(defun eshell-extras--setup-prompt ()
  "Setup Eshell prompt."
  (setf eshell-prompt-function #'eshell-extras--get-prompt
        eshell-prompt-regexp "^.* [λΛ] "
        eshell-highlight-prompt nil))

;;; Entry point

(defun eshell-extras-setup ()
  "Setup Eshell extensions."
  (eshell-extras--setup-command)
  (eshell-extras--setup-prompt))

(provide 'eshell-extras)
;;; eshell-extras.el ends here
