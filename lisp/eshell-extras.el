;;; eshell-extras.el --- Extra Eshell extensions  -*- lexical-binding: t; -*-

;;; Commentary:

;; Extra extensions for Eshell.

;;; Code:

(require 'esh-mode)
(require 'em-ls)
(require 'em-unix)
(require 'em-dirs)
(require 'em-prompt)
(require 'em-hist)
(require 'vc-git)
(require 'ring)
(eval-when-compile
  (require 'subr-x))

;;; Face and option

(defcustom eshell-extras-prompt-git-state-indicators-alist
  '((clean . nil)
    (dirty . "*"))
  "Indicators for different Git working tree states."
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

(defface eshell-extras-autosuggest-suggestion
  '((t :inherit shadow))
  "Face used by autosuggest suggestions."
  :group 'eshell-hist)

;;; Command

;; Common
(defun eshell/mkcd (directory)
  "Make DIRECTORY and cd to it."
  (eshell/mkdir "-p" directory)
  (eshell/cd directory))

;; Show command status
(defun eshell-extras--set-process (&rest _)
  "Set `mode-line-process' when a command is started."
  (setf mode-line-process ":run"))

(defun eshell-extras--clear-process (&rest _)
  "Clear `mode-line-process' when end a command is finished."
  (setf mode-line-process nil))

;; Buffer content
(defun eshell-extras-clear-buffer ()
  "Clear Eshell buffer."
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

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
    (cdr (assq state eshell-extras-prompt-git-state-indicators-alist))))

(defun eshell-extras--get-prompt ()
  "Return a prompt for Eshell."
  (let* ((directory (eshell-extras--get-directory))
         (env (bound-and-true-p conda-current-environment))
         (branch (eshell-extras--get-git-branch))
         (status (and branch (eshell-extras--get-git-status))))
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
     ;; Last command status
     (propertize "λ" 'face (if (zerop eshell-last-command-status)
                               'eshell-extras-prompt-exit-success
                             'eshell-extras-prompt-exit-error))
     " ")))

(defun eshell-extras--setup-prompt ()
  "Setup Eshell prompt."
  (setf eshell-prompt-function #'eshell-extras--get-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-highlight-prompt nil))

;;; Autosuggest

(defvar-local eshell-extras--autosuggest-previous-input nil
  "Previous input for offering autosuggest suggestions.")
(defvar-local eshell-extras--autosuggest-suggestion-overlay nil
  "Overlay used to display autosuggest suggestions.")

(defvar eshell-extras-autosuggest-suggestion-map (make-sparse-keymap)
  "Keymap used on autosuggest suggestion overlays.")

(defun eshell-extras--get-input ()
  "Get the current input for offering autosuggest suggestions."
  (save-excursion
    (goto-char (point-max))
    (eshell-bol)
    (and (/= (point) (point-max))
         (buffer-substring-no-properties (point) (point-max)))))

(defun eshell-extras--get-suggestion (input)
  "Get the autosuggest suggestion for INPUT."
  (catch 'done
    (dolist (element (ring-elements eshell-history-ring))
      (when (string-prefix-p input element)
        (throw 'done (substring-no-properties element))))))

(defun eshell-extras--show-suggestion (input)
  "Show the autosuggest suggestion for INPUT.
Return the overlay made."
  (when-let* ((suggestion (eshell-extras--get-suggestion input))
              (overlay (make-overlay (point-max) (point-max) nil nil t))
              (string (propertize
                       (substring suggestion (length input))
                       'face 'eshell-extras-autosuggest-suggestion
                       'cursor 0)))
    (overlay-put overlay 'after-string string)
    (overlay-put overlay 'window (selected-window))
    (overlay-put overlay 'keymap eshell-extras-autosuggest-suggestion-map)
    overlay))

(defun eshell-extras--update-suggestion ()
  "Update autosuggest suggestion after commands."
  (let ((input (eshell-extras--get-input)))
    (unless (equal input eshell-extras--autosuggest-previous-input)
      (when eshell-extras--autosuggest-suggestion-overlay
        (delete-overlay eshell-extras--autosuggest-suggestion-overlay)
        (setf eshell-extras--autosuggest-suggestion-overlay nil))
      (setf eshell-extras--autosuggest-previous-input input)
      (when input
        (setf eshell-extras--autosuggest-suggestion-overlay
              (eshell-extras--show-suggestion input))))))

(defun eshell-extras-accept-suggestion ()
  "Insert the whole autosuggest suggestion."
  (interactive "^")
  (when-let* ((overlay eshell-extras--autosuggest-suggestion-overlay)
              (suggestion (overlay-get overlay 'after-string)))
    (let (deactivate-mark)
      (insert (substring-no-properties suggestion)))))

(defun eshell-extras-accept-suggestion-word (&optional arg)
  "Insert ARG words of the autosuggest suggestion."
  (interactive "^p")
  (when-let* ((overlay eshell-extras--autosuggest-suggestion-overlay)
              (suggestion (overlay-get overlay 'after-string)))
    (let (deactivate-mark)
      (save-excursion
        (insert (substring-no-properties suggestion)))
      (forward-word arg)
      (delete-region (point) (line-end-position)))))

(defun eshell-extras--turn-on-autosuggest ()
  "Turn on Eshell autosuggest."
  (add-hook 'post-command-hook #'eshell-extras--update-suggestion
            nil t))

(defun eshell-extras--setup-autosuggest ()
  "Setup Eshell autosuggest."
  (add-hook 'eshell-mode-hook #'eshell-extras--turn-on-autosuggest))

;;; Entry point

(defun eshell-extras-setup ()
  "Setup Eshell extensions."
  (eshell-extras--setup-command)
  (eshell-extras--setup-prompt)
  (eshell-extras--setup-autosuggest))

(provide 'eshell-extras)
;;; eshell-extras.el ends here
