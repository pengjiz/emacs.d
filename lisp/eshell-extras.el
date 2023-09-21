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
(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist))

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

(defface eshell-extras-autosuggest-suggestion
  '((t :inherit shadow))
  "Face used by autosuggest to display the suggestion."
  :group 'eshell-hist)

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
  (advice-add 'eshell-command-started :before #'eshell-extras--set-process)
  (advice-add 'eshell-command-finished :after #'eshell-extras--clear-process))

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
  "Return a prompt string."
  (let* ((directory (eshell-extras--get-directory))
         (env (and (fboundp 'conda-get-current-environment)
                   (conda-get-current-environment)))
         (branch (eshell-extras--get-git-branch))
         (status (and branch (eshell-extras--get-git-status)))
         (separator (if (file-remote-p default-directory) "Λ" "λ"))
         (exit (if (eshell-exit-success-p)
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
  "Setup prompt."
  (setf eshell-prompt-function #'eshell-extras--get-prompt
        eshell-prompt-regexp "^.* [λΛ] "
        eshell-highlight-prompt nil))

;;; Autosuggest

(defvar-local eshell-extras--autosuggest-active nil
  "Whether autosuggest is active.")
(defvar-local eshell-extras--autosuggest-completion nil
  "Completion data for autosuggest.")
(defvar-local eshell-extras--autosuggest-overlay nil
  "Overlay for autosuggest display.")

(defun eshell-extras--autosuggest-valid-p ()
  "Return t if the current autosuggest state is valid."
  (and eshell-extras--autosuggest-completion
       (let-alist eshell-extras--autosuggest-completion
         (and (<= .start (point) .end)
              (eq .tick (buffer-modified-tick))))))

(defun eshell-extras--autosuggest-deactivate ()
  "Clear autosuggest state and display."
  (setf eshell-extras--autosuggest-active nil
        eshell-extras--autosuggest-completion nil)
  (when (overlayp eshell-extras--autosuggest-overlay)
    (delete-overlay eshell-extras--autosuggest-overlay)))

(defun eshell-extras--get-history-match (input)
  "Return history match for INPUT."
  (catch 'done
    (dolist (candidate (and (ring-p eshell-history-ring)
                            (ring-elements eshell-history-ring)))
      (when (string-prefix-p input candidate)
        (throw 'done candidate)))))

(defun eshell-extras--get-history-suffix (start end)
  "Return history suffix for the input between START and END."
  (when-let* ((input (buffer-substring-no-properties start end))
              (match (eshell-extras--get-history-match input))
              (suffix (substring-no-properties match (length input))))
    (and (not (string-empty-p suffix))
         suffix)))

(defun eshell-extras--get-autosuggest-completion ()
  "Return completion data for autosuggest."
  (and (not (eshell-interactive-process))
       (let ((start (save-excursion (eshell-bol) (point)))
             (end (point-max)))
         (and (< start end)
              (<= eshell-last-output-end start (point))
              `((start . ,start)
                (end . ,end)
                (tick . ,(buffer-modified-tick))
                (suffix . ,(eshell-extras--get-history-suffix start end)))))))

(defun eshell-extras--get-autosuggest-display (suffix)
  "Return a string for autosuggest display based SUFFIX."
  ;; NOTE: Newline characters cannot have the cursor property so we prepend a
  ;; space to the string in that case.
  (let* ((display (copy-sequence (or (and (eq ?\n (aref suffix 0))
                                          (concat " " suffix))
                                     suffix)))
         (end (length display))
         (face 'eshell-extras-autosuggest-suggestion))
    (put-text-property 0 end 'face face display)
    (put-text-property 0 1 'cursor end display)
    display))

(defun eshell-extras--autosuggest-update ()
  "Update autosuggest state and display."
  (when (and eshell-extras--autosuggest-active
             (not (eshell-extras--autosuggest-valid-p)))
    (eshell-extras--autosuggest-deactivate))

  (unless eshell-extras--autosuggest-active
    (setf eshell-extras--autosuggest-completion
          (while-no-input (eshell-extras--get-autosuggest-completion)))
    (unless (listp eshell-extras--autosuggest-completion)
      (setf eshell-extras--autosuggest-completion nil))

    (let-alist eshell-extras--autosuggest-completion
      (when (and .suffix .end)
        (move-overlay eshell-extras--autosuggest-overlay .end .end)
        (overlay-put eshell-extras--autosuggest-overlay 'after-string
                     (eshell-extras--get-autosuggest-display .suffix))
        (setf eshell-extras--autosuggest-active t)))))

(defun eshell-extras-autosuggest-accept ()
  "Accept the current suggestion from autosuggest."
  (interactive "^")
  (when eshell-extras--autosuggest-active
    (goto-char (overlay-start eshell-extras--autosuggest-overlay))
    (let* ((completion eshell-extras--autosuggest-completion)
           (suffix (cdr (assq 'suffix completion)))
           (deactivate-mark nil))
      (insert-and-inherit suffix))))

(defun eshell-extras--get-accepted-words (suffix start arg)
  "Return accepted words from START after inserting SUFFIX.
ARG is directly passed to `forward-word'."
  (with-silent-modifications
    (catch 'done
      (atomic-change-group
        (save-excursion
          (insert-and-inherit suffix)
          (goto-char start)
          (forward-word arg)
          (throw 'done
                 (and (> (point) start)
                      (buffer-substring-no-properties start (point)))))))))

(defun eshell-extras-autosuggest-accept-word (&optional arg)
  "Accept ARG words of the current suggestion from autosuggest."
  (interactive "^p")
  (when eshell-extras--autosuggest-active
    (let* ((overlay eshell-extras--autosuggest-overlay)
           (completion eshell-extras--autosuggest-completion)
           (start (overlay-start overlay))
           (suffix (cdr (assq 'suffix completion))))
      (goto-char start)
      (let ((words (eshell-extras--get-accepted-words suffix start arg))
            (deactivate-mark nil))
        (when words
          (insert-and-inherit words))))))

(defun eshell-extras-autosuggest-bind (command)
  "Return COMMAND when applicable for autosuggest."
  (let ((overlay eshell-extras--autosuggest-overlay))
    (and (overlayp overlay)
         (= (point) (overlay-start overlay))
         command)))

(defvar eshell-extras-autosuggest-map
  (let ((map (make-sparse-keymap))
        (full (list 'menu-item ""
                    #'eshell-extras-autosuggest-accept
                    :filter #'eshell-extras-autosuggest-bind))
        (word (list 'menu-item ""
                    #'eshell-extras-autosuggest-accept-word
                    :filter #'eshell-extras-autosuggest-bind)))
    (define-key map [remap forward-char] full)
    (define-key map [remap move-end-of-line] full)
    (define-key map [remap forward-word] word)
    map)
  "Keymap used by autosuggest.")

(defun eshell-extras--turn-on-autosuggest ()
  "Turn on autosuggest."
  (setf eshell-extras--autosuggest-overlay
        (make-overlay (point) (point) nil t))
  (add-hook 'post-command-hook #'eshell-extras--autosuggest-update nil t)
  (add-hook 'change-major-mode-hook
            #'eshell-extras--autosuggest-deactivate
            nil t))

(defun eshell-extras--setup-autosuggest ()
  "Setup autosuggest."
  (cl-pushnew (cons 'eshell-extras--autosuggest-active
                    eshell-extras-autosuggest-map)
              minor-mode-map-alist
              :test #'eq :key #'car)
  (add-hook 'eshell-mode-hook #'eshell-extras--turn-on-autosuggest))

;;; Entry point

(defun eshell-extras-setup ()
  "Setup Eshell extensions."
  (eshell-extras--setup-command)
  (eshell-extras--setup-prompt)
  (eshell-extras--setup-autosuggest))

(provide 'eshell-extras)
;;; eshell-extras.el ends here
