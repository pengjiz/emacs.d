;;; conda.el --- Integration with conda  -*- lexical-binding: t -*-

;;; Commentary:

;; Simple integration with the conda environment system.

;;; Code:

(require 'json)
(eval-when-compile
  (require 'cl-lib))

;;; Option

(defgroup conda nil
  "Integration with conda."
  :group 'processes)

(defcustom conda-environment-directories
  #'conda-get-configured-environment-directories
  "List of directories for named environments.
This could also be a function that returns such a list."
  :type '(choice (repeat directory)
                 function))

(defcustom conda-default-environment
  nil
  "Default environment to use when reading an environment."
  :type '(choice string
                 (const :tag "None" nil))
  :safe #'string-or-null-p)

(defcustom conda-preserve-envvar-names
  '("TERM" "SHLVL" "PWD" "_")
  "List of envvar names to preserve."
  :type '(repeat string))

(defcustom conda-ignore-envvar-names
  '("PS1" "DISPLAY")
  "List of envvar names to ignore."
  :type '(repeat string))

(defcustom conda-activate-command
  "conda activate \"$0\" >&2 && env -0"
  "Shell command to activate an environment."
  :type 'string)

(defcustom conda-deactivate-command
  "conda deactivate >&2 && env -0"
  "Shell command to deactivate an environment."
  :type 'string)

(defcustom conda-pre-activate-hook
  nil
  "Hook to run before activating an environment."
  :type 'hook)

(defcustom conda-post-activate-hook
  nil
  "Hook to run after activating an environment."
  :type 'hook)

(defcustom conda-pre-deactivate-hook
  nil
  "Hook to run before deactivating an environment."
  :type 'hook)

(defcustom conda-post-deactivate-hook
  nil
  "Hook to run after deactivating an environment."
  :type 'hook)

;;; Shell command

(defun conda--insert-log (logfile command args status)
  "Insert contents of LOGFILE into the log buffer.
Shell COMMAND and ARGS are inserted before the log, and STATUS is
inserted after the log."
  (with-current-buffer (get-buffer-create " *conda*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (shell-quote-argument shell-file-name) " "
              (shell-quote-argument shell-command-switch) " "
              (format "'%s'" command) " "
              (mapconcat #'shell-quote-argument args " ") "\n")
      (insert-file-contents logfile)
      (goto-char (point-max))
      (insert (format "Process exited with status %s" status)))))

(defvar conda--command-history nil "History for shell commands.")

(defun conda--read-command (prompt &optional default)
  "Read a shell command with PROMPT and DEFAULT command."
  (let ((input (read-shell-command (format-prompt prompt default)
                                   nil 'conda--command-history default)))
    (or (and (string-empty-p (or input "")) default)
        input)))

(defun conda--shell-run (command &rest args)
  "Run shell COMMAND with ARGS and return the output."
  (let ((logfile (make-temp-file "emacs-conda")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process shell-file-name
                               nil (list t logfile) nil
                               shell-command-switch command args)))
            (conda--insert-log logfile command args status)
            (when (eq status 0)
              (buffer-substring-no-properties (point-min) (point-max)))))
      (when (file-exists-p logfile)
        (delete-file logfile)))))

;;; Read environment

(defvar conda--configured-environment-directory-command
  "conda config --json --show envs_dirs"
  "Shell command for getting configured environment directories.")

(defun conda-get-configured-environment-directories ()
  "Get directories for named environments by querying configuration."
  (when-let* ((json-array-type 'list)
              (command conda--configured-environment-directory-command)
              (output (ignore-errors (conda--shell-run command))))
    (cdr (assq 'envs_dirs (json-read-from-string output)))))

(defun conda--get-named-environments ()
  "Get all named environments."
  (mapcan (lambda (directory)
            (when (file-directory-p directory)
              (let ((environments nil))
                (dolist (file (directory-files directory t "^[^.]"))
                  (when (file-directory-p file)
                    (push (file-name-nondirectory file) environments)))
                (nreverse environments))))
          (if (functionp conda-environment-directories)
              (funcall conda-environment-directories)
            conda-environment-directories)))

(defvar conda--directory-completion-table
  (let ((directories (apply-partially #'completion-table-with-predicate
                                      #'completion-file-name-table
                                      #'file-directory-p t)))
    (completion-table-with-quoting directories
                                   #'substitute-in-file-name
                                   #'completion--sifn-requote))
  "Completion table for directories.")

(defun conda--get-environment-completion-table ()
  "Get a completion table for environments."
  (let ((names (conda--get-named-environments)))
    (lambda (input predicate action)
      (let ((table (if (file-name-directory input)
                       conda--directory-completion-table
                     names)))
        (complete-with-action action table input predicate)))))

(defun conda--environment-valid-p (environment table)
  "Return non-nil if ENVIRONMENT is valid for completion TABLE."
  (let ((completion-ignore-case nil))
    (and (not (string-empty-p (or environment "")))
         (test-completion environment table))))

(defvar conda--environment-history nil "History for environments.")

(defun conda--read-environment (prompt)
  "Read an environment with PROMPT."
  (let* ((history-add-new-input nil)
         (minibuffer-completing-file-name t)
         (table (conda--get-environment-completion-table))
         (default (and (conda--environment-valid-p conda-default-environment
                                                   table)
                       conda-default-environment))
         (input (completing-read (format-prompt prompt default) table nil t
                                 nil 'conda--environment-history default))
         (named (not (file-name-directory input)))
         (environment (or (and named input)
                          (substitute-in-file-name input))))
    (add-to-history 'conda--environment-history environment)
    (or (and named environment)
        (expand-file-name environment))))

;;; Activate and deactivate environments

(defvar python-shell-virtualenv-root)

(defvar conda--environment-stack nil "Stack of activated environments.")
(put 'conda--environment-stack 'risky-local-variable t)

(defun conda-environment-effective-p (&optional buffer)
  "Return non-nil if environment is effective in BUFFER.
BUFFER defaults to the current buffer."
  (let ((directory (buffer-local-value 'default-directory
                                       (or buffer (current-buffer)))))
    (and (not (file-remote-p directory))
         (not (local-variable-p 'process-environment buffer))
         (not (local-variable-p 'exec-path buffer))
         (not (local-variable-p 'python-shell-virtualenv-root buffer)))))

(defun conda-get-current-environment-label (&optional top)
  "Return a label for currently active environments.
If TOP is non-nil, return only the top one on the stack."
  (when conda--environment-stack
    (let ((indicators nil)
          (active t))
      (dolist (environment (or (and top (list (car conda--environment-stack)))
                               conda--environment-stack))
        (when active
          (let ((indicator (car environment)))
            (push (or (and (file-name-directory indicator)
                           (abbreviate-file-name indicator))
                      indicator)
                  indicators)))
        (setf active (cdr environment)))
      (string-join (nreverse indicators) ":"))))

(defun conda--ignore-envvar-p (envvar)
  "Return non-nil if ENVVAR should be ignored."
  (catch 'done
    (dolist (prefix (mapcar (lambda (name) (concat name "="))
                            conda-ignore-envvar-names))
      (when (string-prefix-p prefix envvar)
        (throw 'done t)))))

(defun conda--update-environment (envvars)
  "Update current environment with ENVVARS."
  (unless envvars
    (error "No envvars provided"))
  (with-temp-buffer
    (let ((new nil)
          (saved nil))
      (dolist (name conda-preserve-envvar-names)
        (when-let* ((value (getenv name)))
          (push (cons name value) saved)))
      (dolist (envvar envvars)
        (unless (conda--ignore-envvar-p envvar)
          (push envvar new)))
      (setf process-environment new)
      (dolist (envvar saved)
        (setenv (car envvar) (cdr envvar)))
      (setf python-shell-virtualenv-root (getenv "CONDA_PREFIX"))
      (when-let* ((path (getenv "PATH")))
        (setf exec-path (nconc (parse-colon-path path)
                               (list exec-directory)))))))

(defun conda-activate (environment &optional command)
  "Activate an ENVIRONMENT with shell COMMAND.
COMMAND defaults to `conda-activate-command'."
  (interactive
   (list (conda--read-environment "Activate environment")
         (and current-prefix-arg
              (conda--read-command "Activate command"
                                   conda-activate-command))))
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (when (string-empty-p (or environment ""))
    (user-error "Environment not specified"))

  (run-hooks 'conda-pre-activate-hook)
  (let ((prefix (message "Activating environment %s..." environment))
        (output (conda--shell-run (or command conda-activate-command)
                                  environment)))
    (if (not output)
        (message "%sprocess exited abnormally" prefix)
      (conda--update-environment (split-string output "\0" t))
      (let* ((level (getenv "CONDA_SHLVL"))
             (stacked (getenv (format "CONDA_STACKED_%s" level))))
        (push (cons (getenv "CONDA_DEFAULT_ENV")
                    (equal stacked "true"))
              conda--environment-stack))
      (run-hooks 'conda-post-activate-hook)
      (message "%sdone" prefix))))

(defun conda-deactivate (&optional command)
  "Deactivate the top environment on stack with shell COMMAND.
COMMAND defaults to `conda-deactivate-command'."
  (interactive
   (list (and current-prefix-arg
              (conda--read-command "Deactivate command"
                                   conda-deactivate-command))))
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (unless conda--environment-stack
    (user-error "No environment to deactivate"))

  (run-hooks 'conda-pre-deactivate-hook)
  (let* ((environment (conda-get-current-environment-label t))
         (prefix (message "Deactivating environment %s..." environment))
         (output (conda--shell-run (or command conda-deactivate-command))))
    (if (not output)
        (message "%sprocess exited abnormally" prefix)
      (conda--update-environment (split-string output "\0" t))
      (pop conda--environment-stack)
      (run-hooks 'conda-post-deactivate-hook)
      (message "%sdone" prefix))))

;;; Flycheck integration

(declare-function flycheck-buffer "ext:flycheck")
(declare-function flycheck-defined-checkers "ext:flycheck")
(declare-function flycheck-checker-supports-major-mode-p "ext:flycheck")
(declare-function flycheck-reset-enabled-checker "ext:flycheck")

(defun conda--reset-flycheck-enabled-checkers ()
  "Reset Flycheck enabled checkers for all Python and R buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (bound-and-true-p flycheck-mode)
                 (memq major-mode '(python-mode ess-r-mode)))
        ;; Avoid rechecking too early
        (cl-letf (((symbol-function 'flycheck-buffer) #'ignore))
          (dolist (checker (flycheck-defined-checkers))
            (when (flycheck-checker-supports-major-mode-p checker)
              (flycheck-reset-enabled-checker checker))))
        (flycheck-buffer)))))

(with-eval-after-load 'flycheck
  (add-hook 'conda-post-activate-hook
            #'conda--reset-flycheck-enabled-checkers)
  (add-hook 'conda-post-deactivate-hook
            #'conda--reset-flycheck-enabled-checkers))

(provide 'conda)
;;; conda.el ends here
