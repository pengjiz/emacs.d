;;; conda.el --- Integration with conda  -*- lexical-binding: t -*-

;;; Commentary:

;; Simple integration with the conda environment system.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;; Option

(defgroup conda nil
  "Integration with conda."
  :group 'python)

(defcustom conda-environment-directories
  `(,(expand-file-name (convert-standard-filename ".conda/envs/") "~"))
  "List of directories for environments."
  :type '(repeat directory))

(defcustom conda-default-environment
  nil
  "Name of the default environment."
  :type 'string
  :safe #'stringp)

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

;;; Find environments

(defun conda--get-environments ()
  "Get all environments."
  (mapcan (lambda (directory)
            (when (file-directory-p directory)
              (let (environments)
                (dolist (file (directory-files directory t "^[^.]"))
                  (when (file-directory-p file)
                    (push (file-name-nondirectory file) environments)))
                (nreverse environments))))
          conda-environment-directories))

(defun conda--get-environment-path (environment)
  "Get the path of ENVIRONMENT."
  (catch 'done
    (dolist (directory conda-environment-directories)
      (let ((path (expand-file-name environment directory)))
        (when (file-directory-p path)
          (throw 'done path))))))

;;; Activate and deactivate environments

(defvar python-shell-virtualenv-root)
(defvar eshell-path-env)

(defvar conda-current-environment nil
  "Name of the currently active environment.")

(defvar conda--previous-path-env nil
  "Previous value of the PATH environment variable.")
(defvar conda--previous-exec-path nil
  "Previous value of variable `exec-path'.")

(defun conda-activate (environment &optional show-message)
  "Activate the environment named ENVIRONMENT.
When SHOW-MESSAGE is non-nil, display helpful messages."
  (interactive
   (list (unless (file-remote-p default-directory)
           (completing-read "Environment: "
                            (conda--get-environments)
                            nil t))
         t))
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (unless (and environment (not (string-empty-p environment)))
    (user-error "Conda environment not specified"))

  (let* ((path (conda--get-environment-path environment))
         ;; FIXME: This may not work on all platforms.
         (bin-path (and path (expand-file-name "bin" path))))
    (unless (and path bin-path (file-directory-p bin-path))
      (error "%s is not a valid conda environment" environment))

    (conda-deactivate show-message)
    (setf conda-current-environment environment)
    (run-hooks 'conda-pre-activate-hook)

    ;; Builtin Python mode
    (setf python-shell-virtualenv-root (file-name-as-directory path))

    ;; exec-path
    (setf conda--previous-exec-path exec-path)
    (cl-pushnew bin-path exec-path :test #'equal)

    ;; PATH environment variable
    (setf conda--previous-path-env (getenv "PATH"))
    (setenv "PATH" (string-join exec-path path-separator))

    ;; Virtual environment
    (setenv "VIRTUAL_ENV" path)
    (setenv "CONDA_PREFIX" path)

    ;; Eshell
    (setf (default-value 'eshell-path-env) (getenv "PATH"))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'eshell-mode)
                   (not (file-remote-p default-directory)))
          (setf eshell-path-env (getenv "PATH")))))

    (run-hooks 'conda-post-activate-hook)
    (when show-message
      (message "Conda environment %s activated" conda-current-environment))))

(defun conda-deactivate (&optional show-message)
  "Deactivate the current environment.
When SHOW-MESSAGE is non-nil, display helpful messages."
  (interactive (list t))
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))

  (setf conda-current-environment nil)
  (run-hooks 'conda-pre-deactivate-hook)

  ;; Builtin Python mode
  (setf python-shell-virtualenv-root nil)

  ;; exec-path
  (when conda--previous-exec-path
    (setf exec-path conda--previous-exec-path
          conda--previous-exec-path nil))

  ;; PATH environment variable
  (when conda--previous-path-env
    (setenv "PATH" conda--previous-path-env)
    (setf conda--previous-path-env nil))

  ;; Virtual environment
  (setenv "VIRTUAL_ENV" nil)
  (setenv "CONDA_PREFIX" nil)

  ;; Eshell
  (setf (default-value 'eshell-path-env) (getenv "PATH"))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eq major-mode 'eshell-mode)
                 (not (file-remote-p default-directory)))
        (setf eshell-path-env (getenv "PATH")))))

  (run-hooks 'conda-post-deactivate-hook)
  (when show-message
    (message "Conda environment deactivated")))

(defun conda-activate-default (&optional show-message)
  "Activate the default environment.
When SHOW-MESSAGE is non-nil, display helpful messages."
  (interactive (list t))
  (if conda-default-environment
      (conda-activate conda-default-environment show-message)
    (error "Default conda environment not set")))

;;; Flycheck integration

(declare-function flycheck-defined-checkers "ext:flycheck")
(declare-function flycheck-checker-get "ext:flycheck")
(declare-function flycheck-reset-enabled-checker "ext:flycheck")

(defun conda--reset-flycheck-enabled-checkers ()
  "Reset Flycheck enabled checker for all Python buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eq major-mode 'python-mode)
                 (bound-and-true-p flycheck-mode))
        (dolist (checker (flycheck-defined-checkers 'modes))
          (when (memq 'python-mode (flycheck-checker-get checker 'modes))
            (flycheck-reset-enabled-checker checker)))))))

(with-eval-after-load 'flycheck
  (add-hook 'conda-post-activate-hook
            #'conda--reset-flycheck-enabled-checkers)
  (add-hook 'conda-post-deactivate-hook
            #'conda--reset-flycheck-enabled-checkers))

(provide 'conda)
;;; conda.el ends here
