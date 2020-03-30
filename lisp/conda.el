;;; conda.el --- Integration with Conda  -*- lexical-binding: t -*-

;;; Commentary:

;; Simple integration with the Python Conda tool.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;; Option

(defgroup conda nil
  "Integration with Conda."
  :group 'python)

(defcustom conda-environment-directories
  (list
   (expand-file-name (convert-standard-filename ".conda/envs/") "~"))
  "A list of directories of the Conda environments."
  :type '(repeat directory))

(defcustom conda-default-environment
  nil
  "The default environment for a project."
  :type 'string
  :safe #'stringp)
(make-variable-buffer-local 'conda-default-environment)

(defcustom conda-pre-activate-hook
  nil
  "The hook run before activating an environment."
  :type 'hook)

(defcustom conda-post-activate-hook
  nil
  "The hook run after activating an environment."
  :type 'hook)

(defcustom conda-pre-deactivate-hook
  nil
  "The hook run before deactivating an environment."
  :type 'hook)

(defcustom conda-post-deactivate-hook
  nil
  "The hook run after deactivating an environment."
  :type 'hook)

;;; Find environments

(defun conda--get-environments ()
  "Get all Conda environments."
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
  "Previous value of the `exec-path' variable.")

(defun conda-activate (environment &optional show-message)
  "Activate Conda ENVIRONMENT.
When SHOW-MESSAGE is non-nil, display helpful messages."
  (interactive
   (list (completing-read "Environment: "
                          (conda--get-environments)
                          nil t)
         (prefix-numeric-value current-prefix-arg)))
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (let* ((path (conda--get-environment-path environment))
         ;; NOTE: This may not work on Windows
         (bin-path (expand-file-name "bin" path)))
    ;; Error when the environment is not found
    (unless path
      (error "%s is not a valid Conda environment" environment))

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
    ;;
    ;; NOTE: This will update all Eshell buffers, which is what I usually want.
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
  (interactive "p")
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
  ;;
  ;; NOTE: This will update all Eshell buffers, which is what I usually
  ;; want.
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
  (interactive "p")
  (if conda-default-environment
      (conda-activate conda-default-environment show-message)
    (error "Default environment not set")))

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
  (add-hook 'conda-post-activate-hook #'conda--reset-flycheck-enabled-checkers)
  (add-hook 'conda-post-deactivate-hook #'conda--reset-flycheck-enabled-checkers))

(provide 'conda)
;;; conda.el ends here
