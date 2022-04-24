;;; conda.el --- Integration with conda  -*- lexical-binding: t -*-

;;; Commentary:

;; Simple integration with the conda environment system.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'let-alist))

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
              (let ((environments nil))
                (dolist (file (directory-files directory t "^[^.]"))
                  (when (file-directory-p file)
                    (push (file-name-nondirectory file) environments)))
                (nreverse environments))))
          conda-environment-directories))

(defun conda--get-environment-root (environment)
  "Get the root of ENVIRONMENT."
  (catch 'done
    (dolist (directory conda-environment-directories)
      (let ((filename (expand-file-name environment directory)))
        (when (file-directory-p filename)
          (throw 'done filename))))))

(defun conda--get-environment-spec (environment)
  "Get the spec of ENVIRONMENT."
  (when-let* ((root (conda--get-environment-root environment))
              (filename (expand-file-name "bin" root))
              (bin (and (file-directory-p filename) filename)))
    `((name . ,(substring-no-properties environment))
      (root . ,root)
      (bin . ,bin))))

;;; Activate and deactivate environments

(defvar python-shell-virtualenv-root)
(defvar eshell-path-env)

(defvar conda--current-environment-spec nil
  "Spec of the current environment.")
(put 'conda--current-environment-spec 'risky-local-variable t)

(defun conda-get-current-environment ()
  "Return the name of the current environment."
  (and (not (file-remote-p default-directory))
       (cdr (assq 'name conda--current-environment-spec))))

(defun conda--get-path-directory-name (directory)
  "Get the name of DIRECTORY suitable for search path."
  (and directory (directory-file-name directory)))

(defun conda--path-add (directory search-path)
  "Return a search path with DIRECTORY in SEARCH-PATH."
  (let ((target (file-name-as-directory directory))
        (directories (parse-colon-path search-path)))
    (cl-pushnew target directories :test #'equal)
    (mapconcat #'conda--get-path-directory-name
               directories path-separator)))

(defun conda--path-remove (directory search-path)
  "Return a search path with DIRECTORY not in SEARCH-PATH."
  (let ((target (file-name-as-directory directory))
        (directories (parse-colon-path search-path)))
    (mapconcat #'conda--get-path-directory-name
               (delete target directories) path-separator)))

(defun conda--prevent-local-virtualenv ()
  "Prevent local virtualenv when appropriate."
  (when (and conda--current-environment-spec
             (not (file-remote-p default-directory)))
    (kill-local-variable 'python-shell-virtualenv-root)))

(defun conda--unset-local-virtualenv ()
  "Unset virtualenv locally when appropriate."
  (when (and conda--current-environment-spec
             (file-remote-p default-directory)
             (not (local-variable-p 'python-shell-virtualenv-root)))
    (make-local-variable 'python-shell-virtualenv-root)
    (setf python-shell-virtualenv-root nil)))

(defun conda-activate (environment &optional show-message)
  "Activate the environment named ENVIRONMENT.
When SHOW-MESSAGE is non-nil, display helpful messages."
  (interactive
   (list (and (not (file-remote-p default-directory))
              (memq system-type '(gnu/linux darwin))
              (completing-read "Environment: " (conda--get-environments)
                               nil t))
         t))
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (unless (memq system-type '(gnu/linux darwin))
    (user-error "Current operating system not supported"))
  (when (string-empty-p (or environment ""))
    (user-error "Conda environment not specified"))

  (let ((spec (conda--get-environment-spec environment)))
    (unless spec
      (error "%s is not a valid conda environment" environment))
    (when conda--current-environment-spec
      (conda-deactivate show-message))
    (setf conda--current-environment-spec spec)
    (run-hooks 'conda-pre-activate-hook)

    (let-alist spec
      (with-temp-buffer
        (cl-pushnew .bin exec-path :test #'equal)
        (setenv "PATH" (conda--path-add .bin (getenv "PATH")))
        (setenv "VIRTUAL_ENV" .root)
        (setenv "CONDA_PREFIX" .root)
        (setf (default-value 'eshell-path-env) (getenv "PATH")
              python-shell-virtualenv-root (file-name-as-directory .root)))

      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (local-variable-p 'exec-path)
            (cl-pushnew .bin exec-path :test #'equal))
          (when (local-variable-p 'process-environment)
            (setenv "PATH" (conda--path-add .bin (getenv "PATH")))
            (setenv "VIRTUAL_ENV" .root)
            (setenv "CONDA_PREFIX" .root))
          (when (and (eq major-mode 'eshell-mode)
                     (not (file-remote-p default-directory)))
            (setf eshell-path-env (getenv "PATH")))
          (conda--prevent-local-virtualenv)
          (when (derived-mode-p 'org-mode 'python-mode)
            (conda--unset-local-virtualenv)))))

    (run-hooks 'conda-post-activate-hook)
    (when show-message
      (message "Conda environment %s activated"
               (or (cdr (assq 'name spec)) "unknown")))))

(defun conda-deactivate (&optional show-message)
  "Deactivate the current environment.
When SHOW-MESSAGE is non-nil, display helpful messages."
  (interactive (list t))
  (when (file-remote-p default-directory)
    (user-error "Remote hosts not supported"))
  (unless (memq system-type '(gnu/linux darwin))
    (user-error "Current operating system not supported"))
  (unless conda--current-environment-spec
    (user-error "No current conda environment"))

  (let-alist conda--current-environment-spec
    (setf conda--current-environment-spec nil)
    (run-hooks 'conda-pre-deactivate-hook)

    (with-temp-buffer
      (setf exec-path (delete .bin exec-path))
      (setenv "PATH" (conda--path-remove .bin (getenv "PATH")))
      (setenv "VIRTUAL_ENV" nil)
      (setenv "CONDA_PREFIX" nil)
      (setf (default-value 'eshell-path-env) (getenv "PATH")
            python-shell-virtualenv-root nil))

    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (local-variable-p 'exec-path)
          (setf exec-path (delete .bin exec-path)))
        (when (local-variable-p 'process-environment)
          (setenv "PATH" (conda--path-remove .bin (getenv "PATH")))
          (setenv "VIRTUAL_ENV" nil)
          (setenv "CONDA_PREFIX" nil))
        (when (and (eq major-mode 'eshell-mode)
                   (not (file-remote-p default-directory)))
          (setf eshell-path-env (getenv "PATH")))))

    (run-hooks 'conda-post-deactivate-hook)
    (when show-message
      (message "Conda environment %s deactivated" (or .name "unknown")))))

(defun conda-activate-default (&optional show-message)
  "Activate the default environment.
When SHOW-MESSAGE is non-nil, display helpful messages."
  (interactive (list t))
  (if conda-default-environment
      (conda-activate conda-default-environment show-message)
    (error "Default conda environment not set")))

;;; Basic integration

(add-hook 'hack-local-variables-hook #'conda--prevent-local-virtualenv)
(with-eval-after-load 'python
  (add-hook 'python-mode-hook #'conda--unset-local-virtualenv))
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'conda--unset-local-virtualenv))

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
