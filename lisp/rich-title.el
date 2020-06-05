;;; rich-title.el --- Informative frame title  -*- lexical-binding: t -*-

;;; Commentary:

;; An informative frame title format.

;;; Code:

;;; File information

(defun rich-title--get-file-name ()
  "Return the file name or the default directory."
  (abbreviate-file-name (or buffer-file-name default-directory)))

;;; Project information

(declare-function projectile-project-name "ext:projectile")
(declare-function projectile-project-type "ext:projectile")

(defvar-local rich-title--project-name nil "Current project name.")

(defun rich-title--get-project-name ()
  "Return the project name."
  (let ((name (projectile-project-name))
        (type (projectile-project-type)))
    (and name
         (not (equal "-" name))
         (concat " ["
                 name
                 (and type (format ":%s" type))
                 "]"))))

(defun rich-title--set-project-name ()
  "Set project name and update frame title."
  (when (bound-and-true-p projectile-mode)
    (setf rich-title--project-name (rich-title--get-project-name))
    (force-mode-line-update)))

(defun rich-title--setup-project ()
  "Setup project system."
  (add-hook 'find-file-hook #'rich-title--set-project-name))

;;; Org clock

(defvar org-clock-effort)
(defvar org-clock-task-overrun)
(defvar org-clock-task-overrun-text)
(defvar org-clock-heading)
(defvar org-mode-line-string)
(defvar org-clock-string-limit)
(declare-function org-clock-get-clocked-time "org-clock")
(declare-function org-clock-notify-once-if-expired "org-clock")
(declare-function org-clock-get-clock-string "org-clock")
(declare-function org-clock--mode-line-heading "org-clock")

(defun rich-title--get-org-clock-string ()
  "Form a clock string that will be shown in the frame title.

This is a customized version of `org-clock-get-clock-string'."
  (let* ((clocked-time (org-clock-get-clocked-time))
         (clock-string
          (concat "["
                  (org-duration-from-minutes clocked-time)
                  (and org-clock-effort
                       (concat
                        "/"
                        (org-duration-from-minutes
                         (org-duration-to-minutes org-clock-effort))))
                  (and org-clock-task-overrun org-clock-task-overrun-text)
                  "] "
                  org-clock-heading)))
    (if (and (> org-clock-string-limit 0)
             (> (length clock-string) org-clock-string-limit))
        (substring clock-string 0 org-clock-string-limit)
      clock-string)))

(defun rich-title--update-org-clock-mode-line (&optional refresh)
  "Update frame title with clock information.
When REFRESH is non-nil, refresh the cached heading first.

This is a customized version of `org-clock-update-mode-line'."
  (if org-clock-effort
      (org-clock-notify-once-if-expired)
    (setf org-clock-task-overrun nil))
  (when refresh
    (setf org-clock-heading (org-clock--mode-line-heading)))
  (setf org-mode-line-string (org-clock-get-clock-string))
  (force-mode-line-update))

(defun rich-title--setup-org-clock ()
  "Setup Org clock."
  (with-eval-after-load 'org-clock
    (setf (symbol-function 'org-clock-get-clock-string)
          #'rich-title--get-org-clock-string
          (symbol-function 'org-clock-update-mode-line)
          #'rich-title--update-org-clock-mode-line)))

;;; Entry point

(defvar org-clock-frame-title-format)

(defconst rich-title--base-format
  '(""
    invocation-name
    "  "
    (:eval (rich-title--get-file-name))
    rich-title--project-name)
  "Base frame title format.")

(defconst rich-title--format-with-clock
  `(,@rich-title--base-format "  " org-mode-line-string)
  "Frame title format with Org clock string.")

(defun rich-title-setup ()
  "Setup frame title."
  (rich-title--setup-project)
  (rich-title--setup-org-clock)

  (setf frame-title-format rich-title--base-format)
  (with-eval-after-load 'org-clock
    (setf org-clock-frame-title-format rich-title--format-with-clock)))

(provide 'rich-title)
;;; rich-title.el ends here
