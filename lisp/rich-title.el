;;; rich-title.el --- Informative frame title  -*- lexical-binding: t -*-

;;; Commentary:

;; An informative frame title configuration.

;;; Code:

(require 'tab-bar)

;;; Tab information

(defun rich-title--get-tab-info ()
  "Return tab information of the current frame."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (num-tabs (length tabs))
         (tab (tab-bar--current-tab))
         (index (1+ (tab-bar--current-tab-index tabs))))
    (format " [%s]"
            (if (cdr (assq 'explicit-name tab))
                (format "%s/%s:%s" index num-tabs (cdr (assq 'name tab)))
              (format "%s/%s" index num-tabs)))))

;;; File information

(defun rich-title--get-file-info ()
  "Return basic file information."
  (abbreviate-file-name (or buffer-file-name default-directory)))

;;; Project

(declare-function projectile-project-name "ext:projectile")
(declare-function projectile-project-type "ext:projectile")

(defvar-local rich-title--project nil "Current project information.")

(defun rich-title--update-project ()
  "Update `rich-title--project' and frame title."
  (when (and (bound-and-true-p projectile-mode)
             (not (file-remote-p default-directory)))
    (let ((name (projectile-project-name))
          (type (projectile-project-type)))
      (setf rich-title--project
            (and name
                 (not (equal name "-"))
                 (format " [%s]"
                         (if type
                             (format "%s:%s" name type)
                           name)))))
    (force-mode-line-update)))

(defun rich-title--setup-project ()
  "Setup project system."
  (add-hook 'find-file-hook #'rich-title--update-project))

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
  "Return Org clock string that will be shown in the frame title.

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
  "Update `org-mode-line-string' with Org clock and frame title.
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
  '("GNU Emacs"
    (:eval (rich-title--get-tab-info))
    " - "
    (:eval (rich-title--get-file-info))
    rich-title--project)
  "Base frame title format.")

(defconst rich-title--org-clock-format
  `(,@rich-title--base-format "  " org-mode-line-string)
  "Frame title format with Org clock.")

(defun rich-title-setup ()
  "Setup frame title."
  (rich-title--setup-project)
  (rich-title--setup-org-clock)

  (setf frame-title-format rich-title--base-format)
  (with-eval-after-load 'org-clock
    (setf org-clock-frame-title-format rich-title--org-clock-format)))

(provide 'rich-title)
;;; rich-title.el ends here
