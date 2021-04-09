;;; rich-title.el --- Informative frame title  -*- lexical-binding: t -*-

;;; Commentary:

;; An informative frame title configuration.

;;; Code:

(require 'tab-bar)
(require 'project)
(eval-when-compile
  (require 'subr-x))

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

(defvar-local rich-title--project nil "Current project information.")

(defun rich-title--update-project ()
  "Update `rich-title--project'."
  (unless (file-remote-p default-directory)
    (if-let* ((project (project-current))
              (root (file-name-directory (project-root project)))
              (name (file-name-nondirectory (directory-file-name root))))
        (setf rich-title--project (format " [%s]" name))
      (setf rich-title--project nil))
    (force-mode-line-update)))

(defun rich-title--setup-project ()
  "Setup project system."
  (add-hook 'find-file-hook #'rich-title--update-project))

;;; Org clock

(defvar org-mode-line-string)
(declare-function org-clock-get-clock-string "org-clock")
(declare-function org-clock-update-mode-line "org-clock")

(defvar rich-title--org-clock nil "Current Org clock information.")

(defun rich-title--tweak-org-clock-string (result)
  "Return Org clock string appropriately based on RESULT."
  (when-let* ((index (string-match-p "(" result)))
    (concat (substring result 1 index)
            (substring result (1+ index) -1))))

(defun rich-title--update-org-clock (&rest _)
  "Update `rich-title--org-clock'."
  (setf rich-title--org-clock
        (substring-no-properties org-mode-line-string))
  (force-mode-line-update))

(defun rich-title--setup-org-clock ()
  "Setup Org clock."
  (with-eval-after-load 'org-clock
    (advice-add #'org-clock-get-clock-string :filter-return
                #'rich-title--tweak-org-clock-string)
    (advice-add #'org-clock-update-mode-line :after
                #'rich-title--update-org-clock)))

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
  `(,@rich-title--base-format
    " ("
    rich-title--org-clock
    ")")
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
