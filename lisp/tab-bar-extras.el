;;; tab-bar-extras.el --- Extra tab bar extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra extensions for the tab bar.

;;; Code:

(require 'tab-bar)
(require 'vc-hooks)
(require 'project)
(eval-when-compile
  (require 'subr-x))

;;; Face

(defface tab-bar-extras-frame-name
  '((t :inherit mode-line-buffer-id))
  "Face used by the frame name."
  :group 'tab-bar-faces)

(defface tab-bar-extras-project-root
  '((t :inherit vc-up-to-date-state))
  "Face used by the project root directory."
  :group 'tab-bar-faces)

;;; Tab bar item

;; Frame information
(defun tab-bar-extras--get-frame-info ()
  "Return frame information."
  (let* ((client (and (frame-parameter nil 'client) "@"))
         (raw (and (not (display-graphic-p))
                   (frame-parameter nil 'name)))
         (name (and raw (propertize raw 'face 'tab-bar-extras-frame-name))))
    (if (or client name)
        (concat " " client name " ")
      "")))

;; File information
(defvar-local tab-bar-extras--project-root nil
  "Project root directory for the current buffer.")
(put 'tab-bar-extras--project-root 'risky-local-variable t)

(defun tab-bar-extras--update-project-root ()
  "Update `tab-bar-extras--project-root'."
  (unless (file-remote-p default-directory)
    (if-let* ((project (project-current))
              (root (project-root project)))
        (setf tab-bar-extras--project-root
              (abbreviate-file-name (file-name-as-directory root)))
      (setf tab-bar-extras--project-root nil))
    (force-mode-line-update)))

(defun tab-bar-extras--get-file-info ()
  "Return file information."
  (let ((root tab-bar-extras--project-root))
    (concat " "
            (if (and buffer-file-name root)
                (concat (propertize root 'face 'tab-bar-extras-project-root)
                        (file-relative-name buffer-file-name root))
              (abbreviate-file-name (or buffer-file-name default-directory)))
            " ")))

;; Global information
(defun tab-bar-extras--get-global-info ()
  "Return global information."
  (let ((global (string-trim (format-mode-line global-mode-string))))
    (if (string-empty-p global)
        global
      (concat " " global " "))))

;; Tweak items
(defun tab-bar-extras--tweak-keymap (result)
  "Return a keymap for the tab bar based on RESULT."
  (let* ((frame (tab-bar-extras--get-frame-info))
         (file (tab-bar-extras--get-file-info))
         (global (tab-bar-extras--get-global-info))
         (width (+ (length frame) (length file) (length global)))
         (space `(space :align-to (- right ,width)))
         (padding (propertize " " 'display space)))
    `(,(car result)
      ,(cadr result)
      ,@(nthcdr (or (and tab-bar-history-mode 6) 2) result)
      (align-right menu-item ,padding ignore)
      (frame menu-item ,frame ignore :help "Frame information")
      (file menu-item ,file ignore :help "File information")
      (global menu-item ,global ignore :help "Global information"))))

;;; Entry point

(defun tab-bar-extras-setup ()
  "Setup extra extensions for the tab bar."
  (add-hook 'find-file-hook #'tab-bar-extras--update-project-root)
  (advice-add #'tab-bar-make-keymap-1 :filter-return
              #'tab-bar-extras--tweak-keymap))

(provide 'tab-bar-extras)
;;; tab-bar-extras.el ends here
