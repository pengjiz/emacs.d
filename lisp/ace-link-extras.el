;;; ace-link-extras.el --- Extra ace-link extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra extensions for ace-link.

;;; Code:

(require 'ace-link)
(eval-when-compile
  (require 'subr-x))

;;; Go to a generic address

(defun ace-link-extras--goto-addr (pos)
  "Open the generic address at POS."
  (when (number-or-marker-p pos)
    (goto-char pos)
    (if-let* ((reference-url
               (catch 'done
                 (dolist (overlay (overlays-at (point)))
                   (when-let* ((url (overlay-get overlay 'bug-reference-url)))
                     (throw 'done url))))))
        (browse-url reference-url)
      (goto-address-at-point))))

(defun ace-link-extras--collect-addr ()
  "Collect generic addresses."
  (let (candidates)
    (dolist (overlay (overlays-in (window-start) (window-end)))
      (when (or (overlay-get overlay 'goto-address)
                (overlay-get overlay 'bug-reference-url))
        (push (overlay-start overlay) candidates)))
    (nreverse candidates)))

;;; Entry point

(defun ace-link-extras-setup ()
  "Setup ace-link extensions."
  (setf (symbol-function 'ace-link--addr-action)
        #'ace-link-extras--goto-addr
        (symbol-function 'ace-link--addr-collect)
        #'ace-link-extras--collect-addr))

(provide 'ace-link-extras)
;;; ace-link-extras.el ends here
