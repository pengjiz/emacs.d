;;; erc-extras.el --- Extra ERC extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra extensions for ERC.

;;; Code:

(require 'erc)
(require 'erc-services)
(require 'erc-desktop-notifications)
(eval-when-compile
  (require 'subr-x))

;;; Option

(defcustom erc-extras-hide-notification-messages
  t
  "Whether to hide messages in desktop notifications."
  :type 'boolean
  :group 'erc-notifications)

;;; NickServ identification

(defun erc-extras--normalize-nickserv-password (password)
  "Normalize PASSWORD for NickServ identification."
  (and (not (string-empty-p password))
       password))

(defun erc-extras--get-nickserv-password (nickname)
  "Return NickServ password for NICKNAME."
  (let ((network (erc-network))
        (server (erc-with-server-buffer erc-session-server)))
    (erc-extras--normalize-nickserv-password
     (or (and erc-nickserv-passwords
              (cdr (assoc nickname
                          (cadr (assq network
                                      erc-nickserv-passwords)))))
         (and erc-use-auth-source-for-nickserv-password
              (auth-source-pick-first-password :host server
                                               :port "nickserv"
                                               :user nickname
                                               :require '(:secret)))
         (and erc-prompt-for-nickserv-password
              (read-passwd (format "NickServ password for %s on %s: "
                                   nickname network)))))))

;;; Hide notification messages

(defun erc-extras--adjust-notification-message (args)
  "Adjust notification message appropriately based on ARGS."
  (let ((nickname (nth 0 args))
        (private (nth 2 args)))
    (cond ((and erc-extras-hide-notification-messages private)
           (list nickname "Sent you a message" private))
          (erc-extras-hide-notification-messages
           (list nickname "Mentioned you" private))
          (t args))))

;;; Entry point

(defun erc-extras-setup ()
  "Setup ERC extensions."
  (setf (symbol-function 'erc-nickserv-get-password)
        #'erc-extras--get-nickserv-password)
  (advice-add 'erc-notifications-notify :filter-args
              #'erc-extras--adjust-notification-message))

(provide 'erc-extras)
;;; erc-extras.el ends here
