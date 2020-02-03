;;; erc-extras.el --- Extra ERC extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra extensions for ERC.

;;; Code:

(require 'erc)
(require 'erc-services)
(eval-when-compile
  (require 'subr-x))

;;; NickServ identification

(defun erc-extras--search-nickserv-auth-source-password (nickname)
  "Search NickServ password for NICKNAME with auth-source."
  (when-let* ((entry (car (auth-source-search :max 1
                                              :host erc-session-server
                                              :port "nickserv"
                                              :user nickname
                                              :require '(:secret))))
              (secret (plist-get entry :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun erc-extras--search-nickserv-variable-password (nickname)
  "Search NickServ password for NICKNAME in `erc-nickserv-passwords'."
  (when erc-nickserv-passwords
    (cdr (assoc nickname
                (cadr (assq (erc-network) erc-nickserv-passwords))))))

(defun erc-extras--call-nickserv-identify-function (nickname)
  "Call function to identify NICKNAME to NickServ."
  (let ((password (and erc-prompt-for-nickserv-password
                       (read-passwd (format "NickServ password for %s on %s: "
                                            nickname
                                            (or (erc-network)
                                                "unknown network"))))))
    (unless (and password
                 (not (equal password "")))
      (setf password
            (or (erc-extras--search-nickserv-variable-password nickname)
                (erc-extras--search-nickserv-auth-source-password nickname))))
    (erc-nickserv-identify password)))

;;; Setup

(defun erc-extras-setup ()
  "Setup ERC extensions."
  (setf (symbol-function 'erc-nickserv-call-identify-function)
        #'erc-extras--call-nickserv-identify-function))

(provide 'erc-extras)
;;; erc-extras.el ends here
