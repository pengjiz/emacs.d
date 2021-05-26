;;; erc-extras.el --- Extra ERC extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra extensions for ERC.

;;; Code:

(require 'erc)
(require 'erc-services)
(eval-when-compile
  (require 'subr-x))

;;; NickServ identification

(defvar erc-extras--nickserv-read-password erc-prompt-for-nickserv-password
  "Whether to ask for NickServ password.")

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
  (let ((password (and erc-extras--nickserv-read-password
                       (read-passwd (format "NickServ password for %s on %s: "
                                            nickname
                                            (or (erc-network)
                                                "unknown network"))))))
    (when (or (not password) (string-empty-p password))
      (setf password
            (or (erc-extras--search-nickserv-variable-password nickname)
                (erc-extras--search-nickserv-auth-source-password nickname))))
    (erc-nickserv-identify password)))

;; NOTE: By default ERC will not identify nickname if both variables for
;; prompting password and nickserv passwords are nil. Here we locally bind it to
;; t to force identification.
(defun erc-extras--force-nickserv-identification (fn &rest args)
  "Apply FN on ARGS, but force NickServ identification."
  (let ((erc-extras--nickserv-read-password erc-prompt-for-nickserv-password)
        (erc-prompt-for-nickserv-password t))
    (apply fn args)))

;;; Entry point

(defun erc-extras-setup ()
  "Setup ERC extensions."
  (setf (symbol-function 'erc-nickserv-call-identify-function)
        #'erc-extras--call-nickserv-identify-function)
  (advice-add #'erc-nickserv-identify-autodetect :around
              #'erc-extras--force-nickserv-identification)
  (advice-add #'erc-nickserv-identify-on-connect :around
              #'erc-extras--force-nickserv-identification)
  (advice-add #'erc-nickserv-identify-on-nick-change :around
              #'erc-extras--force-nickserv-identification))

(provide 'erc-extras)
;;; erc-extras.el ends here
