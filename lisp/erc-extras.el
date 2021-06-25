;;; erc-extras.el --- Extra ERC extensions  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra extensions for ERC.

;;; Code:

(require 'erc)
(require 'erc-services)

;;; NickServ identification

(defun erc-extras--search-nickserv-password (nickname)
  "Search NickServ password for NICKNAME."
  (or (and erc-nickserv-passwords
           (cdr (assoc nickname
                       (cadr (assq (erc-network)
                                   erc-nickserv-passwords)))))
      (let ((server (erc-with-server-buffer erc-session-server)))
        (auth-source-pick-first-password :host server
                                         :port "nickserv"
                                         :user nickname
                                         :require '(:secret)))))

;; HACK: ERC will not identify to NickServ if the both variables for passwords
;; are nil. So here we use a dummy entry to force identification because we also
;; query auth-source for passwords.
(defun erc-extras--force-nickserv-identification (fn &rest args)
  "Apply FN on ARGS, but force NickServ identification."
  (let ((erc-nickserv-passwords (or erc-nickserv-passwords '((dummy)))))
    (apply fn args)))

;;; Entry point

(defun erc-extras-setup ()
  "Setup ERC extensions."
  (setf (symbol-function 'erc-nickserv-get-password)
        #'erc-extras--search-nickserv-password)
  (advice-add #'erc-nickserv-identify-autodetect :around
              #'erc-extras--force-nickserv-identification)
  (advice-add #'erc-nickserv-identify-on-connect :around
              #'erc-extras--force-nickserv-identification)
  (advice-add #'erc-nickserv-identify-on-nick-change :around
              #'erc-extras--force-nickserv-identification))

(provide 'erc-extras)
;;; erc-extras.el ends here
