;;; appt-extras.el --- Extra appointment utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; Extra appointment utilities.

;;; Code:

(require 'appt)
(require 'notifications nil t)

;;; Option

(defcustom appt-extras-include-org-agenda
  t
  "Whether to include appointments from Org agenda files."
  :type 'boolean
  :group 'appt)

(defcustom appt-extras-org-agenda-update-interval
  60
  "Interval in minutes for updating Org agenda appointments."
  :type '(choice integer
                 (const :tag "Never" nil))
  :group 'appt)

(defcustom appt-extras-activate-on-setup
  t
  "Whether to activate appointment checking system on setup."
  :type 'boolean
  :group 'appt)

;;; Notify

(defun appt-extras--notify (title body)
  "Show notification with TITLE and BODY text."
  (if (fboundp 'notifications-notify)
      (notifications-notify :title title :body body)
    (message "%s: %s" title body)))

;;; Appointment

(defun appt-extras--display-appointment (time _ message)
  "Display MESSAGE for appointment due in TIME minutes."
  (let ((template "%s [in %s min]"))
    (if (listp message)
        (dotimes (index (length message))
          (appt-extras--notify "Appt" (format template
                                              (nth index message)
                                              (nth index time))))
      (appt-extras--notify "Appt" (format template message time)))))

(defun appt-extras--update-org-agenda ()
  "Update appointments from Org agenda files."
  (when appt-timer
    ;; Keep only appointments from the diary file
    (let (appointments)
      (dolist (appointment appt-time-msg-list)
        (unless (nth 2 appointment)
          (push appointment appointments)))
      (setf appt-time-msg-list (nreverse appointments)))
    (org-agenda-to-appt)))

(defun appt-extras-activate ()
  "Activate appointment checking system."
  (interactive)
  (unless appt-timer
    (appt-activate 1))
  (with-eval-after-load 'org
    (when appt-extras-include-org-agenda
      (let ((interval (and appt-extras-org-agenda-update-interval
                           (* 60 appt-extras-org-agenda-update-interval))))
        (run-at-time 1 interval #'appt-extras--update-org-agenda)))))

;;; Set reminder

(defun appt-extras-set-reminder (time message)
  "Show MESSAGE at TIME.
TIME is directly passed to `run-at-time'."
  (interactive (list (read-string "Remind at time: ")
                     (read-string "Message: ")))
  (run-at-time time nil #'appt-extras--notify "Reminder" message))

;;; Entry point

(defun appt-extras-setup ()
  "Setup appointment extensions."
  (setf appt-display-format 'window
        appt-disp-window-function #'appt-extras--display-appointment
        appt-delete-window-function #'ignore)
  (when appt-extras-activate-on-setup
    (appt-extras-activate)))

(provide 'appt-extras)
;;; appt-extras.el ends here
