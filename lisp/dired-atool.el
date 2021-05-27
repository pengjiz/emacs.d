;;; dired-atool.el --- Dired integration for atool  -*- lexical-binding: t -*-

;;; Commentary:

;; Dired integration for atool to pack or unpack files easily.

;;; Code:

(require 'dired)
(require 'dired-aux)
(eval-when-compile
  (require 'subr-x))

;;; Option

(defgroup dired-atool nil
  "Dired integration for atool."
  :group 'dired)

(defcustom dired-atool-aunpack-program
  "aunpack"
  "The program name for aunpack."
  :type 'string)

(defcustom dired-atool-aunpack-extra-options
  '("--explain")
  "Extra options passing to aunpack."
  :type '(repeat string))

(defcustom dired-atool-apack-program
  "apack"
  "The program name for apack."
  :type 'string)

(defcustom dired-atool-apack-extra-options
  '("--explain")
  "Extra options passing to apack."
  :type '(repeat string))

(defcustom dired-atool-use-trash
  t
  "Whether to use trash.
Non-nil means following `delete-by-moving-to-trash' when deleting
files or directories."
  :type 'boolean)

(defcustom dired-atool-do-revert
  t
  "Whether to automatically revert Dired buffers.
Non-nil means reverting all Dired buffers related to the
destination directory automatically after operations."
  :type 'boolean)

;;; Run program

(defconst dired-atool--process-buffer-name "*dired-atool*"
  "Buffer name for atool process.")

(defun dired-atool--sentinel (process event)
  "Perform actions for PROCESS based on EVENT."
  (when (memq (process-status process) '(exit signal))
    (when-let* ((destination (and dired-atool-do-revert
                                  (process-get process 'destination))))
      (dired-fun-in-all-buffers destination nil #'revert-buffer))
    (if (string-match-p "finished" event)
        (message "%s finished" (process-name process))
      (message "%s exited abnormally" (process-name process)))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert (format "%s exited with code %d at %s\n"
                      (string-join (process-command process) " ")
                      (process-exit-status process)
                      (current-time-string))))))

(defsubst dired-atool--flatten (args)
  "Flatten ARGS by one level."
  (apply #'append
         (mapcar (lambda (arg) (if (listp arg)
                                   arg
                                 (list arg)))
                 args)))

(defun dired-atool--run (destination program &rest args)
  "Start PROGRAM with ARGS.
DESTINATION is the destination directory for which all relevant
Dired buffers may be reverted when the process exits."
  (let* ((flat-args (dired-atool--flatten args))
         (process (apply #'start-process
                         (file-name-nondirectory program)
                         dired-atool--process-buffer-name
                         program
                         flat-args)))
    (process-put process 'destination destination)
    (set-process-sentinel process #'dired-atool--sentinel)))

;;; Unpack files

(defun dired-atool-do-unpack (&optional arg)
  "Unpack files with aunpack in Dired.
ARG is directly passed to `dired-get-marked-files'."
  (interactive "P")
  (let* ((archives (dired-get-marked-files nil arg nil nil t))
         (files (mapcar #'dired-make-relative archives))
         (prompt (format "Unpack %s to: " (dired-mark-prompt arg files)))
         (directory (dired-dwim-target-directory))
         (destination (expand-file-name (dired-mark-pop-up nil 'uncompress files
                                                           #'read-directory-name
                                                           prompt directory))))
    (when (or (file-remote-p default-directory)
              (file-remote-p destination))
      (user-error "Remote hosts not supported"))
    (unless (file-exists-p destination)
      (dired-create-directory destination))
    (let ((default-directory destination))
      (dired-atool--run destination
                        dired-atool-aunpack-program
                        "--each"
                        dired-atool-aunpack-extra-options
                        archives))))

;;; Pack files

(defun dired-atool-do-pack (&optional arg)
  "Pack files with apack in Dired.
ARG is directly passed to `dired-get-marked-files'."
  (interactive "P")
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (prompt (format "Pack %s to: " (dired-mark-prompt arg files)))
         (directory (dired-dwim-target-directory))
         (target (expand-file-name (dired-mark-pop-up nil 'compress files
                                                      #'read-file-name
                                                      prompt directory)))
         (destination (file-name-directory target)))
    (when (or (file-remote-p default-directory)
              (file-remote-p destination))
      (user-error "Remote hosts not supported"))
    (unless (file-exists-p destination)
      (dired-create-directory destination))
    (when (and (file-exists-p target)
               (yes-or-no-p (format "File %s exists. %s it? "
                                    target
                                    (if (and dired-atool-use-trash
                                             delete-by-moving-to-trash)
                                        "Trash"
                                      "Delete"))))
      (delete-file target dired-atool-use-trash))
    (dired-atool--run destination
                      dired-atool-apack-program
                      dired-atool-apack-extra-options
                      target
                      files)))

(provide 'dired-atool)
;;; dired-atool.el ends here
