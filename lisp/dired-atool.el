;;; dired-atool.el --- Dired integration for atool -*- lexical-binding: t -*-

;;; Commentary:

;; Dired integration for atool to pack or unpack files easily.

;;; Code:

(require 'dired)
(require 'dired-aux)
(eval-when-compile
  (require 'subr-x))

(defgroup dired-atool nil
  "Dired integration for atool."
  :group 'dired)

(defcustom dired-atool-aunpack-program
  "aunpack"
  "The program name for aunpack."
  :type 'string
  :group 'dired-atool)

(defcustom dired-atool-aunpack-extra-options
  '("--explain")
  "Extra options passing to aunpack."
  :type '(repeat string)
  :group 'dired-atool)

(defcustom dired-atool-apack-program
  "apack"
  "The program name for apack."
  :type 'string
  :group 'dired-atool)

(defcustom dired-atool-apack-extra-options
  '("--explain")
  "Extra options passing to apack."
  :type '(repeat string)
  :group 'dired-atool)

(defcustom dired-atool-using-trash
  t
  "Whether to use trash.
Non-nil means following `delete-by-moving-to-trash' when deleting
files or directories."
  :type 'boolean
  :group 'dired-atool)

;;; Run program

(defvar dired-atool--process-buffer-name "*dired-atool*")

(defun dired-atool--sentinel (process event)
  "Report status of PROCESS based on EVENT."
  (when (memq (process-status process) '(exit signal))
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
         (mapcar (lambda (x) (if (listp x) x (list x)))
                 args)))

(defun dired-atool--run (program &rest args)
  "Start PROGRAM with ARGS."
  (let* ((flat-args (dired-atool--flatten args))
         (process (apply #'start-process
                         (file-name-nondirectory program)
                         dired-atool--process-buffer-name
                         program
                         flat-args)))
    (set-process-sentinel process #'dired-atool--sentinel)))

;;; Unpack files

(defun dired-atool-do-unpack (&optional arg)
  "Unpack files with aunpack in Dired.
ARG is directly passed to `dired-get-marked-files'."
  (interactive "P")
  (let* ((files (dired-get-marked-files nil arg))
         (destination (read-directory-name "Unpack to: "
                                           (dired-dwim-target-directory))))
    (when (or (file-remote-p default-directory)
              (file-remote-p destination))
      (user-error "Remote hosts not supported"))
    (if (file-exists-p destination)
        ;; If the destination directory exists, we directly unpack to it and let
        ;; atool to decide if a new directory is needed.
        (let ((default-directory (expand-file-name destination)))
          (dired-atool--run dired-atool-aunpack-program
                            "--each"
                            dired-atool-aunpack-extra-options
                            files))
      ;; If the destination does not exist, we make it and extract files to it.
      (make-directory destination t)
      (dired-atool--run dired-atool-aunpack-program
                        (concat "--extract-to=" destination)
                        "--each"
                        dired-atool-aunpack-extra-options
                        files))))

;;; Pack files

(defun dired-atool-do-pack (&optional arg)
  "Pack files with apack in Dired.
ARG is directly passed to `dired-get-marked-files'."
  (interactive "P")
  (let* ((files (dired-get-marked-files t arg))
         (destination (read-file-name "Pack to: "
                                      (dired-dwim-target-directory))))
    (when (or (file-remote-p default-directory)
              (file-remote-p destination))
      (user-error "Remote hosts not supported"))
    (make-directory (file-name-directory destination) t)
    (when (and (file-exists-p destination)
               (yes-or-no-p (format "File %s exists. Remove it before packing? "
                                    destination)))
      (delete-file destination dired-atool-using-trash))
    (dired-atool--run dired-atool-apack-program
                      dired-atool-apack-extra-options
                      destination
                      files)))

(provide 'dired-atool)
;;; dired-atool.el ends here
