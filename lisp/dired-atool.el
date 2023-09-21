;;; dired-atool.el --- Dired integration for atool  -*- lexical-binding: t -*-

;;; Commentary:

;; Dired integration for atool to pack or unpack files easily.

;;; Code:

(require 'dired)
(require 'dired-aux)
(eval-when-compile
  (require 'let-alist))

;;; Option

(defgroup dired-atool nil
  "Dired integration for atool."
  :group 'dired)

(defcustom dired-atool-aunpack-program
  "aunpack"
  "Name of the aunpack program."
  :type 'string)

(defcustom dired-atool-aunpack-extra-options
  '("--explain")
  "Extra options passed to aunpack."
  :type '(repeat string))

(defcustom dired-atool-apack-program
  "apack"
  "Name of the apack program."
  :type 'string)

(defcustom dired-atool-apack-extra-options
  '("--explain")
  "Extra options passed to apack."
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

(defun dired-atool--sentinel (process event)
  "Perform actions for PROCESS based on EVENT."
  (when (memq (process-status process) '(exit signal))
    (let-alist (process-get process 'operation)
      (when (and dired-atool-do-revert .destination)
        (dired-fun-in-all-buffers .destination nil #'revert-buffer))
      (let ((buffer (process-buffer process))
            (inhibit-read-only t))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert (format "%s: process exited with code %d [%s]\n\n"
                          (capitalize (or .name "unknown"))
                          (process-exit-status process)
                          (format-time-string "%FT%T%z"))))
        (with-current-buffer (get-buffer-create "*dired-atool*")
          (goto-char (point-max))
          (insert-buffer-substring buffer))
        (when .description
          (message "%s...%s" .description
                   (if (string-search "finished" event)
                       "done"
                     "process exited abnormally")))
        (kill-buffer buffer)))))

(defun dired-atool--run (operation program &rest args)
  "Start PROGRAM with ARGS.
OPERATION stores information for the current operation."
  (let* ((name (file-name-nondirectory program))
         (buffer (generate-new-buffer (format " *%s*" name)))
         (flat-args (flatten-tree args)))
    (with-current-buffer buffer
      (insert (format "%s: %s [%s]\n"
                      (capitalize (or (cdr (assq 'name operation))
                                      "unknown"))
                      (mapconcat #'shell-quote-argument
                                 (cons program flat-args) " ")
                      (format-time-string "%FT%T%z"))))
    ;; NOTE: Encrypted archives are not yet supported and using a pipe here
    ;; normally makes the process fail immediately when it tries to read a
    ;; password from the user.
    (let* ((process-connection-type nil)
           (process (apply #'start-process name buffer program flat-args)))
      (process-put process 'operation operation)
      (set-process-sentinel process #'dired-atool--sentinel))))

;;; Unpack files

(defun dired-atool-do-unpack (&optional arg)
  "Unpack files with aunpack in Dired.
ARG is directly passed to `dired-get-marked-files'."
  (interactive "P")
  (let* ((archives (dired-get-marked-files nil arg nil nil t))
         (files (mapcar #'dired-make-relative archives))
         (count (length files))
         (prompt (format "Unpack %s to: " (dired-mark-prompt arg files)))
         (directory (dired-dwim-target-directory))
         (input (dired-mark-pop-up nil 'uncompress files
                                   #'read-directory-name prompt directory))
         (destination (file-name-as-directory (expand-file-name input)))
         (description (concat (format "Unpack: %d file" count)
                              (and (/= count 1) "s")))
         (operation `((name . "unpack")
                      (destination . ,destination)
                      (description . ,description))))
    (when (or (file-remote-p default-directory)
              (file-remote-p destination))
      (user-error "Remote hosts not supported"))
    (message "%s..." description)
    (unless (file-directory-p destination)
      (dired-create-directory destination))
    (let ((default-directory destination))
      (dired-atool--run operation
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
         (count (length files))
         (prompt (format "Pack %s to: " (dired-mark-prompt arg files)))
         (directory (dired-dwim-target-directory))
         (target (expand-file-name (dired-mark-pop-up nil 'compress files
                                                      #'read-file-name
                                                      prompt directory)))
         (destination (file-name-directory target))
         (description (concat (format "Pack: %d file" count)
                              (and (/= count 1) "s")))
         (operation `((name . "pack")
                      (destination . ,destination)
                      (description . ,description))))
    (when (or (file-remote-p default-directory)
              (file-remote-p destination))
      (user-error "Remote hosts not supported"))
    (message "%s..." description)
    (unless (file-directory-p destination)
      (dired-create-directory destination))
    (when (and (file-exists-p target)
               (yes-or-no-p (format "File %s exists; %s it first? "
                                    target
                                    (if (and dired-atool-use-trash
                                             delete-by-moving-to-trash)
                                        "trash"
                                      "delete"))))
      (delete-file target dired-atool-use-trash))
    (dired-atool--run operation
                      dired-atool-apack-program
                      dired-atool-apack-extra-options
                      target
                      files)))

(provide 'dired-atool)
;;; dired-atool.el ends here
