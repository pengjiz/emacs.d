;;; auctex-latexmk.el --- AUCTeX Latexmk support  -*- lexical-binding: t -*-

;;; Commentary:

;; Latexmk support for AUCTeX.

;;; Code:

(require 'latex)
(eval-when-compile
  (require 'rx)
  (require 'cl-lib))

;;; Run command

(defun auctex-latexmk--run (name command file)
  "Create process for NAME using COMMAND on FILE with Latexmk."
  (unless (memq TeX-engine '(default xetex luatex))
    (error "Unsupported engine for Latexmk"))
  (when (or (and (memq TeX-engine '(xetex luatex))
                 (or (not TeX-PDF-mode) (TeX-PDF-from-DVI)))
            (and (eq TeX-engine 'default)
                 (not TeX-PDF-mode) TeX-DVI-via-PDFTeX))
    (error "Unsupported engine and output combination for Latexmk"))
  (let ((TeX-sentinel-default-function #'auctex-latexmk--sentinel))
    (TeX-run-TeX name command file)))

;;; Command sentinel

(defun auctex-latexmk--sentinel (process name)
  "Cleanup Latexmk output buffer after running PROCESS for NAME."
  (rx-let ((prefix (seq bol "Latexmk: "))
           (rule (name) (seq bol "Run number " (1+ digit)
                             " of rule '" name "'")))
    (save-excursion
      (goto-char (point-max))
      (cond
       ((re-search-backward (rx (rule (or "latex" "pdflatex"
                                          "xelatex" "lualatex")))
                            nil t)
        (forward-line 5)
        (let ((start (point)))
          (re-search-forward (rx prefix) nil t)
          (forward-line 0)
          (save-restriction
            (narrow-to-region start (point))
            (goto-char (point-min))
            (TeX-LaTeX-sentinel process name)))
        (if (/= 0 (process-exit-status process))
            (setf TeX-command-next TeX-command-default)
          (when (and (equal TeX-output-extension "xdv")
                     (re-search-forward (rx (rule "xdvipdfmx")) nil t))
            (setf TeX-output-extension "pdf"))))
       ((= 0 (process-exit-status process))
        (message "%s: %s" name (if (re-search-backward (rx prefix
                                                           "Nothing to do"
                                                           " for '")
                                                       nil t)
                                   "document already up-to-date"
                                 "done"))
        (setf TeX-command-next (or (with-current-buffer TeX-command-buffer
                                     (and TeX-PDF-mode (TeX-PDF-from-DVI)))
                                   TeX-command-Show)))
       (t
        (message "%s process exited abnormally" name)
        (setf TeX-command-next TeX-command-default))))))

;;; Expand options

(defun auctex-latexmk--expand-options ()
  "Get Latexmk options."
  (concat "-verbose"
          (TeX--output-dir-arg " -outdir=")
          (cl-case TeX-engine
            ((xetex) " -xelatex")
            ((luatex) " -lualatex")
            ((default) (or (and TeX-PDF-mode (not (TeX-PDF-from-DVI))
                                " -pdflatex")
                           " -latex")))))

;;; Entry point

(defconst auctex-latexmk--command-name "Latexmk"
  "Command name for Latexmk.")

(defconst auctex-latexmk--command
  `(,auctex-latexmk--command-name
    "latexmk %(latexmkopts) %S%(mode)%(file-line-error) %(extraopts) %t"
    auctex-latexmk--run nil (LaTeX-mode docTeX-mode)
    :help "Run Latexmk")
  "Command for Latexmk.")

;; NOTE: In addition to the next command from the output buffer, AUCTeX also
;; suggests command from file information like timestamps. So we tweak the guess
;; when appropriate.
(defun auctex-latexmk--tweak-next-command (result)
  "Return the next command appropriately based on RESULT."
  (let ((command (TeX-process-get-variable (TeX-active-master)
                                           'TeX-command-default)))
    (or (and (equal command auctex-latexmk--command-name)
             (member result `(,TeX-command-BibTeX ,TeX-command-Biber "Index"))
             auctex-latexmk--command-name)
        result)))

;; NOTE: The default command is set inside the mode definition, so we have to
;; set it by the mode hook.
(defun auctex-latexmk--set-default ()
  "Set Latexmk as the default command."
  (setf TeX-command-default auctex-latexmk--command-name))

(defun auctex-latexmk-setup ()
  "Setup Latexmk support for AUCTeX."
  (cl-pushnew '("%(latexmkopts)" auctex-latexmk--expand-options)
              TeX-expand-list-builtin
              :test #'equal :key #'car)
  (cl-pushnew auctex-latexmk--command TeX-command-list :test #'equal :key #'car)
  (cl-pushnew "\\.fdb_latexmk" LaTeX-clean-intermediate-suffixes :test #'equal)
  (add-hook 'LaTeX-mode-hook #'auctex-latexmk--set-default)
  (advice-add 'TeX-command-default :filter-return
              #'auctex-latexmk--tweak-next-command))

(provide 'auctex-latexmk)
;;; auctex-latexmk.el ends here
