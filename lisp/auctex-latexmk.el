;;; auctex-latexmk.el --- AUCTeX Latexmk support  -*- lexical-binding: t -*-

;;; Commentary:

;; Latexmk support for AUCTeX.

;;; Code:

(require 'tex-buf)
(eval-when-compile
  (require 'rx)
  (require 'cl-lib))

;;; Run command

(defun auctex-latexmk--run (name command file)
  "Create process for NAME using COMMAND on FILE with Latexmk."
  (let ((TeX-sentinel-default-function #'auctex-latexmk--sentinel))
    (TeX-run-TeX name command file)))

;;; Command sentinel

(defconst auctex-latexmk--rule-regexp
  (rx bol
      "Run number "
      (1+ digit)
      " of rule '"
      (group (1+ (or alnum space)))
      "'")
  "Pattern for Latexmk rule output.")

(defun auctex-latexmk--sentinel (process name)
  "Cleanup Latexmk output buffer after running PROCESS for NAME."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (let ((succeeded (re-search-forward "finished at" nil t))
          (updated (re-search-backward auctex-latexmk--rule-regexp nil t))
          (rule-name (TeX-match-buffer 1)))
      (cond
       (updated
        (if (not (string-match-p "latex" rule-name))
            (message "%s: rule `%s' %s"
                     name rule-name
                     (if succeeded "finished" "exited abnormally"))
          (forward-line 5)
          (let ((start (point)))
            (re-search-forward "^Latexmk:" nil t)
            (beginning-of-line)
            (save-restriction
              (narrow-to-region start (point))
              (goto-char (point-min))
              (TeX-LaTeX-sentinel process name)))))
       (succeeded
        (message "%s: document is up to date" name))
       (t
        (message "%s: exited abnormally" name)))
      ;; NOTE: Latexmk will execute appropriate commands, so we will always use
      ;; it as next command unless it successfully builds the document, in which
      ;; case we should view the document.
      (if succeeded
          (setf TeX-command-next TeX-command-Show)
        (setf TeX-command-next TeX-command-default)))))

;;; Expand options

(defun auctex-latexmk--expand-options ()
  "Get Latexmk options."
  (concat (TeX--output-dir-arg " -outdir=")
          (cl-case TeX-engine
            ((xetex) " -xelatex")
            ((luatex) " -lualatex")
            ((default) (or (and TeX-PDF-mode " -output-format=pdf")
                           " -output-format=dvi")))))

;;; Entry point

(defconst auctex-latexmk--command
  '("Latexmk"
    "latexmk%(latexmkopts) %S%(mode)%(file-line-error) %(extraopts) %t"
    auctex-latexmk--run
    nil (latex-mode) :help "Run Latexmk")
  "Command for Latexmk.")

;; NOTE: In addition to the next command from the output buffer, AUCTeX also
;; suggests command from file information like timestamps. So we tweak the guess
;; when appropriate.
(defun auctex-latexmk--tweak-next-command (result)
  "Return the next command appropriately based on RESULT."
  (if (member result (list TeX-command-BibTeX TeX-command-Biber))
      TeX-command-default
    result))

;; NOTE: The default command is set inside the mode definition, so we have to
;; set it by the mode hook.
(defun auctex-latexmk--set-default ()
  "Set Latexmk as the default command."
  (setf TeX-command-default (car auctex-latexmk--command)))

(defun auctex-latexmk-setup ()
  "Setup Latexmk support for AUCTeX."
  (cl-pushnew '("%(latexmkopts)" auctex-latexmk--expand-options) TeX-expand-list
              :test #'equal :key #'car)
  (cl-pushnew auctex-latexmk--command TeX-command-list :test #'equal :key #'car)
  (cl-pushnew "\\.fdb_latexmk" LaTeX-clean-intermediate-suffixes :test #'equal)
  (add-hook 'LaTeX-mode-hook #'auctex-latexmk--set-default)
  (advice-add #'TeX-command-default :filter-return
              #'auctex-latexmk--tweak-next-command))

(provide 'auctex-latexmk)
;;; auctex-latexmk.el ends here
