;;; auctex-latexmk.el --- AUCTeX Latexmk support -*- lexical-binding: t -*-

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
      ;; NOTE: Latexmk will take care of executions of appropriate commands, so
      ;; we will always use it as next command unless it successfully builds the
      ;; document, in which case we should view the document.
      (if succeeded
          (setf TeX-command-next TeX-command-Show)
        (setf TeX-command-next TeX-command-default)))))

;;; Expand arguments

(defun auctex-latexmk--expand-output-mode ()
  "Get Latexmk output mode."
  (cl-case TeX-engine
    ((xetex) "-xelatex ")
    ((luatex) "-lualatex ")
    ((default) (if TeX-PDF-mode
                   "-pdf "
                 "-dvi -pdf- "))
    (otherwise "")))

;;; Entry point

(defconst auctex-latexmk--command
  "latexmk %(latexmk-out)%S%(mode)%(file-line-error) %(extraopts) %t"
  "AUCTeX command string for Latexmk.")
(defconst auctex-latexmk--intermediate-suffixes
  '("\\.fdb_latexmk" "\\.aux\\.bak" "\\.fls")
  "Suffixes of intermediate files generated by Latexmk.")

;; NOTE: In addition to getting the next command from the output buffer, AUCTeX
;; also suggests command from files (timestamp, etc.). So we tweak the guess
;; when appropriate.
(defun auctex-latexmk--tweak-next-command (fn &rest args)
  "Apply FN on ARGS, but tweak the return value."
  (let ((result (apply fn args)))
    (if (member result (list TeX-command-BibTeX TeX-command-Biber))
        TeX-command-default
      result)))

;; NOTE: With all the special treatments done, it is better to just set the
;; default command as Latexmk. The default command is set in the mode
;; definition, so we have to set it by the mode hook.
(defun auctex-latexmk--set-default ()
  "Set Latexmk as the default command."
  (setf TeX-command-default "Latexmk"))

(defun auctex-latexmk-setup ()
  "Setup Latexmk support for AUCTeX."
  (cl-pushnew '("%(latexmk-out)" auctex-latexmk--expand-output-mode)
              TeX-expand-list
              :test #'equal :key #'car)
  (cl-pushnew `("Latexmk" ,auctex-latexmk--command auctex-latexmk--run nil
                (latex-mode) :help "Run Latexmk")
              TeX-command-list
              :test #'equal :key #'car)
  (dolist (suffix auctex-latexmk--intermediate-suffixes)
    (cl-pushnew suffix LaTeX-clean-intermediate-suffixes :test #'equal))
  (add-hook 'LaTeX-mode-hook #'auctex-latexmk--set-default)
  (advice-add #'TeX-command-default :around
              #'auctex-latexmk--tweak-next-command))

(provide 'auctex-latexmk)
;;; auctex-latexmk.el ends here
