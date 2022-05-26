;;; change-language.el --- Easily change language  -*- lexical-binding: t -*-

;;; Commentary:

;; Easily change all language related things for the current buffer.

;;; Code:

(require 'ispell)
(require 'typo nil t)

;;; Option

(defgroup change-language nil
  "Easily change language."
  :group 'convenience)

(defcustom change-language-languages
  '("English" "German")
  "List of known language names."
  :type '(repeat string))

(defcustom change-language-functions
  '(change-language-change-ispell-dictionary
    change-language-change-input-method
    change-language-change-typo-language)
  "Functions to call when changing language."
  :type 'hook)

(defcustom change-language-ispell-dictionary-alist
  '(("English" . "en_US")
    ("German" . "de_DE"))
  "Ispell dictionaries for languages."
  :type '(alist :key-type string
                :value-type string))

(defcustom change-language-input-method-alist
  '(("German" . "german-postfix"))
  "Input methods for languages."
  :type '(alist :key-type string
                :value-type string))

(defcustom change-language-typo-language-alist
  '(("English" . "English")
    ("German" . "German"))
  "Language names used by typo for languages."
  :type '(alist :key-type string
                :value-type string))

;;; Core

(defvar-local change-language-current-language nil
  "Language for the current buffer.")

(defun change-language (language &optional show-message)
  "Change to LANGUAGE for the current buffer.
When SHOW-MESSAGE is non-nil, display helpful messages."
  (interactive
   (let* ((prompt (format-prompt "Language" "unspecified"))
          (input (completing-read prompt change-language-languages nil t)))
     (list (and (not (string-empty-p input)) input) t)))
  (unless (or (not language)
              (member language change-language-languages))
    (error "Invalid language name %S" language))
  (run-hook-with-args 'change-language-functions language)
  (setf change-language-current-language language)
  (when show-message
    (message "Local language changed to %s"
             (or change-language-current-language "unspecified"))))

;;; Ispell

(defun change-language-change-ispell-dictionary (language)
  "Change to the Ispell dictionary for LANGUAGE."
  (let* ((dictionaries change-language-ispell-dictionary-alist)
         (dictionary (and language (cdr (assoc language dictionaries)))))
    (ispell-change-dictionary (or dictionary "default"))))

;;; Input method

(defun change-language-change-input-method (language)
  "Change to the input method for LANGUAGE."
  (let* ((methods change-language-input-method-alist)
         (method (and language (cdr (assoc language methods)))))
    (activate-input-method method)))

;;; Typography style

(defun change-language-change-typo-language (language)
  "Change to the typo language for LANGUAGE."
  (when (fboundp 'typo-change-language)
    (let* ((names change-language-typo-language-alist)
           (name (and language (cdr (assoc language names)))))
      (typo-change-language (or name (default-value 'typo-language))))))

(provide 'change-language)
;;; change-language.el ends here
