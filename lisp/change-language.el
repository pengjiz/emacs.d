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
  '("German"
    "Default")
  "List of languages to be considered."
  :type '(repeat string))

(defcustom change-language-functions
  '(change-language-change-ispell-dictionary
    change-language-change-input-method
    change-language-change-typography-style)
  "Hook to be run when changing language."
  :type 'hook)

(defcustom change-language-ispell-dictionaries-alist
  '(("English" . "en_US")
    ("German" . "de_DE"))
  "Ispell dictionaries for languages."
  :type '(alist :keytype string
                :value-type string))

(defcustom change-language-input-methods-alist
  '(("German" . "german-postfix"))
  "Input methods for languages."
  :type '(alist :keytype string
                :value-type string))

(defcustom change-language-typography-styles-alist
  '(("German" . "German")
    ("English" . "English"))
  "Typography styles for languages."
  :type '(alist :keytype string
                :value-type string))

;;; Core

(defvar-local change-language-current-language nil
  "The language for the current buffer.")

(defun change-language (&optional arg)
  "Change language for the current buffer.
With non-nil ARG force changing to the language selected."
  (interactive "P")
  (let ((language (completing-read "Language: "
                                   change-language-languages
                                   nil t)))
    (when (or arg
              (not (equal change-language-current-language
                          language)))
      (run-hook-with-args 'change-language-functions language)
      (setf change-language-current-language language))))

;;; Ispell

(defun change-language-change-ispell-dictionary (language)
  "Change to the Ispell dictionary for LANGUAGE."
  (let ((dictionary (cdr (assoc language
                                change-language-ispell-dictionaries-alist))))
    (ispell-change-dictionary (or dictionary "default"))))

;;; Input method

(defun change-language-change-input-method (language)
  "Change to the input method for LANGUAGE."
  (let ((input-method (cdr (assoc language
                                  change-language-input-methods-alist))))
    ;; NOTE: Do not use set-input-method because it changes the default input
    ;; method globally. Also this function accepts nil as input method and will
    ;; deactivate the current enabled input method in that case.
    (activate-input-method input-method)))

;;; Typography style

(declare-function typo-change-language "ext:typo")

(defun change-language-change-typography-style (language)
  "Change to the typography style for LANGUAGE."
  (when (fboundp #'typo-change-language)
    (let ((style (cdr (assoc language
                             change-language-typography-styles-alist))))
      (typo-change-language (or style (default-value 'typo-language))))))

(provide 'change-language)
;;; change-language.el ends here
