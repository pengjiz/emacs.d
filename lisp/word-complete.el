;;; word-complete.el --- Complete word at point  -*- lexical-binding: t -*-

;;; Commentary:

;; Complete the word at point using a dictionary.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Option

(defgroup word-complete nil
  "Complete word at point."
  :group 'convenience)

(defcustom word-complete-dictionary
  (convert-standard-filename "/usr/share/dict/words")
  "A plain word-list dictionary to use for completion.
In this file, each line should be a word."
  :type '(choice file
                 (const :tag "None" nil)))

;;; Get words

(defvar word-complete--words nil "Words from the dictionary.")

(defun word-complete--get-words (dictionary)
  "Return all the words from the DICTIONARY file."
  (unless word-complete--words
    (setf word-complete--words
          (split-string
           ;; Extracted from f
           (decode-coding-string
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (setf buffer-file-coding-system 'binary)
              (insert-file-contents-literally dictionary)
              (buffer-substring-no-properties (point-min) (point-max)))
            'utf-8)
           "[\n\r]" t)))
  word-complete--words)

;;; Completion interface

(defun word-complete--completion-at-point ()
  "Complete word at point according to a dictionary.
The dictionary file is specified by `word-complete-dictionary'."
  (when (and word-complete-dictionary
             (file-readable-p word-complete-dictionary))
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (start (or (car bounds) (point)))
           (end (or (cdr bounds) (point))))
      (list start end
            (lambda (input predicate action)
              (if (eq action 'metadata)
                  '(metadata (category . word))
                (let* ((dictionary word-complete-dictionary)
                       (words (word-complete--get-words dictionary)))
                  (complete-with-action action words input predicate))))))))

(with-eval-after-load 'minibuffer
  (cl-pushnew '(word (styles basic substring)) completion-category-defaults
              :test #'eq :key #'car))

(defun word-complete--get-completion-settings ()
  "Return alternative completion settings for words."
  (let (settings)
    (dolist (setting (cdr (assq 'word completion-category-overrides)))
      (unless (eq (car setting) 'styles)
        (push setting settings)))
    (push '(styles substring) settings)
    `((word . ,settings))))

;;; Command

(defvar word-complete--pre-completion-buffer nil
  "Current buffer before completing word.")
(defvar word-complete--pre-completion-settings nil
  "Completion settings before completing word.")

(defun word-complete--pre-completion-setup (alternative)
  "Setup before completing word.
If ALTERNATIVE is non-nil, set alternative completion settings as well."
  (unless completion-in-region-mode
    (setf word-complete--pre-completion-buffer (current-buffer)
          word-complete--pre-completion-settings completion-category-overrides)
    (add-hook 'completion-at-point-functions
              #'word-complete--completion-at-point
              nil t)
    (when alternative
      (setf completion-category-overrides
            (word-complete--get-completion-settings)))
    (add-hook 'completion-in-region-mode-hook
              #'word-complete--post-completion-cleanup)))

(defun word-complete--post-completion-cleanup ()
  "Cleanup after completing word."
  (unless completion-in-region-mode
    (with-current-buffer word-complete--pre-completion-buffer
      (remove-hook 'completion-at-point-functions
                   #'word-complete--completion-at-point
                   t)
      (setf completion-category-overrides
            word-complete--pre-completion-settings))
    (setf word-complete--pre-completion-buffer nil
          word-complete--pre-completion-settings nil)
    (remove-hook 'completion-in-region-mode-hook
                 #'word-complete--post-completion-cleanup)))

(defun word-complete (&optional arg)
  "Complete word using command `completion-at-point'.
If ARG is non-nil, consider the characters at point form a
substring of other words."
  (interactive "P")
  (word-complete--pre-completion-setup arg)
  (completion-at-point)
  (word-complete--post-completion-cleanup))

;;; Company integration

(defvar company-begin-commands)
(defvar company-continue-commands)

(with-eval-after-load 'company
  (when (and (listp company-begin-commands)
             (memq 'completion-at-point company-begin-commands))
    (cl-pushnew 'word-complete company-begin-commands :test #'eq))
  (when (and (listp company-continue-commands)
             (memq 'completion-at-point company-continue-commands))
    (cl-pushnew 'word-complete
                (if (eq (car company-continue-commands) 'not)
                    (cdr company-continue-commands)
                  company-continue-commands)
                :test #'eq)))

(provide 'word-complete)
;;; word-complete.el ends here
