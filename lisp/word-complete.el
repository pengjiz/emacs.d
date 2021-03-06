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
            (lambda (string pred action)
              (if (eq action 'metadata)
                  '(metadata (category . word))
                (complete-with-action
                 action
                 (word-complete--get-words word-complete-dictionary)
                 string
                 pred)))))))

(with-eval-after-load 'minibuffer
  (cl-pushnew '(word (styles basic substring)) completion-category-defaults
              :test #'eq :key #'car))

(defun word-complete--get-completion-override ()
  "Return alternative completion settings for words."
  (let (settings)
    (dolist (setting (cdr (or (assq 'word completion-category-overrides)
                              (assq 'word completion-category-defaults))))
      (unless (eq (car setting) 'styles)
        (push setting settings)))
    (push '(styles substring) settings)
    (cons 'word settings)))

;;; Command

(defun word-complete (&optional arg)
  "Complete word using command `completion-at-point'.
If ARG is non-nil, consider the characters at point form a
substring of other words."
  (interactive "P")
  (let* ((overrides (if arg
                        `(,(word-complete--get-completion-override))
                      completion-category-overrides))
         (completion-category-overrides overrides)
         (completion-at-point-functions '(word-complete--completion-at-point)))
    (completion-at-point)))

(provide 'word-complete)
;;; word-complete.el ends here
