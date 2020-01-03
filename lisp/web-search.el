;;; web-search.el --- Define and query search engines  -*- lexical-binding: t; -*-

;;; Commentary:

;; Easily define and query search engines.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defmacro web-search-def-engine (name url-template prompt)
  "Define a command with NAME to use a search engine.

The search engine address is formed with URL-TEMPLATE and the user input
read with `read-string' and PROMPT."
  `(defun ,(intern (format "web-search-%s" name)) (query)
     (interactive
      (let* ((default (if (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))
                        (thing-at-point 'symbol t)))
             (input (read-string
                     (concat ,prompt
                             (and default
                                  (not (string-empty-p default))
                                  (format " (%s)" default))
                             ": ")
                     nil nil default)))
        (list input)))
     (browse-url (format ,url-template (url-hexify-string query)))))

;;; Search engines

;; Google
(web-search-def-engine "google"
                       "https://www.google.com/search?q=%s"
                       "Google")

;; Wikipedia
(web-search-def-engine "wikipedia"
                       "https://en.wikipedia.org/wiki/%s"
                       "Wikipedia")

;; Wiktionary
(web-search-def-engine "wiktionary"
                       "https://en.wiktionary.org/wiki/%s"
                       "Wiktionary")

;; GitHub
(web-search-def-engine "github"
                       "https://github.com/search?q=%s"
                       "GitHub")

(provide 'web-search)
;;; web-search.el ends here
