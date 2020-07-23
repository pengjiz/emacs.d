;;; web-search.el --- Define and query search engines  -*- lexical-binding: t; -*-

;;; Commentary:

;; Easily define and query search engines.

;;; Code:

(eval-when-compile
  (require 'subr-x))

;;; Core

(defmacro web-search-define-engine (name url-template title)
  "Define a command with NAME to use a search engine.

The address is formed with URL-TEMPLATE and the user input read
with a prompt constructed with TITLE."
  (declare (indent 1)
           (debug (&define name stringp stringp)))
  `(defun ,(intern (format "web-search-%s" name)) (query)
     ,(format "Search %s with QUERY." title)
     (interactive
      (let* ((default (if (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))
                        (thing-at-point 'symbol t)))
             (input (read-string
                     (concat ,title
                             (and default
                                  (not (string-empty-p default))
                                  (format " (%s)" default))
                             ": ")
                     nil nil default)))
        (list input)))
     (browse-url (format ,url-template (url-hexify-string query)))))

;;; Search engine

;; Google
(web-search-define-engine google
  "https://www.google.com/search?q=%s"
  "Google")

;; Wikipedia
(web-search-define-engine wikipedia
  "https://en.wikipedia.org/wiki/%s"
  "Wikipedia")

;; Wiktionary
(web-search-define-engine wiktionary
  "https://en.wiktionary.org/wiki/%s"
  "Wiktionary")

;; GitHub
(web-search-define-engine github
  "https://github.com/search?q=%s"
  "GitHub")

;; MDN CSS
(web-search-define-engine mdn-css
  "https://developer.mozilla.org/en-US/docs/Web/CSS/%s"
  "MDN CSS")

(provide 'web-search)
;;; web-search.el ends here
