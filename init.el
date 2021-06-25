;;; init.el --- Main initialization  -*- lexical-binding: t; -*-

;;; Commentary:

;; Main initialization file.

;;; Code:

;;; Boot

(progn ; fundamental
  (setf ad-redefinition-action 'accept)
  (setf create-lockfiles nil
        delete-by-moving-to-trash t)
  (prefer-coding-system 'utf-8))

(progn ; user interface
  (setf ring-bell-function #'ignore
        (symbol-function 'yes-or-no-p) #'y-or-n-p
        echo-keystrokes 0.1)

  (blink-cursor-mode 0)
  (setf visible-cursor nil
        x-stretch-cursor t)

  (push '(font . "Source Code Pro-12") default-frame-alist)
  (setf frame-resize-pixelwise t
        window-resize-pixelwise t
        (default-value 'indicate-empty-lines) t))

(progn ; package
  (require 'package)
  (setf package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'project)))

(progn ; requires
  (eval-when-compile
    (require 'cl-lib)
    (require 'subr-x)
    (require 'rx)
    (require 'use-package))
  (require 'bind-key))

(progn ; helpers
  (defconst init-etc-directory
    (expand-file-name (convert-standard-filename "etc/")
                      user-emacs-directory)
    "The directory where packages put their configuration files.")
  (defconst init-var-directory
    (expand-file-name (convert-standard-filename "var/")
                      user-emacs-directory)
    "The directory where packages put their persistent data files.")
  (defconst init-sync-directory
    (expand-file-name (convert-standard-filename "Sync/")
                      "~")
    "The directory where files are synchronized among machines.")
  (make-directory init-etc-directory t)
  (make-directory init-var-directory t)
  (make-directory init-sync-directory t)

  (defun init--etc (file)
    "Expand FILE relative to `init-etc-directory'."
    (expand-file-name (convert-standard-filename file)
                      init-etc-directory))

  (defun init--var (file)
    "Expand FILE relative to `init-var-directory'."
    (expand-file-name (convert-standard-filename file)
                      init-var-directory))

  (defun init--sync (file)
    "Expand FILE relative to `init-sync-directory'."
    (expand-file-name (convert-standard-filename file)
                      init-sync-directory))

  ;; NOTE: To manipulate non-interactive functions we need to use
  ;; non-interactive functions, so that they will not become interactive.
  (defun init-ignore (&rest _)
    "Do nothing and return nil."
    nil))

(progn ; startup
  (setf inhibit-default-init t
        inhibit-startup-screen t
        inhibit-startup-buffer-menu t
        inhibit-startup-echo-area-message nil)

  (setf initial-buffer-choice t
        initial-scratch-message nil
        initial-major-mode #'fundamental-mode))

;;; Initialization

(progn ; general customization
  (setf custom-file (init--var "custom.el"))
  (load custom-file t t t)

  (dolist (key '("M-`" "M-=" "M-$" "M-z" "C-z" "C-x C-z" "C-x C-u" "C-x C-l"
                 "C-x m" "C-x 4 m" "C-x 5 m"))
    (unbind-key key)))

(use-package modus-themes
  :ensure t
  :bind ("C-c t n" . modus-themes-toggle)
  :init (modus-themes-load-themes)
  :config (modus-themes-load-vivendi))

(use-package faces
  :defer t
  :config
  (set-face-attribute 'fixed-pitch nil :family "Source Code Pro")
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans"))

(use-package liteline
  :load-path "lisp"
  :defer t
  :hook (after-init . liteline-setup))

(use-package rich-title
  :load-path "lisp"
  :defer t
  :hook (after-init . rich-title-setup))

(use-package transient
  :ensure t
  :defer t
  :init
  (setf transient-history-file (init--var "transient/history.el")
        transient-levels-file (init--var "transient/levels.el")
        transient-values-file (init--var "transient/values.el")))

(use-package url
  :defer t
  :init (setf url-configuration-directory (init--var "url/")))

(use-package url-cache
  :defer t
  :init (setf url-cache-directory (init--var "url/cache/")))

(use-package request
  :ensure t
  :defer t
  :init (setf request-storage-directory (init--var "request/")))

(use-package server
  :config
  (unless (or (daemonp) (server-running-p))
    (server-start)))

(use-package with-editor
  :ensure t
  :hook ((shell-mode eshell-mode) . with-editor-export-editor)
  :init (shell-command-with-editor-mode))

;;; General utility

(progn ; disabled commands
  (dolist (command '(erase-buffer
                     narrow-to-region
                     narrow-to-page
                     set-goal-column
                     scroll-left
                     dired-find-alternate-file))
    (put command 'disabled nil)))

(use-package simple
  :bind (([remap just-one-space] . cycle-spacing)
         ([remap downcase-word] . downcase-dwim)
         ([remap capitalize-word] . capitalize-dwim)
         ([remap upcase-word] . upcase-dwim)
         ("C-c b c" . clone-indirect-buffer)
         ("C-c t v" . visual-line-mode)
         ("C-c t l" . toggle-truncate-lines)
         ("C-c t q" . auto-fill-mode)
         ("C-c t p" . visible-mode)
         ("C-c a l" . list-processes))
  :hook ((text-mode bibtex-mode) . auto-fill-mode)
  :init
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode)
  :config
  (setf extended-command-suggest-shorter nil)
  (setf completion-show-help nil)
  (setf kill-do-not-save-duplicates t
        save-interprogram-paste-before-kill t)
  (setf set-mark-command-repeat-pop t)
  (setf shell-command-dont-erase-buffer 'end-last-out
        async-shell-command-display-buffer nil))

(use-package simple-extras
  :load-path "lisp"
  :config
  (simple-extras-setup)
  (dolist (hook '(prog-mode-hook protobuf-mode-hook))
    (add-hook hook #'simple-extras-auto-fill-comments-mode))

  (bind-keys ("M-z" . simple-extras-unfill-paragraph)
             ("M-=" . simple-extras-eval-and-replace-last-sexp)
             :map completion-in-region-mode-map
             ("C-<tab>" . simple-extras-force-completion-at-point)))

(use-package savehist
  :init (setf savehist-file (init--var "savehist"))
  :config (savehist-mode))

(use-package undo-propose
  :ensure t
  :defer t
  :bind ("C-x u" . undo-propose))

(use-package delsel
  :config (delete-selection-mode))

(use-package expand-region
  :ensure t
  :defer t
  :bind* ("C-=" . er/expand-region))

(use-package bug-reference
  :defer t
  :hook ((prog-mode protobuf-mode) . bug-reference-prog-mode)
  :bind (("C-c t u" . bug-reference-mode)
         ("C-c t U" . bug-reference-prog-mode)
         :map bug-reference-map
         ("C-c C-o" . bug-reference-push-button)))

(use-package goto-addr
  :defer t
  :hook (((prog-mode
           protobuf-mode
           TeX-mode
           conf-mode
           yaml-mode)
          . goto-address-prog-mode)
         ((rst-mode
           comint-mode
           cider-repl-mode
           eshell-mode)
          . goto-address-mode))
  :bind (("C-c t a" . goto-address-mode)
         ("C-c t A" . goto-address-prog-mode)
         :map goto-address-highlight-keymap
         ("C-c C-o" . goto-address-at-point)))

(use-package subword
  :defer t
  :hook ((prog-mode protobuf-mode) . subword-mode)
  :bind ("C-c t b" . subword-mode))

(use-package align
  :defer t
  :bind (("C-c x a" . align)
         ("C-c x A" . align-regexp)
         ("C-c x z" . align-current)
         ("C-c x Z" . align-entire)))

(use-package sort
  :defer t
  :bind (("C-c x r" . sort-lines)
         ("C-c x R" . sort-columns)
         ("C-c x d" . delete-duplicate-lines)))

(use-package typo
  :ensure t
  :defer t
  :bind (("C-c t t" . typo-mode)
         ("C-c t T" . typo-change-language)))

(use-package mule-cmds
  :defer t
  :no-require t
  :bind ("C-c t i" . toggle-input-method)
  :init (setf default-input-method "TeX"))

(use-package kkc
  :defer t
  :init (setf kkc-init-file-name (init--var "kkcrc")))

(use-package change-language
  :load-path "lisp"
  :defer t
  :bind ("C-c t m" . change-language))

(use-package re-builder
  :defer t
  :bind ("C-c m r" . re-builder)
  :config (setf reb-re-syntax 'string))

(use-package calc
  :defer t
  :bind (([remap calc-dispatch] . quick-calc)
         ("C-c m c" . calc)
         ("C-c x c" . calc-grab-region)
         ("C-c x C" . calc-grab-rectangle))
  :init
  (defvar org-babel-load-languages)
  (with-eval-after-load 'org
    (cl-pushnew '(calc . t) org-babel-load-languages :test #'eq :key #'car))
  :config (setf calc-gnuplot-default-device "qt"))

(use-package calc-ext
  :defer t
  :after calc
  :bind (:map calc-mode-map ("C-c m c" . calc-reset)))

(use-package calc-yank
  :defer t
  :after calc
  :bind ("C-c e c" . calc-copy-to-buffer))

(use-package help
  :defer t
  :config (setf help-window-select t))

(use-package info
  :defer t
  :bind ("C-c m i" . info))

(use-package man
  :defer t
  :bind ("C-c m k" . man)
  :config (setf Man-notify-method 'aggressive))

(use-package htmlize
  :ensure t
  :defer t)

(use-package edit-indirect
  :ensure t
  :defer t)

(use-package poporg
  :ensure t
  :defer t
  :bind ("C-c x o" . poporg-dwim))

(use-package diff
  :defer t
  :bind (("C-c f =" . diff)
         ("C-c b =" . diff-buffer-with-file)))

(use-package ediff
  :defer t
  :config
  (setf ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(use-package proced
  :defer t
  :bind ("C-c a L" . proced))

(use-package proced-narrow
  :ensure t
  :defer t
  :after proced
  :bind (:map proced-mode-map ("/" . proced-narrow)))

;;; Pair

(use-package elec-pair
  :config (electric-pair-mode))

(use-package paren
  :config
  (setf show-paren-when-point-inside-paren t)
  (show-paren-mode))

(use-package lisp
  :defer t
  :no-require t
  :bind (("C-x C-u" . delete-pair)
         ("C-x C-l" . raise-sexp)
         ("C-c x p" . check-parens)))

;;; Whitespace

(progn ; general whitespace
  (setf (default-value 'fill-column) 80)
  (setf sentence-end-double-space nil))

(use-package whitespace
  :defer t
  :bind (("C-c t w" . whitespace-mode)
         ("C-c t W" . whitespace-toggle-options)
         ("C-c x w" . whitespace-cleanup))
  :hook ((prog-mode
          protobuf-mode
          text-mode
          bibtex-mode
          conf-mode)
         . init--enable-whitespace)
  :init
  (defun init--enable-whitespace ()
    (add-hook 'hack-local-variables-hook #'whitespace-mode nil t))
  :config
  (setf whitespace-style '(face
                           indentation
                           space-after-tab
                           space-before-tab
                           tab-mark
                           trailing
                           lines-tail)
        whitespace-line-column nil)

  (defun init--set-cleanup-whitespace-style (fn &rest args)
    "Apply FN on ARGS but explicitly set whitespace style for cleanup."
    (let ((whitespace-style '(empty
                              indentation
                              space-before-tab
                              space-after-tab
                              trailing)))
      (apply fn args)))
  (advice-add #'whitespace-cleanup :around
              #'init--set-cleanup-whitespace-style)
  (advice-add #'whitespace-cleanup-region :around
              #'init--set-cleanup-whitespace-style))

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode))

;;; Indentation

(progn ; tab
  (setf (default-value 'indent-tabs-mode) nil
        (default-value 'tab-width) 8)
  (setf tab-always-indent t))

(use-package electric
  :config (electric-indent-mode))

;;; Editing visual

(use-package hideshow
  :defer t
  :bind (:map hs-minor-mode-map ("C-c @ t" . hs-toggle-hiding))
  :hook ((prog-mode protobuf-mode bibtex-mode) . hs-minor-mode)
  :init (setf hs-minor-mode-map (make-sparse-keymap)))

(use-package outline
  :defer t
  :bind (;; -
         :map outline-minor-mode-map
         ("C-c @ SPC" . outline-mark-subtree)
         ("C-c @ p" . outline-previous-visible-heading)
         ("C-c @ n" . outline-next-visible-heading)
         ("C-c @ b" . outline-backward-same-level)
         ("C-c @ f" . outline-forward-same-level))
  :hook ((prog-mode protobuf-mode TeX-mode) . outline-minor-mode)
  :init (setf outline-minor-mode-map (make-sparse-keymap)))

(use-package bicycle
  :ensure t
  :defer t
  :after outline
  :bind (;; -
         :map outline-minor-mode-map
         ("C-<tab>" . bicycle-cycle)
         ("<backtab>" . bicycle-cycle-global)))

(use-package hl-line
  :config
  (setf hl-line-sticky-flag nil)
  (global-hl-line-mode))

(use-package page-break-lines
  :ensure t
  :defer t
  :hook ((compilation-mode help-mode) . page-break-lines-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((prog-mode protobuf-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :defer t
  :bind ("C-c t o" . rainbow-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :hook ((prog-mode protobuf-mode) . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :ensure t
  :config (hes-mode))

(use-package hl-todo
  :ensure t
  :defer t
  :hook ((prog-mode
          protobuf-mode
          TeX-mode
          conf-mode
          yaml-mode)
         . hl-todo-mode))

(use-package visual-fill-column
  :ensure t
  :defer t
  :bind ("C-c t c" . visual-fill-column-mode)
  :config
  (setf (default-value 'visual-fill-column-center-text) t
        (default-value 'visual-fill-column-fringes-outside-margins) nil)
  (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package page-turner
  :load-path "lisp"
  :ensure visual-fill-column
  :config
  (setf page-turner-prose-family "DejaVu Serif")
  (page-turner-setup))

;;; Move point

(use-package beginend
  :ensure t
  :config (beginend-global-mode))

(use-package avy
  :ensure t
  :defer t
  :bind* (("C-;" . avy-goto-char-in-line)
          ("C-'" . avy-goto-char-2))
  :bind (("C-z" . avy-resume)
         :map isearch-mode-map
         ([remap avy-goto-char-2] . avy-isearch))
  :config
  ;; Smart case search
  (setf avy-case-fold-search nil)
  (setf avy-all-windows nil
        avy-all-windows-alt t)
  (setf avy-dispatch-alist
        '((?x . avy-action-kill-move)
          (?X . avy-action-kill-stay)
          (?t . avy-action-teleport)
          (?m . avy-action-mark)
          (?n . avy-action-copy)
          (?y . avy-action-yank)
          (?z . avy-action-zap-to-char))))

(use-package imenu
  :defer t
  :bind ("M-g i" . imenu)
  :config
  (setf imenu-auto-rescan t)
  (setf imenu-space-replacement nil))

;;; Search & replace

(use-package isearch
  :defer t
  :config
  (setf isearch-allow-scroll t)
  (setf isearch-lazy-count t))

;;; Buffer

;; Protect a few special buffers
(progn ; special buffers
  (defun init--protect-special-buffers ()
    "Protect special buffers from being killed."
    (or (not (member (buffer-name (current-buffer))
                     '("*scratch*" "*Messages*")))
        (ignore (bury-buffer))))
  (add-hook 'kill-buffer-query-functions #'init--protect-special-buffers))

(use-package autorevert
  :bind ("C-c t g" . auto-revert-mode)
  :init (global-auto-revert-mode))

(use-package uniquify
  :config
  (setf uniquify-buffer-name-style 'forward
        uniquify-trailing-separator-p t))

(use-package ibuffer
  :defer t
  :bind (([remap list-buffers] . ibuffer)
         :map ibuffer-mode-map
         ("C-c C-o" . ibuffer-visit-buffer-1-window))
  :config
  (setf ibuffer-use-other-window t)
  (let ((state '(mark modified read-only locked " "))
        (info '((name 18 18 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " ")))
    (setf ibuffer-formats `((,@state ,@info filename)
                            (,@state ,@info process)))))

(use-package ibuf-ext
  :defer t
  :config (setf ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-vc
  :ensure t
  :defer t
  :hook (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root)
  :config
  (let ((state '(mark modified read-only locked vc-status-mini " "))
        (info '((name 18 18 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " ")))
    (setf ibuffer-formats `((,@state ,@info vc-relative-file)
                            (,@state ,@info process)))))

;;; File

(use-package files
  :bind (("C-c b g" . revert-buffer)
         ("C-c b r" . rename-buffer)
         ("C-c b R" . rename-uniquely))
  :config
  (let ((list-prefix (init--var "auto-save/sessions/"))
        (save-directory (init--var "auto-save/saves/")))
    (make-directory save-directory t)
    (setf auto-save-list-file-prefix list-prefix
          auto-save-file-name-transforms `((".*" ,save-directory t))))
  (setf backup-by-copying t
        delete-old-versions t
        version-control t
        backup-directory-alist `((,tramp-file-name-regexp . nil)
                                 ("." . ,(init--var "backups/"))))

  (setf view-read-only t)
  (setf save-abbrevs 'silently)
  (setf require-final-newline t)

  (setf confirm-nonexistent-file-or-buffer t)
  (defun init--ensure-directory-for-file ()
    "Ensure that the directory for file exists."
    (make-directory (file-name-directory buffer-file-name) t))
  (add-hook 'find-file-not-found-functions #'init--ensure-directory-for-file))

(use-package files-x
  :defer t
  :bind (("C-c f v" . add-file-local-variable)
         ("C-c f V" . add-file-local-variable-prop-line)
         ("C-c f b" . add-dir-local-variable)))

(use-package files-extras
  :load-path "lisp"
  :defer t
  :bind (("C-c f r" . files-extras-find-recent-file)
         ("C-c f R" . files-extras-find-recent-file-other-window)))

(use-package ffap
  :defer t
  :config (setf ffap-machine-p-known 'reject))

;; Automatically make scripts executable
(use-package executable
  :defer t
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; Automatically update timestamps
(use-package time-stamp
  :defer t
  :hook (before-save . time-stamp)
  :config (setf time-stamp-format "%Y-%02m-%02dT%02H:%02M:%02S%:z"))

(use-package tramp
  :defer t
  :init
  (setf tramp-persistency-file-name (init--var "tramp/persistency")
        tramp-auto-save-directory (init--var "tramp/auto-save/")
        tramp-histfile-override t))

(use-package saveplace
  :init (setf save-place-file (init--var "places"))
  :config (save-place-mode))

(use-package recentf
  :init
  (setf recentf-save-file (init--var "recentf"))
  (setf recentf-auto-cleanup 300)
  :config
  (setf recentf-max-saved-items 100)
  (setf recentf-exclude '("/elpa/" "/var/" "/\\.git/" "/Trash/"))
  (recentf-mode))

(use-package bookmark
  :defer t
  :init (setf bookmark-default-file (init--var "bookmarks"))
  :config
  (unless (file-exists-p bookmark-default-file)
    (dolist (bookmark '(("finances" . "ledger/finances.ledger")))
      (let ((name (car bookmark))
            (file (init--sync (cdr bookmark))))
        (when (file-exists-p file)
          (cl-pushnew `(,name . ((filename . ,file))) bookmark-alist
                      :test #'equal :key #'car))))))

(use-package dired
  :defer t
  :bind (([remap list-directory] . dired)
         :map dired-mode-map
         ("e" . browse-url-of-dired-file)
         ("K" . dired-kill-subdir))
  :config
  (setf dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  (setf dired-auto-revert-buffer t)
  (setf dired-dwim-target t)
  (setf dired-listing-switches "-alhFv --group-directories-first")
  (setf dired-garbage-files-regexp
        (rx bos
            (or (and (1+ nonl)
                     (or (and "." (or "aux" "bbl" "blg" "brf"
                                      "log" "nav" "snm" "toc"
                                      "vrb" "bcf" "idx" "fls"
                                      "run.xml" "synctex.gz" "fdb_latexmk"
                                      "out" "pyc" "elc" "o" "egg-info"
                                      "bak" "orig" "rej"))
                         "-blx.bib"))
                "__pycache__"
                "auto"
                "ltxpng"
                (and "__minted" (1+ nonl)))
            eos))

  (dolist (key '("c" "Z" "P"))
    (unbind-key key dired-mode-map)))

;; Writable Dired
(use-package wdired
  :defer t
  :config (setf wdired-allow-to-change-permissions t))

(use-package dired-aux
  :defer t
  :config (setf dired-isearch-filenames 'dwim))

(use-package dired-x
  :defer t
  :bind* (("C-x C-j" . dired-jump)
          ("C-x 4 C-j" . dired-jump-other-window))
  :bind (:map dired-mode-map (")" . dired-omit-mode))
  :hook (dired-mode . dired-omit-mode)
  :init
  (setf dired-bind-info nil
        dired-bind-vm nil
        dired-bind-man nil)
  :config
  (setf dired-omit-files "\\`[#.]\\|[#~]\\'"
        dired-omit-extensions nil))

;; More colors in Dired
(use-package diredfl
  :ensure t
  :defer t
  :hook (dired-mode . diredfl-mode))

;; Filtering in Dired
(use-package dired-narrow
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(use-package dired-git-info
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map ("[" . dired-git-info-mode)))

(use-package dired-rsync
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map ("V" . dired-rsync))
  :init
  (with-eval-after-load 'dired-x
    (bind-key "V" #'dired-rsync dired-mode-map)))

(use-package dired-atool
  :load-path "lisp"
  :defer t
  :after dired
  :bind (;; -
         :map dired-mode-map
         ("Z" . dired-atool-do-unpack)
         ("P" . dired-atool-do-pack)))

(use-package image-dired
  :defer t
  :after dired
  :init
  (setf image-dired-dir (init--var "image-dired/")
        image-dired-db-file (init--var "image-dired/db")
        image-dired-gallery-dir (init--var "image-dired/gallery/")
        image-dired-temp-image-file (init--var "image-dired/temp")
        image-dired-temp-rotate-image-file (init--var "image-dired/rotate-temp")))

(use-package disk-usage
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map ("]" . disk-usage-here)))

(use-package trashed
  :ensure t
  :defer t
  :bind ("C-c f t" . trashed))

;;; Security

(use-package password-cache
  :defer t
  :config (setf password-cache-expiry 60))

(use-package auth-source
  :defer t
  :config
  (setf auth-sources `(,(init--sync "misc/authinfo.gpg")))
  (setf auth-source-save-behavior nil)
  (setf auth-source-cache-expiry 3600))

(use-package epa
  :defer t
  :config (setf epa-popup-info-window nil))

;;; Window & frame

(progn ; scroll
  (setf scroll-conservatively 101
        scroll-preserve-screen-position 'always
        hscroll-margin 0
        hscroll-step 1))

(use-package mwheel
  :defer t
  :config
  ;; Move slowly by default
  (setf mouse-wheel-scroll-amount '(1 ((shift) . 5))
        mouse-wheel-progressive-speed nil))

(use-package window
  :defer t
  :no-require t
  :bind (("C-c w s" . split-window-below)
         ("C-c w v" . split-window-right)
         ("C-c w k" . delete-window)
         ("C-c w o" . delete-other-windows)
         ("C-c w l" . delete-other-windows-vertically)
         ("C-c w t" . window-toggle-side-windows)
         ("C-x C-z" . window-toggle-side-windows)
         ("C-c w =" . balance-windows)
         ("C-c w f" . fit-window-to-buffer)
         ("C-c b d" . display-buffer))
  :init
  (setf scroll-error-top-bottom t)
  (setf fit-window-to-buffer-horizontally t)
  (setf display-buffer-alist
        `(;; Auxiliary
          (,(rx bos (or "CAPTURE-"
                        "*Org Src"
                        "*Edit Formulas*"
                        "*poporg:"
                        "*edit-indirect"
                        "*Ledger Schedule*"))
           (display-buffer-reuse-window
            display-buffer-below-selected)
           (window-height . 20)
           (window-min-height . 20)
           (reusable-frames . nil))
          ;; Help
          (,(rx bos (or "*Man"
                        "*Help*"
                        "*help"
                        "*Org Help*"
                        "*TeX Help*"
                        "*Anaconda*"
                        "*tide-documentation*"
                        "*Racer Help*"
                        "*cider-doc*"
                        "*cider-inspect*"
                        "*Geiser documentation*"))
           (display-buffer-reuse-window
            display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 15)
           (window-min-height . 15)
           (reusable-frames . nil))
          ;; Application
          (,(rx bos (or "Trash Can"
                        "*Occur"
                        "*xref*"
                        "*Apropos*"
                        " *Agenda Commands*"
                        " *Org todo*"
                        "*Org Select*"
                        "*Org Attach*"
                        "*Org Links*"
                        "*Org Note*"
                        "*Org Export Dispatcher*"
                        "*Org Lint*"
                        "*Org Clock*"
                        "*Clock Task Select*"
                        "*Calendar*"
                        "*RefTeX Select*"
                        "*Bookmark List*"
                        "*Key*"
                        "*Keys*"
                        "*Proced*"
                        "*Process List*"
                        "*Flycheck errors*"
                        "*TeX errors*"
                        "*BibTeX validation errors*"
                        "*R dired*"
                        "*Ibuffer*"
                        "*eshell"
                        (and (1+ nonl) "-eshell*")))
           (display-buffer-reuse-window
            display-buffer-at-bottom)
           (window-height . 15)
           (preserve-size . (nil . t))
           (reusable-frames . nil))
          ;; Command
          (,(rx bos (or "*compilation"
                        "*Compile-Log*"
                        "*Backtrace*"
                        "*Warnings*"
                        "*Error*"
                        "*Shell Command Output*"
                        "*Async Shell Command*"
                        "*firestarter*"
                        "*Macroexpansion*"
                        "*Ledger Report*"
                        "*Ledger Error*"
                        "*Gnuplot Trail*"
                        "*skewer-error*"
                        "*cider-error*"
                        "*cider-test-report*"
                        "*Geiser dbg*"
                        "*HsCompilation*"
                        "*R view"
                        "*S objects*"
                        "*S search list*"
                        "*ess-output*"
                        (and (1+ nonl) " output*")))
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-height . 10)
           (preserve-size . (nil . t))
           (reusable-frames . nil))
          ;; Information
          (,(rx bos (or " *Metahelp*"
                        "*Local Variables*"
                        "*Disabled Command*"
                        "*Messages*"
                        "*Reconcile*"
                        "*Fancy Diary Entries*"
                        "*Holidays*"
                        "*Phases of Moon*"
                        "*Gnuplot Commands*"
                        "*Completions*"
                        "*Quail Completions*"
                        " *Input History*"
                        "*cider-repl-history*"))
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (side . bottom)
           (slot . 1)
           (window-height . 10)
           (preserve-size . (nil . t))
           (reusable-frames . nil)))))

(use-package window-extras
  :load-path "lisp"
  :config (window-extras-setup))

(use-package winner
  :bind (;; -
         :map winner-mode-map
         ("C-c w u" . winner-undo)
         ("C-c w r" . winner-redo))
  :init
  (setf winner-dont-bind-my-keys t)
  (winner-mode))

(use-package ace-window
  :ensure t
  :defer t
  :bind* ("M-o" . ace-window)
  :bind (([remap other-window] . ace-window)
         ("C-c w w" . ace-window))
  :init
  (setf aw-dispatch-alist
        '((?c aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?m delete-other-windows "Delete Other Windows")
          (?y aw-copy-window "Copy Window")
          (?t aw-move-window "Move Window")
          (?x aw-execute-command-other-window "Execute Command Other Window")
          (?o aw-flip-window)
          (?? aw-show-dispatch-help)))
  (setf aw-make-frame-char nil)
  :config
  (setf aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setf aw-minibuffer-flag t))

(use-package tab-bar
  :defer t
  :bind (("C-c s c" . tab-bar-new-tab)
         ("C-c s k" . tab-bar-close-tab)
         ("C-c s o" . tab-bar-close-other-tabs)
         ("C-c s j" . tab-bar-select-tab-by-name)
         ("C-c s s" . tab-bar-switch-to-recent-tab)
         ("C-c s n" . tab-bar-switch-to-next-tab)
         ("C-c s p" . tab-bar-switch-to-prev-tab)
         ("C-c s m" . tab-bar-move-tab)
         ("C-c s r" . tab-bar-rename-tab))
  :config
  (setf tab-bar-show nil)
  (setf tab-bar-new-tab-choice "*scratch*")
  (setf tab-bar-tab-name-function #'tab-bar-tab-name-all))

;;; Completion

(progn ; general completion
  (setf enable-recursive-minibuffers t)
  (setf completion-ignore-case t
        read-buffer-completion-ignore-case t)

  (unbind-key "C-<tab>" minibuffer-local-map))

(use-package minibuffer
  :defer t
  :bind (([remap complete-symbol] . completion-at-point)
         :map minibuffer-local-completion-map
         ("C-<tab>" . minibuffer-force-complete))
  :config
  (setf read-file-name-completion-ignore-case t)
  (setf completion-styles '(basic substring initials partial-completion)))

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode))

(use-package minibuf-eldef
  :init (setf minibuffer-eldef-shorten-default t)
  :config (minibuffer-electric-default-mode))

(use-package company
  :ensure t
  :defer t
  :bind (;; -
         :map company-mode-map
         ([remap dabbrev-completion] . company-complete)
         :map company-active-map
         ([remap beginning-of-buffer] . company-select-first)
         ([remap end-of-buffer] . company-select-last)
         ([remap dabbrev-completion] . company-other-backend))
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          c-mode
          c++-mode
          rust-mode
          haskell-mode
          ess-r-mode
          python-mode
          sh-mode
          html-mode
          css-mode
          js2-mode
          typescript-mode
          LaTeX-mode
          markdown-mode
          rst-mode
          org-mode
          ledger-mode)
         . init--enable-company)
  :init
  (setf company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend)
        company-backends '(company-files company-dabbrev-code company-dabbrev)
        company-transformers '(company-sort-prefer-same-case-prefix))

  (defun init--enable-company ()
    (when-let* ((backends (cond ((derived-mode-p 'emacs-lisp-mode
                                                 'clojure-mode
                                                 'rust-mode
                                                 'sh-mode
                                                 'css-mode
                                                 'latex-mode
                                                 'ledger-mode)
                                 '(company-capf))
                                ((derived-mode-p 'c-mode 'c++-mode)
                                 '(company-c-headers company-etags))
                                ((derived-mode-p 'js2-mode 'typescript-mode)
                                 '(company-tide))
                                ((derived-mode-p 'python-mode)
                                 '(company-anaconda))
                                ((derived-mode-p 'haskell-mode)
                                 '(dante-company)))))
      (make-local-variable 'company-backends)
      (dolist (backend (nreverse backends))
        (push backend company-backends)))
    (company-mode))
  :config
  (setf company-idle-delay 0.5)
  (setf company-show-numbers t
        company-format-margin-function nil))

(use-package company-dabbrev
  :defer t
  :after company
  :config
  (setf company-dabbrev-ignore-case t
        company-dabbrev-downcase nil)
  (setf company-dabbrev-ignore-buffers
        (lambda (buffer)
          (or (memq (buffer-local-value 'major-mode buffer)
                    '(image-mode doc-view-mode dired-mode))
              (string-match-p "\\` \\*" (buffer-name buffer))))))

(use-package company-dabbrev-code
  :defer t
  :after company
  :config (setf company-dabbrev-code-everywhere t))

(use-package hippie-exp
  :defer t
  :bind (([remap dabbrev-expand] . hippie-expand)
         ([remap dabbrev-completion] . hippie-expand))
  :config
  (setf hippie-expand-ignore-buffers
        '("\\` \\*" doc-view-mode image-mode dired-mode))
  (setf hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-line
          try-expand-dabbrev
          try-expand-line-all-buffers
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

(use-package word-complete
  :load-path "lisp"
  :defer t
  :bind (([remap ispell-complete-word] . word-complete)
         ("C-c e d" . word-complete)))

;;; Expansion

(use-package abbrev
  :defer t
  :bind (("C-c t r" . abbrev-mode)
         ("C-c e e" . expand-abbrev))
  :init (setf abbrev-file-name (init--var "abbrev-defs"))
  :hook ((text-mode bibtex-mode) . abbrev-mode))

(use-package tempo
  :defer t
  :config (setf tempo-interactive t))

(use-package autoinsert
  :defer t
  :bind ("C-c e t" . auto-insert)
  :init
  (setf auto-insert t)
  (setf auto-insert-directory (init--etc "insert/")
        auto-insert-alist nil))

;;; Lint

(use-package flycheck
  :ensure t
  :defer t
  :bind (("C-c t e" . flycheck-mode)
         :map flycheck-mode-map
         ("M-g l" . flycheck-list-errors))
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          c-mode
          c++-mode
          rust-mode
          haskell-mode
          ess-r-mode
          js2-mode
          typescript-mode
          python-mode
          sh-mode
          LaTeX-mode
          markdown-mode
          ledger-mode)
         . flycheck-mode)
  :config (setf flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flycheck-inline
  :ensure t
  :defer t
  :hook (flycheck-mode . flycheck-inline-mode))

;;; Spell check

(use-package ispell
  :defer t
  :bind ("C-c t S" . ispell-change-dictionary))

(use-package flyspell
  :defer t
  :bind (("C-c t s" . flyspell-mode)
         ("C-c x s" . flyspell-region))
  :hook (((text-mode bibtex-mode) . flyspell-mode)
         ((prog-mode protobuf-mode) . flyspell-prog-mode))
  :init
  (setf flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (setf flyspell-use-meta-tab nil
        flyspell-mode-map (make-sparse-keymap))
  :config
  (setf flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t))

(use-package flyspell-correct
  :ensure t
  :defer t
  :after flyspell
  :bind (:map flyspell-mode-map ("M-$" . flyspell-correct-at-point)))

;;; VCS

(use-package vc-hooks
  :defer t
  :config
  (setf vc-handled-backends '(Git))
  (setf vc-follow-symlinks t))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g g" . magit-status)
         ("C-c g d" . magit-dispatch)
         ("C-c g f" . magit-file-dispatch))
  :init (setf magit-define-global-key-bindings nil)
  :config
  (setf magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setf magit-save-repository-buffers 'dontask)
  (setf magit-revision-show-gravatars nil
        magit-diff-refine-hunk t)
  ;; Show submodules in status buffer
  (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-modules
                          #'magit-insert-stashes t))

(use-package git-commit
  :defer t
  :after magit
  :config
  (setf git-commit-summary-max-length 50)
  (defun init--setup-git-commit-mode ()
    (setf fill-column 72))
  (add-hook 'git-commit-mode-hook #'init--setup-git-commit-mode)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

;; Show edits
(use-package diff-hl
  :ensure t
  :bind (;; -
         :map diff-hl-mode-map
         ("C-c g p" . diff-hl-previous-hunk)
         ("C-c g n" . diff-hl-next-hunk)
         :map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook (dired-mode . diff-hl-dired-mode)
  :init (global-diff-hl-mode)
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :ensure t
  :defer t)

;;; Project

(use-package project
  :defer t
  :init (setf project-list-file (init--var "projects"))
  :config
  (setf project-vc-merge-submodules nil)
  (setf project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")))

  (dolist (key '("s" "v"))
    (unbind-key key project-prefix-map)))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode))

;;; ElDoc

(use-package eldoc
  :defer t
  :config
  (setf eldoc-echo-area-use-multiline-p nil)
  (setf eldoc-message-function #'message)
  ;; Describe the character at point by default
  (setf eldoc-documentation-function #'describe-char-eldoc))

;;; Cross reference

(use-package xref
  :defer t
  :config
  (when (executable-find "rg")
    (setf xref-search-program 'ripgrep)))

(use-package etags
  :defer t
  :bind ("C-c c t" . visit-tags-table)
  :config (setf tags-revert-without-query t))

;;; Compile

(use-package compile
  :defer t
  :bind (("C-c c c" . compile)
         ("C-c c C" . recompile))
  :config
  (setf compilation-ask-about-save nil
        compilation-always-kill t)
  (setf compilation-scroll-output 'first-error))

(use-package firestarter
  :ensure t
  :defer t
  :bind ("C-c c f" . firestarter-mode)
  :config (setf firestarter-default-type t))

;;; Comint

(use-package comint
  :defer t
  :bind (;; -
         :map comint-mode-map
         ([remap comint-delchar-or-maybe-eof] . delete-char)
         ("C-c a a" . comint-send-eof))
  :config
  (setf comint-prompt-read-only nil
        comint-scroll-to-bottom-on-input 'this))

;;; Eshell

(use-package eshell
  :defer t
  :bind (("C-x m" . eshell)
         ("C-c a e" . eshell)))

(use-package esh-mode
  :defer t
  :init (setf eshell-directory-name (init--var "eshell/"))
  :config
  (setf eshell-scroll-to-bottom-on-input 'this)

  (defun init--setup-eshell-mode ()
    (bind-keys :map eshell-mode-map
               ("C-x m" . eshell-life-is-too-much)
               ("C-c a a" . eshell-life-is-too-much)))
  (add-hook 'eshell-mode-hook #'init--setup-eshell-mode))

(use-package esh-var
  :defer t
  :config (setf eshell-modify-global-environment t))

(use-package esh-module
  :defer t
  :config
  (setf eshell-modules-list '(eshell-alias
                              eshell-basic
                              eshell-cmpl
                              eshell-dirs
                              eshell-glob
                              eshell-hist
                              eshell-ls
                              eshell-pred
                              eshell-prompt
                              eshell-script
                              eshell-term
                              eshell-tramp
                              eshell-unix)))

(use-package em-alias
  :defer t
  :init (setf eshell-aliases-file (init--etc "eshell/aliases")))

(use-package em-glob
  :defer t
  :config
  (setf eshell-glob-case-insensitive t
        eshell-error-if-no-glob t))

(use-package em-hist
  :defer t
  :config
  (setf eshell-history-size 2000
        eshell-input-filter #'eshell-input-filter-initial-space))

(use-package em-script
  :defer t
  :init
  (setf eshell-rc-script (init--etc "eshell/profile")
        eshell-login-script (init--etc "eshell/login")))

(use-package eshell-z
  :ensure t
  :after eshell)

(use-package eshell-extras
  :load-path "lisp"
  :after eshell
  :config
  (eshell-extras-setup)

  (bind-keys ([remap eshell-truncate-buffer] . eshell-extras-clear-buffer)
             :map eshell-extras-autosuggest-suggestion-map
             ([remap forward-char] . eshell-extras-accept-suggestion)
             ([remap move-end-of-line] . eshell-extras-accept-suggestion)
             ([remap forward-word] . eshell-extras-accept-suggestion-word)))

;;; Debugging

(use-package gud
  :defer t
  :bind (("C-c c g" . gud-gdb)
         ("C-c c p" . pdb))
  :config (setf gud-pdb-command-name "python -m pdb"))

(use-package rmsbolt
  :ensure t
  :defer t
  :bind ("C-c c r" . rmsbolt-mode)
  :config (setf rmsbolt-automatic-recompile nil))

;;; Browser

(use-package shr-color
  :defer t
  :config (setf shr-color-visible-luminance-min 70))

(use-package eww
  :defer t
  :bind ("C-c f e" . eww-open-file)
  :init
  (make-directory (init--var "eww/") t)
  (setf eww-bookmarks-directory (init--var "eww/"))
  :config
  (setf eww-search-prefix "https://www.google.com/search?q=")

  (defun init--use-fundamental-mode-for-eww (fn &rest args)
    "Apply FN on ARGS, but force using `fundamental-mode' to view page source."
    (cl-letf (((symbol-function 'mhtml-mode) #'fundamental-mode)
              ((symbol-function 'html-mode) #'fundamental-mode))
      (apply fn args)))
  (advice-add #'eww-view-source :around #'init--use-fundamental-mode-for-eww))

(use-package elfeed
  :ensure t
  :defer t
  :init
  (setf elfeed-db-directory (init--sync "misc/elfeed/db/"))
  (with-eval-after-load 'recentf
    (push "/elfeed/db/" recentf-exclude))
  :bind ("C-c m w" . elfeed)
  :config
  ;; Load feeds
  (load (init--sync "misc/elfeed/feeds.el") t t t)
  ;; Mark old entries as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "10 days ago"
                                :remove 'unread)))

(use-package elfeed-search
  :defer t
  :after elfeed
  :config (setf elfeed-search-filter "@1-month-ago"))

(use-package elfeed-show
  :defer t
  :after elfeed
  :init
  (setf elfeed-enclosure-default-dir
        (expand-file-name (convert-standard-filename "Downloads/") "~"))
  :config (setf elfeed-show-entry-switch #'pop-to-buffer-same-window))

;;; Viewer

(use-package doc-view
  :defer t
  :bind (:map doc-view-mode-map ("&" . browse-url-of-file))
  :config
  (setf doc-view-continuous t)
  (setf doc-view-resolution 300))

(use-package doc-view-extras
  :load-path "lisp"
  :after doc-view
  :config (doc-view-extras-setup))

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :init (setf nov-save-place-file (init--var "nov-places")))

;;; Chatting

(use-package erc
  :defer t
  :bind ("C-c m e" . erc-tls)
  :init
  (setf erc-modules '(autojoin
                      button
                      completion
                      fill
                      irccontrols
                      list
                      match
                      move-to-prompt
                      netsplit
                      networks
                      noncommands
                      notifications
                      readonly
                      ring
                      services
                      stamp
                      spelling
                      truncate))
  (setf erc-header-line-format "%n %aon %t (%m,%l)"
        erc-header-line-uses-help-echo-p nil)
  :config
  (setf erc-nick "pengjiz"
        erc-try-new-nick-p nil)
  (setf erc-user-full-name "Pengji Zhang")
  (setf erc-prompt-for-password nil)
  (setf erc-lurker-hide-list '("JOIN" "PART" "QUIT")))

(use-package erc-join
  :defer t
  :config
  ;; Join channels only after identifying
  (setf erc-autojoin-timing 'ident
        erc-autojoin-delay 0))

(use-package erc-services
  :defer t
  :init
  (setf erc-nickserv-identify-mode 'autodetect)
  (setf erc-prompt-for-nickserv-password nil))

(use-package erc-button
  :defer t
  :config
  (setf erc-button-alist
        '(('nicknames 0 erc-button-buttonize-nicks erc-nick-popup 0)
          (erc-button-url-regexp 0 t browse-url 0)
          ("<URL: *\\([^<> ]+\\) *>" 0 t browse-url 1)
          ("\\s-\\(@\\([0-9][0-9][0-9]\\)\\)" 1 t erc-button-beats-to-time 2))))

(use-package erc-truncate
  :defer t
  :config (setf erc-max-buffer-size 100000))

(use-package erc-extras
  :load-path "lisp"
  :after erc
  :config (erc-extras-setup))

;;; Calendar

(use-package calendar
  :defer t
  :bind ("C-c m d" . calendar)
  :init
  (let ((sync-directory (init--sync "misc/")))
    (make-directory sync-directory t)
    (setf diary-file (expand-file-name "diary" sync-directory)))
  :config
  (setf calendar-mark-holidays-flag t
        calendar-chinese-all-holidays-flag t)
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  (setf calendar-date-display-form calendar-iso-date-display-form)
  (calendar-set-date-style 'iso)
  (setf calendar-mode-line-format nil
        (symbol-function 'calendar-set-mode-line) #'init-ignore))

(use-package holidays
  :defer t
  :init
  (setf holiday-bahai-holidays nil
        holiday-islamic-holidays nil
        holiday-hebrew-holidays nil
        holiday-christian-holidays nil))

(use-package solar
  :defer t
  :config
  (setf calendar-location-name "Pittsburgh, PA"
        calendar-latitude 40.4
        calendar-longitude -79.9))

(use-package diary-lib
  :defer t
  :config (setf diary-comment-start "##"))

(use-package appt
  :defer t
  :config
  (setf appt-display-diary nil
        appt-display-mode-line nil)
  (setf appt-display-interval 10
        appt-message-warning-time 20))

(use-package appt-extras
  :load-path "lisp"
  :defer t
  :bind ("C-c o r" . appt-extras-set-reminder)
  :hook (after-init . appt-extras-setup))

;;; Emacs Lisp

(use-package elisp-mode
  :defer t
  :bind (;; -
         :map emacs-lisp-mode-map
         ("C-c C-r" . eval-region)
         ("C-c C-b" . eval-buffer)
         ("C-c a a" . ielm))
  :init
  (with-eval-after-load 'org
    (cl-pushnew '(emacs-lisp . t) org-babel-load-languages
                :test #'eq :key #'car)))

(use-package eros
  :ensure t
  :after elisp-mode
  :config (eros-mode))

(use-package emacs-lisp-snippets
  :load-path "lisp"
  :defer t
  :commands emacs-lisp-snippets-file-template
  :init
  (with-eval-after-load 'autoinsert
    (define-auto-insert '("\\.el\\'" . "Emacs Lisp file template")
      #'emacs-lisp-snippets-file-template)))

(use-package ielm
  :defer t
  :bind ("C-c a i" . ielm)
  :config (setf ielm-prompt-read-only nil))

;;; C & C++ & AWK

(use-package cc-mode
  :defer t
  :bind (:map c-mode-base-map ("C-c C-b" . c-context-line-break))
  :init
  (with-eval-after-load 'org
    (cl-pushnew '(awk . t) org-babel-load-languages :test #'eq :key #'car))
  :config
  ;; A modified style from K&R style
  (c-add-style "common" '("k&r" (c-basic-offset . 4)))
  ;; A style for protobuf-mode
  (c-add-style "protobuf" '("common"
                            (c-basic-offset . 2)
                            (indent-tabs-mode . nil)))

  (setf c-default-style '((awk-mode . "awk")
                          (protobuf-mode . "protobuf")
                          (other . "common"))))

(use-package cmacexp
  :defer t
  :config (setf c-macro-prompt-flag t))

(use-package c-snippets
  :load-path "lisp"
  :defer t
  :commands c-snippets-header-template
  :init
  (with-eval-after-load 'autoinsert
    (define-auto-insert `(,(rx "." (or "h" "H" "hh" "hpp" "hxx" "h++") eos)
                          . "C/C++ header template")
      #'c-snippets-header-template)))

(use-package company-c-headers
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package cmake-ide
  :ensure t
  :after cc-mode
  :config (cmake-ide-setup))

;;; Clojure

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t
  :after clojure-mode
  :bind (:map clojure-mode-map ("C-c a a" . cider)))

(use-package cider-eval
  :defer t
  :after cider
  :config (setf cider-save-file-on-load t))

(use-package cider-repl
  :defer t
  :after cider
  :bind (:map cider-repl-mode-map ("C-c a a" . cider-quit))
  :config (setf cider-repl-display-help-banner nil))

(use-package nrepl-client
  :defer t
  :after cider
  :config (setf nrepl-hide-special-buffers t))

(use-package flycheck-clj-kondo
  :ensure t
  :after flycheck)

;;; Racket

(use-package geiser-mode
  :ensure geiser
  :defer t
  :bind (:map geiser-mode-map ("C-c a a" . run-geiser)))

(use-package geiser-repl
  :ensure geiser
  :defer t
  :bind (:map geiser-repl-mode-map ("C-c a a" . geiser-repl-exit))
  :init
  (make-directory (init--var "geiser/") t)
  (setf geiser-repl-history-filename (init--var "geiser/history"))
  :config
  (setf geiser-repl-company-p nil)
  (setf geiser-repl-read-only-prompt-p nil
        geiser-repl-read-only-output-p nil))

(use-package geiser-autodoc
  :ensure geiser
  :defer t
  :config (setf geiser-autodoc-delay eldoc-idle-delay))

(use-package geiser-racket
  :ensure t
  :defer t)

(use-package scribble-mode
  :ensure t
  :defer t)

;;; Rust

(use-package rust-mode
  :ensure t
  :defer t)

(use-package racer
  :ensure t
  :defer t
  :hook (rust-mode . racer-mode)
  :config (setf racer-complete-insert-argument-placeholders nil))

(use-package flycheck-rust
  :ensure t
  :defer t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

;; pest parser
(use-package pest-mode
  :load-path "lisp"
  :defer t
  :commands pest-mode
  :mode "\\.pest\\'")

;;; Haskell

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (dolist (key '("C-c C-l" "C-c C-b" "C-c C-t" "C-c C-i"))
    (unbind-key key haskell-mode-map)))

(use-package haskell
  :defer t
  :after haskell-mode
  :bind (;; -
         :map interactive-haskell-mode-map
         ("C-c a a" . haskell-interactive-bring)
         ("C-c C-l" . haskell-process-load-file)
         ("C-c C-r" . haskell-process-reload))
  :hook (haskell-mode . interactive-haskell-mode)
  :init (setf interactive-haskell-mode-map (make-sparse-keymap))
  :config
  (setf haskell-interactive-popup-errors nil
        haskell-interactive-mode-read-only nil
        haskell-interactive-prompt-read-only nil)
  (setf haskell-process-auto-import-loaded-modules t
        haskell-process-suggest-remove-import-lines t
        haskell-process-show-overlays nil))

(use-package haskell-interactive-mode
  :defer t
  :after haskell-mode
  :bind (;; -
         :map haskell-interactive-mode-map
         ("C-c a a" . haskell-interactive-kill)
         ("C-c M-o" . haskell-interactive-mode-clear)))

(use-package haskell-indentation
  :defer t
  :after haskell-mode
  :hook (haskell-mode . haskell-indentation-mode))

(use-package haskell-collapse
  :defer t
  :after haskell-mode
  :bind (;; -
         :map haskell-collapse-mode-map
         ("C-c @ t" . haskell-hide-toggle)
         ("C-c @ T" . haskell-hide-toggle-all))
  :hook (haskell-mode . haskell-collapse-mode)
  :init (setf haskell-collapse-mode-map (make-sparse-keymap)))

(use-package dante
  :ensure t
  :defer t
  :bind (;; -
         :map dante-mode-map
         ("C-c C-c" . dante-eval-block)
         ("C-c C-t" . dante-type-at)
         ("C-c TAB" . dante-info))
  :hook (haskell-mode . dante-mode)
  :init (setf dante-mode-map (make-sparse-keymap))
  :config
  (with-eval-after-load 'company
    (setf (default-value 'company-backends)
          (delq #'dante-company (default-value 'company-backends)))))

;;; R

(use-package ess
  :ensure t
  :defer t
  :init
  (setf ess-write-to-dribble nil)
  (setf ess-history-directory (init--var "ess/history/"))
  (make-directory ess-history-directory t)

  (with-eval-after-load 'org
    (cl-pushnew '(R . t) org-babel-load-languages :test #'eq :key #'car))
  :config
  (setf ess-style 'RStudio)
  (setf ess-use-ido nil
        ess-use-flymake nil)
  (setf ess-ask-for-ess-directory nil
        ess-auto-width 'window
        inferior-R-args "--no-save"))

(use-package ess-inf
  :defer t
  :after ess
  :bind (:map inferior-ess-mode-map ("C-c a a" . ess-quit)))

(use-package ess-r-mode
  :defer t
  :after ess
  :bind (;; -
         :map ess-r-mode-map
         ("C-c a a" . run-ess-r)
         ("C-c a m" . ess-rdired)
         (";" . ess-cycle-assign)
         :map inferior-ess-r-mode-map
         ("C-c a m" . ess-rdired)
         (";" . ess-cycle-assign)))

;;; SGML

(use-package sgml-mode
  :defer t
  :config (unbind-key "C-c C-v" sgml-mode-map))

(use-package html-snippets
  :load-path "lisp"
  :defer t
  :commands html-snippets-file-template
  :init
  (with-eval-after-load 'autoinsert
    (define-auto-insert '(html-mode . "HTML file template")
      #'html-snippets-file-template)))

(use-package nxml-mode
  :defer t
  :config
  (setf nxml-slash-auto-complete-flag t)
  (setf nxml-attribute-indent 2))

;; Serve files
(use-package simple-httpd
  :ensure t
  :defer t
  :bind ("C-c m s" . httpd-serve-directory)
  :init
  (setf httpd-host 'local
        httpd-port 8017))

;;; CSS

(use-package css-mode
  :defer t
  :bind (:map css-mode-map ("C-c C-f" . css-cycle-color-format))
  :init (setf css-mode-map (make-sparse-keymap))
  :config (setf css-indent-offset 2))

;;; JavaScript & TypeScript

(use-package js
  :defer t
  :config
  (setf js-indent-level 2
        js-switch-indent-offset 2
        js-chain-indent nil)

  (dolist (key '("C-c M-:" "C-c C-j" "C-M-x"))
    (unbind-key key js-mode-map)))

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.jsm?\\'"
  :interpreter "node"
  :config
  (setf js2-skip-preprocessor-directives t)
  (setf js2-highlight-level 3
        js2-highlight-external-variables nil)
  (setf js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-missing-semi-warning nil))

(use-package typescript-mode
  :ensure t
  :defer t
  :config (setf typescript-indent-level 2))

(use-package tide
  :ensure t
  :defer t
  :hook ((js2-mode typescript-mode) . init--enable-tide)
  :init
  (setf tide-completion-setup-company-backend nil)

  (defun init--enable-tide ()
    (unless (file-remote-p default-directory)
      (tide-setup)))
  :config (setf tide-completion-enable-autoimport-suggestions nil))

(use-package skewer-mode
  :ensure t
  :defer t
  :bind (:map skewer-mode-map ("C-c a a" . run-skewer))
  :hook (js2-mode . skewer-mode))

(use-package skewer-repl
  :defer t
  :after skewer-mode
  :bind (:map skewer-mode-map ("C-c a m" . skewer-repl)))

(use-package skewer-css
  :ensure skewer-mode
  :defer t
  :bind (:map skewer-css-mode-map ("C-c a a" . run-skewer))
  :hook (css-mode . skewer-css-mode))

(use-package skewer-html
  :ensure skewer-mode
  :defer t
  :bind (:map skewer-html-mode-map ("C-c a a" . run-skewer))
  :hook (html-mode . skewer-html-mode))

(use-package flycheck-npm
  :load-path "lisp"
  :ensure flycheck
  :defer t
  :hook ((html-mode css-mode js2-mode typescript-mode) . flycheck-npm-setup))

;;; Python

(use-package python
  :defer t
  :bind (:map python-mode-map ("C-c a a" . run-python))
  :init
  (with-eval-after-load 'org
    (cl-pushnew '(python . t) org-babel-load-languages :test #'eq :key #'car))
  :config
  (setf python-indent-guess-indent-offset-verbose nil
        python-indent-offset 4)

  (defun init--setup-python-mode ()
    (setf fill-column 79))
  (add-hook 'python-mode-hook #'init--setup-python-mode)

  (defun init--setup-inferior-python-mode ()
    (kill-local-variable 'comint-prompt-read-only))
  (add-hook 'inferior-python-mode-hook #'init--setup-inferior-python-mode))

(use-package anaconda-mode
  :ensure t
  :defer t
  :bind (;; -
         :map anaconda-mode-map
         ([remap python-describe-at-point] . anaconda-mode-show-doc)
         ([remap xref-find-definitions] . anaconda-mode-find-definitions)
         ([remap xref-find-references] . anaconda-mode-find-references)
         ("C-c C-v" . anaconda-mode-find-assignments))
  :hook (python-mode . init--enable-anaconda)
  :init
  (setf anaconda-mode-map (make-sparse-keymap))
  (setf anaconda-mode-installation-directory (init--var "anaconda-mode/"))

  (defun init--enable-anaconda ()
    (unless (file-remote-p default-directory)
      (anaconda-mode)
      (anaconda-eldoc-mode)))
  :config (setf anaconda-mode-eldoc-as-single-line t))

(use-package company-anaconda
  :ensure t
  :defer t)

(use-package conda
  :load-path "lisp"
  :defer t
  :bind (("C-c a p" . conda-activate)
         ("C-c a P" . conda-activate-default)
         ("C-c a o" . conda-deactivate)))

(use-package pip-requirements
  :ensure t
  :defer t)

;;; Shell script

(use-package sh-script
  :defer t
  :bind (:map sh-mode-map ("C-c a a" . sh-show-shell))
  :init
  (setf sh-shell-file "/bin/bash")

  (with-eval-after-load 'org
    (cl-pushnew '(shell . t) org-babel-load-languages :test #'eq :key #'car))
  :config (setf sh-basic-offset 2))

;;; Lua

(use-package lua-mode
  :ensure t
  :defer t
  :config (setf lua-indent-level 2))

;;; Text

(use-package text-mode
  :defer t
  :mode ("/LICENSE\\'" "/UNLICENSE\\'")
  :config
  (with-eval-after-load 'autoinsert
    (define-auto-insert '("/UNLICENSE\\'" . "The Unlicense")
      "unlicense")))

;;; LaTeX

(use-package tex-site
  :ensure auctex)

(use-package tex
  :defer t
  :after tex-site
  :bind (;; -
         :map TeX-mode-map
         ([remap TeX-complete-symbol] . completion-at-point)
         ("M-g L" . TeX-error-overview))
  :hook ((TeX-mode . TeX-PDF-mode)
         (TeX-mode . TeX-source-correlate-mode))
  :config
  (setf (default-value 'TeX-master) nil
        (default-value 'TeX-engine) 'luatex
        TeX-parse-self t
        TeX-auto-save t)
  (setf TeX-electric-sub-and-superscript t)
  (setf TeX-source-correlate-method 'synctex)
  (setf TeX-clean-confirm nil)
  (setf TeX-command-list
        '(("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
          ("TeXcount" "texcount -unicode -inc %t" TeX-run-shell nil
           (latex-mode) :help "Run TeXcount")
          ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil (latex-mode)
           :help "Check LaTeX file for common mistakes")
          ("Clean" "TeX-clean" TeX-run-function nil t
           :help "Delete generated intermediate files")
          ("Clean All" "(TeX-clean t)" TeX-run-function nil t
           :help "Delete generated intermediate and output files")
          ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))

(use-package tex-buf
  :defer t
  :after tex-site
  :config (setf TeX-save-query nil))

(use-package tex-fold
  :defer t
  :after tex-site
  :bind (:map TeX-fold-keymap ("\\" . prettify-symbols-mode))
  :hook (TeX-mode . TeX-fold-mode))

(use-package tex-style
  :defer t
  :after tex-site
  :config
  (setf LaTeX-csquotes-open-quote "\\enquote{"
        LaTeX-csquotes-close-quote "}"))

(use-package latex
  :defer t
  :after tex-site
  :bind (;; -
         :map LaTeX-math-keymap
         ("o" . LaTeX-math-frac))
  :hook (LaTeX-mode . LaTeX-math-mode)
  :config
  (setf LaTeX-babel-hyphen nil)

  (defun init--setup-LaTeX-mode ()
    (make-local-variable 'TeX-electric-math)
    (setf TeX-electric-math '("\\(" . "\\)")))
  (add-hook 'LaTeX-mode-hook #'init--setup-LaTeX-mode))

(use-package preview
  :defer t
  :after tex-site
  :config (setf preview-auto-cache-preamble nil))

(use-package auctex-latexmk
  :load-path "lisp"
  :after latex
  :config (auctex-latexmk-setup))

(use-package latex-snippets
  :load-path "lisp"
  :defer t
  :commands latex-snippets-file-template
  :init
  (with-eval-after-load 'autoinsert
    (define-auto-insert '(latex-mode . "LaTeX file template")
      #'latex-snippets-file-template)))

(use-package reftex
  :defer t
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setf reftex-plug-into-AUCTeX t)
  ;; Offer a guess but ask for confirmation
  (setf reftex-insert-label-flags '(t t))
  (setf reftex-cite-format 'biblatex))

(use-package reftex-toc
  :defer t
  :config (unbind-key "d" reftex-toc-mode-map))

(use-package bibtex
  :defer t
  :bind (:map bibtex-mode-map ("M-g L" . bibtex-validate))
  :init (setf bibtex-dialect 'biblatex)
  :config
  (setf bibtex-align-at-equal-sign t)
  (setf bibtex-entry-format '(opts-or-alts
                              required-fields
                              numerical-fields
                              whitespace
                              realign
                              last-comma
                              delimiters
                              unify-case
                              braces
                              strings
                              sort-fields))
  (setf bibtex-autokey-year-length 4
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-titlewords 1
        bibtex-autokey-titleword-length 10)

  (unbind-key "C-c $" bibtex-mode-map))

;;; Ledger

(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (setf ledger-default-date-format ledger-iso-date-format
        ledger-post-amount-alignment-at :decimal
        ledger-copy-transaction-insert-blank-line-after t)
  (setf ledger-schedule-file (init--sync "ledger/schedule"))
  (setf ledger-report-resize-window nil
        ledger-report-use-header-line t)
  (setf ledger-reports
        (mapcar
         (lambda (report)
           (list (car report)
                 (format "%s %s" "%(binary) -f %(ledger-file)" (cdr report))))
         '(("On hand" . "balance Assets Liabilities")
           ("Account" . "register %(account)")
           ("Expenses (weekly)" . "register Expenses -W")
           ("Expenses (monthly)" . "register Expenses -M")
           ("Cash flow (this month)" . "balance Income Expenses --invert -p %(month)")))))

(use-package flycheck-ledger
  :ensure t
  :after flycheck
  :config
  (setf flycheck-ledger-pedantic t
        flycheck-ledger-explicit t))

;;; Org

(use-package org
  :defer t
  :init
  (make-directory (init--var "org/") t)
  (setf org-babel-load-languages nil)
  :config
  (setf org-directory (init--sync "org/"))
  (make-directory org-directory t)

  (setf org-default-notes-file (expand-file-name "inbox.org" org-directory)
        org-agenda-files `(,org-directory))
  (setf org-archive-location ".archive.org::")

  (setf org-adapt-indentation nil)
  (setf org-special-ctrl-a/e t
        org-catch-invisible-edits 'show-and-error)
  (setf org-startup-folded 'content)
  (setf org-image-actual-width '(300))
  (setf org-highlight-latex-and-related '(latex entities))
  (setf org-use-sub-superscripts '{})

  (setf org-modules '(org-id ol-docview ol-eww org-habit))
  (setf org-export-backends '(ascii html latex))
  (setf org-file-apps '((auto-mode . emacs)
                        (directory . emacs)))

  (setf org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w)" "|" "CANCELLED(c)")))
  (setf org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)
  (setf org-log-into-drawer t
        org-log-done 'time)

  (setf org-tag-persistent-alist '(("note" . ?n)
                                   ("project" . ?p)))
  (setf org-tags-exclude-from-inheritance '("project"))

  (defun init--load-org-babel-languages ()
    "Load all Org Babel languages once."
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
    ;; Only load languages once
    (remove-hook 'org-mode-hook #'init--load-org-babel-languages))
  (add-hook 'org-mode-hook #'init--load-org-babel-languages))

(use-package org-goto
  :defer t
  :config (setf org-goto-interface 'outline-path-completion))

(use-package org-refile
  :defer t
  :config
  (setf org-refile-targets '((nil . (:maxlevel . 5))
                             (org-agenda-files . (:maxlevel . 5)))
        org-refile-allow-creating-parent-nodes 'confirm)
  (setf org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))

(use-package org-archive
  :defer t
  :config
  (setf org-archive-default-command #'org-archive-set-tag)
  (setf org-archive-file-header-format nil))

(use-package org-id
  :defer t
  :init (setf org-id-locations-file (init--var "org/id-locations")))

(use-package org-duration
  :defer t
  :config (setf org-duration-format 'h:mm))

(use-package org-lint
  :defer t
  :after org
  :bind (:map org-mode-map ("M-g L" . org-lint)))

;; Link
(use-package ol
  :defer t
  :bind ("C-c o l" . org-store-link))

;; Capture
(use-package org-capture
  :defer t
  :bind ("C-c o c" . org-capture)
  :config
  (setf org-capture-templates
        '(("t" "Task" entry (file+headline "" "Tasks")
           "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i"
           :empty-lines 1)
          ("n" "Note" entry (file+headline "" "Notes")
           "* %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i"
           :empty-lines 1))))

;; Agenda
(use-package org-agenda
  :defer t
  :bind ("C-c o a" . org-agenda)
  :config
  (setf org-agenda-window-setup 'only-window
        org-agenda-restore-windows-after-quit t)
  (setf org-agenda-block-separator "")
  (setf org-agenda-span 'day
        org-agenda-start-on-weekday 0)
  (setf org-stuck-projects '("+project/-DONE-CANCELLED" ("NEXT") nil "")))

;; Clock
(use-package org-clock
  :defer t
  :init
  (setf org-clock-persist-file (init--var "org/clock-save.el"))
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate))
  :config
  (setf org-clock-out-remove-zero-time-clocks t)
  (setf org-clock-persist 'history)
  (setf org-clock-task-overrun-text "*"
        org-clock-clocked-in-display 'frame-title)
  (setf org-show-notification-timeout 10)

  (defun init--confirm-exit-when-clocking ()
    "Ask for confirmation on exit with a running clock."
    (or (not (org-clocking-p))
        (progn
          (org-clock-goto)
          (yes-or-no-p "A running clock exists; exit anyway? "))))
  (add-hook 'kill-emacs-query-functions #'init--confirm-exit-when-clocking))

(use-package org-mru-clock
  :ensure t
  :defer t
  :bind (("C-c o i" . org-mru-clock-in)
         ("C-c o u" . org-mru-clock-select-recent-task)))

;; Source code
(use-package org-src
  :defer t
  :config
  (setf org-src-preserve-indentation t)
  (setf org-src-window-setup 'other-window))

(use-package ob-http
  :ensure t
  :defer t
  :after org
  :init (cl-pushnew '(http . t) org-babel-load-languages :test #'eq :key #'car))

;; Export
(use-package ox
  :defer t
  :config (setf org-export-coding-system 'utf-8))

(use-package ox-latex
  :defer t
  :config
  (setf org-latex-compiler "lualatex"
        org-latex-pdf-process '("latexmk %f"))

  (setf org-latex-listings t)
  (push '("" "listings") org-latex-packages-alist)
  (push '("" "color") org-latex-packages-alist)

  (push '("koma-article"
          "\\documentclass[11pt]{scrartcl}"
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        org-latex-classes))

(use-package ox-html
  :defer t
  :config
  (setf org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-preamble nil
        org-html-postamble nil
        org-html-htmlize-output-type 'css
        org-html-head-include-default-style nil
        org-html-head (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
                              (concat "file://" (init--etc "css/org.css")))))

;;; Markdown

(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.Rmd\\'"
  :config
  (setf (default-value 'markdown-enable-math) t)
  (setf markdown-fontify-code-blocks-natively t)
  (setf markdown-max-image-size '(300 . nil))
  (setf markdown-css-paths `(,(concat "file://" (init--etc "css/pandoc.css")))
        markdown-command '("pandoc" "--section-divs" "--from=markdown" "--to=html5")))

;;; ReStructuredText

(use-package rst
  :defer t
  :bind (;; -
         :map rst-mode-map
         ("M-n" . rst-forward-section)
         ("M-p" . rst-backward-section)
         ("M-RET" . rst-insert-list)
         ("C-c C-j" . rst-insert-list)))

;;; Graphviz DOT

(use-package graphviz-dot-mode
  :load-path "lisp"
  :defer t
  :commands graphviz-dot-mode
  :mode ("\\.gv\\'" "\\.dot\\'")
  :init
  (with-eval-after-load 'org
    (push '("dot" . graphviz-dot) org-src-lang-modes)
    (cl-pushnew '(dot . t) org-babel-load-languages :test #'eq :key #'car)))

;;; GLSL

(use-package glsl-mode
  :ensure t
  :defer t
  :bind (;; -
         :map glsl-mode-map
         ("C-M-q" . c-indent-exp)
         ("C-c C-\\" . c-backslash-region)
         ("C-c C-n" . c-forward-conditional)
         ("C-c C-p" . c-backward-conditional)
         ("C-c C-u" . c-up-conditional)
         ([remap backward-sentence] . c-beginning-of-statement)
         ([remap forward-sentence] . c-end-of-statement))
  :init (setf glsl-mode-map (make-sparse-keymap)))

;;; Assembly

(use-package nasm-mode
  :ensure t
  :defer t
  :mode "\\.nasm\\'")

;;; BNF

(use-package bnf-mode
  :ensure t
  :defer t)

;;; SQL

(use-package sql
  :defer t
  :bind (;; -
         :map sql-mode-map
         ("C-c C-p" . sql-set-product)
         ("C-c a a" . sql-product-interactive))
  :init
  (with-eval-after-load 'org
    (cl-pushnew '(sqlite . t) org-babel-load-languages :test #'eq :key #'car)
    (cl-pushnew '(sql . t) org-babel-load-languages :test #'eq :key #'car))
  :config (setf sql-product 'sqlite))

(use-package sqlup-mode
  :ensure t
  :defer t
  :hook (sql-mode . sqlup-mode))

;;; Env file

(use-package dotenv-mode
  :ensure t
  :defer t)

;;; JSON

(use-package json-mode
  :ensure t
  :defer t)

;;; jq

(use-package jq-mode
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'org
    (cl-pushnew '(jq . t) org-babel-load-languages :test #'eq :key #'car))

  (with-eval-after-load 'json-mode
    (bind-key "C-c C-c" #'jq-interactively json-mode-map)))

;;; YAML

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (defun init--setup-yaml-mode ()
    (flyspell-mode 0)
    (auto-fill-mode 0)
    (abbrev-mode 0))
  (add-hook 'yaml-mode-hook #'init--setup-yaml-mode))

;;; CSV

(use-package csv-mode
  :ensure t
  :defer t)

;;; Protobuf

(use-package protobuf-mode
  :ensure t
  :defer t)

;;; GraphQL

(use-package graphql-mode
  :ensure t
  :defer t)

(use-package ob-graphql
  :ensure t
  :defer t
  :after org
  :init (cl-pushnew '(graphql . t) org-babel-load-languages :test #'eq :key #'car))

;;; init.el ends here
