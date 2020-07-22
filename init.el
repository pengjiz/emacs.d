;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

;;; Boot

(progn ; fundamental
  (setf gc-cons-threshold 100000000)
  (defun my-set-gc-cons-threshold ()
    "Set `gc-cons-threshold' to a normal value."
    (setf gc-cons-threshold 10000000))
  (add-hook 'emacs-startup-hook #'my-set-gc-cons-threshold)

  (setf load-prefer-newer t
        ad-redefinition-action 'accept
        debugger-stack-frame-as-list t)
  (setf create-lockfiles nil
        delete-by-moving-to-trash t)
  (setf (default-value 'mode-line-format) nil
        frame-title-format nil)
  (prefer-coding-system 'utf-8))

(progn ; package
  (require 'package)
  (setf package-enable-at-startup nil)
  (setf package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(progn ; requires
  (eval-when-compile
    (require 'cl-lib)
    (require 'subr-x)
    (require 'rx)
    (require 'use-package))
  (require 'bind-key))

(progn ; helpers
  (defconst my-etc-directory
    (expand-file-name (convert-standard-filename "etc/")
                      user-emacs-directory)
    "The directory where packages put their configuration files.")
  (defconst my-var-directory
    (expand-file-name (convert-standard-filename "var/")
                      user-emacs-directory)
    "The directory where packages put their persistent data files.")
  (defconst my-sync-directory
    (expand-file-name (convert-standard-filename "Sync/")
                      "~")
    "The directory where files are synchronized among machines.")
  (make-directory my-etc-directory t)
  (make-directory my-var-directory t)
  (make-directory my-sync-directory t)

  (defun my-expand-etc-file-name (file)
    "Expand FILE relative to `my-etc-directory'."
    (expand-file-name (convert-standard-filename file)
                      my-etc-directory))

  (defun my-expand-var-file-name (file)
    "Expand FILE relative to `my-var-directory'."
    (expand-file-name (convert-standard-filename file)
                      my-var-directory))

  (defun my-expand-sync-file-name (file)
    "Expand FILE relative to `my-sync-directory'."
    (expand-file-name (convert-standard-filename file)
                      my-sync-directory))

  ;; NOTE: To manipulate non-interactive functions we need to use
  ;; non-interactive functions, so that they will not become interactive.
  (defun my-ignore (&rest _)
    "Do nothing and return nil."
    nil))

(progn ; startup
  (setf inhibit-default-init t
        inhibit-startup-screen t
        inhibit-startup-buffer-menu t
        initial-buffer-choice t
        initial-scratch-message nil
        initial-major-mode #'fundamental-mode)
  ;; NOTE: A non-nil value for this variable will trigger some weird logic. So
  ;; we always keep it nil and modify the function instead.
  (setf inhibit-startup-echo-area-message nil)
  (unless (daemonp)
    (setf (symbol-function 'display-startup-echo-area-message) #'my-ignore)))

(progn ; user interface
  (when (fboundp #'tool-bar-mode) (tool-bar-mode 0))
  (when (fboundp #'scroll-bar-mode) (scroll-bar-mode 0))
  (menu-bar-mode 0)
  (tooltip-mode 0)
  (blink-cursor-mode 0)
  (setf ring-bell-function #'ignore
        echo-keystrokes 0.25
        use-dialog-box nil
        (symbol-function 'yes-or-no-p) #'y-or-n-p
        mode-line-default-help-echo nil
        frame-resize-pixelwise t
        window-resize-pixelwise t
        (default-value 'indicate-empty-lines) t
        (default-value 'cursor-in-non-selected-windows) nil
        visible-cursor nil
        x-stretch-cursor t))

;;; Initialization

(progn ; general customization
  ;; Use the custom file to store machine-specific settings
  (setf custom-file (my-expand-var-file-name "custom.el"))
  (load custom-file t t t)

  (dolist (key '(;; compose mail
                 "C-x m"
                 "C-x 4 m"
                 ;; tmm
                 "M-`"
                 ;; suspend frame
                 "C-z"
                 "C-x C-z"
                 ;; zap to char
                 "M-z"
                 ;; back to indentation
                 "M-m"
                 ;; upcase & downcase region
                 "C-x C-u"
                 "C-x C-l"
                 ;; count words
                 "M-="
                 ;; ispell word
                 "M-$"))
    (unbind-key key)))

(use-package hydra
  :ensure t
  :defer t
  :config
  (setf lv-use-separator t)
  (setf hydra-look-for-remap t))

(use-package transient
  :ensure t
  :defer t
  :init
  (setf transient-history-file (my-expand-var-file-name "transient/history.el")
        transient-levels-file (my-expand-var-file-name "transient/levels.el")
        transient-values-file (my-expand-var-file-name "transient/values.el")))

(use-package alert
  :ensure t
  :defer t
  :bind ("C-c o r" . my-set-reminder)
  :init
  (setf alert-default-style 'libnotify)

  (defun my-set-reminder (min msg)
    "Show MSG after MIN minutes."
    (interactive (list (read-number "Minutes: " 10)
                       (read-string "Message: ")))
    (run-with-timer (* min 60) nil #'alert msg :title "Reminder"))
  :config (setf alert-fade-time 15))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t)
  (push '(font . "Source Code Pro-12") default-frame-alist))

(use-package url
  :defer t
  :init (setf url-configuration-directory (my-expand-var-file-name "url/")))

(use-package url-cache
  :defer t
  :init (setf url-cache-directory (my-expand-var-file-name "url/cache/")))

(use-package request
  :ensure t
  :defer t
  :init (setf request-storage-directory (my-expand-var-file-name "request/")))

(use-package server
  :when (display-graphic-p)
  :config
  (unless (server-running-p)
    (server-start)))

;; Use Emacs as EDITOR
(use-package with-editor
  :ensure t
  :hook ((shell-mode eshell-mode) . my-export-editor)
  :init
  (defun my-export-editor ()
    "Call `with-editor-export-editor' but suppress messages."
    (let ((inhibit-message t))
      (with-editor-export-editor)))

  (shell-command-with-editor-mode))

;;; General utility

(progn ; disabled commands
  (put #'narrow-to-defun 'disabled nil)
  (put #'narrow-to-region 'disabled nil)
  (put #'narrow-to-page 'disabled nil)
  (put #'erase-buffer 'disabled nil)

  ;; A less intrusive way to handle invocation of disabled commands
  (defun my-show-disabled-command (&optional command _)
    "Show a message for disabled COMMAND without invoking it."
    (message "Disabled command %s invoked" (or command this-command)))
  (setf disabled-command-function #'my-show-disabled-command))

(use-package simple
  :defer t
  :bind (([remap just-one-space] . cycle-spacing)
         ([remap downcase-word] . downcase-dwim)
         ([remap capitalize-word] . capitalize-dwim)
         ([remap upcase-word] . upcase-dwim)
         ("C-c b e" . erase-buffer)
         ("C-c b c" . clone-indirect-buffer-other-window)
         ("C-c t v" . visual-line-mode)
         ("C-c t l" . toggle-truncate-lines)
         ("C-c t q" . auto-fill-mode)
         ("C-c a l" . list-processes)
         ("M-g n" . hydra-errors/next-error)
         ("M-g p" . hydra-errors/previous-error))
  :hook ((text-mode bibtex-mode) . auto-fill-mode)
  :init
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode)

  (defhydra hydra-errors ()
    "Errors"
    ("p" previous-error "previous")
    ("n" next-error "next")
    ("<" first-error "first"))
  :config
  (setf kill-do-not-save-duplicates t
        save-interprogram-paste-before-kill t)
  (setf set-mark-command-repeat-pop t)
  (setf shell-command-dont-erase-buffer 'end-last-out
        async-shell-command-display-buffer nil))

(use-package simple-extras
  :load-path "lisp"
  :config
  (dolist (hook '(prog-mode-hook protobuf-mode-hook))
    (add-hook hook #'simple-extras-auto-fill-comments-mode))

  (bind-keys ("C-c e r" . simple-extras-eval-last-sexp-and-replace)
             ([remap move-beginning-of-line] . simple-extras-move-beginning-of-line)
             :map visual-line-mode-map
             ([remap move-beginning-of-line] . simple-extras-move-beginning-of-visual-line))

  (defvar ess-roxy-mode-map)
  (with-eval-after-load 'ess-roxy
    (bind-key [remap move-beginning-of-line] #'simple-extras-move-beginning-of-ess-line
              ess-roxy-mode-map))

  (simple-extras-setup))

(use-package simple-snippets
  :load-path "lisp"
  :defer t
  :commands (simple-snippets-email simple-snippets-time)
  :init
  (with-eval-after-load 'text-mode
    (define-abbrev text-mode-abbrev-table ";email"
      "" #'simple-snippets-email :system t)
    (define-abbrev text-mode-abbrev-table ";time"
      "" #'simple-snippets-time :system t)))

(use-package undo-propose
  :ensure t
  :defer t
  :bind ("C-x u" . undo-propose))

(use-package delsel
  :config (delete-selection-mode))

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

(use-package unfill
  :ensure t
  :defer t
  :bind ([remap fill-paragraph] . unfill-toggle)
  :init
  (defvar c-mode-base-map)
  (with-eval-after-load 'cc-mode
    (bind-key [remap c-fill-paragraph] #'unfill-toggle c-mode-base-map)))

(use-package move-dup
  :ensure t
  :defer t
  :bind (("M-<up>" . md-move-lines-up)
         ("M-<down>" . md-move-lines-down)
         ("C-M-<up>" . md-duplicate-up)
         ("C-M-<down>" . md-duplicate-down)))

(use-package swap-regions
  :ensure t
  :defer t
  :bind ("C-c x t" . swap-regions))

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

(use-package change-language
  :load-path "lisp"
  :defer t
  :bind ("C-c t m" . change-language))

(use-package expand-region
  :ensure t
  :defer t
  :bind* ("C-=" . er/expand-region))

(use-package wgrep
  :ensure t
  :defer t
  :config (setf wgrep-auto-save-buffer t))

(use-package autorevert
  :config
  (setf global-auto-revert-non-file-buffers t)
  (setf auto-revert-verbose nil)
  (global-auto-revert-mode))

(use-package saveplace
  :init (setf save-place-file (my-expand-var-file-name "places"))
  :config (save-place-mode))

(use-package savehist
  :init (setf savehist-file (my-expand-var-file-name "savehist"))
  :config
  (setf savehist-additional-variables '(search-ring regexp-search-ring))
  (setf savehist-autosave-interval 60)
  (savehist-mode))

(use-package re-builder
  :defer t
  :bind ("C-c m r" . re-builder)
  :config (setf reb-re-syntax 'string))

(use-package calc
  :defer t
  :bind (([remap calc-dispatch] . quick-calc)
         ("C-c x c" . calc-grab-region)
         ("C-c x C" . calc-grab-rectangle)
         ("C-c m c" . calc))
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
  :no-require t
  :config (setf help-window-select t))

(use-package info
  :defer t
  :bind ("C-c m i" . info))

(use-package man
  :defer t
  :bind ("C-c m k" . man)
  :config
  ;; Select man buffers after it is displayed
  (setf Man-notify-method 'aggressive))

(use-package edit-indirect
  :ensure t
  :defer t)

(use-package poporg
  :ensure t
  :defer t
  :bind ("C-c x o" . poporg-dwim))

(use-package interleave
  :ensure t
  :defer t
  :init
  (setf interleave-org-notes-dir-list '("."))

  (with-eval-after-load 'org
    (bind-key "C-c o p" #'interleave-mode org-mode-map))

  (with-eval-after-load 'doc-view
    (bind-key "C-c o p" #'interleave-open-notes-file-for-pdf
              doc-view-mode-map)))

(use-package htmlize
  :ensure t
  :defer t)

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
  :hook ((TeX-update-style . whitespace-mode)
         ((prog-mode
           protobuf-mode
           text-mode
           bibtex-mode
           conf-mode)
          . my-enable-whitespace))
  :init
  (defun my-enable-whitespace ()
    (unless (bound-and-true-p TeX-mode-p)
      (add-hook 'hack-local-variables-hook #'whitespace-mode nil t)))
  :config
  (setf whitespace-style '(face
                           indentation
                           space-after-tab
                           space-before-tab
                           tab-mark
                           trailing
                           lines-tail)
        whitespace-line-column nil)

  (defun my-set-whitespaces-to-clean (fn &rest args)
    "Apply FN on ARGS but explicitly set whitespaces to clean."
    (let ((whitespace-style '(empty
                              indentation
                              space-before-tab
                              space-after-tab
                              trailing)))
      (apply fn args)))
  (advice-add #'whitespace-cleanup :around #'my-set-whitespaces-to-clean)
  (advice-add #'whitespace-cleanup-region :around #'my-set-whitespaces-to-clean))

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
  :hook ((prog-mode protobuf-mode bibtex-mode) . my-enable-hideshow)
  :init
  (setf hs-minor-mode-map (make-sparse-keymap))

  ;; NOTE: Workaround an issue with Idris prover. I do not actually use the
  ;; prover but the script mode is anyway activated when quitting the process
  ;; and because it has no comment start and end so hideshow activation will
  ;; fail and terminate the quitting process.
  (defun my-enable-hideshow ()
    (unless (eq major-mode 'idris-prover-script-mode)
      (hs-minor-mode))))

(use-package outline
  :defer t
  :bind (;; -
         :map outline-minor-mode-map
         ("C-c @ SPC" . outline-mark-subtree)
         ("C-c @ C-p" . outline-previous-visible-heading)
         ("C-c @ C-n" . outline-next-visible-heading)
         ("C-c @ C-b" . outline-backward-same-level)
         ("C-c @ C-f" . outline-forward-same-level)
         ("C-c @ p" . hydra-outline/outline-previous-visible-heading)
         ("C-c @ n" . hydra-outline/outline-next-visible-heading)
         ("C-c @ b" . hydra-outline/outline-backward-same-level)
         ("C-c @ f" . hydra-outline/outline-forward-same-level))
  :hook ((prog-mode protobuf-mode TeX-mode) . outline-minor-mode)
  :init
  (setf outline-minor-mode-map (make-sparse-keymap))

  (defhydra hydra-outline ()
    "Outline"
    ("p" outline-previous-visible-heading "previous")
    ("n" outline-next-visible-heading "next")
    ("b" outline-backward-same-level "backward")
    ("f" outline-forward-same-level "forward")))

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
          TeX-update-style
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
  (setf split-window-preferred-function #'visual-fill-column-split-window-sensibly)
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
  :bind* (("C-'" . avy-goto-char)
          ("C-;" . avy-goto-char-2))
  :bind (("M-m" . avy-goto-char-in-line)
         ("M-g g" . avy-goto-line)
         ("C-z" . avy-resume)
         :map isearch-mode-map
         ([remap avy-goto-char] . avy-isearch)
         ([remap avy-goto-char-2] . avy-isearch))
  :config
  ;; NOTE: Set this to nil will enable smart case search.
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
  :config (setf imenu-auto-rescan t))

(use-package imenu-anywhere
  :ensure t
  :defer t
  :bind ("M-g I" . imenu-anywhere))

(use-package dumb-jump
  :ensure t
  :defer t
  :after xref
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t))

;;; Search & replace

(use-package isearch
  :defer t
  :no-require t
  :config (setf isearch-allow-scroll t))

(use-package swiper
  :ensure t
  :defer t
  :bind (("M-s j" . swiper)
         :map swiper-map
         ([remap avy-goto-char] . swiper-avy)
         ([remap avy-goto-char-2] . swiper-avy)
         :map isearch-mode-map
         ([remap swiper] . swiper-from-isearch)))

(use-package web-search
  :load-path "lisp"
  :defer t
  :commands web-search-mdn-css
  :bind (("M-s b g" . web-search-google)
         ("M-s b w" . web-search-wikipedia)
         ("M-s b d" . web-search-wiktionary)
         ("M-s b c" . web-search-github))
  :init
  (with-eval-after-load 'css-mode
    (setf (symbol-function 'css-lookup-symbol) #'web-search-mdn-css)))

;;; Buffer

;; Protect a few special buffers
(progn ; special buffers
  (defun my-protect-special-buffers ()
    "Protect special buffers from being killed."
    (or (not (member (buffer-name (current-buffer))
                     '("*scratch*" "*Messages*")))
        (ignore (bury-buffer))))
  (add-hook 'kill-buffer-query-functions #'my-protect-special-buffers))

;; Unique buffer name
(use-package uniquify
  :config
  (setf uniquify-buffer-name-style 'forward)
  (setf uniquify-after-kill-buffer-p t))

(progn ; recursive minibuffers
  (setf enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode))

(use-package ibuffer
  :defer t
  :bind (([remap list-buffers] . ibuffer)
         :map ibuffer-mode-map
         ("C-c C-o" . ibuffer-visit-buffer-1-window))
  :config
  (setf ibuffer-use-other-window t)
  (setf ibuffer-expert t)

  ;; Human-readable buffer size
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  (setf ibuffer-formats
        '((mark modified read-only " "
                (name 22 22 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename)
          (mark modified read-only " "
                (name 22 22 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                process))))

(use-package ibuf-ext
  :defer t
  :config (setf (default-value 'ibuffer-show-empty-filter-groups) nil))

(use-package ibuffer-vc
  :ensure t
  :defer t
  :hook (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root)
  :config
  (setf ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                vc-relative-file)
          (mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                process))))

;;; File

(use-package files
  :bind (("C-c f g" . revert-buffer)
         ("C-c b g" . revert-buffer)
         ("C-c b r" . rename-buffer)
         ("C-c b R" . rename-uniquely))
  :config
  (setf auto-save-list-file-prefix (my-expand-var-file-name "auto-save/saves-")
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (setf backup-by-copying t
        delete-old-versions t
        version-control t
        backup-directory-alist `(("." . ,(my-expand-var-file-name "backups/"))))

  (setf view-read-only t)
  (setf save-abbrevs 'silently)
  (setf require-final-newline t)

  (setf confirm-nonexistent-file-or-buffer t)
  (defun my-create-parent-directory ()
    "Create the parent directory if it is non-existent."
    (make-directory (file-name-directory buffer-file-name) t))
  (add-hook 'find-file-not-found-functions #'my-create-parent-directory))

(use-package files-x
  :defer t
  :bind (("C-c f v" . add-file-local-variable)
         ("C-c f V" . add-file-local-variable-prop-line)
         ("C-c f b" . add-dir-local-variable)))

(use-package files-extras
  :load-path "lisp"
  :defer t
  :bind (("C-c f w" . files-extras-copy-filename)
         ("C-c f l" . files-extras-list-files)))

(use-package ffap
  :defer t
  :config (setf ffap-machine-p-known 'reject))

;; Automatically set +x for scripts
(use-package executable
  :defer t
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; Automatically update timestamps
(use-package time-stamp
  :defer t
  :hook (before-save . time-stamp)
  :config (setf time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z"))

(use-package tramp
  :defer t
  :init
  (setf tramp-persistency-file-name (my-expand-var-file-name "tramp/persistency")
        tramp-auto-save-directory (my-expand-var-file-name "tramp/auto-save/")
        tramp-backup-directory-alist backup-directory-alist
        ;; Do not save tramp history
        tramp-histfile-override t)
  (setf tramp-default-method "ssh"))

(use-package recentf
  :init
  (setf recentf-save-file (my-expand-var-file-name "recentf"))
  (setf recentf-auto-cleanup 120)
  :config
  (setf recentf-max-saved-items 50)
  (setf recentf-exclude '("/elpa/" "/var/" "/\\.git/" "/Trash/"))
  (recentf-mode))

(use-package bookmark
  :defer t
  :init (setf bookmark-default-file (my-expand-var-file-name "bookmarks"))
  :config
  (unless (file-exists-p bookmark-default-file)
    (dolist (item '(("finances" . "ledger/finances.ledger")
                    ("bookcase" . "bookcase/inventory.org")))
      (let ((filename (my-expand-sync-file-name (cdr item))))
        (when (file-exists-p filename)
          (cl-pushnew `(,(car item) . ((filename . ,filename))) bookmark-alist
                      :test #'equal :key #'car))))))

(use-package dired
  :defer t
  :bind (([remap list-directory] . dired)
         :map dired-mode-map
         ("e" . browse-url-of-dired-file)
         ("K" . dired-kill-subdir)
         ([remap dired-up-directory] . my-dired-up-directory))
  :config
  (setf dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  (setf dired-auto-revert-buffer t)
  (setf dired-dwim-target t)
  (setf dired-listing-switches "-alhFv --group-directories-first"
        dired-ls-F-marks-symlinks t)
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
  (put #'dired-find-alternate-file 'disabled nil)

  (dolist (key '("c" "Z" "P"))
    (unbind-key key dired-mode-map))

  (defun my-dired-up-directory ()
    "Go to the parent directory."
    (interactive)
    (let ((buffer (current-buffer)))
      (dired-up-directory)
      (unless (or (eq buffer (current-buffer))
                  (get-buffer-window buffer t))
        (kill-buffer buffer)))))

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
  (setf dired-omit-verbose nil)
  (setf dired-omit-files "\\`[#.]\\|[#~]\\'"
        dired-omit-extensions nil))

;; More colors in Dired buffers
(use-package diredfl
  :ensure t
  :defer t
  :hook (dired-mode . diredfl-mode))

;; Narrow in Dired
(use-package dired-narrow
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; Show file git info
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
  (setf image-dired-dir (my-expand-var-file-name "image-dired/")
        image-dired-db-file (my-expand-var-file-name "image-dired/db")
        image-dired-gallery-dir (my-expand-var-file-name "image-dired/gallery/")
        image-dired-temp-image-file (my-expand-var-file-name "image-dired/temp")
        image-dired-temp-rotate-image-file (my-expand-var-file-name "image-dired/rotate-temp")))

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
  (setf auth-sources `(,(my-expand-sync-file-name "misc/authinfo.gpg")))
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

  ;; Manage how windows are displayed
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
           (reusable-frames . nil))
          ;; Help
          (,(rx bos (or "*Man"
                        "*Help"
                        "*TeX Help*"
                        "*Anaconda*"
                        "*tide-documentation*"
                        "*Racer Help*"
                        "*idris-holes*"
                        "*idris-info*"
                        "*cider-doc*"
                        "*cider-inspect*"
                        "*Geiser documentation*"))
           (display-buffer-reuse-window
            display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 15)
           (reusable-frames . nil))
          ;; Application
          (,(rx bos (or "Trash Can"
                        "*eshell"
                        "*Occur"
                        "*ivy-occur"
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
                        (and (1+ nonl) "Buffers*")))
           (display-buffer-reuse-window
            display-buffer-at-bottom)
           (window-height . 15)
           (preserve-size . (nil . t))
           (reusable-frames . nil))
          ;; Command
          (,(rx bos (or "*compilation"
                        "*dired-atool*"
                        "*Compile-Log*"
                        "*Backtrace*"
                        "*Warnings*"
                        "*Error*"
                        "*Shell Command Output*"
                        "*Async Shell Command*"
                        "*firestarter*"
                        "*Macroexpansion*"
                        "*Ledger Report*"
                        "*Gnuplot Trail*"
                        "*skewer-error*"
                        "*cider-error*"
                        "*cider-test-report*"
                        "*Geiser dbg*"
                        "*idris-notes*"
                        "*HsCompilation*"
                        (and (1+ nonl) " output*")))
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-height . 10)
           (preserve-size . (nil . t))
           (reusable-frames . nil))
          ;; Information
          (,(rx bos (or "*Local Variables*"
                        "*Reconcile*"
                        "*Fancy Diary Entries*"
                        "*Holidays*"
                        "*Phases of Moon*"
                        "*Quail Completions*"
                        "*Gnuplot Commands*"))
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
  :defer t
  :bind (("C-c w u" . hydra-winner/winner-undo)
         ("C-c w r" . hydra-winner/winner-redo))
  :init
  (setf winner-dont-bind-my-keys t)
  (winner-mode)

  (defhydra hydra-winner ()
    "Winner"
    ("u" winner-undo "undo")
    ("r" winner-redo "redo")))

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

;;; Completion

(use-package minibuffer
  :defer t
  :bind ([remap complete-symbol] . completion-at-point))

(use-package ivy
  :ensure t
  :defer t
  :bind (("M-z" . ivy-resume)
         :map ivy-minibuffer-map
         ([remap hydra-ivy/body] . ivy-dispatching-done))
  :init (ivy-mode)
  :config
  (setf ivy-count-format "(%d/%d) ")
  (setf ivy-use-selectable-prompt t
        ivy-wrap t)
  (setf ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'abbreviate)
  (setf ivy-read-action-format-function #'ivy-read-action-format-columns))

(use-package ivy-avy
  :ensure t
  :defer t
  :after ivy
  :bind (;; -
         :map ivy-minibuffer-map
         ([remap avy-goto-char] . ivy-avy)
         ([remap avy-goto-char-2] . ivy-avy)))

(use-package counsel
  :ensure t
  :defer t
  :after ivy
  :bind (("M-s s" . counsel-rg)
         ("C-c e o" . counsel-colors-web)
         ("C-c e O" . counsel-colors-emacs)
         ("C-c o j" . counsel-org-goto-all)
         :map counsel-ag-map
         ([remap avy-goto-char] . swiper-avy)
         ([remap avy-goto-char-2] . swiper-avy)
         :map counsel-grep-map
         ([remap avy-goto-char] . swiper-avy)
         ([remap avy-goto-char-2] . swiper-avy))
  :init
  (setf counsel-mode-override-describe-bindings t)
  (counsel-mode)

  (defvar company-active-map)
  (with-eval-after-load 'company
    (bind-key [remap company-filter-candidates] #'counsel-company
              company-active-map))

  (with-eval-after-load 'comint
    (bind-keys :map comint-mode-map
               ([remap comint-history-isearch-backward-regexp] . counsel-shell-history)
               ([remap comint-dynamic-list-input-ring] . counsel-shell-history)))

  (defvar inferior-ess-mode-map)
  (with-eval-after-load 'ess-inf
    (bind-key [remap ess-msg-and-comint-dynamic-list-input-ring] #'counsel-shell-history
              inferior-ess-mode-map))

  ;; NOTE: Due to an Eshell bug we cannot bind key in the mode map.
  (with-eval-after-load 'esh-mode
    (bind-keys ([remap eshell-previous-matching-input] . counsel-esh-history)
               ([remap eshell-list-history] . counsel-esh-history)))

  (with-eval-after-load 'outline
    (bind-key "M-g o" #'counsel-outline outline-minor-mode-map))

  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("M-g o" . counsel-outline)
               ([remap org-goto] . counsel-outline)))

  (defvar markdown-mode-map)
  (with-eval-after-load 'markdown-mode
    (bind-key "M-g o" #'counsel-outline markdown-mode-map))
  :config (setf counsel-find-file-ignore-regexp "\\`[#.]\\|[#~]\\'"))

(use-package counsel-extras
  :load-path "lisp"
  :after counsel
  :bind (("C-c f f" . counsel-extras-fd)
         ("C-c f j" . counsel-extras-fd))
  :config (counsel-extras-setup))

(use-package amx
  :ensure t
  :defer t
  :init (setf amx-save-file (my-expand-var-file-name "amx-items")))

(use-package ivy-xref
  :ensure t
  :defer t
  :after ivy
  :init (setf xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package company
  :ensure t
  :defer t
  :bind (;; -
         :map company-mode-map
         ([remap dabbrev-completion] . company-complete)
         :map company-active-map
         ([remap dabbrev-completion] . company-other-backend)
         ("C-j" . company-complete-selection))
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          c-mode
          c++-mode
          cmake-mode
          rust-mode
          haskell-mode
          idris-mode
          ess-r-mode
          python-mode
          html-mode
          css-mode
          js2-mode
          typescript-mode
          LaTeX-mode
          markdown-mode
          rst-mode
          org-mode
          ledger-mode)
         . my-enable-company)
  :init
  (setf company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend)
        company-backends '(company-files
                           company-dabbrev-code
                           company-dabbrev))

  (defun my-enable-company ()
    (when-let* ((backends (cond ((derived-mode-p 'emacs-lisp-mode
                                                 'clojure-mode
                                                 'rust-mode
                                                 'idris-mode
                                                 'css-mode
                                                 'ledger-mode)
                                 '(company-capf))
                                ((derived-mode-p 'c-mode 'c++-mode)
                                 '(company-c-headers
                                   company-etags))
                                ((derived-mode-p 'cmake-mode)
                                 '(company-cmake))
                                ((derived-mode-p 'js2-mode 'typescript-mode)
                                 '(company-tide))
                                ((derived-mode-p 'python-mode)
                                 '(company-anaconda))
                                ((derived-mode-p 'haskell-mode)
                                 '(dante-company))
                                ((derived-mode-p 'latex-mode)
                                 '((company-auctex-macros
                                    company-auctex-symbols
                                    company-auctex-environments)
                                   company-reftex-citations
                                   company-reftex-labels)))))
      (make-local-variable 'company-backends)
      (dolist (backend (nreverse backends))
        (push backend company-backends)))
    (company-mode))
  :config
  (setf company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t))

(use-package company-dabbrev
  :defer t
  :after company
  :config
  (setf company-dabbrev-ignore-case nil
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
  :commands word-complete
  :bind (([remap ispell-complete-word] . word-complete)
         ("C-c e d" . word-complete))
  :init
  (with-eval-after-load 'org
    (bind-key [remap pcomplete] #'word-complete org-mode-map)))

;;; Expansion

(use-package abbrev
  :defer t
  :bind (("C-c t r" . abbrev-mode)
         ("C-c e e" . expand-abbrev))
  :init (setf abbrev-file-name (my-expand-var-file-name "abbrev-defs"))
  :hook ((text-mode bibtex-mode) . abbrev-mode))

(use-package tempo
  :defer t
  :config
  (setf tempo-interactive t)
  (setf tempo-show-completion-buffer nil))

(use-package autoinsert
  :defer t
  :bind ("C-c e t" . auto-insert)
  :init
  (setf auto-insert t)
  (setf auto-insert-directory (my-expand-etc-file-name "insert/")
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
          lua-mode
          LaTeX-mode
          markdown-mode
          ledger-mode)
         . flycheck-mode)
  :config (setf flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flycheck-inline
  :ensure t
  :defer t
  :hook (flycheck-mode . my-enable-flycheck-inline)
  :init
  (defun my-enable-flycheck-inline ()
    (if flycheck-mode
        (flycheck-inline-mode)
      (flycheck-inline-mode 0))))

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

(use-package flyspell-correct-ivy
  :ensure t
  :after (flyspell-correct ivy))

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
         ("C-c g s" . magit-status)
         ("C-c g d" . magit-dispatch)
         ("C-c g f" . magit-file-dispatch))
  :init (setf magit-file-mode-map (make-sparse-keymap))
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
  (defun my-setup-git-commit-mode ()
    (setf fill-column 72))
  (add-hook 'git-commit-mode-hook #'my-setup-git-commit-mode)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

;; Browse history
(use-package git-timemachine
  :ensure t
  :defer t
  :bind (("C-c g t" . git-timemachine)
         :map git-timemachine-mode-map
         ("SPC" . scroll-up-command)
         ("S-SPC" . scroll-down-command)
         ("DEL" . scroll-down-command))
  :config
  (defun my-show-git-timemachine-details-in-header-line (fn &rest args)
    "Apply FN on ARGS but use header line to display details."
    (cl-letf (((symbol-function 'message)
               (lambda (string &rest objects)
                 (setf header-line-format (apply #'format string objects)))))
      (apply fn args)))
  (advice-add #'git-timemachine--show-minibuffer-details :around
              #'my-show-git-timemachine-details-in-header-line))

;; Show edits in the left fringe
(use-package diff-hl
  :ensure t
  :defer t
  :bind (;; -
         :map diff-hl-mode-map
         ("C-c g p" . hydra-hunks/diff-hl-previous-hunk)
         ("C-c g n" . hydra-hunks/diff-hl-next-hunk)
         :map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook (dired-mode . diff-hl-dired-mode)
  :init (global-diff-hl-mode)
  :config
  ;; Magit integration
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; In terminal there is no fringe
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  (defhydra hydra-hunks ()
    "Hunks"
    ("p" diff-hl-previous-hunk "previous")
    ("n" diff-hl-next-hunk "next")))

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

(use-package projectile
  :ensure t
  :init
  (make-directory (my-expand-var-file-name "projectile/") t)
  (setf projectile-cache-file (my-expand-var-file-name "projectile/cache")
        projectile-known-projects-file (my-expand-var-file-name "projectile/bookmarks.eld"))
  :config
  (setf projectile-dynamic-mode-line nil)
  (setf projectile-completion-system (if (fboundp #'ivy-read) 'ivy 'default))
  (setf projectile-commander-methods nil
        (symbol-function 'projectile-commander-bindings) #'my-ignore
        (symbol-function 'projectile-commander) #'projectile-dired)

  (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (dolist (key '("s" "x" "m"))
    (unbind-key key projectile-command-map))
  (bind-keys :map projectile-command-map
             ("x e" . projectile-run-eshell)
             ("x i" . projectile-run-ielm))

  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :defer t
  :after (projectile counsel)
  :bind (;; -
         :map projectile-mode-map
         ([remap projectile-find-file] . counsel-projectile-find-file)
         ([remap projectile-find-file-dwim] . counsel-projectile-find-file-dwim)
         ([remap projectile-find-dir] . counsel-projectile-find-dir)
         ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
         ([remap projectile-switch-project] . counsel-projectile-switch-project)
         :map projectile-command-map
         ("s" . counsel-projectile-rg)
         ("SPC" . counsel-projectile)
         :map counsel-projectile-switch-to-buffer-map
         ("C-k" . ivy-switch-buffer-kill))
  :init (setf counsel-projectile-switch-to-buffer-map (make-sparse-keymap))
  :config
  (setf counsel-projectile-switch-project-action
        '(1
          ("o" counsel-projectile-switch-project-action
           "jump to a project buffer or file")
          ("b" counsel-projectile-switch-project-action-switch-to-buffer
           "jump to a project buffer")
          ("f" counsel-projectile-switch-project-action-find-file
           "jump to a project file")
          ("d" counsel-projectile-switch-project-action-find-dir
           "jump to a project directory")
          ("D" counsel-projectile-switch-project-action-dired
           "open project in dired")
          ("m" counsel-projectile-switch-project-action-find-file-manually
           "find file manually from project root")
          ("k" counsel-projectile-switch-project-action-kill-buffers
           "kill all project buffers"))))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode))

;;; Compile

(use-package compile
  :defer t
  :bind (("C-c c c" . compile)
         ("C-c c C" . recompile))
  :config
  (setf compilation-ask-about-save nil
        compilation-always-kill t)
  (setf compilation-disable-input t)
  (setf compilation-scroll-output 'first-error
        compilation-skip-threshold 2
        compilation-context-lines 3)

  (require 'ansi-color)
  (defun my-apply-ansi-colors-for-compilation ()
    "Apply ANSI colors on compilation results."
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point-max)))))
  (add-hook 'compilation-filter-hook #'my-apply-ansi-colors-for-compilation))

(use-package firestarter
  :ensure t
  :defer t
  :bind ("C-c c f" . firestarter-mode)
  :config (setf firestarter-default-type t))

;;; ElDoc

(use-package eldoc
  :defer t
  :config
  (setf eldoc-echo-area-use-multiline-p nil)
  ;; Describe the unicode char at point by default
  (setf eldoc-documentation-function #'describe-char-eldoc))

(use-package eldoc-lv
  :load-path "lisp"
  :after eldoc
  :config (eldoc-lv-setup))

;;; Comint

(use-package comint
  :defer t
  :bind (;; -
         :map comint-mode-map
         ("TAB" . completion-at-point)
         ([remap comint-delchar-or-maybe-eof] . delete-char)
         ("C-c a a" . comint-send-eof))
  :config
  (setf (default-value 'comint-prompt-read-only) nil
        comint-scroll-to-bottom-on-input 'this))

;;; Debugger

(use-package gud
  :defer t
  :bind (("C-c c g" . gud-gdb)
         ("C-c c p" . pdb))
  :config
  (setf gud-pdb-command-name "python -m pdb")

  (defhydra hydra-debug ()
    "Debug"
    ("n" gud-next "next")
    ("s" gud-step "step")
    ("r" gud-cont "continue")
    ("b" gud-break "break")
    ("d" gud-remove "remove"))
  (bind-keys ("C-x C-a n" . hydra-debug/gud-next)
             ("C-x C-a s" . hydra-debug/gud-step)
             ("C-x C-a r" . hydra-debug/gud-cont)
             ("C-x C-a b" . hydra-debug/gud-break)
             ("C-x C-a d" . hydra-debug/gud-remove)))

;;; Compiler explorer

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
  (make-directory (my-expand-var-file-name "eww/") t)
  (setf eww-bookmarks-directory (my-expand-var-file-name "eww/"))
  :config
  (setf eww-search-prefix "https://www.google.com/search?q=")

  (defun my-use-fundamental-mode-for-eww (fn &rest args)
    "Apply FN on ARGS, but force using `fundamental-mode' to view page source."
    (cl-letf (((symbol-function 'mhtml-mode) #'fundamental-mode)
              ((symbol-function 'html-mode) #'fundamental-mode))
      (apply fn args)))
  (advice-add #'eww-view-source :around #'my-use-fundamental-mode-for-eww))

(use-package elfeed
  :ensure t
  :defer t
  :init
  (setf elfeed-db-directory (my-expand-sync-file-name "misc/elfeed/db/"))
  (with-eval-after-load 'recentf
    (push "/elfeed/db/" recentf-exclude))
  :bind ("C-c m w" . elfeed)
  :config
  ;; Load feeds
  (load (my-expand-sync-file-name "misc/elfeed/feeds.el") t t t)
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
  ;; NOTE: This fixes the weird issue that cursor is not at the beginning of
  ;; buffer after switching when there are tables in the entry.
  (setf elfeed-show-entry-switch #'pop-to-buffer-same-window))

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
  (setf erc-hide-list '("353")
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")))

(use-package erc-join
  :defer t
  :config
  (setf erc-autojoin-channels-alist
        '(("freenode.net" "#emacs" "#archlinux" "#haskell")))
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

(use-package erc-match
  :defer t
  :config
  (setf erc-fools '("rudybot")
        erc-fool-highlight-type 'all))

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
  (let ((sync-directory (my-expand-sync-file-name "misc/")))
    (make-directory sync-directory t)
    (setf diary-file (expand-file-name "diary" sync-directory)))
  :config
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  (setf calendar-mark-holidays-flag t
        calendar-chinese-all-holidays-flag t)
  (setf calendar-date-display-form calendar-iso-date-display-form)
  (calendar-set-date-style 'iso)
  ;; Tweak the mode line
  (setf calendar-mode-line-format nil
        (symbol-function 'calendar-set-mode-line) #'my-ignore))

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
  :bind (("C-c o n" . appt-add)
         ("C-c o d" . appt-delete))
  :init
  ;; NOTE: It seems that those lines must be before the activation, otherwise
  ;; they may not take effects at the very beginning.
  (setf appt-display-diary nil
        appt-audible nil
        appt-display-mode-line nil)
  (setf appt-display-interval 10
        appt-message-warning-time 20)

  (when (fboundp #'alert)
    (setf appt-display-format 'window
          appt-disp-window-function #'my-display-appt-message
          appt-delete-window-function #'ignore)

    (defun my-display-appt-message (min _ msg)
      "Display MSG due in MIN minutes with `alert'."
      (if (listp min)
          (dotimes (i (length msg))
            (alert (concat (nth i msg) " in " (nth i min) " minutes")
                   :title "Appt"))
        (alert (concat msg " in " min " minutes")
               :title "Appt"))))

  ;; NOTE: This is not a normal minor mode, the positive argument is
  ;; essential to turn it on, not toggle.
  (appt-activate 1))

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
  :config (setf ielm-prompt-read-only (default-value 'comint-prompt-read-only)))

(use-package flycheck-cask
  :ensure t
  :defer t
  :after elisp-mode
  :hook (flycheck-mode . flycheck-cask-setup))

(use-package cask-mode
  :ensure t
  :defer t)

;;; Eshell

(use-package eshell
  :defer t
  :bind (("C-x m" . eshell)
         ("C-c a e" . eshell))
  :init (setf eshell-directory-name (my-expand-var-file-name "eshell/"))
  :config
  (setf eshell-scroll-to-bottom-on-input 'this)
  (setf eshell-modify-global-environment t)
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
                              eshell-unix))

  ;; Workaround an Eshell bug
  (defun my-setup-eshell-mode ()
    (dolist (key '("M-s" "M-?" "<backtab>"))
      (unbind-key key eshell-mode-map))
    (bind-keys :map eshell-mode-map
               ([remap eshell-pcomplete] . completion-at-point)
               ([remap pcomplete-expand-and-complete] . completion-at-point)
               ([remap pcomplete-expand] . completion-at-point)
               ("C-x m" . eshell-life-is-too-much)
               ("C-c a a" . eshell-life-is-too-much)))

  (add-hook 'eshell-mode-hook #'my-setup-eshell-mode))

(use-package em-alias
  :defer t
  :init (setf eshell-aliases-file (my-expand-etc-file-name "eshell/aliases")))

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
  (setf eshell-rc-script (my-expand-etc-file-name "eshell/profile")
        eshell-login-script (my-expand-etc-file-name "eshell/login")))

(use-package eshell-z
  :ensure t
  :after eshell)

(use-package eshell-extras
  :load-path "lisp"
  :after eshell
  :config
  (eshell-extras-setup)

  ;; NOTE: Due to an Eshell bug we cannot bind key in the mode map.
  (bind-key [remap eshell-truncate-buffer] #'eshell-extras-clear-buffer)
  (bind-keys :map eshell-extras-autosuggest-suggestion-map
             ([remap forward-char] . eshell-extras-accept-suggestion)
             ([remap move-end-of-line] . eshell-extras-accept-suggestion)
             ([remap forward-word] . eshell-extras-accept-suggestion-word)))

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

(use-package etags
  :defer t
  :bind ("C-c c t" . visit-tags-table)
  :config (setf tags-revert-without-query t))

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
  :bind (:map cider-repl-mode-map ("C-c a a" . cider-quit)))

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
  :bind (:map geiser-mode-map ("C-c a a" . run-geiser))
  :init (setf geiser-active-implementations '(racket)))

(use-package geiser-repl
  :defer t
  :after geiser-mode
  :bind (:map geiser-repl-mode-map ("C-c a a" . geiser-repl-exit))
  :init
  (make-directory (my-expand-var-file-name "geiser/") t)
  (setf geiser-repl-history-filename (my-expand-var-file-name "geiser/history"))
  :config
  (setf geiser-repl-company-p nil)
  (setf geiser-repl-read-only-prompt-p nil
        geiser-repl-read-only-output-p nil))

(use-package geiser-autodoc
  :defer t
  :after geiser-mode
  :config (setf geiser-autodoc-delay eldoc-idle-delay))

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

(use-package haskell-compile
  :defer t
  :after haskell-mode
  :config (setf (default-value 'haskell-compile-ignore-cabal) t))

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

(use-package haskell-hoogle
  :defer t
  :after haskell-mode
  :bind (:map haskell-mode-map ("C-c C-v" . haskell-hoogle))
  :config (setf haskell-hoogle-command nil))

(use-package haskell-extras
  :load-path "lisp"
  :after haskell-mode
  :config (haskell-extras-setup))

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
  (setf dante-methods '(stack bare-ghci))

  (with-eval-after-load 'company
    (setf (default-value 'company-backends)
          (delq #'dante-company (default-value 'company-backends)))))

;;; Idris

(use-package idris-mode
  :ensure t
  :defer t
  :bind (;; -
         :map idris-mode-map
         ("C-c a a" . idris-repl)
         ("C-c a m" . idris-list-holes)
         :map idris-repl-mode-map
         ("C-c a a" . idris-quit))
  :init
  (make-directory (my-expand-var-file-name "idris/") t)
  (setf idris-repl-history-file (my-expand-var-file-name "idris/history.eld"))
  :config
  (setf idris-stay-in-current-window-on-compiler-error t)
  (setf idris-enable-elab-prover t))

;;; R

(use-package ess
  :ensure t
  :defer t
  :init
  (setf ess-write-to-dribble nil)
  (setf ess-history-directory (my-expand-var-file-name "ess/history/"))
  (make-directory ess-history-directory t)

  (with-eval-after-load 'org
    (cl-pushnew '(R . t) org-babel-load-languages :test #'eq :key #'car))
  :config
  (setf ess-style 'RStudio)
  (setf ess-use-ido nil
        ess-use-flymake nil)

  (setf ess-eval-visibly nil
        ess-execute-in-process-buffer t)
  (setf ess-ask-for-ess-directory nil
        ess-auto-width 'window
        inferior-R-args "--no-save"))

(use-package ess-inf
  :defer t
  :after ess
  :bind (:map inferior-ess-mode-map ("C-c a a" . ess-quit))
  :config (unbind-key "M-?" inferior-ess-mode-map))

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
  :bind (:map css-mode-map ("C-c C-d" . css-lookup-symbol))
  :config (setf css-indent-offset 2))

(use-package counsel-css
  :ensure t
  :defer t
  :hook (css-mode . counsel-css-imenu-setup))

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
  :hook ((js2-mode typescript-mode) . my-enable-tide)
  :init
  (defun my-enable-tide ()
    (unless (file-remote-p default-directory)
      (tide-setup)))
  :config
  (setf tide-completion-enable-autoimport-suggestions nil
        tide-completion-detailed t)
  (setf tide-always-show-documentation t)

  (with-eval-after-load 'company
    (setf (default-value 'company-backends)
          (delq #'company-tide (default-value 'company-backends)))))

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

  (defun my-setup-python-mode ()
    (setf fill-column 79))
  (add-hook 'python-mode-hook #'my-setup-python-mode)

  (defun my-setup-inferior-python-mode ()
    (kill-local-variable 'comint-prompt-read-only))
  (add-hook 'inferior-python-mode-hook #'my-setup-inferior-python-mode))

(use-package anaconda-mode
  :ensure t
  :defer t
  :bind (;; -
         :map anaconda-mode-map
         ([remap python-describe-at-point] . anaconda-mode-show-doc)
         ([remap xref-find-definitions] . anaconda-mode-find-definitions)
         ([remap xref-find-references] . anaconda-mode-find-references)
         ("C-c C-v" . anaconda-mode-find-assignments))
  :hook (python-mode . my-enable-anaconda)
  :init
  (setf anaconda-mode-map (make-sparse-keymap))
  (setf anaconda-mode-installation-directory (my-expand-var-file-name "anaconda-mode/"))

  (defun my-enable-anaconda ()
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
  (setf TeX-source-correlate-start-server nil
        TeX-source-correlate-method 'synctex)
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

  (defun my-setup-LaTeX-mode ()
    (make-local-variable 'TeX-electric-math)
    (setf TeX-electric-math '("\\(" . "\\)")))
  (add-hook 'LaTeX-mode-hook #'my-setup-LaTeX-mode))

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

(use-package reftex-cite
  :defer t
  :config
  (defun my-query-reftex-citation-search-regexps (default)
    "Query for regular expressions of search queries.
DEFAULT is the default value.

This is a replacement for `reftex--query-search-regexps'."
    (split-string (read-string (format "Regex [&& Regex...] (%s): " default)
                               nil 'reftex-cite-regexp-hist default)
                  "[ \t]*&&[ \t]*"))
  (setf (symbol-function 'reftex--query-search-regexps)
        #'my-query-reftex-citation-search-regexps))

(use-package company-auctex
  :ensure t
  :defer t)

(use-package company-reftex
  :ensure t
  :defer t)

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
  (setf ledger-schedule-file (my-expand-sync-file-name "ledger/schedule"))
  (setf ledger-report-resize-window nil
        ledger-report-use-header-line t)
  (setf ledger-reports
        (mapcar
         (lambda (x)
           (list (car x)
                 (format "%s %s" "%(binary) -f %(ledger-file)" (cdr x))))
         '(("On hand" . "balance Assets Liabilities")
           ("Account" . "register %(account)")
           ("Expenses (weekly)" . "register Expenses -W")
           ("Expenses (monthly)" . "register Expenses -M")
           ("Cash flow (this month)" . "balance Income Expenses --invert -p %(month)")))))

(use-package flycheck-ledger
  :ensure t
  :after flycheck
  :config (setf flycheck-ledger-pedantic t
                flycheck-ledger-explicit t))

;;; Org

(use-package org
  :defer t
  :bind ("C-c o l" . org-store-link)
  :init
  (make-directory (my-expand-var-file-name "org/") t)
  (setf org-babel-load-languages nil)
  :config
  ;; File
  (setf org-directory (my-expand-sync-file-name "org/"))
  (make-directory org-directory t)

  (setf org-default-notes-file (expand-file-name "inbox.org" org-directory)
        org-agenda-files `(,org-directory))
  (setf org-archive-location "::* Archived")

  ;; General
  (setf org-adapt-indentation nil
        org-startup-indented t)
  (setf org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t)
  (setf org-special-ctrl-a/e t
        org-catch-invisible-edits 'show-and-error)
  (setf org-goto-interface 'outline-path-completion)
  (setf org-image-actual-width '(300))
  (setf org-modules '(org-id org-docview org-eww org-habit))
  (setf org-export-backends '(ascii html latex))
  (setf org-file-apps '((auto-mode . emacs)
                        (directory . emacs)))

  ;; LaTeX fragment and entity
  (setf org-highlight-latex-and-related '(latex entities))
  (setf org-use-sub-superscripts '{})
  (setf org-format-latex-options (plist-put org-format-latex-options
                                            :scale 1.2))

  ;; Refile
  (setf org-refile-targets '((nil . (:maxlevel . 5))
                             (org-agenda-files . (:maxlevel . 5)))
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes 'confirm
        org-outline-path-complete-in-steps nil)

  ;; Task management
  (setf org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w)" "|" "CANCELLED(c)")))
  (setf org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)
  (setf org-log-into-drawer t
        org-log-done 'time)

  ;; Tag
  (setf org-tag-persistent-alist '(("note" . ?n))))

(use-package org-id
  :defer t
  :init (setf org-id-locations-file (my-expand-var-file-name "org/id-locations")))

(use-package org-attach
  :defer t
  :config (setf org-attach-file-list-property nil))

(use-package org-lint
  :defer t
  :after org
  :bind (:map org-mode-map ("M-g L" . org-lint)))

(use-package org-radiobutton
  :ensure t
  :defer t
  :hook (org-mode . org-radiobutton-mode))

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
  (setf org-agenda-start-with-log-mode t
        org-agenda-block-separator "")
  (setf org-agenda-span 'day
        org-agenda-start-on-weekday 0)

  (defun my-realign-tags-in-org-agenda ()
    "Realign tags after changing the layout in Org agenda buffers."
    (add-hook 'window-configuration-change-hook #'org-agenda-align-tags nil t))
  (add-hook 'org-agenda-mode-hook #'my-realign-tags-in-org-agenda)

  ;; Add tasks to appt
  ;;
  ;; NOTE: The delay is important. Otherwise it will throw an error occasionally
  ;; when Org is loaded.
  ;;
  ;; NOTE: Do not refresh. Otherwise the manually added entries and the diary
  ;; entries will be cleaned as well. Wrong notifications seem to be better than
  ;; losing notifications.
  (run-with-timer 1 3600 #'org-agenda-to-appt))

;; Clock
(use-package org-duration
  :defer t
  :config (setf org-duration-format 'h:mm))

(use-package org-clock
  :defer t
  :init
  (setf org-clock-persist-file (my-expand-var-file-name "org/clock-save.el"))
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate))
  :config
  (setf org-clock-out-remove-zero-time-clocks t)
  (setf org-clock-persist 'history)
  (setf org-clock-task-overrun-text "*"
        org-clock-clocked-in-display 'frame-title)

  (defun my-confirm-quitting-when-clocking ()
    "Confirm quitting if clocking."
    (or (not (org-clocking-p))
        (progn
          (org-clock-goto)
          (yes-or-no-p "There is a running clock. Still quit? "))))
  (add-hook 'kill-emacs-query-functions #'my-confirm-quitting-when-clocking))

(use-package org-mru-clock
  :ensure t
  :defer t
  :bind (("C-c o i" . org-mru-clock-in)
         ("C-c o u" . org-mru-clock-select-recent-task)))

;; Source code and Babel
(use-package org-src
  :defer t
  :config
  (setf org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)
  (setf org-src-window-setup 'other-window))

(use-package ob
  :defer t
  :hook (org-mode . my-load-org-babel-languages)
  :init
  (defun my-load-org-babel-languages ()
    "Load all Org Babel languages once."
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
    ;; Only load languages once
    (remove-hook 'org-mode-hook #'my-load-org-babel-languages))
  :config
  (setf org-confirm-babel-evaluate nil)

  (defun my-redisplay-org-inline-images ()
    "Redisplay Org inline images."
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))
  (add-hook 'org-babel-after-execute-hook #'my-redisplay-org-inline-images))

(use-package ob-async
  :ensure t
  :after ob)

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

  ;; Source code
  (setf org-latex-listings t)
  (push '("" "listings") org-latex-packages-alist)
  (push '("" "color") org-latex-packages-alist)

  ;; KOMA-Script
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
        org-html-validation-link nil
        org-html-postamble nil
        org-html-htmlize-output-type 'css
        org-html-head-include-default-style nil
        org-html-head (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
                              (concat "file://" (my-expand-etc-file-name "css/org.css")))))

;;; Markdown

(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.Rmd\\'"
  :config
  (setf (default-value 'markdown-enable-math) t)
  (setf markdown-fontify-code-blocks-natively t)
  (setf markdown-max-image-size '(300 . nil))
  (setf markdown-css-paths `(,(concat "file://" (my-expand-etc-file-name "css/pandoc.css")))
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
         ("C-c C-d" . glsl-find-man-page)
         ("C-c C-n" . c-forward-conditional)
         ("C-c C-p" . c-backward-conditional)
         ("C-c C-u" . c-up-conditional)
         ([remap backward-sentence] . c-beginning-of-statement)
         ([remap forward-sentence] . c-end-of-statement))
  :init (setf glsl-mode-map (make-sparse-keymap)))

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

;;; Protobuf

(use-package protobuf-mode
  :ensure t
  :defer t)

;;; YAML

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (defun my-setup-yaml-mode ()
    (flyspell-mode 0)
    (auto-fill-mode 0))
  (add-hook 'yaml-mode-hook #'my-setup-yaml-mode))

;;; CSV

(use-package csv-mode
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

;;; BNF

(use-package bnf-mode
  :ensure t
  :defer t)

;;; Docker

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package docker-compose-mode
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

;;; Lua

(use-package lua-mode
  :ensure t
  :defer t
  :config (setf lua-indent-level 2))

;;; Shell script

(use-package sh-script
  :defer t
  :bind (:map sh-mode-map ("C-c a a" . sh-show-shell))
  :init
  (setf sh-shell-file "/bin/bash")

  (with-eval-after-load 'org
    (cl-pushnew '(shell . t) org-babel-load-languages :test #'eq :key #'car))
  :config (setf sh-basic-offset 2))

;;; Env file

(use-package dotenv-mode
  :ensure t
  :defer t)

;;; Viewer

(use-package doc-view
  :defer t
  :bind (:map doc-view-mode-map ("&" . browse-url-of-file))
  :config
  (setf doc-view-continuous t)
  (setf doc-view-resolution 300)

  (when (and (not (eq doc-view-pdf->png-converter-function
                      #'doc-view-pdf->png-converter-mupdf))
             (executable-find "mutool"))
    (setf doc-view-pdf->png-converter-function
          (lambda (pdf png page callback)
            (doc-view-start-process
             "pdf->png" "mutool"
             `("draw"
               ,(concat "-o" png)
               ,(format "-r%d" (round doc-view-resolution))
               ,pdf
               ,@(when page `(,(format "%d" page))))
             callback)))))

(use-package image-file
  :when (display-graphic-p)
  :config (auto-image-file-mode))

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :init (setf nov-save-place-file (my-expand-var-file-name "nov-places")))

;;; Mode line & frame title

(use-package liteline
  :load-path "lisp"
  :defer t
  :hook (after-init . liteline-setup))

(use-package rich-title
  :load-path "lisp"
  :defer t
  :hook (after-init . rich-title-setup))

;;; init.el ends here
