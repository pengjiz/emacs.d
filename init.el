;;; init.el --- Main initialization  -*- lexical-binding: t; -*-

;;; Commentary:

;; Main initialization file.

;;; Code:

;;; Preface

(progn ; fundamental
  (setf ad-redefinition-action 'accept)
  (setf create-lockfiles nil
        delete-by-moving-to-trash t)
  (prefer-coding-system 'utf-8))

(progn ; user interface
  (setf ring-bell-function #'ignore
        use-short-answers t
        echo-keystrokes 0.1)

  (blink-cursor-mode 0)
  (setf visible-cursor nil
        x-stretch-cursor t)

  (push '(font . "Source Code Pro-12") default-frame-alist)
  (setf frame-resize-pixelwise t
        window-resize-pixelwise t
        (default-value 'indicate-empty-lines) t))

(progn ; helpers
  (eval-and-compile
    (let ((lisp-directory (expand-file-name "lisp" user-emacs-directory)))
      (when (and (file-directory-p lisp-directory)
                 (not (member lisp-directory load-path)))
        (push lisp-directory load-path))))

  (eval-when-compile
    (require 'cl-lib)
    (require 'subr-x)
    (require 'rx)

    (defmacro init--require-when-compile (feature)
      "Require FEATURE only at compile time."
      (declare (debug (form)))
      (when (bound-and-true-p byte-compile-current-file)
        `(eval-when-compile
           (require ,feature)))))

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

;;; Initialization

(progn ; `startup'
  (setf inhibit-startup-screen t
        inhibit-startup-buffer-menu t
        inhibit-startup-echo-area-message nil)
  (setf initial-scratch-message nil
        initial-major-mode #'text-mode))

(progn ; basic customization
  (setf custom-file (init--var "custom.el"))
  (load custom-file t t t)
  (defvar init--org-babel-languages nil
    "Languages to be loaded for Org Babel.")

  (dolist (key '("M-`" "M-$" "M-z" "C-z" "C-x C-z" "C-x C-u" "C-x C-l"
                 "C-x m" "C-x 4 m" "C-x 5 m"))
    (define-key global-map (kbd key) nil))

  (defvar init-protected-map (make-sparse-keymap)
    "Keymap with higher precedence than most of the others.")
  (define-minor-mode init-protected-keys-mode
    "Minor mode to activate `init-protected-map'."
    :group 'convenience :lighter nil :keymap nil
    :global t :init-value t)
  (cl-pushnew `((init-protected-keys-mode . ,init-protected-map))
              emulation-mode-map-alists
              :test #'equal)

  ;; NOTE: Theme files are not in the load path so we cannot use autoloading or
  ;; specify the file for this command.
  (declare-function modus-themes-toggle nil)
  (load-theme 'modus-vivendi)
  (define-key global-map (kbd "C-c t m") #'modus-themes-toggle)

  (with-eval-after-load 'faces
    (set-face-attribute 'fixed-pitch nil :family "Source Code Pro")
    (set-face-attribute 'variable-pitch nil :family "DejaVu Sans")))

(progn ; `package'
  (require 'package)
  (setf package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))

  (defun init-ensure-package (package)
    "Ensure that PACKAGE is installed."
    (unless (package-installed-p package)
      (package--archives-initialize)
      (unless (assq package package-archive-contents)
        (package-refresh-contents))
      (package-install package t)))

  (defun init--record-selected-package ()
    "Record selected packages if any."
    (when-let* ((packages (or package-selected-packages
                              (package--find-non-dependencies))))
      (package--save-selected-packages packages)))
  (unless package-selected-packages
    (add-hook 'after-init-hook #'init--record-selected-package)))

(progn ; `liteline'
  (defvar calendar-mode-line-format)
  (defvar 2C-mode-line-format)
  (autoload 'liteline-setup "liteline")
  (add-hook 'after-init-hook #'liteline-setup)

  (with-eval-after-load 'liteline
    (setf mode-line-position-column-line-format '(" %l:%c"))
    (with-eval-after-load 'calendar
      (setf calendar-mode-line-format nil
            (symbol-function 'calendar-set-mode-line) #'init-ignore))
    (with-eval-after-load 'two-column
      (setf 2C-mode-line-format (default-value 'mode-line-format)))
    (with-eval-after-load 'ediff-wind
      (setf (symbol-function 'ediff-refresh-mode-lines) #'init-ignore))))

(progn ; `transient'
  (init--require-when-compile 'transient)
  (setf transient-history-file (init--var "transient/history.el")
        transient-levels-file (init--var "transient/levels.el")
        transient-values-file (init--var "transient/values.el")))

(progn ; `url'
  (init--require-when-compile 'url)
  (setf url-configuration-directory (init--var "url/"))

  (progn ; `url-cache'
    (init--require-when-compile 'url-cache)
    (setf url-cache-directory (init--var "url/cache/"))))

(progn ; `request'
  (init-ensure-package 'request)
  (init--require-when-compile 'request)
  (setf request-storage-directory (init--var "request/")))

(progn ; `server'
  (require 'server)
  (unless (or (daemonp) (server-running-p))
    (server-start)))

(progn ; `with-editor'
  (init-ensure-package 'with-editor)
  (shell-command-with-editor-mode)
  (dolist (hook '(shell-mode-hook eshell-mode-hook))
    (add-hook hook #'with-editor-export-editor)))

;;; Basic editing

(progn ; disabled commands
  (dolist (command '(erase-buffer
                     narrow-to-region
                     narrow-to-page
                     set-goal-column
                     scroll-left
                     dired-find-alternate-file))
    (put command 'disabled nil)))

(progn ; `simple'
  (column-number-mode)
  (size-indication-mode)
  (dolist (hook '(text-mode-hook bibtex-mode-hook))
    (add-hook hook #'auto-fill-mode))

  (let ((map global-map))
    (define-key map [remap just-one-space] #'cycle-spacing)
    (define-key map [remap downcase-word] #'downcase-dwim)
    (define-key map [remap capitalize-word] #'capitalize-dwim)
    (define-key map [remap upcase-word] #'upcase-dwim)
    (define-key map (kbd "C-c b c") #'clone-indirect-buffer)
    (define-key map (kbd "C-c t v") #'visual-line-mode)
    (define-key map (kbd "C-c t q") #'auto-fill-mode)
    (define-key map (kbd "C-c t p") #'visible-mode)
    (define-key map (kbd "C-c a l") #'list-processes))

  (with-eval-after-load 'simple
    (setf extended-command-suggest-shorter nil)
    (setf completion-show-help nil)
    (setf kill-do-not-save-duplicates t)
    (setf set-mark-command-repeat-pop t)
    (setf async-shell-command-display-buffer nil)

    (define-key minibuffer-local-completion-map
      (kbd "M-`") #'switch-to-completions)
    (define-key completion-in-region-mode-map
      (kbd "M-`") #'switch-to-completions)
    (define-key completion-list-mode-map
      (kbd "M-`") #'delete-completion-window)))

(progn ; `simple-extras'
  (require 'simple-extras)
  (setf mail-user-agent 'simple-extras-mail-user-agent)
  (add-hook 'prog-mode-hook #'simple-extras-auto-fill-comments-mode)

  (define-key global-map (kbd "M-z") #'simple-extras-unfill-paragraph)
  (define-key completion-in-region-mode-map
    (kbd "C-<tab>") #'simple-extras-force-completion-at-point)
  (define-key completion-list-mode-map
    (kbd "C-<return>") #'simple-extras-choose-completion-no-exit))

(progn ; indentation
  (setf (default-value 'indent-tabs-mode) nil
        (default-value 'tab-width) 8)
  (setf tab-always-indent t))

(progn ; `paren'
  (setf show-paren-when-point-inside-paren t))

(progn ; `elec-pair'
  (electric-pair-mode))

(progn ; `lisp'
  (let ((map global-map))
    (define-key map (kbd "C-x C-u") #'delete-pair)
    (define-key map (kbd "C-x C-l") #'raise-sexp)
    (define-key map (kbd "C-c x p") #'check-parens)))

(progn ; `delsel'
  (delete-selection-mode))

(progn ; `subword'
  (add-hook 'prog-mode-hook #'subword-mode)
  (define-key global-map (kbd "C-c t b") #'subword-mode))

(progn ; `align'
  (let ((map global-map))
    (define-key map (kbd "C-c x a") #'align)
    (define-key map (kbd "C-c x A") #'align-regexp)
    (define-key map (kbd "C-c x z") #'align-current)
    (define-key map (kbd "C-c x Z") #'align-entire)))

(progn ; `sort'
  (let ((map global-map))
    (define-key map (kbd "C-c x r") #'sort-lines)
    (define-key map (kbd "C-c x R") #'sort-columns)
    (define-key map (kbd "C-c x d") #'delete-duplicate-lines)))

;;; Whitespace

(progn ; general whitespace
  (setf (default-value 'fill-column) 80
        word-wrap-by-category t)
  (setf sentence-end-double-space nil))

(progn ; `whitespace'
  (init--require-when-compile 'whitespace)

  (dolist (hook '(prog-mode-hook
                  text-mode-hook
                  bibtex-mode-hook
                  conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (let ((map global-map))
    (define-key map (kbd "C-c t w") #'whitespace-mode)
    (define-key map (kbd "C-c t W") #'whitespace-toggle-options)
    (define-key map (kbd "C-c x w") #'whitespace-cleanup))

  (defun init--set-cleanup-whitespace-style (fn &rest args)
    "Apply FN on ARGS but explicitly set whitespace style for cleanup."
    (let ((whitespace-style '(empty
                              indentation
                              space-before-tab
                              space-after-tab
                              trailing)))
      (apply fn args)))

  (with-eval-after-load 'whitespace
    (setf whitespace-style '(face
                             indentation
                             space-after-tab
                             space-before-tab
                             tab-mark
                             trailing
                             lines-tail
                             missing-newline-at-eof)
          whitespace-line-column nil)
    (advice-add #'whitespace-cleanup :around
                #'init--set-cleanup-whitespace-style)
    (advice-add #'whitespace-cleanup-region :around
                #'init--set-cleanup-whitespace-style)))

(progn ; `whitespace-cleanup-mode'
  (init-ensure-package 'whitespace-cleanup-mode)
  (global-whitespace-cleanup-mode))

;;; Completion

(progn ; basic completion
  (setf enable-recursive-minibuffers t)
  (setf completion-ignore-case t
        read-buffer-completion-ignore-case t)
  (define-key minibuffer-local-map (kbd "C-<tab>") nil))

(progn ; `minibuffer'
  (define-key global-map [remap complete-symbol] #'completion-at-point)
  (with-eval-after-load 'minibuffer
    (setf read-file-name-completion-ignore-case t)
    (setf completion-styles '(basic substring initials partial-completion))
    (setf completions-group t)
    (define-key minibuffer-local-completion-map
      (kbd "C-<tab>") #'minibuffer-force-complete)))

(progn ; `mb-depth'
  (minibuffer-depth-indicate-mode))

(progn ; `minibuf-eldef'
  (init--require-when-compile 'minibuf-eldef)
  (setf minibuffer-eldef-shorten-default t)
  (minibuffer-electric-default-mode))

(progn ; `savehist'
  (init--require-when-compile 'savehist)
  (setf savehist-file (init--var "savehist"))
  (savehist-mode))

(progn ; `company'
  (init-ensure-package 'company)
  (init--require-when-compile 'company)
  (declare-function company-select-first "company")
  (declare-function company-select-last "company")
  (declare-function company-other-backend "company")

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

  (dolist (hook '(emacs-lisp-mode-hook
                  clojure-mode-hook
                  scheme-mode-hook
                  c-mode-hook
                  c++-mode-hook
                  rust-mode-hook
                  haskell-mode-hook
                  ess-r-mode-hook
                  python-mode-hook
                  sh-mode-hook
                  html-mode-hook
                  css-mode-hook
                  js2-mode-hook
                  typescript-mode-hook
                  LaTeX-mode-hook
                  markdown-mode-hook
                  rst-mode-hook
                  org-mode-hook
                  ledger-mode-hook))
    (add-hook hook #'init--enable-company))

  (with-eval-after-load 'company
    (setf company-idle-delay 0.5)
    (setf company-show-quick-access t
          company-tooltip-align-annotations t
          company-format-margin-function nil)

    (define-key company-mode-map [remap dabbrev-completion] #'company-complete)
    (let ((map company-active-map))
      (define-key map [remap beginning-of-buffer] #'company-select-first)
      (define-key map [remap end-of-buffer] #'company-select-last)
      (define-key map [remap dabbrev-completion] #'company-other-backend)))

  (progn ; `company-dabbrev'
    (init--require-when-compile 'company-dabbrev)
    (with-eval-after-load 'company-dabbrev
      (setf company-dabbrev-ignore-case t
            company-dabbrev-downcase nil)
      (setf company-dabbrev-ignore-buffers
            (lambda (buffer)
              (or (memq (buffer-local-value 'major-mode buffer)
                        '(image-mode doc-view-mode))
                  (string-match-p "\\` \\*" (buffer-name buffer)))))))

  (progn ; `company-dabbrev-code'
    (init--require-when-compile 'company-dabbrev-code)
    (with-eval-after-load 'company-dabbrev-code
      (setf company-dabbrev-code-everywhere t))))

(progn ; `hippie-exp'
  (init--require-when-compile 'hippie-exp)
  (define-key global-map [remap dabbrev-expand] #'hippie-expand)
  (define-key global-map [remap dabbrev-completion] #'hippie-expand)
  (with-eval-after-load 'hippie-exp
    (setf hippie-expand-ignore-buffers
          '(image-mode doc-view-mode "\\` \\*"))
    (setf hippie-expand-try-functions-list
          '(try-complete-file-name-partially
            try-complete-file-name
            try-expand-line
            try-expand-dabbrev
            try-expand-line-all-buffers
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill))))

(progn ; `word-complete'
  (autoload 'word-complete "word-complete" nil t)
  (define-key global-map [remap ispell-complete-word] #'word-complete)
  (define-key global-map (kbd "C-c e d") #'word-complete))

;;; Expansion

(progn ; `abbrev'
  (setf abbrev-file-name (init--var "abbrev-defs"))
  (dolist (hook '(text-mode-hook bibtex-mode-hook))
    (add-hook hook #'abbrev-mode))
  (define-key global-map (kbd "C-c t r") #'abbrev-mode)
  (define-key global-map (kbd "C-c e e") #'expand-abbrev))

(progn ; `tempo'
  (init--require-when-compile 'tempo)
  (with-eval-after-load 'tempo
    (setf tempo-interactive t)))

(progn ; `autoinsert'
  (init--require-when-compile 'autoinsert)
  (setf auto-insert-directory (init--etc "insert/")
        auto-insert-alist nil)
  (define-key global-map (kbd "C-c e t") #'auto-insert)
  (with-eval-after-load 'autoinsert
    (setf auto-insert t)))

;;; Editing visual

(progn ; `hideshow'
  (init--require-when-compile 'hideshow)
  (declare-function hs-toggle-hiding "hideshow")

  (dolist (hook '(prog-mode-hook bibtex-mode-hook))
    (add-hook hook #'hs-minor-mode))
  (setf hs-minor-mode-map (make-sparse-keymap))
  (with-eval-after-load 'hideshow
    (define-key hs-minor-mode-map (kbd "C-c @ t") #'hs-toggle-hiding)))

(progn ; `outline'
  (init--require-when-compile 'outline)
  (declare-function outline-mark-subtree "outline")
  (declare-function outline-previous-visible-heading "outline")
  (declare-function outline-next-visible-heading "outline")
  (declare-function outline-backward-same-level "outline")
  (declare-function outline-forward-same-level "outline")

  (dolist (hook '(prog-mode-hook TeX-mode-hook))
    (add-hook hook #'outline-minor-mode))
  (setf outline-minor-mode-map (make-sparse-keymap))
  (with-eval-after-load 'outline
    (let ((map outline-minor-mode-map))
      (define-key map (kbd "C-c @ SPC") #'outline-mark-subtree)
      (define-key map (kbd "C-c @ p") #'outline-previous-visible-heading)
      (define-key map (kbd "C-c @ n") #'outline-next-visible-heading)
      (define-key map (kbd "C-c @ b") #'outline-backward-same-level)
      (define-key map (kbd "C-c @ f") #'outline-forward-same-level))))

(progn ; `bicycle'
  (init-ensure-package 'bicycle)
  (with-eval-after-load 'outline
    (let ((map outline-minor-mode-map))
      (define-key map (kbd "C-<tab>") #'bicycle-cycle)
      (define-key map (kbd "<backtab>") #'bicycle-cycle-global))))

(progn ; `hl-line'
  (global-hl-line-mode))

(progn ; `page-break-lines'
  (init-ensure-package 'page-break-lines)
  (dolist (hook '(compilation-mode-hook help-mode-hook))
    (add-hook hook #'page-break-lines-mode)))

(progn ; `rainbow-delimiters'
  (init-ensure-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(progn ; `rainbow-mode'
  (init-ensure-package 'rainbow-mode)
  (define-key global-map (kbd "C-c t o") #'rainbow-mode))

(progn ; `highlight-numbers'
  (init-ensure-package 'highlight-numbers)
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(progn ; `highlight-escape-sequences'
  (init-ensure-package 'highlight-escape-sequences)
  (hes-mode))

(progn ; `hl-todo'
  (init-ensure-package 'hl-todo)
  (dolist (hook '(prog-mode-hook
                  TeX-mode-hook
                  conf-mode-hook
                  yaml-mode-hook))
    (add-hook hook #'hl-todo-mode)))

(progn ; `visual-fill-column'
  (init-ensure-package 'visual-fill-column)
  (init--require-when-compile 'visual-fill-column)
  (declare-function visual-fill-column-adjust "visual-fill-column")

  (define-key global-map (kbd "C-c t c") #'visual-fill-column-mode)
  (with-eval-after-load 'visual-fill-column
    (setf (default-value 'visual-fill-column-center-text) t
          (default-value 'visual-fill-column-fringes-outside-margins) nil)
    (setf visual-fill-column-enable-sensible-window-split t)
    (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust)))

(progn ; `page-turner'
  (init-ensure-package 'visual-fill-column)
  (require 'page-turner)
  (setf page-turner-prose-family "DejaVu Serif")
  (page-turner-setup))

;;; Multilingual environment

(progn ; `mule-cmds'
  (setf default-input-method "TeX"))

(progn ; `kkc'
  (init--require-when-compile 'kkc)
  (setf kkc-init-file-name (init--var "kkcrc")))

(progn ; `typo'
  (init-ensure-package 'typo)
  (autoload 'typo-change-language "typo" nil t)
  (define-key global-map (kbd "C-c t t") #'typo-mode)
  (define-key global-map (kbd "C-c t T") #'typo-change-language))

(progn ; `change-language'
  (autoload 'change-language "change-language" nil t)
  (define-key global-map (kbd "C-c t i") #'change-language))

;;; Movement

(progn ; `isearch'
  (with-eval-after-load 'isearch
    (setf isearch-allow-scroll t)
    (setf isearch-lazy-count t)))

(progn ; `avy'
  (init-ensure-package 'avy)
  (init--require-when-compile 'avy)
  (autoload 'avy-resume "avy" nil t)

  (let ((map init-protected-map))
    (define-key map (kbd "C-;") #'avy-goto-char-in-line)
    (define-key map (kbd "C-'") #'avy-goto-char-2))
  (define-key global-map (kbd "C-z") #'avy-resume)
  (define-key isearch-mode-map [remap avy-goto-char-2] #'avy-isearch)

  (with-eval-after-load 'avy
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
            (?z . avy-action-zap-to-char)))))

(progn ; `imenu'
  (init--require-when-compile 'imenu)
  (define-key global-map (kbd "M-g i") #'imenu)
  (with-eval-after-load 'imenu
    (setf imenu-auto-rescan t)
    (setf imenu-space-replacement nil)))

(progn ; `beginend'
  (init-ensure-package 'beginend)
  (beginend-global-mode))

;;; Window & frame

(progn ; scroll
  (setf scroll-conservatively 101
        scroll-preserve-screen-position 'always
        hscroll-margin 0
        hscroll-step 1))

(progn ; `mwheel'
  (with-eval-after-load 'mwheel
    ;; Move slowly by default
    (setf mouse-wheel-scroll-amount '(1 ((shift) . 5))
          mouse-wheel-progressive-speed nil)))

(progn ; `window'
  (setf scroll-error-top-bottom t)
  (setf fit-window-to-buffer-horizontally t)
  (setf display-buffer-alist
        `(;; Auxiliary
          (,(rx bos (or "CAPTURE-"
                        "*Org Src"
                        "*Edit Formulas*"
                        "*edit-indirect"
                        "*draft mail*"
                        "*Bookmark Annotation Compose*"))
           (display-buffer-reuse-window
            display-buffer-below-selected)
           (window-height . 20)
           (window-min-height . 20)
           (reusable-frames . nil))
          ;; Help
          (,(rx bos (or "*Man"
                        "*Help*"
                        "*help"
                        "*eldoc"
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
                        "*Channels of"
                        "*Ledger Report*"
                        "*Reconcile*"
                        "*RefTeX Select*"
                        "*Key*"
                        "*Keys*"
                        "*Proced*"
                        "*Process List*"
                        "*Flycheck errors*"
                        "*TeX errors*"
                        "*BibTeX validation errors*"
                        "*R dired*"
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
                        "*Gnuplot Trail*"
                        "*Ledger Error*"
                        "*skewer-error*"
                        "*cider-error*"
                        "*cider-test-report*"
                        "*Geiser dbg*"
                        "*HsCompilation*"
                        "*R view"
                        "*S objects*"
                        "*S search list*"
                        "*ess-output*"
                        "*TeX background*"
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
                        "*Bookmark Annotation*"
                        "*Ledger Schedule*"
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
           (reusable-frames . nil))))

  (let ((map global-map))
    (define-key map (kbd "C-c w f") #'fit-window-to-buffer)
    (define-key map (kbd "C-c w l") #'delete-other-windows-vertically)
    (define-key map (kbd "C-c w t") #'window-toggle-side-windows)
    (define-key map (kbd "C-x C-z") #'window-toggle-side-windows)))

(progn ; `window-extras'
  (require 'window-extras)
  (window-extras-setup))

(progn ; `ace-window'
  (init-ensure-package 'ace-window)
  (init--require-when-compile 'ace-window)

  (setf aw-dispatch-alist
        '((?r aw-swap-window "Swap Window")
          (?y aw-copy-window "Copy Window")
          (?t aw-move-window "Move Window")
          (?q aw-delete-window "Delete Window")
          (?o delete-other-windows "Delete Other Windows")
          (?b aw-switch-buffer-other-window "Switch Buffer")
          (?x aw-execute-command-other-window "Execute Command")
          (?z aw-flip-window)
          (?? aw-show-dispatch-help)))
  (setf aw-make-frame-char nil)
  (define-key init-protected-map (kbd "M-o") #'ace-window)

  (with-eval-after-load 'ace-window
    (setf aw-minibuffer-flag t)))

(progn ; `tab-bar'
  (add-hook 'after-init-hook #'tab-bar-history-mode)
  (add-hook 'after-init-hook #'tab-bar-mode)
  (let ((map global-map))
    (define-key map (kbd "C-c s s") #'tab-bar-switch-to-recent-tab)
    (define-key map (kbd "C-c s n") #'tab-bar-switch-to-next-tab)
    (define-key map (kbd "C-c s p") #'tab-bar-switch-to-prev-tab)
    (define-key map (kbd "C-c s f") #'tab-bar-history-forward)
    (define-key map (kbd "C-c s b") #'tab-bar-history-back))

  (with-eval-after-load 'tab-bar
    (setf tab-bar-new-tab-choice "*scratch*")
    (setf tab-bar-close-button-show nil
          tab-bar-format '(tab-bar-format-tabs-groups
                           tab-bar-separator
                           tab-bar-format-align-right
                           tab-bar-format-global))))

(progn ; `tab-bar-extras'
  (autoload 'tab-bar-extras-setup "tab-bar-extras")
  (add-hook 'after-init-hook #'tab-bar-extras-setup)
  (with-eval-after-load 'tab-bar-extras
    (setf tab-bar-format '(tab-bar-format-tabs-groups
                           tab-bar-separator
                           tab-bar-format-align-right
                           tab-bar-extras-format-frame-info
                           tab-bar-format-global
                           tab-bar-extras-format-file-info))))

;;; Buffer

(progn ; special buffers
  (defun init--protect-special-buffers ()
    "Protect special buffers from being killed."
    (or (not (member (buffer-name (current-buffer))
                     '("*scratch*" "*Messages*")))
        (ignore (bury-buffer))))
  (add-hook 'kill-buffer-query-functions #'init--protect-special-buffers))

(progn ; `autorevert'
  (define-key global-map (kbd "C-c t g") #'auto-revert-mode)
  (global-auto-revert-mode))

(progn ; `uniquify'
  (require 'uniquify)
  (setf uniquify-buffer-name-style 'forward
        uniquify-trailing-separator-p t))

(progn ; `ibuffer'
  (init--require-when-compile 'ibuffer)
  (declare-function ibuffer-visit-buffer-1-window "ibuffer")

  (define-key global-map [remap list-buffers] #'ibuffer)
  (with-eval-after-load 'ibuffer
    (let ((state '(mark modified read-only locked " "))
          (info '((name 18 18 :left :elide) " "
                  (size 9 -1 :right) " "
                  (mode 16 16 :left :elide) " ")))
      (setf ibuffer-formats `((,@state ,@info filename)
                              (,@state ,@info process))))
    (let ((map ibuffer-mode-map))
      (define-key map (kbd "C-c C-o") #'ibuffer-visit-buffer-1-window)
      (define-key map (kbd "P") nil)))

  (progn ; `ibuf-ext'
    (init--require-when-compile 'ibuf-ext)
    (with-eval-after-load 'ibuf-ext
      (setf ibuffer-show-empty-filter-groups nil))))

(progn ; `ibuffer-vc'
  (init-ensure-package 'ibuffer-vc)
  (add-hook 'ibuffer-mode-hook #'ibuffer-vc-set-filter-groups-by-vc-root)
  (with-eval-after-load 'ibuffer-vc
    (let ((state '(mark modified read-only locked vc-status-mini " "))
          (info '((name 18 18 :left :elide) " "
                  (size 9 -1 :right) " "
                  (mode 16 16 :left :elide) " ")))
      (setf ibuffer-formats `((,@state ,@info vc-relative-file)
                              (,@state ,@info process))))))

;;; File

(progn ; `files'
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

(progn ; `files-x'
  (let ((map global-map))
    (define-key map (kbd "C-c f v") #'add-file-local-variable)
    (define-key map (kbd "C-c f V") #'add-file-local-variable-prop-line)
    (define-key map (kbd "C-c f d") #'add-dir-local-variable)))

(progn ; `files-extras'
  (autoload 'files-extras-find-recent-file "files-extras" nil t)
  (autoload 'files-extras-find-recent-file-other-window "files-extras" nil t)
  (define-key global-map (kbd "C-x m") #'files-extras-find-recent-file)
  (define-key global-map (kbd "C-x 4 m") #'files-extras-find-recent-file-other-window))

(progn ; `ffap'
  (init--require-when-compile 'ffap)
  (with-eval-after-load 'ffap
    (setf ffap-machine-p-known 'reject)))

;; Automatically make scripts executable
(progn ; `executable'
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

;; Automatically update timestamps
(progn ; `time-stamp'
  (init--require-when-compile 'time-stamp)
  (add-hook 'before-save-hook #'time-stamp)
  (with-eval-after-load 'time-stamp
    (setf time-stamp-format "%Y-%02m-%02dT%02H:%02M:%02S%:z")))

(progn ; `tramp'
  (init--require-when-compile 'tramp)
  (setf tramp-persistency-file-name (init--var "tramp/persistency")
        tramp-auto-save-directory (init--var "tramp/auto-save/"))

  (progn ; `tramp-sh'
    (init--require-when-compile 'tramp-sh)
    (setf tramp-histfile-override t)))

(progn ; `saveplace'
  (init--require-when-compile 'saveplace)
  (setf save-place-file (init--var "places"))
  (save-place-mode))

(progn ; `recentf'
  (init--require-when-compile 'recentf)
  (setf recentf-save-file (init--var "recentf"))
  (setf recentf-auto-cleanup 300)
  (require 'recentf)
  (setf recentf-max-saved-items 100)
  (setf recentf-exclude '("/elpa/" "/var/" "/\\.git/" "/Trash/"))
  (recentf-mode))

(progn ; `bookmark'
  (init--require-when-compile 'bookmark)
  (setf bookmark-default-file (init--var "bookmarks"))
  (with-eval-after-load 'bookmark
    (unless (file-exists-p bookmark-default-file)
      (dolist (bookmark '(("athenaeum" . "athenaeum/catalogue.org")
                          ("finances" . "ledger/finances.ledger")))
        (let ((name (car bookmark))
              (file (init--sync (cdr bookmark))))
          (when (file-exists-p file)
            (cl-pushnew `(,name . ((filename . ,file))) bookmark-alist
                        :test #'equal :key #'car)))))))

(progn ; `dired'
  (init--require-when-compile 'dired)
  (declare-function dired-directory-changed-p "dired")

  (define-key global-map [remap list-directory] #'dired)
  (with-eval-after-load 'dired
    (setf dired-listing-switches "-alhFv --group-directories-first"
          dired-auto-revert-buffer #'dired-directory-changed-p)
    (setf dired-recursive-copies 'always)
    (setf dired-dwim-target t)
    (setf dired-garbage-files-regexp (rx "." (or "bak" "orig" "old") eos))
    (dolist (key '("c" "Z" "P"))
      (define-key dired-mode-map (kbd key) nil)))

  (progn ; `dired-aux'
    (init--require-when-compile 'dired-aux)
    (with-eval-after-load 'dired-aux
      (setf dired-create-destination-dirs 'ask)))

  (progn ; `dired-x'
    (init--require-when-compile 'dired-x)
    (declare-function dired-omit-mode "dired-x")

    (setf dired-bind-info nil
          dired-bind-man nil)
    (with-eval-after-load 'dired
      (add-hook 'dired-mode-hook #'dired-omit-mode)
      (define-key dired-mode-map (kbd ")") #'dired-omit-mode))

    (with-eval-after-load 'dired-x
      (setf dired-omit-files "\\`\\."
            dired-omit-extensions nil)
      (define-key dired-mode-map (kbd "V") nil)))

  (progn ; `wdired'
    (init--require-when-compile 'wdired)
    (with-eval-after-load 'wdired
      (setf wdired-allow-to-change-permissions t))))

;; More colors in Dired
(progn ; `diredfl'
  (init-ensure-package 'diredfl)
  (add-hook 'dired-mode-hook #'diredfl-mode))

;; Live filtering in Dired
(progn ; `dired-narrow'
  (init-ensure-package 'dired-narrow)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "/") #'dired-narrow)))

(progn ; `dired-git-info'
  (init-ensure-package 'dired-git-info)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "{") #'dired-git-info-mode)))

(progn ; `dired-atool'
  (autoload 'dired-atool-do-unpack "dired-atool" nil t)
  (autoload 'dired-atool-do-pack "dired-atool" nil t)
  (with-eval-after-load 'dired
    (let ((map dired-mode-map))
      (define-key map (kbd "Z") #'dired-atool-do-unpack)
      (define-key map (kbd "P") #'dired-atool-do-pack))))

(progn ; `image-dired'
  (init--require-when-compile 'image-dired)
  (setf image-dired-dir (init--var "image-dired/")
        image-dired-db-file (init--var "image-dired/db")
        image-dired-gallery-dir (init--var "image-dired/gallery/")
        image-dired-temp-image-file (init--var "image-dired/temp")
        image-dired-temp-rotate-image-file (init--var "image-dired/rotate-temp")))

(progn ; `disk-usage'
  (init-ensure-package 'disk-usage)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "}") #'disk-usage-here)))

(progn ; `trashed'
  (init-ensure-package 'trashed)
  (define-key global-map (kbd "C-c f t") #'trashed)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "`") #'trashed)))

;;; General tool

(progn ; `password-cache'
  (with-eval-after-load 'password-cache
    (setf password-cache-expiry 60)))

(progn ; `auth-source'
  (with-eval-after-load 'auth-source
    (setf auth-sources `(,(init--sync "misc/authinfo.gpg")))
    (setf auth-source-save-behavior nil)
    (setf auth-source-cache-expiry 3600)))

(progn ; `epa'
  (init--require-when-compile 'epa)
  (with-eval-after-load 'epa
    (setf epa-popup-info-window nil)))

(progn ; `re-builder'
  (init--require-when-compile 're-builder)
  (define-key global-map (kbd "C-c m r") #'re-builder)
  (with-eval-after-load 're-builder
    (setf reb-re-syntax 'string)))

(progn ; `calc'
  (init--require-when-compile 'calc)
  (cl-pushnew 'calc init--org-babel-languages :test #'eq)
  (let ((map global-map))
    (define-key map [remap calc-dispatch] #'quick-calc)
    (define-key map (kbd "C-c m c") #'calc)
    (define-key map (kbd "C-c x c") #'calc-grab-region)
    (define-key map (kbd "C-c x C") #'calc-grab-rectangle))
  (with-eval-after-load 'calc
    (setf calc-gnuplot-default-device "qt"))

  (progn ; `calc-ext'
    (declare-function calc-reset "calc-ext")
    (with-eval-after-load 'calc
      (define-key calc-mode-map (kbd "C-c m c") #'calc-reset)))

  (progn ; `calc-yank'
    (autoload 'calc-copy-to-buffer "calc-yank" nil t)
    (with-eval-after-load 'calc
      (define-key global-map (kbd "C-c e c") #'calc-copy-to-buffer))))

(progn ; `help'
  (with-eval-after-load 'help
    (setf help-window-select t)))

(progn ; `info'
  (define-key global-map (kbd "C-c m i") #'info))

(progn ; `man'
  (init--require-when-compile 'man)
  (define-key global-map (kbd "C-c m k") #'man)
  (with-eval-after-load 'man
    (setf Man-notify-method 'aggressive)))

(progn ; `diff'
  (define-key global-map (kbd "C-c f =") #'diff)
  (define-key global-map (kbd "C-c b =") #'diff-buffer-with-file))

(progn ; `ediff'
  (init--require-when-compile 'ediff)
  (declare-function ediff-setup-windows-plain "ediff-wind")
  (with-eval-after-load 'ediff
    (setf ediff-window-setup-function #'ediff-setup-windows-plain
          ediff-split-window-function #'split-window-horizontally)))

(progn ; `htmlize'
  (init-ensure-package 'htmlize))

(progn ; `edit-indirect'
  (init-ensure-package 'edit-indirect)
  (define-key global-map (kbd "C-c x i") #'edit-indirect-region))

(progn ; `comint'
  (init--require-when-compile 'comint)
  (declare-function comint-send-eof "comint")

  (with-eval-after-load 'comint
    (setf comint-prompt-read-only nil
          comint-scroll-to-bottom-on-input 'this)
    (let ((map comint-mode-map))
      (define-key map [remap comint-delchar-or-maybe-eof] #'delete-char)
      (define-key map (kbd "C-c a a") #'comint-send-eof))))

(progn ; `proced'
  (define-key global-map (kbd "C-c a L") #'proced))

;; Live filtering in Proced
(progn ; `proced-narrow'
  (init-ensure-package 'proced-narrow)
  (init--require-when-compile 'proced)
  (with-eval-after-load 'proced
    (define-key proced-mode-map (kbd "/") #'proced-narrow)))

;;; Eshell

(progn ; `eshell'
  (define-key global-map (kbd "C-c a e") #'eshell)

  (progn ; `esh-mode'
    (init--require-when-compile 'esh-mode)
    (setf eshell-directory-name (init--var "eshell/"))
    (with-eval-after-load 'esh-mode
      (setf eshell-scroll-to-bottom-on-input 'this)))

  (progn ; `esh-var'
    (init--require-when-compile 'esh-var)
    (with-eval-after-load 'esh-var
      (setf eshell-modify-global-environment t)))

  (progn ; `esh-module'
    (with-eval-after-load 'esh-module
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
                                  eshell-unix))))

  (progn ; `em-alias'
    (init--require-when-compile 'em-alias)
    (setf eshell-aliases-file (init--etc "eshell/aliases")))

  (progn ; `em-glob'
    (init--require-when-compile 'em-glob)
    (with-eval-after-load 'em-glob
      (setf eshell-error-if-no-glob t)))

  (progn ; `em-hist'
    (init--require-when-compile 'em-hist)
    (declare-function eshell-input-filter-initial-space "em-hist")
    (with-eval-after-load 'em-hist
      (setf eshell-history-size 2000
            eshell-input-filter #'eshell-input-filter-initial-space)))

  (progn ; `em-script'
    (init--require-when-compile 'em-script)
    (setf eshell-login-script (init--etc "eshell/login")
          eshell-rc-script (init--etc "eshell/profile"))))

(progn ; `eshell-z'
  (init-ensure-package 'eshell-z)
  (with-eval-after-load 'eshell
    (require 'eshell-z)))

(progn ; `eshell-extras'
  (autoload 'eshell-extras-setup "eshell-extras")
  (with-eval-after-load 'eshell
    (eshell-extras-setup)))

;;; Browser & viewer

(progn ; `bug-reference'
  (init--require-when-compile 'bug-reference)
  (declare-function bug-reference-push-button "bug-reference")

  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (define-key global-map (kbd "C-c t u") #'bug-reference-mode)
  (define-key global-map (kbd "C-c t U") #'bug-reference-prog-mode)
  (with-eval-after-load 'bug-reference
    (define-key bug-reference-map (kbd "C-c C-o") #'bug-reference-push-button)))

(progn ; `goto-addr'
  (init--require-when-compile 'goto-addr)

  (dolist (hook '(rst-mode-hook
                  comint-mode-hook
                  cider-repl-mode-hook
                  eshell-mode-hook))
    (add-hook hook #'goto-address-mode))
  (dolist (hook '(prog-mode-hook TeX-mode-hook conf-mode-hook yaml-mode-hook))
    (add-hook hook #'goto-address-prog-mode))

  (define-key global-map (kbd "C-c t a") #'goto-address-mode)
  (define-key global-map (kbd "C-c t A") #'goto-address-prog-mode)

  (with-eval-after-load 'goto-addr
    (define-key goto-address-highlight-keymap
      (kbd "C-c C-o") #'goto-address-at-point)))

(progn ; `eww'
  (init--require-when-compile 'eww)
  (setf eww-bookmarks-directory (init--var "eww/"))
  (make-directory eww-bookmarks-directory t)
  (define-key global-map (kbd "C-c f e") #'eww-open-file)
  (with-eval-after-load 'eww
    (setf eww-search-prefix "https://www.google.com/search?q=")))

(progn ; `elfeed'
  (init-ensure-package 'elfeed)
  (init--require-when-compile 'elfeed)
  (declare-function elfeed-make-tagger "elfeed")

  (setf elfeed-db-directory (init--sync "misc/elfeed/db/"))
  (with-eval-after-load 'recentf
    (push "/elfeed/db/" recentf-exclude))
  (define-key global-map (kbd "C-c m w") #'elfeed)

  (with-eval-after-load 'elfeed
    (load (init--sync "misc/elfeed/feeds.el") t t t)
    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :before "10 days ago"
                                  :remove 'unread)))

  (progn ; `elfeed-search'
    (init--require-when-compile 'elfeed-search)
    (with-eval-after-load 'elfeed-search
      (setf elfeed-search-filter "@1-month-ago")))

  (progn ; `elfeed-show'
    (init--require-when-compile 'elfeed-show)
    (setf elfeed-enclosure-default-dir
          (expand-file-name (convert-standard-filename "Downloads/") "~"))
    (with-eval-after-load 'elfeed-show
      (setf elfeed-show-entry-switch #'pop-to-buffer-same-window))))

(progn ; `doc-view'
  (init--require-when-compile 'doc-view)
  (with-eval-after-load 'doc-view
    (setf doc-view-continuous t)
    (setf doc-view-resolution 300)
    (define-key doc-view-mode-map (kbd "&") #'browse-url-of-file)))

(progn ; `nov'
  (init-ensure-package 'nov)
  (init--require-when-compile 'nov)
  (setf nov-save-place-file (init--var "nov-places"))
  (cl-pushnew '("\\.epub\\'" . nov-mode) auto-mode-alist :test #'equal))

;;; Chatting

(progn ; `erc'
  (init--require-when-compile 'erc)
  ;; NOTE: For some unknown reason the compiler complains that this function may
  ;; not be defined at runtime, but it is actually there.
  (declare-function erc-tls "erc")

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
  (define-key global-map (kbd "C-c m e") #'erc-tls)

  (with-eval-after-load 'erc
    (setf erc-nick "pengjiz"
          erc-try-new-nick-p nil)
    (setf erc-user-full-name "Pengji Zhang")
    (setf erc-lurker-hide-list '("JOIN" "PART" "QUIT")))

  (progn ; `erc-join'
    (init--require-when-compile 'erc-join)
    (with-eval-after-load 'erc-join
      ;; Join channels only after identifying
      (setf erc-autojoin-timing 'ident
            erc-autojoin-delay 0)))

  (progn ; `erc-services'
    (init--require-when-compile 'erc-services)
    (setf erc-nickserv-identify-mode 'autodetect)
    (with-eval-after-load 'erc-services
      (setf erc-use-auth-source-for-nickserv-password t)))

  (progn ; `erc-button'
    (init--require-when-compile 'erc-button)
    (with-eval-after-load 'erc-button
      (setf erc-button-alist
            '(('nicknames 0 erc-button-buttonize-nicks erc-nick-popup 0)
              (erc-button-url-regexp 0 t browse-url 0)
              ("<URL: *\\([^<> ]+\\) *>" 0 t browse-url 1)
              ("\\s-\\(@\\([0-9][0-9][0-9]\\)\\)" 1 t erc-button-beats-to-time 2)))))

  (progn ; `erc-truncate'
    (init--require-when-compile 'erc-truncate)
    (with-eval-after-load 'erc-truncate
      (setf erc-max-buffer-size 100000))))

(progn ; `erc-extras'
  (autoload 'erc-extras-setup "erc-extras")
  (with-eval-after-load 'erc
    (erc-extras-setup)))

;;; Calendar

(progn ; `calendar'
  (init--require-when-compile 'calendar)
  (declare-function calendar-mark-today "calendar")

  (let ((sync-directory (init--sync "misc/")))
    (make-directory sync-directory t)
    (setf diary-file (expand-file-name "diary" sync-directory)))
  (setf calendar-date-style 'iso)
  (define-key global-map (kbd "C-c m d") #'calendar)

  (with-eval-after-load 'calendar
    (setf calendar-mark-holidays-flag t
          calendar-chinese-all-holidays-flag t)
    (add-hook 'calendar-today-visible-hook #'calendar-mark-today))

  (progn ; `holidays'
    (init--require-when-compile 'holidays)
    (setf holiday-bahai-holidays nil
          holiday-islamic-holidays nil
          holiday-hebrew-holidays nil
          holiday-christian-holidays nil))

  (progn ; `solar'
    (init--require-when-compile 'solar)
    (setf calendar-location-name "Pittsburgh, PA"
          calendar-latitude 40.4
          calendar-longitude -79.9))

  (progn ; `diary-lib'
    (init--require-when-compile 'diary-lib)
    (with-eval-after-load 'diary-lib
      (setf diary-comment-start "##")))

  (progn ; `appt'
    (init--require-when-compile 'appt)
    (with-eval-after-load 'appt
      (setf appt-display-diary nil
            appt-display-mode-line nil)
      (setf appt-display-interval 10
            appt-message-warning-time 20))))

(progn ; `appt-extras'
  (autoload 'appt-extras-setup "appt-extras")
  (autoload 'appt-extras-set-reminder "appt-extras" nil t)
  (add-hook 'after-init-hook #'appt-extras-setup)
  (define-key global-map (kbd "C-c o r") #'appt-extras-set-reminder))

;;; General development

(progn ; `project'
  (init--require-when-compile 'project)
  (setf project-list-file (init--var "projects"))
  (with-eval-after-load 'project
    (setf project-vc-merge-submodules nil)
    (setf project-switch-commands '((project-find-file "Find file")
                                    (project-find-dir "Find directory")
                                    (project-find-regexp "Find regexp")
                                    (project-eshell "Eshell")))
    (dolist (key '("s" "v"))
      (define-key project-prefix-map (kbd key) nil))))

(progn ; `editorconfig'
  (init-ensure-package 'editorconfig)
  (editorconfig-mode))

(progn ; `eldoc'
  (with-eval-after-load 'eldoc
    (setf eldoc-echo-area-use-multiline-p nil)
    (setf eldoc-message-function #'message)
    ;; Describe the character at point by default
    (add-hook 'eldoc-documentation-functions #'describe-char-eldoc t)))

(progn ; `xref'
  (init--require-when-compile 'xref)
  (with-eval-after-load 'xref
    (when (executable-find "rg")
      (setf xref-search-program 'ripgrep))))

(progn ; `etags'
  (init--require-when-compile 'etags)
  (define-key global-map (kbd "C-c c t") #'visit-tags-table)
  (with-eval-after-load 'etags
    (setf tags-revert-without-query t)))

(progn ; `compile'
  (init--require-when-compile 'compile)
  (autoload 'recompile "compile" nil t)

  (define-key global-map (kbd "C-c c c") #'compile)
  (define-key global-map (kbd "C-c c C") #'recompile)
  (with-eval-after-load 'compile
    (setf compilation-ask-about-save nil
          compilation-always-kill t)
    (setf compilation-scroll-output 'first-error)))

(progn ; `firestarter'
  (init-ensure-package 'firestarter)
  (init--require-when-compile 'firestarter)
  (define-key global-map (kbd "C-c c f") #'firestarter-mode)
  (with-eval-after-load 'firestarter
    (setf firestarter-default-type t)))

(progn ; `gud'
  (init--require-when-compile 'gud)
  (define-key global-map (kbd "C-c c g") #'gud-gdb)
  (define-key global-map (kbd "C-c c p") #'pdb)
  (with-eval-after-load 'gud
    (setf gud-pdb-command-name "python -m pdb")))

(progn ; `rmsbolt'
  (init-ensure-package 'rmsbolt)
  (init--require-when-compile 'rmsbolt)
  (define-key global-map (kbd "C-c c r") #'rmsbolt-mode)
  (with-eval-after-load 'rmsbolt
    (setf rmsbolt-automatic-recompile nil)))

;;; Lint

(progn ; `flycheck'
  (init-ensure-package 'flycheck)
  (init--require-when-compile 'flycheck)
  (declare-function flycheck-list-errors "flycheck")

  (define-key global-map (kbd "C-c t e") #'flycheck-mode)
  (dolist (hook '(emacs-lisp-mode-hook
                  clojure-mode-hook
                  scheme-mode-hook
                  c-mode-hook
                  c++-mode-hook
                  rust-mode-hook
                  haskell-mode-hook
                  ess-r-mode-hook
                  js2-mode-hook
                  typescript-mode-hook
                  python-mode-hook
                  sh-mode-hook
                  LaTeX-mode-hook
                  ledger-mode-hook))
    (add-hook hook #'flycheck-mode))

  (with-eval-after-load 'flycheck
    (setf flycheck-check-syntax-automatically '(save mode-enabled))
    (define-key flycheck-mode-map (kbd "M-g l") #'flycheck-list-errors)))

(progn ; `flycheck-inline'
  (init-ensure-package 'flycheck-inline)
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;; Spell check

(progn ; `ispell'
  (define-key global-map (kbd "C-c t S") #'ispell-change-dictionary))

(progn; `flyspell'
  (init--require-when-compile 'flyspell)

  (setf flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (setf flyspell-use-meta-tab nil
        flyspell-mode-map (make-sparse-keymap))

  (dolist (hook '(text-mode-hook bibtex-mode-hook))
    (add-hook hook #'flyspell-mode))
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (define-key global-map (kbd "C-c t s") #'flyspell-mode)
  (define-key global-map (kbd "C-c x s") #'flyspell-region)

  (with-eval-after-load 'flyspell
    (setf flyspell-abbrev-p t
          flyspell-use-global-abbrev-table-p t)))

(progn ; `flyspell-correct'
  (init-ensure-package 'flyspell-correct)
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "M-$") #'flyspell-correct-at-point)))

;;; VCS

(progn ; `vc-hooks'
  (with-eval-after-load 'vc-hooks
    (setf vc-handled-backends '(Git))
    (setf vc-follow-symlinks t)))

(progn ; `magit'
  (init-ensure-package 'magit)
  (init--require-when-compile 'magit)
  (declare-function magit-display-buffer-fullframe-status-v1 "magit-mode")
  (declare-function magit-add-section-hook "magit-section")

  (setf magit-define-global-key-bindings nil)
  (let ((map global-map))
    (define-key map (kbd "C-c g g") #'magit-status)
    (define-key map (kbd "C-c g d") #'magit-dispatch)
    (define-key map (kbd "C-c g f") #'magit-file-dispatch))

  (with-eval-after-load 'magit
    (setf magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (setf magit-save-repository-buffers 'dontask)
    (setf magit-revision-show-gravatars nil
          magit-diff-refine-hunk t)
    ;; Show submodules in status buffer
    (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-modules
                            'magit-insert-stashes t))

  (progn ; `magit-extras'
    (init--require-when-compile 'magit-extras)
    (setf magit-bind-magit-project-status nil)))

(progn ; `git-commit'
  (init-ensure-package 'git-commit)
  (init--require-when-compile 'git-commit)
  (declare-function git-commit-turn-on-auto-fill "git-commit")
  (declare-function git-commit-turn-on-flyspell "git-commit")

  (defun init--set-git-commit-fill-column (&rest _)
    "Set fill column for Git commit messages when appropriate."
    (unless (local-variable-p 'fill-column)
      (setf fill-column 72)))

  (with-eval-after-load 'git-commit
    (setf git-commit-summary-max-length 50)
    (advice-add #'git-commit-turn-on-auto-fill :before
                #'init--set-git-commit-fill-column)
    (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)))

;; Show edits
(progn ; `diff-hl'
  (init-ensure-package 'diff-hl)

  (require 'diff-hl)
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)

  (let ((map diff-hl-mode-map))
    (define-key map (kbd "C-c g p") #'diff-hl-previous-hunk)
    (define-key map (kbd "C-c g n") #'diff-hl-next-hunk))
  (define-key diff-hl-command-map (kbd "SPC") #'diff-hl-mark-hunk)

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(progn ; `git-modes'
  (init-ensure-package 'git-modes))

;;; Programming

(progn ; `sh-script'
  (init--require-when-compile 'sh-script)
  (declare-function sh-show-shell "sh-script")

  (cl-pushnew 'shell init--org-babel-languages :test #'eq)
  (setf sh-shell-file "/bin/bash")
  (with-eval-after-load 'sh-script
    (setf sh-basic-offset 2)
    (define-key sh-mode-map (kbd "C-c a a") #'sh-show-shell)))

(progn ; `lua-mode'
  (init-ensure-package 'lua-mode)
  (init--require-when-compile 'lua-mode)
  (with-eval-after-load 'lua-mode
    (setf lua-indent-level 2)))

(progn ; `glsl-mode'
  (init-ensure-package 'glsl-mode)
  (init--require-when-compile 'glsl-mode)
  (declare-function c-indent-exp "cc-cmds")
  (declare-function c-backslash-region "cc-cmds")
  (declare-function c-forward-conditional "cc-cmds")
  (declare-function c-backward-conditional "cc-cmds")
  (declare-function c-up-conditional "cc-cmds")
  (declare-function c-beginning-of-statement "cc-cmds")
  (declare-function c-end-of-statement "cc-cmds")

  (setf glsl-mode-map (make-sparse-keymap))
  (with-eval-after-load 'glsl-mode
    (let ((map glsl-mode-map))
      (define-key map (kbd "C-M-q") #'c-indent-exp)
      (define-key map (kbd "C-c C-\\") #'c-backslash-region)
      (define-key map (kbd "C-c C-n") #'c-forward-conditional)
      (define-key map (kbd "C-c C-p") #'c-backward-conditional)
      (define-key map (kbd "C-c C-u") #'c-up-conditional)
      (define-key map [remap backward-sentence] #'c-beginning-of-statement)
      (define-key map [remap forward-sentence] #'c-end-of-statement))))

(progn ; `nasm-mode'
  (init-ensure-package 'nasm-mode)
  (cl-pushnew '("\\.nasm\\'" . nasm-mode) auto-mode-alist :test #'equal))

(progn ; `bnf-mode'
  (init-ensure-package 'bnf-mode))

(progn ; `graphviz-dot-mode'
  (autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
  (dolist (pattern '("\\.gv\\'" "\\.dot\\'"))
    (cl-pushnew `(,pattern . graphviz-dot-mode) auto-mode-alist :test #'equal))
  (cl-pushnew 'dot init--org-babel-languages :test #'eq)
  (with-eval-after-load 'org
    (push '("dot" . graphviz-dot) org-src-lang-modes)))

(progn ; `gnuplot'
  (init--require-when-compile 'gnuplot)
  (declare-function gnuplot-run-region "gnuplot")
  (declare-function gnuplot-run-buffer "gnuplot")
  (autoload 'gnuplot-mode "gnuplot" nil t)
  (cl-pushnew '("\\.gp\\'" . gnuplot-mode) auto-mode-alist :test #'equal)
  (with-eval-after-load 'gnuplot
    (let ((map gnuplot-mode-map))
      (define-key map (kbd "C-c C-r") #'gnuplot-run-region)
      (define-key map (kbd "C-c C-b") #'gnuplot-run-buffer))))

;;; Emacs Lisp

(progn ; `elisp-mode'
  (cl-pushnew 'emacs-lisp init--org-babel-languages :test #'eq)
  (with-eval-after-load 'elisp-mode
    (let ((map emacs-lisp-mode-map))
      (define-key map (kbd "C-c C-r") #'eval-region)
      (define-key map (kbd "C-c C-b") #'eval-buffer)
      (define-key map (kbd "C-c a a") #'ielm))))

(progn ; `eros'
  (init-ensure-package 'eros)
  (with-eval-after-load 'elisp-mode
    (eros-mode)))

(progn ; `emacs-lisp-snippets'
  (autoload 'emacs-lisp-snippets-file-template "emacs-lisp-snippets" nil t)
  (with-eval-after-load 'autoinsert
    (define-auto-insert '("\\.el\\'" . "Emacs Lisp file template")
      #'emacs-lisp-snippets-file-template)))

(progn ; `ielm'
  (init--require-when-compile 'ielm)
  (define-key global-map (kbd "C-c a i") #'ielm)
  (with-eval-after-load 'ielm
    (setf ielm-prompt-read-only nil)))

;;; C & C++ & AWK

(progn ; `cc-mode'
  (init--require-when-compile 'cc-mode)
  (declare-function c-context-line-break "cc-cmds")

  (cl-pushnew 'awk init--org-babel-languages :test #'eq)
  (setf c-default-style nil)

  (with-eval-after-load 'cc-mode
    (c-add-style "common" '("k&r"
                            (c-basic-offset . 4)
                            (c-doc-comment-style . ((c-mode . doxygen)
                                                    (c++-mode . doxygen)))))
    (dolist (style '((other . "common")
                     (awk-mode . "awk")))
      (cl-pushnew style c-default-style :test #'eq :key #'car))
    (define-key c-mode-base-map (kbd "C-c C-b") #'c-context-line-break)))

(progn ; `cmacexp'
  (init--require-when-compile 'cmacexp)
  (with-eval-after-load 'cmacexp
    (setf c-macro-prompt-flag t)))

(progn ; `c-snippets'
  (autoload 'c-snippets-header-template "c-snippets" nil t)
  (with-eval-after-load 'autoinsert
    (define-auto-insert `(,(rx "." (or "h" "H" "hh" "hpp" "hxx" "h++") eos)
                          . "C/C++ header template")
      #'c-snippets-header-template)))

(progn ; `company-c-headers'
  (init-ensure-package 'company-c-headers))

(progn ; `cmake-mode'
  (init-ensure-package 'cmake-mode))

(progn ; `cmake-ide'
  (init-ensure-package 'cmake-ide)
  (with-eval-after-load 'cc-mode
    (cmake-ide-setup)))

;;; Clojure

(progn ; `clojure-mode'
  (init-ensure-package 'clojure-mode))

(progn ; `cider'
  (init-ensure-package 'cider)
  (init--require-when-compile 'clojure-mode)
  (with-eval-after-load 'clojure-mode
    (define-key clojure-mode-map (kbd "C-c a a") #'cider))

  (progn ; `cider-eval'
    (init--require-when-compile 'cider-eval)
    (with-eval-after-load 'cider-eval
      (setf cider-save-file-on-load t)))

  (progn ; `cider-repl'
    (init--require-when-compile 'cider-repl)
    (declare-function cider-quit "cider-connection")
    (with-eval-after-load 'cider-repl
      (setf cider-repl-display-help-banner nil)
      (define-key cider-repl-mode-map (kbd "C-c a a") #'cider-quit)))

  (progn ; `nrepl-client'
    (init--require-when-compile 'nrepl-client)
    (with-eval-after-load 'nrepl-client
      (setf nrepl-hide-special-buffers t))))

(progn ; `flycheck-clj-kondo'
  (init-ensure-package 'flycheck-clj-kondo)
  (with-eval-after-load 'flycheck
    (require 'flycheck-clj-kondo)))

;;; Racket

(progn ; `geiser'
  (init-ensure-package 'geiser)

  (progn ; `geiser-mode'
    (init--require-when-compile 'geiser-mode)
    (with-eval-after-load 'geiser-mode
      (define-key geiser-mode-map (kbd "C-c a a") #'run-geiser)))

  (progn ; `geiser-repl'
    (init--require-when-compile 'geiser-repl)
    (declare-function geiser-repl-exit "geiser-repl")

    (make-directory (init--var "geiser/") t)
    (setf geiser-repl-history-filename (init--var "geiser/history"))

    (with-eval-after-load 'geiser-repl
      (setf geiser-repl-company-p nil)
      (setf geiser-repl-read-only-prompt-p nil
            geiser-repl-read-only-output-p nil)
      (define-key geiser-repl-mode-map (kbd "C-c a a") #'geiser-repl-exit)))

  (progn ; `geiser-autodoc'
    (init--require-when-compile 'geiser-autodoc)
    (with-eval-after-load 'geiser-autodoc
      (setf geiser-autodoc-delay eldoc-idle-delay))))

(progn ; `geiser-racket'
  (init-ensure-package 'geiser-racket))

(progn ; `scribble-mode'
  (init-ensure-package 'scribble-mode))

;;; Rust

(progn ; `rust-mode'
  (init-ensure-package 'rust-mode))

(progn ; `racer'
  (init-ensure-package 'racer)
  (init--require-when-compile 'racer)
  (add-hook 'rust-mode-hook #'racer-mode)
  (with-eval-after-load 'racer
    (setf racer-complete-insert-argument-placeholders nil)))

(progn ; `flycheck-rust'
  (init-ensure-package 'flycheck-rust)
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; pest parser
(progn ; `pest-mode'
  (autoload 'pest-mode "pest-mode" nil t)
  (cl-pushnew '("\\.pest\\'" . pest-mode) auto-mode-alist :test #'equal))

;;; Haskell

(progn ; `haskell-mode'
  (init-ensure-package 'haskell-mode)
  (init--require-when-compile 'haskell-mode)
  (with-eval-after-load 'haskell-mode
    (dolist (key '("C-c C-l" "C-c C-b" "C-c C-t" "C-c C-i"))
      (define-key haskell-mode-map (kbd key) nil)))

  (progn ; `haskell'
    (init--require-when-compile 'haskell)
    (setf interactive-haskell-mode-map (make-sparse-keymap))
    (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

    (with-eval-after-load 'haskell
      (setf haskell-interactive-popup-errors nil
            haskell-interactive-mode-read-only nil
            haskell-interactive-prompt-read-only nil)
      (setf haskell-process-auto-import-loaded-modules t
            haskell-process-suggest-remove-import-lines t
            haskell-process-show-overlays nil)

      (let ((map interactive-haskell-mode-map))
        (define-key map (kbd "C-c a a") #'haskell-interactive-bring)
        (define-key map (kbd "C-c C-l") #'haskell-process-load-file)
        (define-key map (kbd "C-c C-r") #'haskell-process-reload))))

  (progn ; `haskell-interactive-mode'
    (init--require-when-compile 'haskell-interactive-mode)
    (declare-function haskell-interactive-mode-clear "haskell-interactive-mode")
    (with-eval-after-load 'haskell-interactive-mode
      (let ((map haskell-interactive-mode-map))
        (define-key map (kbd "C-c a a") #'haskell-interactive-kill)
        (define-key map (kbd "C-c M-o") #'haskell-interactive-mode-clear))))

  (progn ; `haskell-indentation'
    (add-hook 'haskell-mode-hook #'haskell-indentation-mode))

  (progn ; `haskell-collapse'
    (init--require-when-compile 'haskell-collapse)
    (declare-function haskell-hide-toggle "haskell-collapse")
    (declare-function haskell-hide-toggle-all "haskell-collapse")

    (add-hook 'haskell-mode-hook #'haskell-collapse-mode)
    (setf haskell-collapse-mode-map (make-sparse-keymap))
    (with-eval-after-load 'haskell-collapse
      (let ((map haskell-collapse-mode-map))
        (define-key map (kbd "C-c @ t") #'haskell-hide-toggle)
        (define-key map (kbd "C-c @ T") #'haskell-hide-toggle-all)))))

(progn ; `dante'
  (init-ensure-package 'dante)
  (init--require-when-compile 'dante)
  (declare-function dante-eval-block "dante")
  (declare-function dante-type-at "dante")
  (declare-function dante-info "dante")

  (add-hook 'haskell-mode-hook #'dante-mode)
  (setf dante-mode-map (make-sparse-keymap))
  (with-eval-after-load 'dante
    (let ((map dante-mode-map))
      (define-key map (kbd "C-c C-c") #'dante-eval-block)
      (define-key map (kbd "C-c C-t") #'dante-type-at)
      (define-key map (kbd "C-c TAB") #'dante-info))
    (with-eval-after-load 'company
      (setf (default-value 'company-backends)
            (delq 'dante-company (default-value 'company-backends))))))

;;; R

(progn ; `ess'
  (init-ensure-package 'ess)
  (init--require-when-compile 'ess)

  (cl-pushnew 'R init--org-babel-languages :test #'eq)
  (setf ess-write-to-dribble nil)
  (setf ess-history-directory (init--var "ess/history/"))
  (make-directory ess-history-directory t)

  (with-eval-after-load 'ess
    (setf ess-style 'RStudio)
    (setf ess-use-ido nil
          ess-use-flymake nil)
    (setf ess-ask-for-ess-directory nil
          ess-auto-width 'window
          inferior-R-args "--no-save"))

  (progn ; `ess-inf'
    (init--require-when-compile 'ess-inf)
    (declare-function ess-quit "ess-inf")
    (with-eval-after-load 'ess-inf
      (define-key inferior-ess-mode-map (kbd "C-c a a") #'ess-quit)))

  (progn ; `ess-r-mode'
    (init--require-when-compile 'ess-r-mode)
    (declare-function ess-cycle-assign "ess-s-lang")
    (with-eval-after-load 'ess-r-mode
      (let ((map ess-r-mode-map))
        (define-key map (kbd "C-c a a") #'run-ess-r)
        (define-key map (kbd "C-c a m") #'ess-rdired)
        (define-key map (kbd ";") #'ess-cycle-assign))
      (let ((map inferior-ess-mode-map))
        (define-key map (kbd "C-c a m") #'ess-rdired)
        (define-key map (kbd ";") #'ess-cycle-assign)))))

;;; Web

(progn ; `sgml-mode'
  (init--require-when-compile 'sgml-mode)
  (with-eval-after-load 'sgml-mode
    (define-key sgml-mode-map (kbd "C-c C-v") nil)))

(progn ; `html-snippets'
  (autoload 'html-snippets-file-template "html-snippets" nil t)
  (with-eval-after-load 'autoinsert
    (define-auto-insert '(html-mode . "HTML file template")
      #'html-snippets-file-template)))

(progn ; `nxml-mode'
  (init--require-when-compile 'nxml-mode)
  (with-eval-after-load 'nxml-mode
    (setf nxml-slash-auto-complete-flag t)
    (setf nxml-attribute-indent 2)))

(progn ; `simple-httpd'
  (init-ensure-package 'simple-httpd)
  (init--require-when-compile 'simple-httpd)
  (define-key global-map (kbd "C-c m s") #'httpd-serve-directory)
  (with-eval-after-load 'simple-httpd
    (setf httpd-host 'local
          httpd-port 8017)))

(progn ; `css-mode'
  (init--require-when-compile 'css-mode)
  (declare-function css-cycle-color-format "css-mode")
  (setf css-mode-map (make-sparse-keymap))
  (with-eval-after-load 'css-mode
    (setf css-indent-offset 2)
    (define-key css-mode-map (kbd "C-c C-f") #'css-cycle-color-format)))

(progn ; `js'
  (init--require-when-compile 'js)
  (with-eval-after-load 'js
    (setf js-indent-level 2
          js-switch-indent-offset 2
          js-chain-indent nil)
    (dolist (key '("C-c M-:" "C-c C-j" "C-M-x"))
      (define-key js-mode-map (kbd key) nil))))

(progn ; `js2-mode'
  (init-ensure-package 'js2-mode)
  (init--require-when-compile 'js2-mode)
  (cl-pushnew '("\\.jsm?\\'" . js2-mode) auto-mode-alist :test #'equal)
  (cl-pushnew '("node" . js2-mode) interpreter-mode-alist :test #'equal)
  (with-eval-after-load 'js2-mode
    (setf js2-skip-preprocessor-directives t)
    (setf js2-highlight-level 3
          js2-highlight-external-variables nil)
    (setf js2-mode-show-parse-errors nil
          js2-mode-show-strict-warnings nil
          js2-strict-missing-semi-warning nil)))

(progn ; `typescript-mode'
  (init-ensure-package 'typescript-mode)
  (init--require-when-compile 'typescript-mode)
  (with-eval-after-load 'typescript-mode
    (setf typescript-indent-level 2)))

(progn ; `tide'
  (init-ensure-package 'tide)
  (init--require-when-compile 'tide)

  (setf tide-completion-setup-company-backend nil)
  (defun init--enable-tide ()
    (unless (file-remote-p default-directory)
      (tide-setup)))
  (dolist (hook '(js2-mode-hook typescript-mode-hook))
    (add-hook hook #'init--enable-tide))

  (with-eval-after-load 'tide
    (setf tide-completion-enable-autoimport-suggestions nil)))

(progn ; `skewer-mode'
  (init-ensure-package 'skewer-mode)
  (init--require-when-compile 'skewer-mode)
  (add-hook 'js2-mode-hook #'skewer-mode)
  (with-eval-after-load 'skewer-mode
    (define-key skewer-mode-map (kbd "C-c a a") #'run-skewer))

  (progn ; `skewer-repl'
    (with-eval-after-load 'skewer-mode
      (define-key skewer-mode-map (kbd "C-c a m") #'skewer-repl)))

  (progn ; `skewer-css'
    (init--require-when-compile 'skewer-css)
    (add-hook 'css-mode-hook #'skewer-css-mode)
    (with-eval-after-load 'skewer-css
      (define-key skewer-css-mode-map (kbd "C-c a a") #'run-skewer)))

  (progn ; `skewer-html'
    (init--require-when-compile 'skewer-html)
    (add-hook 'html-mode-hook #'skewer-html-mode)
    (with-eval-after-load 'skewer-html
      (define-key skewer-html-mode-map (kbd "C-c a a") #'run-skewer))))

(progn ; `flycheck-npm'
  (init-ensure-package 'flycheck)
  (autoload 'flycheck-npm-setup "flycheck-npm")
  (dolist (hook '(html-mode-hook
                  css-mode-hook
                  js2-mode-hook
                  typescript-mode-hook))
    (add-hook hook #'flycheck-npm-setup)))

(progn ; `ob-http'
  (init-ensure-package 'ob-http)
  (cl-pushnew 'http init--org-babel-languages :test #'eq))

;;; Python

(progn ; `python'
  (init--require-when-compile 'python)
  (cl-pushnew 'python init--org-babel-languages :test #'eq)

  (defun init--setup-python-mode ()
    (setf fill-column 79))
  (defun init--setup-inferior-python-mode ()
    (kill-local-variable 'comint-prompt-read-only))

  (with-eval-after-load 'python
    (setf python-indent-guess-indent-offset-verbose nil
          python-indent-offset 4)
    (setf python-shell-interpreter "python")
    (add-hook 'python-mode-hook #'init--setup-python-mode)
    (add-hook 'inferior-python-mode-hook #'init--setup-inferior-python-mode)
    (define-key python-mode-map (kbd "C-c a a") #'run-python)))

(progn ; `anaconda-mode'
  (init-ensure-package 'anaconda-mode)
  (init--require-when-compile 'anaconda-mode)
  (declare-function anaconda-mode-show-doc "anaconda-mode")
  (declare-function anaconda-mode-find-definitions "anaconda-mode")
  (declare-function anaconda-mode-find-references "anaconda-mode")
  (declare-function anaconda-mode-find-assignments "anaconda-mode")

  (setf anaconda-mode-installation-directory (init--var "anaconda-mode/"))
  (defun init--enable-anaconda ()
    (unless (file-remote-p default-directory)
      (anaconda-mode)
      (anaconda-eldoc-mode)))
  (add-hook 'python-mode-hook #'init--enable-anaconda)
  (setf anaconda-mode-map (make-sparse-keymap))

  (with-eval-after-load 'anaconda-mode
    (setf anaconda-mode-eldoc-as-single-line t)
    (let ((map anaconda-mode-map))
      (define-key map [remap python-describe-at-point] #'anaconda-mode-show-doc)
      (define-key map [remap xref-find-definitions] #'anaconda-mode-find-definitions)
      (define-key map [remap xref-find-references] #'anaconda-mode-find-references)
      (define-key map (kbd "C-c C-v") #'anaconda-mode-find-assignments))))

(progn ; `company-anaconda'
  (init-ensure-package 'company-anaconda))

(progn ; `conda'
  (autoload 'conda-activate "conda" nil t)
  (autoload 'conda-activate-default "conda" nil t)
  (autoload 'conda-deactivate "conda" nil t)
  (let ((map global-map))
    (define-key map (kbd "C-c a p") #'conda-activate)
    (define-key map (kbd "C-c a P") #'conda-activate-default)
    (define-key map (kbd "C-c a o") #'conda-deactivate)))

(progn ; `pip-requirements'
  (init-ensure-package 'pip-requirements))

;;; Writing

(progn ; `text-mode'
  (dolist (pattern '("/LICENSE\\'" "/UNLICENSE\\'"))
    (cl-pushnew `(,pattern . text-mode) auto-mode-alist :test #'equal))
  (with-eval-after-load 'autoinsert
    (define-auto-insert '("/UNLICENSE\\'" . "The Unlicense")
      "unlicense")))

(progn ; `markdown-mode'
  (init-ensure-package 'markdown-mode)
  (init--require-when-compile 'markdown-mode)
  (cl-pushnew '("\\.Rmd\\'" . markdown-mode) auto-mode-alist :test #'equal)
  (with-eval-after-load 'markdown-mode
    (setf (default-value 'markdown-enable-math) t)
    (setf markdown-fontify-code-blocks-natively t)
    (setf markdown-max-image-size '(300 . nil))
    (setf markdown-css-paths `(,(concat "file://" (init--etc "css/pandoc.css")))
          markdown-command '("pandoc" "--section-divs" "--from=markdown" "--to=html5"))))

(progn ; `rst'
  (init--require-when-compile 'rst)
  (declare-function rst-forward-section "rst")
  (declare-function rst-backward-section "rst")
  (declare-function rst-insert-list "rst")

  (with-eval-after-load 'rst
    (let ((map rst-mode-map))
      (define-key map (kbd "M-n") #'rst-forward-section)
      (define-key map (kbd "M-p") #'rst-backward-section)
      (define-key map (kbd "M-RET") #'rst-insert-list)
      (define-key map (kbd "C-c C-j") #'rst-insert-list))))

;;; LaTeX

(progn ; `auctex'
  (init-ensure-package 'auctex)
  (defvar japanese-TeX-error-messages)
  (setf japanese-TeX-error-messages nil)

  (progn ; `tex'
    (init--require-when-compile 'tex)
    (declare-function TeX-source-correlate-mode "tex")
    (declare-function TeX-error-overview "tex")

    (with-eval-after-load 'tex
      (setf (default-value 'TeX-master) nil
            (default-value 'TeX-engine) 'luatex)
      (setf TeX-parse-self t
            TeX-auto-save t)
      (setf TeX-save-query nil)
      (setf TeX-electric-sub-and-superscript t)
      (TeX-source-correlate-mode)

      (let ((commands nil))
        (push '("TeXcount" "texcount -utf8 -inc %t"
                TeX-run-background nil (latex-mode)
                :help "Count words in the document")
              commands)
        (dolist (command TeX-command-list)
          (unless (member (car command) '("Print" "Queue" "Spell"))
            (push command commands)))
        (setf TeX-command-list (nreverse commands)))

      (let ((map TeX-mode-map))
        (define-key map [remap TeX-complete-symbol] #'completion-at-point)
        (define-key map (kbd "M-g L") #'TeX-error-overview)
        (define-key map (kbd "C-c C-i") nil)
        (define-key map (kbd "C-c ?") nil))))

  (progn ; `tex-fold'
    (init--require-when-compile 'tex-fold)
    (add-hook 'TeX-mode-hook #'TeX-fold-mode)
    (with-eval-after-load 'tex-fold
      (define-key TeX-fold-keymap (kbd "\\") #'prettify-symbols-mode)))

  (progn ; `tex-style'
    (init--require-when-compile 'tex-style)
    (with-eval-after-load 'tex-style
      (setf LaTeX-csquotes-open-quote "\\enquote{"
            LaTeX-csquotes-close-quote "}")))

  (progn ; `latex'
    (init--require-when-compile 'latex)
    (declare-function LaTeX-math-mode "latex")

    (defun init--setup-LaTeX-mode ()
      (make-local-variable 'TeX-electric-math)
      (setf TeX-electric-math '("\\(" . "\\)"))
      (LaTeX-math-mode))

    (with-eval-after-load 'latex
      (setf LaTeX-default-style "scrartcl")
      (setf LaTeX-babel-hyphen nil)
      (add-hook 'LaTeX-mode-hook #'init--setup-LaTeX-mode)))

  (progn ; `preview'
    (init--require-when-compile 'preview)
    (with-eval-after-load 'preview
      (setf preview-auto-cache-preamble nil))))

(progn ; `auctex-latexmk'
  (autoload 'auctex-latexmk-setup "auctex-latexmk")
  (with-eval-after-load 'latex
    (auctex-latexmk-setup)))

(progn ; `latex-snippets'
  (autoload 'latex-snippets-file-template "latex-snippets" nil t)
  (with-eval-after-load 'autoinsert
    (define-auto-insert '(latex-mode . "LaTeX file template")
      #'latex-snippets-file-template)))

(progn ; `reftex'
  (init--require-when-compile 'reftex)
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  (with-eval-after-load 'reftex
    (setf reftex-plug-into-AUCTeX t)
    ;; Offer a guess but ask for confirmation
    (setf reftex-insert-label-flags '(t t))
    (setf reftex-cite-format 'biblatex))

  (progn ; `reftex-toc'
    (init--require-when-compile 'reftex-toc)
    (with-eval-after-load 'reftex-toc
      (define-key reftex-toc-mode-map (kbd "d") nil))))

(progn ; `bibtex'
  (init--require-when-compile 'bibtex)
  (declare-function bibtex-validate "bibtex")
  (setf bibtex-dialect 'biblatex)

  (with-eval-after-load 'bibtex
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

    (let ((map bibtex-mode-map))
      (define-key map (kbd "M-g L") #'bibtex-validate)
      (define-key map (kbd "C-c $") nil))))

;;; Ledger

(progn ; `ledger-mode'
  (init-ensure-package 'ledger-mode)
  (init--require-when-compile 'ledger-mode)
  (declare-function ledger-schedule-create-auto-buffer "ledger-schedule")

  (defun init--avoid-flycheck-for-ledger-schedule (fn &rest args)
    "Apply FN on ARGS but avoid activating Flycheck."
    (cl-letf (((symbol-function 'flycheck-mode) #'ignore))
      (apply fn args)))

  (with-eval-after-load 'ledger-mode
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
             ("Cash flow (this month)" . "balance Income Expenses --invert -p %(month)"))))
    (advice-add #'ledger-schedule-create-auto-buffer :around
                #'init--avoid-flycheck-for-ledger-schedule)))

(progn ; `flycheck-ledger'
  (init-ensure-package 'flycheck-ledger)
  (init--require-when-compile 'flycheck-ledger)
  (with-eval-after-load 'flycheck
    (require 'flycheck-ledger)
    (setf flycheck-ledger-pedantic t
          flycheck-ledger-explicit t)))

;;; Org

(progn ; `org'
  (init--require-when-compile 'org)
  (make-directory (init--var "org/") t)
  (setf org-babel-load-languages nil)

  (with-eval-after-load 'org
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

    (let ((languages nil))
      (dolist (language init--org-babel-languages)
        (push `(,language . t) languages))
      (org-babel-do-load-languages 'org-babel-load-languages languages)))

  (progn ; `org-goto'
    (init--require-when-compile 'org-goto)
    (with-eval-after-load 'org-goto
      (setf org-goto-auto-isearch nil)))

  (progn ; `org-refile'
    (init--require-when-compile 'org-refile)
    (with-eval-after-load 'org-refile
      (setf org-refile-targets '((nil . (:maxlevel . 5))
                                 (org-agenda-files . (:maxlevel . 5)))
            org-refile-allow-creating-parent-nodes 'confirm)
      (setf org-refile-use-outline-path 'file)))

  (progn ; `org-archive'
    (init--require-when-compile 'org-archive)
    (declare-function org-archive-set-tag "org-archive")
    (with-eval-after-load 'org-archive
      (setf org-archive-default-command #'org-archive-set-tag)
      (setf org-archive-file-header-format nil)))

  (progn ; `org-id'
    (init--require-when-compile 'org-id)
    (setf org-id-locations-file (init--var "org/id-locations")))

  (progn ; `org-duration'
    (init--require-when-compile 'org-duration)
    (with-eval-after-load 'org-duration
      (setf org-duration-format 'h:mm)))

  (progn ; `org-lint'
    (declare-function org-lint "org-lint")
    (with-eval-after-load 'org
      (define-key org-mode-map (kbd "M-g L") #'org-lint)))

  (progn ; `ol'
    (autoload 'org-store-link "ol" nil t)
    (define-key global-map (kbd "C-c o l") #'org-store-link))

  (progn ; `org-capture'
    (init--require-when-compile 'org-capture)
    (define-key global-map (kbd "C-c o c") #'org-capture)
    (with-eval-after-load 'org-capture
      (setf org-capture-templates
            '(("t" "Task" entry (file+headline "" "Tasks")
               "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i"
               :empty-lines 1)
              ("n" "Note" entry (file+headline "" "Notes")
               "* %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i"
               :empty-lines 1)))))

  (progn ; `org-agenda'
    (init--require-when-compile 'org-agenda)
    (define-key global-map (kbd "C-c o a") #'org-agenda)
    (with-eval-after-load 'org-agenda
      (setf org-agenda-window-setup 'only-window
            org-agenda-restore-windows-after-quit t)
      (setf org-agenda-block-separator "")
      (setf org-agenda-span 'day
            org-agenda-start-on-weekday 0)
      (setf org-stuck-projects '("+project/-DONE-CANCELLED" ("NEXT") nil ""))))

  (progn ; `org-clock'
    (init--require-when-compile 'org-clock)
    (setf org-clock-persist-file (init--var "org/clock-save.el"))
    (with-eval-after-load 'org
      (org-clock-persistence-insinuate))
    (with-eval-after-load 'org-clock
      (setf org-clock-out-remove-zero-time-clocks t)
      (setf org-clock-persist 'history)
      (setf org-show-notification-timeout 10)))

  (progn ; `org-src'
    (init--require-when-compile 'org-src)
    (with-eval-after-load 'org-src
      (setf org-src-preserve-indentation t)
      (setf org-src-window-setup 'other-window)))

  (progn ; `ox'
    (init--require-when-compile 'ox)
    (with-eval-after-load 'ox
      (setf org-export-coding-system 'utf-8)))

  (progn ; `ox-latex'
    (init--require-when-compile 'ox-latex)
    (setf org-latex-classes
          '(("article"
             "\\documentclass[11pt]{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
    (with-eval-after-load 'ox-latex
      (setf org-latex-subtitle-format "\\subtitle{%s}"
            org-latex-subtitle-separate t)
      (setf org-latex-compiler "lualatex"
            org-latex-bib-compiler "biber")
      (setf org-latex-pdf-process
            '("latexmk -%latex -interaction=nonstopmode -outdir=%o %f"))))

  (progn ; `ox-html'
    (init--require-when-compile 'ox-html)
    (with-eval-after-load 'ox-html
      (setf org-html-doctype "html5"
            org-html-html5-fancy t
            org-html-preamble nil
            org-html-postamble nil
            org-html-htmlize-output-type 'css
            org-html-head-include-default-style nil
            org-html-head (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
                                  (concat "file://" (init--etc "css/org.css")))))))

(progn ; `org-mru-clock'
  (init-ensure-package 'org-mru-clock)
  (define-key global-map (kbd "C-c o i") #'org-mru-clock-in)
  (define-key global-map (kbd "C-c o u") #'org-mru-clock-select-recent-task))

;;; Data

(progn ; `sql'
  (init--require-when-compile 'sql)
  (declare-function sql-set-product "sql")
  (cl-pushnew 'sqlite init--org-babel-languages :test #'eq)
  (cl-pushnew 'sql init--org-babel-languages :test #'eq)
  (with-eval-after-load 'sql
    (setf sql-product 'sqlite)
    (define-key sql-mode-map (kbd "C-c C-p") #'sql-set-product)
    (define-key sql-mode-map (kbd "C-c a a") #'sql-product-interactive)))

(progn ; `sqlup-mode'
  (init-ensure-package 'sqlup-mode)
  (add-hook 'sql-mode #'sqlup-mode))

(progn ; `dotenv-mode'
  (init-ensure-package 'dotenv-mode))

(progn ; `json-mode'
  (init-ensure-package 'json-mode))

(progn ; `jq-mode'
  (init-ensure-package 'jq-mode)
  (defvar json-mode-map)
  (cl-pushnew 'jq init--org-babel-languages :test #'eq)
  (with-eval-after-load 'json-mode
    (define-key json-mode-map (kbd "C-c C-c") #'jq-interactively)))

(progn ; `yaml-mode'
  (init-ensure-package 'yaml-mode)
  (defun init--setup-yaml-mode ()
    (flyspell-mode 0)
    (auto-fill-mode 0)
    (abbrev-mode 0))
  (add-hook 'yaml-mode-hook #'init--setup-yaml-mode))

(progn ; `csv-mode'
  (init-ensure-package 'csv-mode))

(progn ; `protobuf-mode'
  (init-ensure-package 'protobuf-mode)
  (with-eval-after-load 'protobuf-mode
    (c-add-style "protobuf" '("k&r"
                              (c-basic-offset . 2)
                              (indent-tabs-mode . nil)))
    (cl-pushnew '(protobuf-mode . "protobuf") c-default-style
                :test #'eq :key #'car)))

(progn ; `graphql-mode'
  (init-ensure-package 'graphql-mode))

(progn ; `ob-graphql'
  (init-ensure-package 'ob-graphql)
  (cl-pushnew 'graphql init--org-babel-languages :test #'eq))

;;; init.el ends here
