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
    (dolist (directory `(,(expand-file-name "lisp" user-emacs-directory)
                         "/usr/share/emacs/site-lisp"))
      (when (and (file-directory-p directory)
                 (not (member directory load-path)))
        (push directory load-path))))

  (require 'confige)
  (eval-when-compile
    (require 'cl-lib)
    (require 'rx))

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

(progn ; startup
  (setf inhibit-startup-screen t
        inhibit-startup-buffer-menu t
        inhibit-startup-echo-area-message nil)
  (setf initial-scratch-message nil
        initial-major-mode #'text-mode))

(progn ; basic customization
  (setf custom-file (init--var "custom.el"))
  (load custom-file t nil t)
  (defvar init--org-babel-languages nil "Languages to load for Org Babel.")

  (dolist (key '("M-`" "M-z" "C-z" "C-x C-z" "C-x C-u" "C-x C-l"))
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

  ;; NOTE: Builtin theme files are not in the load path, so autoloading and some
  ;; usual methods may not work for themes.
  (declare-function modus-themes-toggle nil)
  (load-theme 'modus-operandi)
  (define-key global-map (kbd "C-c t m") #'modus-themes-toggle)

  (with-eval-after-load 'faces
    (set-face-attribute 'fixed-pitch nil :family "Source Code Pro")
    (set-face-attribute 'variable-pitch nil :family "DejaVu Sans")))

(progn ; package
  (require 'package)
  (setf package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))

  (defun init--record-selected-packages ()
    "Record selected packages if any."
    (when-let* ((packages (or package-selected-packages
                              (package--find-non-dependencies))))
      (package--save-selected-packages packages)))
  (unless package-selected-packages
    (add-hook 'after-init-hook #'init--record-selected-packages)))

(confige liteline
  (:preface
   (defvar wincom-id-format)
   (defvar calendar-mode-line-format)
   (defvar 2C-mode-line-format)
   (autoload 'liteline-setup "liteline"))
  (:before (add-hook 'after-init-hook #'liteline-setup))
  (:after
   (setf mode-line-position-column-line-format '(" %l:%c"))
   (with-eval-after-load 'window-commander
     (setf wincom-id-format " #%s"))
   (with-eval-after-load 'calendar
     (setf calendar-mode-line-format nil
           (symbol-function 'calendar-set-mode-line) #'init-ignore))
   (with-eval-after-load 'two-column
     (setf 2C-mode-line-format (default-value 'mode-line-format)))
   (with-eval-after-load 'ediff-wind
     (setf (symbol-function 'ediff-refresh-mode-lines) #'init-ignore))))

(confige transient
  :preload t
  (:before
   (setf transient-history-file (init--var "transient/history.el")
         transient-levels-file (init--var "transient/levels.el")
         transient-values-file (init--var "transient/values.el"))))

(confige url
  :preload t
  (:before (setf url-configuration-directory (init--var "url/")))
  (:postface
   (confige url-cache
     :preload t
     (:before (setf url-cache-directory (init--var "url/cache/"))))))

(confige request
  :ensure t :preload t
  (:before (setf request-storage-directory (init--var "request/"))))

(confige server
  :load t
  (:after
   (unless (or (daemonp) (server-running-p))
     (server-start))))

(confige with-editor
  :ensure t
  (:before
   (shell-command-with-editor-mode)
   (dolist (hook '(shell-mode-hook eshell-mode-hook))
     (add-hook hook #'with-editor-export-editor))))

;;; Basic editing

(progn ; disabled commands
  (dolist (command '(erase-buffer
                     narrow-to-region
                     narrow-to-page
                     set-goal-column
                     scroll-left
                     dired-find-alternate-file))
    (put command 'disabled nil)))

(confige simple
  (:before
   (column-number-mode)
   (size-indication-mode)
   (dolist (hook '(text-mode-hook bibtex-mode-hook))
     (add-hook hook #'auto-fill-mode))

   (let ((map global-map))
     (define-key map [remap downcase-word] #'downcase-dwim)
     (define-key map [remap capitalize-word] #'capitalize-dwim)
     (define-key map [remap upcase-word] #'upcase-dwim)
     (define-key map (kbd "C-c b c") #'clone-indirect-buffer)
     (define-key map (kbd "C-c t v") #'visual-line-mode)
     (define-key map (kbd "C-c t q") #'auto-fill-mode)
     (define-key map (kbd "C-c t p") #'visible-mode)
     (define-key map (kbd "C-c a l") #'list-processes)))
  (:after
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

(confige simple-extras
  :load t
  (:after
   (add-hook 'prog-mode-hook #'simple-extras-auto-fill-comments-mode)
   (define-key global-map (kbd "M-z") #'simple-extras-unfill-paragraph)))

(confige paren
  (:before (setf show-paren-when-point-inside-paren t)))

(confige elec-pair
  (:before (electric-pair-mode)))

(confige lisp
  (:before
   (let ((map global-map))
     (define-key map (kbd "C-x C-u") #'delete-pair)
     (define-key map (kbd "C-x C-l") #'raise-sexp)
     (define-key map (kbd "C-c x p") #'check-parens))))

(confige delsel
  (:before (delete-selection-mode)))

(confige subword
  (:before
   (add-hook 'prog-mode-hook #'subword-mode)
   (define-key global-map (kbd "C-c t b") #'subword-mode)))

(confige align
  (:before
   (let ((map global-map))
     (define-key map (kbd "C-c x a") #'align)
     (define-key map (kbd "C-c x A") #'align-regexp)
     (define-key map (kbd "C-c x z") #'align-current)
     (define-key map (kbd "C-c x Z") #'align-entire))))

(confige sort
  (:before
   (let ((map global-map))
     (define-key map (kbd "C-c x r") #'sort-lines)
     (define-key map (kbd "C-c x R") #'sort-columns)
     (define-key map (kbd "C-c x d") #'delete-duplicate-lines))))

;;; Whitespace

(progn ; basic whitespace
  (setf (default-value 'indent-tabs-mode) nil)
  (setf (default-value 'fill-column) 80)
  (setf sentence-end-double-space nil))

(confige whitespace
  :preload t
  (:preface
   (defun init--set-cleanup-whitespace-style (fn &rest args)
     "Apply FN on ARGS but explicitly set whitespace style for cleanup."
     (let ((whitespace-style '(empty
                               indentation
                               space-before-tab
                               space-after-tab
                               trailing)))
       (apply fn args))))
  (:before
   (dolist (hook '(prog-mode-hook
                   text-mode-hook
                   bibtex-mode-hook
                   conf-mode-hook))
     (add-hook hook #'whitespace-mode))
   (let ((map global-map))
     (define-key map (kbd "C-c t w") #'whitespace-mode)
     (define-key map (kbd "C-c t W") #'whitespace-toggle-options)
     (define-key map (kbd "C-c x w") #'whitespace-cleanup)))
  (:after
   (setf whitespace-style '(face
                            indentation
                            space-after-tab
                            space-before-tab
                            tab-mark
                            trailing
                            lines-tail
                            missing-newline-at-eof)
         (default-value 'whitespace-line-column) nil)
   (advice-add 'whitespace-cleanup :around
               #'init--set-cleanup-whitespace-style)
   (advice-add 'whitespace-cleanup-region :around
               #'init--set-cleanup-whitespace-style)))

(confige whitespace-cleanup-mode
  :ensure t
  (:before (global-whitespace-cleanup-mode)))

;;; Completion

(progn ; basic completion
  (setf enable-recursive-minibuffers t)
  (setf completion-ignore-case t
        read-buffer-completion-ignore-case t)
  (define-key minibuffer-local-map (kbd "C-<tab>") nil))

(confige minibuffer
  (:before
   (define-key global-map [remap complete-symbol] #'completion-at-point))
  (:after
   (setf read-file-name-completion-ignore-case t)
   (setf completion-styles '(basic substring initials partial-completion))
   (setf completion-auto-help 'visible)
   (setf completions-group t)))

(confige mb-depth
  (:before (minibuffer-depth-indicate-mode)))

(confige minibuf-eldef
  :preload t
  (:before (minibuffer-electric-default-mode)))

(confige savehist
  :preload t
  (:before
   (setf savehist-file (init--var "savehist"))
   (savehist-mode)))

(confige company
  :ensure t :preload t
  (:preface
   (declare-function company-select-first "ext:company")
   (declare-function company-select-last "ext:company")
   (declare-function company-other-backend "ext:company")

   (defun init--enable-company ()
     (when-let* ((backends (cond ((derived-mode-p 'emacs-lisp-mode
                                                  'clojure-mode
                                                  'scheme-mode
                                                  'sh-mode
                                                  'css-mode
                                                  'LaTeX-mode
                                                  'ledger-mode)
                                  '(company-capf))
                                 ((derived-mode-p 'c-mode 'c++-mode)
                                  '(company-c-headers company-etags))
                                 ((derived-mode-p 'rust-mode)
                                  '(company-etags))
                                 ((derived-mode-p 'js2-mode 'typescript-mode)
                                  '(company-tide))
                                 ((derived-mode-p 'python-mode)
                                  '(company-anaconda))
                                 ((derived-mode-p 'haskell-mode)
                                  '(dante-company)))))
       (make-local-variable 'company-backends)
       (dolist (backend (nreverse backends))
         (push backend company-backends)))
     (company-mode)))
  (:before
   (setf company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                             company-preview-if-just-one-frontend)
         company-backends '(company-files company-dabbrev-code company-dabbrev)
         company-transformers '(company-sort-prefer-same-case-prefix))

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
     (add-hook hook #'init--enable-company)))
  (:after
   (setf company-idle-delay 0.5)
   (setf company-show-quick-access t
         company-tooltip-align-annotations t
         company-format-margin-function nil)

   (define-key company-mode-map [remap dabbrev-completion] #'company-complete)
   (let ((map company-active-map))
     (define-key map [remap beginning-of-buffer] #'company-select-first)
     (define-key map [remap end-of-buffer] #'company-select-last)
     (define-key map [remap dabbrev-completion] #'company-other-backend)))
  (:postface
   (confige company-dabbrev
     :preload t
     (:after
      (setf company-dabbrev-ignore-case t
            company-dabbrev-downcase nil)
      (setf company-dabbrev-ignore-buffers
            (lambda (buffer)
              (or (memq (buffer-local-value 'major-mode buffer)
                        '(image-mode doc-view-mode))
                  (string-match-p "\\` \\*" (buffer-name buffer)))))))

   (confige company-dabbrev-code
     :preload t
     (:after (setf company-dabbrev-code-everywhere t)))))

(confige hippie-exp
  :preload t
  (:before
   (define-key global-map [remap dabbrev-expand] #'hippie-expand)
   (define-key global-map [remap dabbrev-completion] #'hippie-expand))
  (:after
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

;;; Expansion

(confige abbrev
  (:before
   (setf abbrev-file-name (init--var "abbrev-defs"))
   (dolist (hook '(text-mode-hook bibtex-mode-hook))
     (add-hook hook #'abbrev-mode))
   (define-key global-map (kbd "C-c t r") #'abbrev-mode)
   (define-key global-map (kbd "C-c e e") #'expand-abbrev)))

(confige tempo
  :preload t
  (:after (setf tempo-interactive t)))

(confige autoinsert
  :preload t
  (:before
   (setf auto-insert-directory (init--etc "insert/")
         auto-insert-alist nil)
   (define-key global-map (kbd "C-c e t") #'auto-insert))
  (:after (setf (default-value 'auto-insert) t)))

;;; Editing visual

(confige hideshow
  :preload t
  (:preface (declare-function hs-toggle-hiding "hideshow"))
  (:before
   (add-hook 'prog-mode-hook #'hs-minor-mode)
   (setf hs-minor-mode-map (make-sparse-keymap)))
  (:after (define-key hs-minor-mode-map (kbd "C-c @ t") #'hs-toggle-hiding)))

(confige outline
  :preload t
  (:preface
   (declare-function outline-mark-subtree "outline")
   (declare-function outline-previous-visible-heading "outline")
   (declare-function outline-next-visible-heading "outline")
   (declare-function outline-backward-same-level "outline")
   (declare-function outline-forward-same-level "outline"))
  (:before
   (dolist (hook '(prog-mode-hook TeX-mode-hook))
     (add-hook hook #'outline-minor-mode))
   (setf outline-minor-mode-map (make-sparse-keymap)))
  (:after
   (let ((map outline-minor-mode-map))
     (define-key map (kbd "C-c @ SPC") #'outline-mark-subtree)
     (define-key map (kbd "C-c @ p") #'outline-previous-visible-heading)
     (define-key map (kbd "C-c @ n") #'outline-next-visible-heading)
     (define-key map (kbd "C-c @ b") #'outline-backward-same-level)
     (define-key map (kbd "C-c @ f") #'outline-forward-same-level))))

(confige bicycle
  :ensure t
  (:before
   (with-eval-after-load 'outline
     (let ((map outline-minor-mode-map))
       (define-key map (kbd "C-<tab>") #'bicycle-cycle)
       (define-key map (kbd "<backtab>") #'bicycle-cycle-global)))))

(confige hl-line
  (:before (global-hl-line-mode)))

(confige page-break-lines
  :ensure t
  (:before
   (dolist (hook '(compilation-mode-hook help-mode-hook))
     (add-hook hook #'page-break-lines-mode))))

(confige ansi-color
  (:before (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)))

(confige rainbow-delimiters
  :ensure t
  (:before (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(confige rainbow-mode
  :ensure t
  (:before (define-key global-map (kbd "C-c t o") #'rainbow-mode)))

(confige highlight-numbers
  :ensure t
  (:before (add-hook 'prog-mode-hook #'highlight-numbers-mode)))

(confige highlight-escape-sequences
  :ensure t
  (:before (hes-mode)))

(confige hl-todo
  :ensure t
  (:before
   (dolist (hook '(prog-mode-hook
                   TeX-mode-hook
                   conf-mode-hook
                   yaml-mode-hook))
     (add-hook hook #'hl-todo-mode))))

(confige visual-fill-column
  :ensure t :preload t
  (:preface
   (declare-function visual-fill-column-adjust "ext:visual-fill-column"))
  (:before (define-key global-map (kbd "C-c t c") #'visual-fill-column-mode))
  (:after
   (setf (default-value 'visual-fill-column-center-text) t
         (default-value 'visual-fill-column-fringes-outside-margins) nil)
   (setf visual-fill-column-enable-sensible-window-split t)
   (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)))

(confige page-turner
  :ensure (visual-fill-column) :load t
  (:after
   (setf page-turner-prose-family "DejaVu Serif")
   (page-turner-setup)))

;;; Multilingual

(progn ; basic multilingual
  (setf word-wrap-by-category t)
  (setf default-input-method "sgml"
        default-transient-input-method "TeX"))

(confige kkc
  :preload t
  (:before (setf kkc-init-file-name (init--var "kkcrc"))))

(confige change-language
  (:preface (autoload 'change-language "change-language" nil t))
  (:before (define-key global-map (kbd "C-c t i") #'change-language)))

;;; Movement

(confige isearch
  (:after
   (setf isearch-allow-scroll t)
   (setf isearch-lazy-count t)))

(confige avy
  :ensure t :preload t
  (:preface (autoload 'avy-resume "avy" nil t))
  (:before
   (let ((map init-protected-map))
     (define-key map (kbd "C-;") #'avy-goto-char-in-line)
     (define-key map (kbd "C-'") #'avy-goto-char-2))
   (define-key global-map (kbd "C-z") #'avy-resume)
   (define-key isearch-mode-map [remap avy-goto-char-2] #'avy-isearch))
  (:after
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

(confige imenu
  :preload t
  (:before (define-key global-map (kbd "M-g i") #'imenu))
  (:after
   (setf imenu-auto-rescan t)
   (setf imenu-space-replacement nil)))

;;; Window & frame

(progn ; scroll
  (setf scroll-conservatively 101
        scroll-preserve-screen-position 'always
        hscroll-margin 0
        hscroll-step 1))

(confige mwheel
  (:after
   ;; Move slowly by default
   (setf mouse-wheel-scroll-amount '(1 ((shift) . 5))
         mouse-wheel-progressive-speed nil)))

(progn ; window
  (setf scroll-error-top-bottom t)
  (setf fit-window-to-buffer-horizontally t)
  (setf display-buffer-alist
        `(;; Auxiliary
          (,(rx bos (or "CAPTURE-"
                        "*Org Src"
                        "*Edit Formulas*"
                        "*edit-indirect"
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
          (,(rx bos (or " *Agenda Commands*"
                        " *Org todo*"
                        "Trash Can"
                        "*Occur"
                        "*xref*"
                        "*Apropos*"
                        "*Org Select*"
                        "*Org Attach*"
                        "*Org Links*"
                        "*Org Note*"
                        "*Org Export Dispatcher*"
                        "*Org Lint*"
                        "*Org Clock*"
                        "*Clock Task Select*"
                        "*Calendar*"
                        "*Ledger Report*"
                        "*Reconcile*"
                        "*RefTeX Select*"
                        "*Key*"
                        "*Keys*"
                        "*eww history*"
                        "*eww bookmarks*"
                        "*eww buffers*"
                        "*Buffer List*"
                        "*Process List*"
                        "*Proced*"
                        "*Flycheck errors*"
                        "*TeX errors*"
                        "*BibTeX validation errors*"
                        "*R dired*"
                        "*eshell"
                        (seq "*" (1+ nonl) "-eshell*")))
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
                        "*Flycheck error messages*"
                        "*Ledger Error*"
                        "*cider-error*"
                        "*cider-test-report*"
                        "*Geiser dbg*"
                        "*HsCompilation*"
                        "*R view"
                        "*S objects*"
                        "*S search list*"
                        "*ess-output*"
                        "*TeX background*"
                        (seq "*" (1+ nonl) (or " output*"
                                               "-compilation*"))))
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-height . 10)
           (preserve-size . (nil . t))
           (reusable-frames . nil))
          ;; Information
          (,(rx bos (or " *Metahelp*"
                        " *Input History*"
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
                        "*cider-repl-history*"))
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (side . bottom)
           (slot . 1)
           (window-height . 10)
           (preserve-size . (nil . t))
           (reusable-frames . nil))))

  (let ((map global-map))
    (define-key map (kbd "C-c w t") #'window-swap-states)
    (define-key map (kbd "C-c w f") #'fit-window-to-buffer)
    (define-key map (kbd "C-c w l") #'delete-other-windows-vertically)
    (define-key map (kbd "C-c w s") #'window-toggle-side-windows)
    (define-key map (kbd "C-x C-z") #'window-toggle-side-windows)))

(confige window-extras
  :load t
  (:after (window-extras-setup)))

(confige window-commander
  :ensure t :preload t
  (:preface
   (declare-function wincom-display-mode-line-conditional "ext:window-commander")
   (declare-function wincom-select "ext:window-commander"))
  (:before
   (setf wincom-scope 'current)
   (setf wincom-display-lighter nil)
   (setf wincom-mode-map (make-sparse-keymap))
   (add-hook 'after-init-hook #'wincom-mode t))
  (:after
   (add-hook 'wincom-mode-hook #'wincom-display-mode-line-conditional)
   (define-key wincom-mode-map (kbd "M-o") #'wincom-select)))

(confige tab-bar
  (:before (add-hook 'window-setup-hook #'tab-bar-mode))
  (:after
   (setf tab-bar-new-tab-choice "*scratch*")
   (setf tab-bar-close-button-show nil
         tab-bar-format '(tab-bar-format-tabs-groups
                          tab-bar-separator
                          tab-bar-format-align-right
                          tab-bar-format-global))
   (tab-bar-history-mode)

   (add-hook 'tab-bar-mode-hook #'tab-bar--undefine-keys)
   (let ((map global-map))
     (define-key map (kbd "C-c s s") #'tab-bar-switch-to-recent-tab)
     (define-key map (kbd "C-c s n") #'tab-bar-switch-to-next-tab)
     (define-key map (kbd "C-c s p") #'tab-bar-switch-to-prev-tab))
   (let ((map tab-bar-history-mode-map))
     (define-key map (kbd "C-c s f") #'tab-bar-history-forward)
     (define-key map (kbd "C-c s b") #'tab-bar-history-back))))

(confige tab-bar-extras
  (:preface (autoload 'tab-bar-extras-setup "tab-bar-extras"))
  (:before (add-hook 'after-init-hook #'tab-bar-extras-setup))
  (:after
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
    (or (not (member (buffer-name) '("*scratch*" "*Messages*")))
        (ignore (bury-buffer))))
  (add-hook 'kill-buffer-query-functions #'init--protect-special-buffers))

(confige autorevert
  (:before
   (global-auto-revert-mode)
   (define-key global-map (kbd "C-c t g") #'auto-revert-mode)))

(confige uniquify
  :load t
  (:after
   (setf uniquify-buffer-name-style 'forward
         uniquify-trailing-separator-p t)))

(confige ibuffer
  :preload t
  (:preface (declare-function ibuffer-visit-buffer-1-window "ibuffer"))
  (:before (define-key global-map [remap list-buffers] #'ibuffer))
  (:after
   (let ((state '(mark modified read-only locked " ")))
     (setf ibuffer-formats `((,@state
                              (name 18 18 :left :elide) " "
                              (size 9 -1 :right) " "
                              (mode 16 16 :left :elide) " "
                              filename-and-process)
                             (,@state name))))
   (let ((map ibuffer-mode-map))
     (define-key map (kbd "C-c C-o") #'ibuffer-visit-buffer-1-window)
     (define-key map (kbd "P") nil)))
  (:postface
   (confige ibuf-ext
     :preload t
     (:preface
      (defun init--set-ibuffer-filter-groups ()
        "Set default Ibuffer filter groups."
        (setf ibuffer-filter-groups (cdar ibuffer-saved-filter-groups))))
     (:before
      (with-eval-after-load 'ibuffer
        (require 'ibuf-ext))
      (setf ibuffer-saved-filter-groups
            '(("By purpose"
               ("Development" (or (saved . "programming")
                                  (saved . "configuration")))
               ("Writing" (saved . "writing"))
               ("Executing" (saved . "interpreter"))
               ("Reading" (saved . "reading"))
               ("Communication" (saved . "communication")))
              ("By association"
               ("File" (visiting-file))
               ("Directory" (derived-mode . dired-mode))
               ("Process" (process)))))
      (add-hook 'ibuffer-mode-hook #'init--set-ibuffer-filter-groups))
     (:after
      (setf ibuffer-show-empty-filter-groups nil)
      (setf ibuffer-saved-filters
            '(("programming"
               (and (derived-mode . prog-mode)
                    (not (derived-mode . js-json-mode))))
              ("configuration"
               (or (derived-mode . conf-mode)
                   (derived-mode . js-json-mode)
                   (derived-mode . yaml-mode)))
              ("writing"
               (or (and (derived-mode . text-mode)
                        (not (derived-mode . yaml-mode)))
                   (derived-mode . bibtex-mode)))
              ("interpreter"
               (or (derived-mode . comint-mode)
                   (derived-mode . cider-repl-mode)
                   (derived-mode . eshell-mode)))
              ("reading"
               (or (derived-mode . doc-view-mode)
                   (derived-mode . nov-mode)
                   (derived-mode . elfeed-show-mode)
                   (derived-mode . elfeed-search-mode)
                   (derived-mode . eww-mode)))
              ("communication"
               (or (derived-mode . rcirc-mode)
                   (derived-mode . notmuch-hello-mode)
                   (derived-mode . notmuch-search-mode)
                   (derived-mode . notmuch-show-mode)))))))))

;;; File

(progn ; files
  (let ((prefix (init--var "auto-save/sessions/"))
        (directory (init--var "auto-save/saves/")))
    (make-directory directory t)
    (setf auto-save-list-file-prefix prefix
          auto-save-file-name-transforms `((".*" ,directory t))))

  (setf make-backup-files nil
        backup-by-copying t
        delete-old-versions t)
  (put 'make-backup-files 'permanent-local t)
  (defun init--prepare-for-making-backups (&optional arg)
    "Prepare for making backups appropriately based on ARG."
    (when (and (not make-backup-files)
               (memq arg '(4 16 64)))
      (make-local-variable 'make-backup-files)
      (setf make-backup-files t)))
  (advice-add 'save-buffer :before #'init--prepare-for-making-backups)

  (setf view-read-only t)
  (setf save-abbrevs 'silently)
  (setf require-final-newline t)

  (setf confirm-nonexistent-file-or-buffer t)
  (defun init--ensure-directory-for-file ()
    "Ensure that the directory for file exists."
    (make-directory (file-name-directory buffer-file-name) t))
  (add-hook 'find-file-not-found-functions #'init--ensure-directory-for-file))

(confige files-x
  (:before
   (let ((map global-map))
     (define-key map (kbd "C-c f v") #'add-file-local-variable)
     (define-key map (kbd "C-c f V") #'add-file-local-variable-prop-line)
     (define-key map (kbd "C-c f d") #'add-dir-local-variable))))

(confige ffap
  :preload t
  (:after (setf ffap-machine-p-known 'accept)))

(confige executable
  (:before
   (add-hook 'after-save-hook
             #'executable-make-buffer-file-executable-if-script-p)))

(confige time-stamp
  :preload t
  (:before (add-hook 'before-save-hook #'time-stamp))
  (:after
   (setf (default-value 'time-stamp-format) "%Y-%02m-%02dT%02H:%02M:%02S%:z")))

(confige tramp
  :preload t
  (:before
   (setf tramp-persistency-file-name (init--var "tramp/persistency")
         tramp-auto-save-directory (init--var "tramp/auto-save/")))
  (:postface
   (confige tramp-sh
     :preload t
     (:before (setf tramp-histfile-override t)))))

(confige saveplace
  :preload t
  (:before
   (setf save-place-file (init--var "places"))
   (save-place-mode)))

(confige recentf
  :preload t :load t
  (:before
   (setf recentf-save-file (init--var "recentf"))
   (setf recentf-auto-cleanup 300))
  (:after
   (setf recentf-max-saved-items 100)
   (setf recentf-exclude '("/elpa/" "/var/" "/\\.git/" "/Trash/"))
   (recentf-mode)
   (define-key recentf-mode-map (kbd "C-c f r") #'recentf-open)))

(confige bookmark
  :preload t
  (:before (setf bookmark-default-file (init--var "bookmarks")))
  (:after
   (unless (file-exists-p bookmark-default-file)
     (dolist (bookmark '(("athenaeum" . "athenaeum/catalogue.org")
                         ("finances" . "ledger/finances.ledger")))
       (let ((name (car bookmark))
             (file (init--sync (cdr bookmark))))
         (when (file-exists-p file)
           (cl-pushnew `(,name . ((filename . ,file))) bookmark-alist
                       :test #'equal :key #'car)))))))

(confige dired
  :preload t
  (:preface (declare-function dired-directory-changed-p "dired"))
  (:before (define-key global-map [remap list-directory] #'dired))
  (:after
   (setf dired-listing-switches "-alhF"
         dired-auto-revert-buffer #'dired-directory-changed-p)
   (setf dired-recursive-copies 'always)
   (setf dired-dwim-target t)
   (setf dired-garbage-files-regexp (rx "." (or "bak" "orig" "old") eos))
   (dolist (key '("c" "Z" "P" "N" "I"))
     (define-key dired-mode-map (kbd key) nil)))
  (:postface
   (confige dired-aux
     :preload t
     (:after (setf dired-create-destination-dirs 'ask)))

   (confige dired-x
     :preload t
     (:preface (declare-function dired-omit-mode "dired-x"))
     (:before
      (with-eval-after-load 'dired
        (add-hook 'dired-mode-hook #'dired-omit-mode)
        (define-key dired-mode-map (kbd ")") #'dired-omit-mode)))
     (:after
      (setf dired-omit-files "\\`\\."
            dired-omit-extensions nil)
      (define-key dired-mode-map (kbd "V") nil)))))

(confige wdired
  :preload t
  (:after (setf wdired-allow-to-change-permissions t)))

(confige diredfl
  :ensure t
  (:before (add-hook 'dired-mode-hook #'diredfl-mode)))

(confige dired-narrow
  :ensure t
  (:before
   (with-eval-after-load 'dired
     (define-key dired-mode-map (kbd "/") #'dired-narrow))))

(confige dired-git-info
  :ensure t
  (:before
   (with-eval-after-load 'dired
     (define-key dired-mode-map (kbd "{") #'dired-git-info-mode))))

(confige dired-atool
  (:preface
   (autoload 'dired-atool-do-unpack "dired-atool" nil t)
   (autoload 'dired-atool-do-pack "dired-atool" nil t))
  (:before
   (with-eval-after-load 'dired
     (let ((map dired-mode-map))
       (define-key map (kbd "Z") #'dired-atool-do-unpack)
       (define-key map (kbd "P") #'dired-atool-do-pack)))))

(confige image-dired
  :preload t
  (:before
   (setf image-dired-dir (init--var "image-dired/")
         image-dired-tags-db-file (init--var "image-dired/db")
         image-dired-temp-rotate-image-file (init--var "image-dired/rotate-temp"))))

(confige disk-usage
  :ensure t
  (:before
   (with-eval-after-load 'dired
     (define-key dired-mode-map (kbd "}") #'disk-usage-here))))

(confige trashed
  :ensure t
  (:before
   (define-key global-map (kbd "C-c f t") #'trashed)
   (with-eval-after-load 'dired
     (define-key dired-mode-map (kbd "`") #'trashed))))

;;; General tool

(confige password-cache
  (:after (setf password-cache-expiry 60)))

(confige auth-source
  (:after
   (setf auth-sources `(,(init--sync "misc/authinfo.gpg")))
   (setf auth-source-save-behavior nil)
   (setf auth-source-cache-expiry 3600)))

(confige epa
  :preload t
  (:after (setf epa-popup-info-window nil)))

(confige re-builder
  :preload t
  (:before (define-key global-map (kbd "C-c m r") #'re-builder))
  (:after (setf reb-re-syntax 'string)))

(confige calc
  :preload t
  (:before
   (cl-pushnew 'calc init--org-babel-languages :test #'eq)
   (let ((map global-map))
     (define-key map [remap calc-dispatch] #'quick-calc)
     (define-key map (kbd "C-c m c") #'calc)
     (define-key map (kbd "C-c x c") #'calc-grab-region)
     (define-key map (kbd "C-c x C") #'calc-grab-rectangle)))
  (:after (setf calc-gnuplot-default-device "qt"))
  (:postface
   (confige calc-ext
     (:preface (declare-function calc-reset "calc-ext"))
     (:before
      (with-eval-after-load 'calc
        (define-key calc-mode-map (kbd "C-c m c") #'calc-reset))))

   (confige calc-yank
     (:preface (autoload 'calc-copy-to-buffer "calc-yank" nil t))
     (:before
      (with-eval-after-load 'calc
        (define-key global-map (kbd "C-c e c") #'calc-copy-to-buffer))))))

(confige help
  (:after (setf help-window-select t)))

(confige info
  (:before (define-key global-map (kbd "C-c m i") #'info)))

(confige man
  :preload t
  (:before (define-key global-map (kbd "C-c m k") #'man))
  (:after (setf Man-notify-method 'aggressive)))

(confige diff
  (:before
   (define-key global-map (kbd "C-c f =") #'diff)
   (define-key global-map (kbd "C-c b =") #'diff-buffer-with-file)))

(confige ediff
  :preload t
  (:preface (declare-function ediff-setup-windows-plain "ediff-wind"))
  (:after
   (setf ediff-window-setup-function #'ediff-setup-windows-plain
         ediff-split-window-function #'split-window-horizontally)))

(confige htmlize
  :ensure t)

(confige edit-indirect
  :ensure t
  (:before (define-key global-map (kbd "C-c x i") #'edit-indirect-region)))

(confige comint
  :preload t
  (:preface (declare-function comint-send-eof "comint"))
  (:after
   (setf comint-prompt-read-only nil)
   (let ((map comint-mode-map))
     (define-key map [remap comint-delchar-or-maybe-eof] #'delete-char)
     (define-key map (kbd "C-c a a") #'comint-send-eof))))

(confige proced
  :preload t
  (:before (define-key global-map (kbd "C-c a L") #'proced))
  (:after (setf proced-enable-color-flag t)))

(confige proced-narrow
  :ensure t
  (:before
   (with-eval-after-load 'proced
     (define-key proced-mode-map (kbd "/") #'proced-narrow))))

;;; Eshell

(confige eshell
  (:before (define-key global-map (kbd "C-c a e") #'eshell))
  (:postface
   (confige esh-mode
     :preload t
     (:before (setf eshell-directory-name (init--var "eshell/"))))

   (confige esh-var
     :preload t
     (:after (setf eshell-modify-global-environment t)))

   (confige esh-module
     (:after
      (setf eshell-modules-list '(eshell-alias
                                  eshell-basic
                                  eshell-cmpl
                                  eshell-dirs
                                  eshell-extpipe
                                  eshell-glob
                                  eshell-hist
                                  eshell-ls
                                  eshell-pred
                                  eshell-prompt
                                  eshell-script
                                  eshell-term
                                  eshell-tramp
                                  eshell-unix))))

   (confige em-alias
     :preload t
     (:before (setf eshell-aliases-file (init--etc "eshell/aliases"))))

   (confige em-glob
     :preload t
     (:after (setf eshell-error-if-no-glob t)))

   (confige em-hist
     :preload t
     (:preface (declare-function eshell-input-filter-initial-space "em-hist"))
     (:after
      (setf eshell-history-size 2000
            eshell-input-filter #'eshell-input-filter-initial-space)))

   (confige em-script
     :preload t
     (:before
      (setf eshell-login-script (init--etc "eshell/login")
            eshell-rc-script (init--etc "eshell/profile"))))))

(confige eshell-z
  :ensure t
  (:before
   (with-eval-after-load 'eshell
     (require 'eshell-z))))

(confige eshell-extras
  (:preface (autoload 'eshell-extras-setup "eshell-extras"))
  (:before
   (with-eval-after-load 'eshell
     (eshell-extras-setup))))

;;; Browser & viewer

(confige browse-url
  (:after (setf browse-url-secondary-browser-function #'eww-browse-url)))

(confige bug-reference
  :preload t
  (:preface (declare-function bug-reference-push-button "bug-reference"))
  (:before
   (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
   (define-key global-map (kbd "C-c t u") #'bug-reference-mode)
   (define-key global-map (kbd "C-c t U") #'bug-reference-prog-mode))
  (:after
   (define-key bug-reference-map (kbd "C-c C-o") #'bug-reference-push-button)))

(confige goto-addr
  :preload t
  (:before
   (dolist (hook '(rst-mode-hook
                   comint-mode-hook
                   cider-repl-mode-hook
                   eshell-mode-hook))
     (add-hook hook #'goto-address-mode))
   (dolist (hook '(prog-mode-hook TeX-mode-hook conf-mode-hook yaml-mode-hook))
     (add-hook hook #'goto-address-prog-mode))

   (define-key global-map (kbd "C-c t a") #'goto-address-mode)
   (define-key global-map (kbd "C-c t A") #'goto-address-prog-mode))
  (:after
   (define-key goto-address-highlight-keymap
               (kbd "C-c C-o") #'goto-address-at-point)))

(confige eww
  :preload t
  (:preface
   (defun init--setup-eww-mode ()
     (let ((default (default-value 'browse-url-browser-function)))
       (unless (eq default browse-url-browser-function)
         (make-local-variable 'browse-url-secondary-browser-function)
         (setf browse-url-secondary-browser-function default)))))
  (:before
   (setf eww-bookmarks-directory (init--var "eww/"))
   (make-directory eww-bookmarks-directory t)
   (define-key global-map (kbd "C-c f e") #'eww-open-file))
  (:after (add-hook 'eww-mode-hook #'init--setup-eww-mode)))

(confige elfeed
  :ensure t :preload t
  (:preface (declare-function elfeed-make-tagger "ext:elfeed"))
  (:before
   (setf elfeed-db-directory (init--sync "misc/elfeed/db/"))
   (with-eval-after-load 'recentf
     (push "/elfeed/db/" recentf-exclude))
   (define-key global-map (kbd "C-c m w") #'elfeed))
  (:after
   (setf elfeed-feeds
         (let ((filename (init--sync "misc/elfeed/feeds.eld")))
           (and (file-regular-p filename)
                (ignore-errors
                  (with-temp-buffer
                    (insert-file-contents filename)
                    (let ((read-circle nil))
                      (read (current-buffer))))))))
   (add-hook 'elfeed-new-entry-hook
             (elfeed-make-tagger :before "10 days ago"
                                 :remove 'unread)))
  (:postface
   (confige elfeed-search
     :preload t
     (:after (setf elfeed-search-filter "@1-month-ago")))

   (confige elfeed-show
     :preload t
     (:before
      (setf elfeed-enclosure-default-dir
            (expand-file-name (convert-standard-filename "Downloads/") "~")))
     (:after (setf elfeed-show-entry-switch #'pop-to-buffer-same-window)))))

(confige doc-view
  :preload t
  (:after (setf doc-view-resolution 150)))

(confige nov
  :ensure t :preload t
  (:before
   (setf nov-save-place-file (init--var "nov-places"))
   (cl-pushnew '("\\.epub\\'" . nov-mode) auto-mode-alist :test #'equal)))

;;; Communication

(confige sendmail
  (:preface (declare-function sendmail-send-it "sendmail"))
  (:after
   (setf send-mail-function #'sendmail-send-it)
   (setf mail-envelope-from 'header)))

(confige message
  (:after
   (setf message-confirm-send t)
   (setf message-make-forward-subject-function '(message-forward-subject-fwd)
         message-citation-line-format "%N [%F %R %z] wrote:\n")
   (setf message-mail-alias-type 'ecomplete
         message-expand-name-databases nil
         message-expand-name-standard-ui t
         message-self-insert-commands nil)
   (add-hook 'message-mode-hook #'use-hard-newlines)))

(confige ecomplete
  :preload t
  (:before (setf ecomplete-database-file (init--var "ecompleterc"))))

(confige notmuch
  :preload t
  (:preface (autoload 'notmuch "notmuch" nil t))
  (:before
   (setf read-mail-command #'notmuch)
   (define-key global-map (kbd "C-c m e") #'notmuch))
  (:postface
   (confige notmuch-hello
     (:after
      (setf notmuch-hello-sections
            '(notmuch-hello-insert-saved-searches
              notmuch-hello-insert-alltags))
      (setf notmuch-hello-thousands-separator ""
            notmuch-show-empty-saved-searches t
            notmuch-show-all-tags-list t)
      (setf (default-value 'notmuch-search-oldest-first) nil)
      (setf notmuch-saved-searches
            `((:name "recent" :query "tag:inbox and date:1w.." :key ,(kbd "r"))
              (:name "unread" :query "tag:unread" :key ,(kbd "u"))
              (:name "flagged" :query "tag:flagged" :key ,(kbd "f"))))))

   (confige notmuch-mua
     :load t
     (:before
      (setf notmuch-fcc-dirs "personal/Sent +sent +inbox -unread"
            notmuch-draft-folder "personal/Drafts")
      (setf notmuch-address-use-company nil)
      (setf mail-user-agent 'notmuch-user-agent))
     (:after
      (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)))))

(confige rcirc
  :preload t
  (:preface
   (declare-function rcirc-omit-mode "rcirc")
   (declare-function rcirc-next-active-buffer "rcirc"))
  (:before
   (setf rcirc-log-directory (init--var "rcirc-log/"))
   (setf rcirc-track-minor-mode-map (make-sparse-keymap))
   (define-key global-map (kbd "C-c m t") #'rcirc))
  (:after
   (setf rcirc-server-alist '(("irc.libera.chat" :port 6697 :encryption tls))
         rcirc-default-nick "pengjiz"
         rcirc-default-user-name "pengjiz"
         rcirc-default-full-name "Pengji Zhang")
   (when-let* ((server "irc.libera.chat")
               (nickname "pengjiz")
               (target (list :host server
                             :port "nickserv"
                             :user nickname))
               (password (apply #'auth-source-pick-first-password
                                :require '(:secret) target)))
     (push (list server 'sasl nickname password) rcirc-authinfo))

   (setf rcirc-read-only-flag nil)
   (setf rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
         rcirc-omit-unless-requested '("NAMES"))
   (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
   (rcirc-track-minor-mode)
   (define-key rcirc-track-minor-mode-map
               (kbd "C-c m T") #'rcirc-next-active-buffer)))

;;; Calendar

(confige calendar
  :preload t
  (:preface (declare-function calendar-mark-today "calendar"))
  (:before
   (let ((directory (init--sync "misc/")))
     (make-directory directory t)
     (setf diary-file (expand-file-name "diary" directory)))
   (setf calendar-date-style 'iso)
   (define-key global-map (kbd "C-c m d") #'calendar))
  (:after
   (setf calendar-mark-holidays-flag t
         calendar-chinese-all-holidays-flag t)
   (add-hook 'calendar-today-visible-hook #'calendar-mark-today))
  (:postface
   (confige holidays
     :preload t
     (:before
      (setf holiday-bahai-holidays nil
            holiday-islamic-holidays nil
            holiday-hebrew-holidays nil
            holiday-christian-holidays nil)))

   (confige solar
     :preload t
     (:before
      (setf calendar-location-name "Pittsburgh, PA"
            calendar-latitude 40.4
            calendar-longitude -79.9)))

   (confige diary-lib
     :preload t
     (:after (setf diary-comment-start "##")))

   (confige appt
     :preload t
     (:after
      (setf appt-display-diary nil
            appt-display-mode-line nil)
      (setf appt-display-interval 10
            appt-message-warning-time 20)))))

(confige appt-extras
  (:preface
   (autoload 'appt-extras-setup "appt-extras")
   (autoload 'appt-extras-set-reminder "appt-extras" nil t))
  (:before
   (add-hook 'after-init-hook #'appt-extras-setup)
   (define-key global-map (kbd "C-c o r") #'appt-extras-set-reminder)))

;;; General development

(confige project
  :preload t
  (:preface (declare-function project-prefixed-buffer-name "project"))
  (:before (setf project-list-file (init--var "projects")))
  (:after
   (setf project-vc-merge-submodules nil)
   (setf project-compilation-buffer-name-function #'project-prefixed-buffer-name
         project-kill-buffers-display-buffer-list t)
   (setf project-switch-commands '((project-find-file "Find file")
                                   (project-find-dir "Find directory")
                                   (project-find-regexp "Find regexp")
                                   (project-eshell "Eshell")))
   (dolist (key '("s" "v"))
     (define-key project-prefix-map (kbd key) nil))))

(confige editorconfig
  :ensure t
  (:before (editorconfig-mode)))

(confige eldoc
  (:after
   (setf eldoc-echo-area-use-multiline-p nil)
   (setf eldoc-message-function #'message)
   ;; Describe the character at point by default
   (add-hook 'eldoc-documentation-functions #'describe-char-eldoc t)))

(confige xref
  :preload t
  (:after (setf xref-search-program 'ripgrep)))

(confige etags
  :preload t
  (:before (define-key global-map (kbd "C-c c t") #'visit-tags-table))
  (:after (setf tags-revert-without-query t)))

(confige compile
  :preload t
  (:before (define-key global-map (kbd "C-c c c") #'compile))
  (:after
   (setf (default-value 'compile-command) "make")
   (setf compilation-ask-about-save nil)))

(confige firestarter
  :ensure t :preload t
  (:before (define-key global-map (kbd "C-c c f") #'firestarter-mode))
  (:after (setf firestarter-default-type t)))

(confige gud
  :preload t
  (:before
   (define-key global-map (kbd "C-c c g") #'gud-gdb)
   (define-key global-map (kbd "C-c c p") #'pdb))
  (:after (setf gud-pdb-command-name "python -m pdb")))

(confige rmsbolt
  :ensure t :preload t
  (:before (define-key global-map (kbd "C-c c r") #'rmsbolt-mode))
  (:after (setf rmsbolt-automatic-recompile 'on-save)))

;;; Lint

(confige flycheck
  :ensure t :preload t
  (:preface
   (declare-function flycheck-buffer "ext:flycheck")
   (declare-function flycheck-next-error "ext:flycheck")
   (declare-function flycheck-previous-error "ext:flycheck")
   (declare-function flycheck-list-errors "ext:flycheck"))
  (:before
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
   (define-key global-map (kbd "C-c t e") #'flycheck-mode)
   (setf flycheck-command-map (make-sparse-keymap)))
  (:after
   (setf flycheck-check-syntax-automatically '(save mode-enabled))
   (setf flycheck-standard-error-navigation nil)
   (setf (symbol-function 'flycheck-maybe-display-error-at-point-soon)
         #'init-ignore)

   (let ((map flycheck-mode-map))
     (define-key map (kbd "M-n") #'flycheck-next-error)
     (define-key map (kbd "M-p") #'flycheck-previous-error)
     (define-key map (kbd "M-g l") #'flycheck-list-errors))
   (let ((map flycheck-command-map))
     (define-key map (kbd "!") #'flycheck-buffer)
     (define-key map (kbd "n") #'flycheck-next-error)
     (define-key map (kbd "p") #'flycheck-previous-error)
     (define-key map (kbd "l") #'flycheck-list-errors))))

;;; Spell check

(confige ispell
  (:before
   (define-key global-map (kbd "C-c t S") #'ispell-change-dictionary)
   (define-key global-map (kbd "C-c e d") #'ispell-complete-word)))

(confige flyspell
  :preload t
  (:before
   (setf flyspell-issue-welcome-flag nil
         flyspell-issue-message-flag nil)
   (setf flyspell-use-meta-tab nil
         flyspell-mode-map (make-sparse-keymap))

   (dolist (hook '(text-mode-hook bibtex-mode-hook rcirc-mode-hook))
     (add-hook hook #'flyspell-mode))
   (add-hook 'prog-mode-hook #'flyspell-prog-mode)
   (define-key global-map (kbd "C-c t s") #'flyspell-mode))
  (:after
   (setf flyspell-abbrev-p t
         flyspell-use-global-abbrev-table-p t)
   (define-key flyspell-mode-map (kbd "C-c x s") #'flyspell-region)))

;;; VCS

(confige vc
  :preload t
  (:before
   (setf vc-handled-backends '(Git))
   (setf vc-follow-symlinks t
         vc-make-backup-files t))
  (:after (setf vc-find-revision-no-save t)))

(confige magit
  :ensure t :preload t
  (:preface
   (declare-function magit-display-buffer-fullframe-status-v1 "ext:magit-mode")
   (declare-function magit-restore-window-configuration "ext:magit-mode")
   (declare-function magit-add-section-hook "ext:magit-section"))
  (:before
   (setf magit-define-global-key-bindings nil)
   (let ((map global-map))
     (define-key map (kbd "C-c g g") #'magit-status)
     (define-key map (kbd "C-c g d") #'magit-dispatch)
     (define-key map (kbd "C-c g f") #'magit-file-dispatch)))
  (:after
   (setf magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
         magit-bury-buffer-function #'magit-restore-window-configuration)
   (setf magit-save-repository-buffers 'dontask)
   (setf magit-revision-show-gravatars nil)
   ;; Show submodules in status buffer
   (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-modules
                           'magit-insert-stashes t))
  (:postface
   (confige magit-extras
     :preload t
     (:before (setf magit-bind-magit-project-status nil)))))

(confige git-commit
  :ensure t :preload t
  (:preface
   (declare-function git-commit-turn-on-flyspell "ext:git-commit")
   (defun init--set-git-commit-fill-column (&rest _)
     "Set fill column for Git commit messages when appropriate."
     (unless (local-variable-p 'fill-column)
       (setf fill-column 72))))
  (:after
   (setf git-commit-summary-max-length 50)
   (advice-add 'git-commit-turn-on-auto-fill :before
               #'init--set-git-commit-fill-column)
   (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)))

(confige diff-hl
  :ensure t :load t
  (:after
   (setf diff-hl-draw-borders nil)
   (global-diff-hl-mode)
   (add-hook 'dired-mode-hook #'diff-hl-dired-mode)

   (let ((map diff-hl-mode-map))
     (define-key map (kbd "C-c g p") #'diff-hl-previous-hunk)
     (define-key map (kbd "C-c g n") #'diff-hl-next-hunk))
   (define-key diff-hl-command-map (kbd "SPC") #'diff-hl-mark-hunk)

   (with-eval-after-load 'magit
     (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
     (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(confige git-modes
  :ensure t)

;;; Programming

(confige sh-script
  :preload t
  (:preface (declare-function sh-show-shell "sh-script"))
  (:before
   (cl-pushnew 'shell init--org-babel-languages :test #'eq)
   (setf sh-shell-file "/bin/bash"))
  (:after
   (setf sh-basic-offset 2)
   (define-key sh-mode-map (kbd "C-c a a") #'sh-show-shell)))

(confige lua-mode
  :ensure t :preload t
  (:after (setf lua-indent-level 2)))

(confige glsl-mode
  :ensure t :preload t
  (:preface
   (declare-function c-indent-exp "cc-cmds")
   (declare-function c-backslash-region "cc-cmds")
   (declare-function c-forward-conditional "cc-cmds")
   (declare-function c-backward-conditional "cc-cmds")
   (declare-function c-up-conditional "cc-cmds")
   (declare-function c-beginning-of-statement "cc-cmds")
   (declare-function c-end-of-statement "cc-cmds"))
  (:before (setf glsl-mode-map (make-sparse-keymap)))
  (:after
   (let ((map glsl-mode-map))
     (define-key map (kbd "C-M-q") #'c-indent-exp)
     (define-key map (kbd "C-c C-\\") #'c-backslash-region)
     (define-key map (kbd "C-c C-n") #'c-forward-conditional)
     (define-key map (kbd "C-c C-p") #'c-backward-conditional)
     (define-key map (kbd "C-c C-u") #'c-up-conditional)
     (define-key map [remap backward-sentence] #'c-beginning-of-statement)
     (define-key map [remap forward-sentence] #'c-end-of-statement))))

(confige nasm-mode
  :ensure t
  (:before
   (cl-pushnew '("\\.nasm\\'" . nasm-mode) auto-mode-alist :test #'equal)))

(confige bnf-mode
  :ensure t)

(confige graphviz-dot-mode
  (:preface (autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t))
  (:before
   (dolist (pattern '("\\.gv\\'" "\\.dot\\'"))
     (cl-pushnew `(,pattern . graphviz-dot-mode) auto-mode-alist :test #'equal))
   (cl-pushnew 'dot init--org-babel-languages :test #'eq)
   (with-eval-after-load 'org
     (push '("dot" . graphviz-dot) org-src-lang-modes))))

(confige gnuplot
  :preload t
  (:preface (autoload 'gnuplot-mode "gnuplot" nil t))
  (:before
   (cl-pushnew '("\\.gp\\'" . gnuplot-mode) auto-mode-alist :test #'equal)))

;;; Emacs Lisp

(confige elisp-mode
  (:before (cl-pushnew 'emacs-lisp init--org-babel-languages :test #'eq))
  (:after
   (let ((map emacs-lisp-mode-map))
     (define-key map (kbd "C-c C-r") #'eval-region)
     (define-key map (kbd "C-c C-b") #'eval-buffer)
     (define-key map (kbd "C-c a a") #'ielm))))

(confige emacs-lisp-snippets
  (:preface
   (autoload 'emacs-lisp-snippets-file-template "emacs-lisp-snippets" nil t))
  (:before
   (with-eval-after-load 'autoinsert
     (define-auto-insert '("\\.el\\'" . "Emacs Lisp file template")
       #'emacs-lisp-snippets-file-template))))

(confige ielm
  :preload t
  (:before (define-key global-map (kbd "C-c a i") #'ielm))
  (:after (setf ielm-prompt-read-only nil)))

;;; C & C++ & AWK

(confige cc-mode
  :preload t
  (:preface (declare-function c-context-line-break "cc-cmds"))
  (:before
   (cl-pushnew 'awk init--org-babel-languages :test #'eq)
   (setf c-default-style nil))
  (:after
   (c-add-style "common" '("k&r"
                           (c-basic-offset . 4)
                           (c-doc-comment-style . ((c-mode . doxygen)
                                                   (c++-mode . doxygen)))))
   (dolist (style '((other . "common")
                    (awk-mode . "awk")))
     (cl-pushnew style c-default-style :test #'eq :key #'car))
   (define-key c-mode-base-map (kbd "C-c C-b") #'c-context-line-break)))

(confige cmacexp
  :preload t
  (:after (setf c-macro-prompt-flag t)))

(confige c-snippets
  (:preface (autoload 'c-snippets-header-template "c-snippets" nil t))
  (:before
   (with-eval-after-load 'autoinsert
     (define-auto-insert `(,(rx "." (or "h" "H" "hh" "hpp" "hxx" "h++") eos)
                           . "C/C++ header template")
       #'c-snippets-header-template))))

(confige company-c-headers
  :ensure t)

(confige cmake-mode
  :ensure t)

(confige cmake-ide
  :ensure t
  (:before
   (with-eval-after-load 'cc-mode
     (cmake-ide-setup))))

;;; Clojure

(confige clojure-mode
  :ensure t)

(confige cider
  :ensure t
  (:preface (defvar clojure-mode-map))
  (:before
   (with-eval-after-load 'clojure-mode
     (define-key clojure-mode-map (kbd "C-c a a") #'cider)))
  (:postface
   (confige cider-eval
     :preload t
     (:after (setf cider-save-file-on-load t)))

   (confige cider-repl
     :preload t
     (:preface (declare-function cider-quit "ext:cider-connection"))
     (:after
      (setf cider-repl-display-help-banner nil)
      (define-key cider-repl-mode-map (kbd "C-c a a") #'cider-quit)))

   (confige nrepl-client
     :preload t
     (:after (setf nrepl-hide-special-buffers t)))))

(confige flycheck-clj-kondo
  :ensure t
  (:before
   (with-eval-after-load 'flycheck
     (require 'flycheck-clj-kondo))))

;;; Racket

(confige geiser
  :ensure t
  (:before (make-directory (init--var "geiser/") t))
  (:postface
   (confige geiser-mode
     :preload t
     (:after (define-key geiser-mode-map (kbd "C-c a a") #'geiser)))

   (confige geiser-repl
     :preload t
     (:preface (declare-function geiser-repl-exit "ext:geiser-repl"))
     (:before (setf geiser-repl-history-filename (init--var "geiser/history")))
     (:after
      (setf geiser-repl-read-only-prompt-p nil
            geiser-repl-read-only-output-p nil)
      (define-key geiser-repl-mode-map (kbd "C-c a a") #'geiser-repl-exit)))

   (confige geiser-autodoc
     :preload t
     (:after (setf geiser-autodoc-delay eldoc-idle-delay)))))

(confige geiser-racket
  :ensure t)

(confige scribble-mode
  :ensure t)

;;; Rust

(confige rust-mode
  :ensure t)

(confige flycheck-rust
  :ensure t
  (:before
   (with-eval-after-load 'rust-mode
     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

;; pest parser
(confige pest-mode
  (:preface (autoload 'pest-mode "pest-mode" nil t))
  (:before
   (cl-pushnew '("\\.pest\\'" . pest-mode) auto-mode-alist :test #'equal)))

;;; Haskell

(confige haskell-mode
  :ensure t :preload t
  (:after
   (setf haskell-completing-read-function #'completing-read)
   (setf haskell-interactive-popup-errors nil
         haskell-interactive-mode-read-only nil
         haskell-interactive-prompt-read-only nil)
   (setf haskell-process-auto-import-loaded-modules t
         haskell-process-suggest-remove-import-lines t
         haskell-process-show-overlays nil)
   (dolist (key '("C-c C-l" "C-c C-b" "C-c C-t" "C-c C-i"))
     (define-key haskell-mode-map (kbd key) nil)))
  (:postface
   (confige haskell
     :preload t
     (:before
      (setf interactive-haskell-mode-map (make-sparse-keymap))
      (add-hook 'haskell-mode-hook #'interactive-haskell-mode))
     (:after
      (let ((map interactive-haskell-mode-map))
        (define-key map (kbd "C-c a a") #'haskell-interactive-bring)
        (define-key map (kbd "C-c C-l") #'haskell-process-load-file)
        (define-key map (kbd "C-c C-r") #'haskell-process-reload))))

   (confige haskell-interactive-mode
     :preload t
     (:preface
      (declare-function haskell-interactive-mode-clear
                        "ext:haskell-interactive-mode"))
     (:after
      (let ((map haskell-interactive-mode-map))
        (define-key map (kbd "C-c a a") #'haskell-interactive-kill)
        (define-key map (kbd "C-c M-o") #'haskell-interactive-mode-clear))))

   (confige haskell-indentation
     (:before (add-hook 'haskell-mode-hook #'haskell-indentation-mode)))

   (confige haskell-collapse
     :preload t
     (:preface
      (declare-function haskell-hide-toggle "ext:haskell-collapse")
      (declare-function haskell-hide-toggle-all "ext:haskell-collapse"))
     (:before
      (add-hook 'haskell-mode-hook #'haskell-collapse-mode)
      (setf haskell-collapse-mode-map (make-sparse-keymap)))
     (:after
      (let ((map haskell-collapse-mode-map))
        (define-key map (kbd "C-c @ t") #'haskell-hide-toggle)
        (define-key map (kbd "C-c @ T") #'haskell-hide-toggle-all))))))

(confige dante
  :ensure t :preload t
  (:preface
   (declare-function dante-eval-block "ext:dante")
   (declare-function dante-type-at "ext:dante")
   (declare-function dante-info "ext:dante"))
  (:before
   (add-hook 'haskell-mode-hook #'dante-mode)
   (setf dante-mode-map (make-sparse-keymap)))
  (:after
   (let ((map dante-mode-map))
     (define-key map (kbd "C-c C-c") #'dante-eval-block)
     (define-key map (kbd "C-c C-t") #'dante-type-at)
     (define-key map (kbd "C-c TAB") #'dante-info))
   (with-eval-after-load 'company
     (setf (default-value 'company-backends)
           (delq 'dante-company (default-value 'company-backends))))))

;;; R

(confige ess
  :ensure t :preload t
  (:before
   (cl-pushnew 'R init--org-babel-languages :test #'eq)
   (setf ess-write-to-dribble nil)
   (setf ess-history-directory (init--var "ess/history/"))
   (make-directory ess-history-directory t))
  (:after
   (setf ess-style 'RStudio)
   (setf ess-use-ido nil
         ess-use-flymake nil)
   (setf ess-ask-for-ess-directory nil
         ess-auto-width 'window
         inferior-R-args "--no-save"))
  (:postface
   (confige ess-inf
     :preload t
     (:preface (declare-function ess-quit "ext:ess-inf"))
     (:after (define-key inferior-ess-mode-map (kbd "C-c a a") #'ess-quit)))

   (confige ess-r-mode
     :preload t
     (:preface (declare-function ess-cycle-assign "ext:ess-s-lang"))
     (:after
      (let ((map ess-r-mode-map))
        (define-key map (kbd "C-c a a") #'run-ess-r)
        (define-key map (kbd "C-c a m") #'ess-rdired)
        (define-key map (kbd ";") #'ess-cycle-assign))
      (let ((map inferior-ess-r-mode-map))
        (define-key map (kbd "C-c a m") #'ess-rdired)
        (define-key map (kbd ";") #'ess-cycle-assign))))))

;;; Web

(confige sgml-mode
  :preload t
  (:after (define-key sgml-mode-map (kbd "C-c C-v") nil)))

(confige html-snippets
  (:preface (autoload 'html-snippets-file-template "html-snippets" nil t))
  (:before
   (with-eval-after-load 'autoinsert
     (define-auto-insert '(html-mode . "HTML file template")
       #'html-snippets-file-template))))

(confige nxml-mode
  :preload t
  (:after
   (setf nxml-slash-auto-complete-flag t)
   (setf nxml-attribute-indent 2)))

(confige css-mode
  :preload t
  (:preface (declare-function css-cycle-color-format "css-mode"))
  (:before (setf css-mode-map (make-sparse-keymap)))
  (:after
   (setf css-indent-offset 2)
   (define-key css-mode-map (kbd "C-c C-f") #'css-cycle-color-format)))

(confige js
  :preload t
  (:after
   (setf js-indent-level 2
         js-switch-indent-offset 2
         js-chain-indent nil)
   (dolist (key '("C-c M-:" "C-c C-j" "C-M-x"))
     (define-key js-mode-map (kbd key) nil))))

(confige js2-mode
  :ensure t :preload t
  (:before
   (cl-pushnew '(js-mode . js2-mode) major-mode-remap-alist :test #'equal))
  (:after
   (setf js2-skip-preprocessor-directives t)
   (setf js2-highlight-level 3
         js2-highlight-external-variables nil)
   (setf js2-mode-show-parse-errors nil
         js2-mode-show-strict-warnings nil
         js2-strict-missing-semi-warning nil)))

(confige typescript-mode
  :ensure t :preload t
  (:after (setf typescript-indent-level 2)))

(confige tide
  :ensure t :preload t
  (:preface
   (defun init--enable-tide ()
     (unless (file-remote-p default-directory)
       (tide-setup))))
  (:before
   (setf tide-completion-setup-company-backend nil)
   (dolist (hook '(js2-mode-hook typescript-mode-hook))
     (add-hook hook #'init--enable-tide)))
  (:after (setf tide-completion-enable-autoimport-suggestions nil)))

(confige flycheck-npm
  :ensure (flycheck)
  (:preface (autoload 'flycheck-npm-setup "flycheck-npm"))
  (:before
   (dolist (hook '(html-mode-hook
                   css-mode-hook
                   js2-mode-hook
                   typescript-mode-hook))
     (add-hook hook #'flycheck-npm-setup))))

(confige ob-http
  :ensure t
  (:before (cl-pushnew 'http init--org-babel-languages :test #'eq)))

;;; Python

(confige python
  :preload t
  (:preface
   (defun init--setup-python-mode ()
     (setf fill-column 79))
   (defun init--setup-inferior-python-mode ()
     (kill-local-variable 'comint-prompt-read-only)))
  (:before (cl-pushnew 'python init--org-babel-languages :test #'eq))
  (:after
   (setf python-indent-guess-indent-offset-verbose nil
         python-indent-offset 4)
   (setf python-shell-interpreter "python")
   (add-hook 'python-mode-hook #'init--setup-python-mode)
   (add-hook 'inferior-python-mode-hook #'init--setup-inferior-python-mode)
   (define-key python-mode-map (kbd "C-c a a") #'run-python)))

(confige anaconda-mode
  :ensure t :preload t
  (:preface
   (declare-function anaconda-mode-show-doc "ext:anaconda-mode")
   (declare-function anaconda-mode-find-definitions "ext:anaconda-mode")
   (declare-function anaconda-mode-find-references "ext:anaconda-mode")
   (declare-function anaconda-mode-find-assignments "ext:anaconda-mode")

   (defun init--enable-anaconda ()
     (unless (file-remote-p default-directory)
       (anaconda-mode)
       (anaconda-eldoc-mode))))
  (:before
   (setf anaconda-mode-installation-directory (init--var "anaconda-mode/"))
   (add-hook 'python-mode-hook #'init--enable-anaconda)
   (setf anaconda-mode-map (make-sparse-keymap)))
  (:after
   (setf anaconda-mode-eldoc-as-single-line t)
   (let ((map anaconda-mode-map))
     (define-key map [remap python-describe-at-point] #'anaconda-mode-show-doc)
     (define-key map [remap xref-find-definitions] #'anaconda-mode-find-definitions)
     (define-key map [remap xref-find-references] #'anaconda-mode-find-references)
     (define-key map (kbd "C-c C-v") #'anaconda-mode-find-assignments))))

(confige company-anaconda
  :ensure t)

(confige conda
  (:preface
   (autoload 'conda-activate "conda" nil t)
   (autoload 'conda-deactivate "conda" nil t))
  (:before
   (let ((map global-map))
     (define-key map (kbd "C-c a c") #'conda-activate)
     (define-key map (kbd "C-c a d") #'conda-deactivate))))

(confige pip-requirements
  :ensure t)

;;; Writing

(confige text-mode
  (:before
   (dolist (pattern '("/LICENSE\\'" "/UNLICENSE\\'"))
     (cl-pushnew `(,pattern . text-mode) auto-mode-alist :test #'equal))
   (with-eval-after-load 'autoinsert
     (define-auto-insert '("/UNLICENSE\\'" . "The Unlicense")
       "unlicense"))))

(confige markdown-mode
  :ensure t :preload t
  (:before
   (cl-pushnew '("\\.Rmd\\'" . markdown-mode) auto-mode-alist :test #'equal))
  (:after
   (setf (default-value 'markdown-enable-math) t)
   (setf markdown-fontify-code-blocks-natively t)
   (setf markdown-max-image-size '(300 . nil))
   (setf markdown-css-paths `(,(concat "file://" (init--etc "css/pandoc.css")))
         markdown-command '("pandoc" "--section-divs" "--from=markdown" "--to=html5"))))

(confige rst
  :preload t
  (:preface
   (declare-function rst-forward-section "rst")
   (declare-function rst-backward-section "rst")
   (declare-function rst-insert-list "rst"))
  (:after
   (let ((map rst-mode-map))
     (define-key map (kbd "M-n") #'rst-forward-section)
     (define-key map (kbd "M-p") #'rst-backward-section)
     (define-key map (kbd "M-RET") #'rst-insert-list)
     (define-key map (kbd "C-c C-j") #'rst-insert-list))))

;;; LaTeX

(confige auctex
  :ensure t
  (:preface (defvar japanese-TeX-error-messages))
  (:before (setf japanese-TeX-error-messages nil))
  (:postface
   (confige tex
     :preload t
     (:preface
      (declare-function TeX-source-correlate-mode "ext:tex")
      (declare-function TeX-error-overview "ext:tex"))
     (:after
      (setf (default-value 'TeX-master) nil
            (default-value 'TeX-engine) 'luatex)
      (setf TeX-parse-self t
            TeX-auto-save t)
      (setf TeX-save-query nil)
      (setf TeX-electric-sub-and-superscript t)
      (TeX-source-correlate-mode)

      (let ((commands nil))
        (push '("TeXcount" "texcount -utf8 -inc %t"
                TeX-run-background nil (LaTeX-mode)
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

   (confige tex-fold
     :preload t
     (:before (add-hook 'TeX-mode-hook #'TeX-fold-mode))
     (:after (define-key TeX-fold-keymap (kbd "\\") #'prettify-symbols-mode)))

   (confige tex-style
     :preload t
     (:after
      (setf LaTeX-csquotes-open-quote "\\enquote{"
            LaTeX-csquotes-close-quote "}")))

   (confige latex
     :preload t
     (:preface
      (declare-function LaTeX-math-mode "ext:latex")
      (defun init--setup-LaTeX-mode ()
        (make-local-variable 'TeX-electric-math)
        (setf TeX-electric-math '("\\(" . "\\)"))
        (LaTeX-math-mode)))
     (:after
      (setf LaTeX-default-style "scrartcl")
      (setf LaTeX-babel-hyphen nil)
      (add-hook 'LaTeX-mode-hook #'init--setup-LaTeX-mode)))

   (confige preview
     :preload t
     (:after (setf preview-auto-cache-preamble nil)))))

(confige auctex-latexmk
  (:preface (autoload 'auctex-latexmk-setup "auctex-latexmk"))
  (:before
   (with-eval-after-load 'latex
     (auctex-latexmk-setup))))

(confige latex-snippets
  (:preface (autoload 'latex-snippets-file-template "latex-snippets" nil t))
  (:before
   (with-eval-after-load 'autoinsert
     (define-auto-insert '(LaTeX-mode . "LaTeX file template")
       #'latex-snippets-file-template))))

(confige reftex
  :preload t
  (:before (add-hook 'LaTeX-mode-hook #'reftex-mode))
  (:after
   (setf reftex-plug-into-AUCTeX t)
   ;; Offer a guess but ask for confirmation
   (setf reftex-insert-label-flags '(t t))
   (setf reftex-cite-format 'biblatex))
  (:postface
   (confige reftex-toc
     :preload t
     (:after (define-key reftex-toc-mode-map (kbd "d") nil)))))

(confige bibtex
  :preload t
  (:preface (declare-function bibtex-validate "bibtex"))
  (:before (setf bibtex-dialect 'biblatex))
  (:after
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

(confige ledger-mode
  :ensure t :preload t
  (:preface
   (defun init--early-setup-ledger-mode ()
     (when (and (derived-mode-p 'ledger-mode)
                (equal (buffer-name) ledger-schedule-buffer-name))
       (make-local-variable 'ledger-mode-hook)
       (remove-hook 'ledger-mode-hook #'flycheck-mode t))))
  (:after
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
   (add-hook 'change-major-mode-after-body-hook #'init--early-setup-ledger-mode)))

(confige flycheck-ledger
  :ensure t :preload t
  (:before
   (with-eval-after-load 'flycheck
     (require 'flycheck-ledger)
     (setf flycheck-ledger-pedantic t
           flycheck-ledger-explicit t))))

;;; Org

(confige org
  :preload t
  (:before
   (make-directory (init--var "org/") t)
   (setf org-babel-load-languages nil))
  (:after
   (setf org-directory (init--sync "org/"))
   (make-directory org-directory t)

   (setf org-default-notes-file (expand-file-name "inbox.org" org-directory)
         org-agenda-files `(,org-directory))
   (setf org-archive-location ".archive.org::")

   (setf org-adapt-indentation nil)
   (setf org-special-ctrl-a/e t
         org-fold-catch-invisible-edits 'show-and-error)
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
  (:postface
   (confige org-goto
     :preload t
     (:after (setf org-goto-auto-isearch nil)))

   (confige org-refile
     :preload t
     (:after
      (setf org-refile-targets '((nil . (:maxlevel . 5))
                                 (org-agenda-files . (:maxlevel . 5)))
            org-refile-allow-creating-parent-nodes 'confirm)
      (setf org-refile-use-outline-path 'file)))

   (confige org-archive
     :preload t
     (:preface (declare-function org-archive-set-tag "org-archive"))
     (:after
      (setf org-archive-default-command #'org-archive-set-tag)
      (setf org-archive-file-header-format nil)))

   (confige org-id
     :preload t
     (:before (setf org-id-locations-file (init--var "org/id-locations"))))

   (confige org-duration
     :preload t
     (:after (setf org-duration-format 'h:mm)))

   (confige org-lint
     (:preface (declare-function org-lint "org-lint"))
     (:before
      (with-eval-after-load 'org
        (define-key org-mode-map (kbd "M-g L") #'org-lint))))

   (confige ol
     (:preface (autoload 'org-store-link "ol" nil t))
     (:before (define-key global-map (kbd "C-c o l") #'org-store-link)))

   (confige org-capture
     :preload t
     (:before (define-key global-map (kbd "C-c o c") #'org-capture))
     (:after
      (setf org-capture-templates
            '(("t" "Task" entry (file+headline "" "Tasks")
               "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i"
               :empty-lines 1)
              ("n" "Note" entry (file+headline "" "Notes")
               "* %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i"
               :empty-lines 1)))))

   (confige org-agenda
     :preload t
     (:before (define-key global-map (kbd "C-c o a") #'org-agenda))
     (:after
      (setf org-agenda-window-setup 'only-window
            org-agenda-restore-windows-after-quit t)
      (setf org-agenda-block-separator "")
      (setf org-agenda-span 'day
            org-agenda-start-on-weekday 0)
      (setf org-stuck-projects '("+project/-DONE-CANCELLED" ("NEXT") nil ""))))

   (confige org-clock
     :preload t
     (:before
      (setf org-clock-persist-file (init--var "org/clock-save.el"))
      (with-eval-after-load 'org
        (org-clock-persistence-insinuate)))
     (:after
      (setf org-clock-out-remove-zero-time-clocks t)
      (setf org-clock-persist 'history)
      (setf org-show-notification-timeout 10)))

   (confige org-src
     :preload t
     (:after
      (setf org-src-preserve-indentation t)
      (setf org-src-window-setup 'other-window)))

   (confige ox
     :preload t
     (:after (setf org-export-coding-system 'utf-8)))

   (confige ox-latex
     :preload t
     (:before
      (setf org-latex-classes
            '(("article"
               "\\documentclass[11pt]{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
     (:after
      (setf org-latex-subtitle-format "\\subtitle{%s}"
            org-latex-subtitle-separate t)
      (setf org-latex-compiler "lualatex"
            org-latex-bib-compiler "biber")
      (setf org-latex-pdf-process
            '("latexmk -%latex -interaction=nonstopmode -outdir=%o %f"))))

   (confige ox-html
     :preload t
     (:after
      (setf org-html-doctype "html5"
            org-html-html5-fancy t
            org-html-preamble nil
            org-html-postamble nil
            org-html-htmlize-output-type 'css
            org-html-head-include-default-style nil
            org-html-head (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
                                  (concat "file://" (init--etc "css/org.css"))))))))

(confige org-mru-clock
  :ensure t
  (:before
   (define-key global-map (kbd "C-c o i") #'org-mru-clock-in)
   (define-key global-map (kbd "C-c o u") #'org-mru-clock-select-recent-task)))

;;; Data

(confige sql
  :preload t
  (:preface (declare-function sql-set-product "sql"))
  (:before
   (cl-pushnew 'sqlite init--org-babel-languages :test #'eq)
   (cl-pushnew 'sql init--org-babel-languages :test #'eq))
  (:after
   (setf sql-product 'sqlite)
   (define-key sql-mode-map (kbd "C-c C-p") #'sql-set-product)
   (define-key sql-mode-map (kbd "C-c a a") #'sql-product-interactive)))

(confige sqlup-mode
  :ensure t
  (:before (add-hook 'sql-mode #'sqlup-mode)))

(confige dotenv-mode
  :ensure t)

(confige jq-mode
  :ensure t :preload t
  (:before
   (cl-pushnew 'jq init--org-babel-languages :test #'eq)
   (with-eval-after-load 'js
     (define-key js-json-mode-map (kbd "C-c C-c") #'jq-interactively)))
  (:after (setf jq-interactive-font-lock-mode #'js-json-mode)))

(confige yaml-mode
  :ensure t
  (:preface
   (defun init--early-setup-yaml-mode ()
     (when (derived-mode-p 'yaml-mode)
       (make-local-variable 'text-mode-hook)
       (dolist (fn '(flyspell-mode auto-fill-mode abbrev-mode))
         (remove-hook 'text-mode-hook fn t)))))
  (:after
   (add-hook 'change-major-mode-after-body-hook #'init--early-setup-yaml-mode)))

(confige csv-mode
  :ensure t)

(confige protobuf-mode
  :ensure t
  (:after
   (c-add-style "protobuf" '("k&r"
                             (c-basic-offset . 2)
                             (indent-tabs-mode . nil)))
   (cl-pushnew '(protobuf-mode . "protobuf") c-default-style
               :test #'eq :key #'car)))

(confige graphql-mode
  :ensure t)

(confige ob-graphql
  :ensure t
  (:before (cl-pushnew 'graphql init--org-babel-languages :test #'eq)))

;;; init.el ends here
