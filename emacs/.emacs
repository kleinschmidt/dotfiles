(unless (getenv "LANG") (setenv "LANG" "en_US.UTF-8"))
(setq custom-file "~/.emacs.d/customized.el")

;; straight?
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; local ~/emacs.d/lisp/
;; (let ((default-directory "~/.emacs.d/lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

(tool-bar-mode -1)

(setq dired-listing-switches "-alh")

;; no scroll bar, even in new windows
(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (modify-frame-parameters frame
                                      '((vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)))))
;; remove scroll bars on first window to open
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; tabs = evil
(setq-default indent-tabs-mode nil)

;; fill at 80 chars
(setq-default fill-column 80)

;; auto-fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; show column/line numbers
(setq column-number-mode t)
(setq line-number-mode t)

;; prefer vertical splits
(setq split-width-threshold 160)
(setq split-height-threshold nil)

;; use zap-up-to-char instead of zap-to-char
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; compile with C-x C-m
(global-set-key (kbd "C-x C-m") 'compile)

;; solarized theme
(use-package solarized-theme
  :ensure t)

;; expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package swiper
  :requires ivy
  :bind (("C-s" . swiper)
         ("C-S-s" . isearch-forward)
         ("C-r" . swiper)))

(use-package counsel
  :requires ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-ag)))


(use-package ace-jump-mode
  :ensure t
  :bind (("C-." . ace-jump-mode)
         ("C-," . ace-jump-mode-pop-mark))
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package ace-window
  :ensure t
  :bind ("M-." . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; allow ace window jump in term character mode
;; send C-<backspace> as backspace instead of kill-word
(use-package term
  :bind (:map term-raw-map
              ("M-." . ace-window)
              ("C-<backspace>" . term-send-backspace)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))


;; pdf-tools
;; (use-package pdf-tools
;;   :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
;;   :init
;;   (pdf-tools-install)
;;   :config
;;   (setq pdf-annot-list-listed-types '(caret file highlight squiggly strike-out text underline unknown))
;;   :bind (:map pdf-view-mode-map
;;               ("C-s" . isearch-forward)
;;               ("C-r" . isearch-backward)
;;               ("M-h" . pdf-annot-add-highlight-markup-annotation)))

;; ess
(use-package ess
  :ensure t
  :ensure julia-mode
  :init
  (require 'ess-site)
  (push '("\\.jl\\'" . julia-mode) auto-mode-alist)
  (delete-dups auto-mode-alist)
  :config
  (setq ess-use-flymake nil)
  ;; a dirty hack: ess requires julia-mode, which adds an entry for "\\.jl\\'"
  ;; to the auto-mode-alist.  so it's impossible to shadow it using the same key
  (add-hook 'ess-mode-hook
            (lambda ()
              (ess-set-style 'RStudio 'quiet)
              (ess-toggle-underscore t)
              (ess-toggle-underscore nil)))
  (defun ess-pipe ()
    (interactive)
    "Insert a pipe"
    (if (char-equal (char-before) ? )
        ()
      (insert " "))
    (insert "%>%")
    (ess-roxy-newline-and-indent))
  :bind ("C-c C-m" . ess-pipe))

(use-package stan-mode
  :ensure t
  :requires ess)

;; TODO: once #308 is merged (or some other fix for #219/#287), install this
;; from MELPA again, but for now install into .emacs.d/lisp/ via
;;
;; gh repo clone nnicandro/emacs-jupyter && cd emacs-jupyter && git checkout origin/fix-219

;; jupyter integration (mostly for julia)
(use-package jupyter
  :straight t
  :config
  (setq jupyter-repl-echo-eval-p t))

;; or:
;; (straight-use-package '(jupyter :local-repo "~/.emacs.d/lisp/emacs-jupyter/"))

;; julia mode
(use-package julia-mode
  :ensure julia-repl
  :ensure fill-column-indicator
  :mode "\\.jl\\'"
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (add-hook 'julia-mode-hook (lambda () (setq show-trailing-whitespace t))))

(define-derived-mode jldoctest-mode julia-mode "Julia Doctest"
  "Julia Doctest mode")

;; Function to switch on adpative-wrap-prefix-mode for visual-line-mode
;; when appropriate.
;; http://stackoverflow.com/questions/13559061/emacs-how-to-keep-the-indentation-level-of-a-very-long-wrapped-line
(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

(use-package adaptive-wrap
  :ensure t)

;;; markdown mode
(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  ;; predicate to prevent flyspell checking in code blocks (inline and
  ;; fenced)
  ;; http://emacs.stackexchange.com/questions/20230/how-to-make-flyspell-ignore-code-blocks-in-markdown
  (defun markdown-mode-flyspell-verify ()
    "Used for `flyspell-generic-check-word-predicate' in text modes."
    ;; (point) is next character after the word, need to check 1 back
    (let ((f (get-text-property (- (point) 1) 'face)))
      (not (memq f '(markdown-pre-face
                     markdown-inline-code-face
                     markdown-language-keyword-face)))))
  (put 'markdown-mode 'flyspell-mode-predicate 'markdown-mode-flyspell-verify)
  ;; reftex in markdown mode
  (defvar markdown-cite-format)
  (setq markdown-cite-format
        '(
          (?\C-m . "[@%l]")
          (?p . "[@%l]")
          (?t . "@%l")
          )
        )
  (defun markdown-reftex-citation ()
    (interactive)
    (let ((reftex-cite-format markdown-cite-format)
          (reftex-cite-key-separator "; @"))
      (reftex-citation)))
  :bind (:map markdown-mode-map
              ("M-<right>" . markdown-demote)
              ("M-<left>" . markdown-promote)
              ("M-<up>" . markdown-move-up)
              ("M-<down>" . markdown-move-down)))

(defun grunt ()
  "Run grunt"
  (interactive)
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
        (result (call-process-shell-command grunt-cmd nil grunt-buffer t))
        (output (with-current-buffer grunt-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Grunt completed without errors"))
          (t
           (message nil)
           (split-window-vertically)
           (set-window-buffer (next-window) grunt-buffer)))))

(setq grunt-cmd "grunt --no-color")

;; disable electric indent in js2-mode, and make default for .js
(use-package js2-mode
  :ensure t
  :mode "\\.js?\\'"
  :config
  (setq js2-basic-offset 2)
  :bind (:map js2-mode-map
         ("C-c g" . grunt)))
;; (add-hook 'js2-mode-hook (lambda () (electric-indent-local-mode -1)))

(use-package restclient
  :ensure t)

(use-package ob-restclient
  :ensure t
  :after restclient)


;; Bind magit-status to C-c i
(use-package magit
  :ensure t
  :bind (("C-c j" . magit-status)))

(use-package forge
  :ensure t
  :after magit)

;; Set up TRAMP to re-use existing SSH connections to servers
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
   "-o ControlMaster=auto -o ControlPersist=yes"))

(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link))
  :config
  (setq git-link-use-commit t))

;; AUCTeX fontification
;; apacite citation macros
(setq font-latex-match-reference-keywords
      '(("cite" "[{")
        ("citeA" "[{")
        ("citeNP" "[{")
        ("citeauthor" "[{")
        ("citeyear" "[{")
        ("citeyearNP" "[{")
        ("citeauthorNP" "[{")
        ("nocite" "[{")
        ("nocitemeta" "[{")
        ("fullcite" "[{")
        ("fullciteA" "[{")
        ("fullciteNP" "[{")
        ("fullciteauthor" "[{")
        ("fullciteauthorNP" "[{")
        ("shortcite" "[{")
        ("shortciteA" "[{")
        ("shortciteNP" "[{")
        ("shortciteauthor" "[{")
        ("shortciteauthorNP" "[{")))

;; Default bibliography location:
(setq reftex-default-bibliography
      (list (concat (file-name-as-directory (getenv "HOME"))
                    "Documents/papers/library-clean.bib")))

(use-package ivy-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography
        '("~/Documents/papers/zotero.bib"))
  (setq bibtex-completion-pdf-field "File")
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
  (ivy-add-actions
   'ivy-bibtex
   '(("O" ivy-bibtex-open-pdf "Open PDF (if present)")))
  :bind ("M-[" . ivy-bibtex))


;; Use latexmk with auctex (package installed via MELPA)
(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup))

;; Manually downloaded matlab mode
;; (require 'matlab-mode)

;; wc-mode
(use-package wc-mode
  :ensure t)


;; web-mode/swig
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.swig\\'")
  :config
  (setq web-mode-engines-alist
        '(("django" . "\\.html?\\'"))))


;; polymode for r markdown
(use-package polymode
  :ensure t
  :mode (("\\.org\\'" . org-mode)))

(use-package poly-markdown
  :ensure t
  ;; poly-markdown-mode auto-detects chunk types.
  :mode (("\\.jmd\\'" . poly-markdown-mode)
         ("\\.Rmd" . poly-markdown-mode)))

;; mac switch meta key
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper)
        (message "Option is meta")
        )
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      (message "Option is not meta")
      )
    )
  )
(global-set-key (kbd "C-;") 'mac-switch-meta)

;; set option to meta by default
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)

;; mac-style bindings for new/close window (frame)
(global-set-key (kbd "H-n") 'make-frame)
(global-set-key (kbd "H-w") 'delete-frame)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; smerge command prefix to C-c v
(setq smerge-command-prefix "\C-cv")

;; org mode prettification
(use-package org
  :ensure org-bullets
  :ensure org-plus-contrib
  :ensure counsel
  :config
  (setq org-confirm-babel-evaluate nil)
  (add-to-list 'org-structure-template-alist
               '("jl" . "src jupyter-julia"))
  (let* ((headline `(:inherit default :weight bold :height 1.0)))
    (custom-theme-set-faces 'user
                            `(org-level-1 ((t (,@headline))))
                            `(org-level-2 ((t (,@headline))))
                            `(org-level-3 ((t (,@headline))))
                            `(org-level-4 ((t (,@headline))))
                            `(org-level-5 ((t (,@headline))))
                            `(org-level-6 ((t (,@headline))))
                            `(org-level-7 ((t (,@headline))))
                            `(org-level-8 ((t (,@headline))))
                            )
    )
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-hide-emphasis-markers nil)
  (setq org-directory "~/work/notes/")
  (setq org-capture-templates
        '(("j" "Journal" entry
           (file+olp+datetree "~/work/notes/journal.org")
           "* %?\n  Entered on %U  From %i\n  %a")
          ("J" "Journal (other date)" entry
           (file+olp+datetree "~/work/notes/journal.org")
           "* %?\n  Entered on %U  From %i\n  %a"
           :time-prompt t)
          ("d" "Daily task" entry
           (file+olp+datetree "~/work/notes/journal.org")
           "* TODO %?\n  Created on %U\n  %i\n  %a"
           :time-prompt t)
          ("i" "Inbox" entry
           (file "inbox.org")
           "* TODO %?\n/Entered on/ %U")
          ("n" "Note" entry
           (file "notes.org")
           "* Note (%a)\n  /Entered on/ %U\n" "\n" "%?")
          ("m" "Meeting" entry
           (file+headline "agenda.org" "Future")
           "* %? :meeting:\n%^T"
           :time-prompt t)))
  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "NOPE(x)")))
  (setq org-refile-targets
        '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
          ("agenda.org" :regexp . "\\(Past\\|Future\\)")
          ("ideas.org" :level . 1)
          (nil :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  ;; log NEXT time with an ACTIVATED time stamp
  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
  (setq org-log-done 'time)
  ;; custom agenda view
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"

           (;; scheduled things for day
            (agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline 'regexp ":@canceled"))
                     (org-deadline-warning-days 0)
                     (org-agenda-span 1)))
            ;; list of next tasks
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            ;; deadlines for next week
            (agenda ""
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'regexp "\\* \\(DONE\\|NOPE\\)"))
                     (org-agenda-overriding-header "Deadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "Inbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))
          ("W" "Weekly review"
           agenda ""
           ((org-agenda-start-day "-7d")
            (org-agenda-span 7)
            (org-agenda-start-on-weekday 1)
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'regexp "\\(TODO\\|NEXT\\)"))
            (org-agenda-start-with-log-mode '(closed))))))
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (julia . t)
     (jupyter . t)
     (restclient . t)))
  (defun org-todo-buffer ()
    "Create new indirect buffer with sparse tree of undone TODO items"
    (interactive)
    (clone-indirect-buffer "*org TODO undone*" t)
    (org-show-todo-tree nil) ; mimics interactive usage
    (org-remove-occur-highlights)
    )
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . counsel-org-capture)
         ("C-c i" . org-capture-inbox)
         ("C-c t" . org-todo-buffer)))

;;------------------------------------------------------------------------------
;; change font size for current frame

(defconst default-face-height 100)

(defun set-frame-face-height (&optional new-height)
  "set the :height attribute of the default face for the current frame"
  (interactive)
  (if new-height
      (set-face-attribute 'default (selected-frame) :height new-height)
    (set-frame-face-height default-face-height))
)

(defun plus-frame-face-height (height-increment)
  "Increase default font face :height"
  (interactive)
  (set-frame-face-height (+ (face-attribute 'default :height) height-increment)))

(defun inc-frame-face-height (&optional increment)
  ":height + 10"
  (interactive)
  (if increment
      (plus-frame-face-height increment)
    (plus-frame-face-height 10)))

(defun dec-frame-face-height (&optional decrement)
  ":height - 10"
  (interactive)
  (if decrement
      (plus-frame-face-height (- decrement))
    (plus-frame-face-height -10)))

(global-set-key (kbd "C-c C-=") 'inc-frame-face-height)
(global-set-key (kbd "C-c C--") 'dec-frame-face-height)

;; copy over PATH variable from the shell

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs (list "LOLCOMMITS_DELAY"
                                        "LOLCOMMITS_FORK"
                                        "LOLCOMMITS_STEALTH"
                                        "LOLCOMMITS_DEVICE"
                                        "AWS_PROFILE"
                                        "AWS_DEFAULT_REGION")))


;; auto-follow compilation buffer, stopping at first error
(setq compilation-scroll-output 'first-error)

(use-package pkgbuild-mode
  :ensure t
  :mode "PKGBUILD\\'")

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)

(use-package yaml-mode
  :ensure t
  :mode "\\.[yY][aA]?[mM][lL]\\'")

(use-package projectile
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :demand t
  :config
  (counsel-projectile-mode)
  :bind ("C-c p" . projectile-command-map))

(use-package ag
  :ensure t
  :requires wgrep-ag
  :bind (("C-c K" . ag)
         :map ag-mode-map
         ("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package wgrep-ag
  :ensure t)
(use-package wgrep
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 3))

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'")
  :hook
  (terraform-mode . terraform-format-on-save-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("[Dd]ockerfile"))

(use-package olivetti
  :ensure t)

(use-package bazel
  :ensure t)

(use-package protobuf-mode
  :ensure t)

;; (defconst my-cc-style
;;   '("gnu"
;;     (c-offsets-alist . ((innamespace . [0])))))

;; (c-add-style "my-cc-style" my-cc-style)
(defun no-namespace-indent ()
  (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'no-namespace-indent)

(use-package cython-mode
  :ensure t)

(use-package clang-format
  :ensure t
  :config
  (defun my-clang-format (arg)
    (interactive "P")
    (if arg
        (clang-format-region)
      (clang-format-buffer)))
  :bind (("C-c f" . my-clang-format)))

(use-package vterm
    :ensure t)

(load custom-file)
(put 'narrow-to-region 'disabled nil)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
