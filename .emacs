(unless (getenv "LANG") (setenv "LANG" "en_US.UTF-8"))

(setq package-enable-at-startup nil) (package-initialize)

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

;; local ~/emacs.d/lisp/
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/build/use-package")
(require 'use-package)

;; compile with C-x C-m
(global-set-key (kbd "C-x C-m") 'compile)

;; melpa 
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

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
(use-package term
  :bind (:map term-raw-map
              ("M-." . ace-window)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))


;; pdf-tools
(use-package pdf-tools
  :ensure t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config
  (setq pdf-annot-list-listed-types '(caret file highlight squiggly strike-out text underline unknown))
  (pdf-tools-install)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)
              ("M-h" . pdf-annot-add-highlight-markup-annotation)))

;; ess
(use-package ess
  :ensure julia-mode
  :config
  ;; a dirty hack: ess requires julia-mode, which adds an entry for "\\.jl\\'"
  ;; to the auto-mode-alist.  so it's impossible to shadow it using the same key
  (setq auto-mode-alist (rassq-delete-all 'julia-mode auto-mode-alist))
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
    (ess-newline-and-indent))
  :bind ("C-c C-m" . ess-pipe))

(use-package stan-mode
  :requires ess)

;; julia mode
(use-package julia-mode
  :ensure julia-repl
  :init
  ;; a dirty hack: ess requires julia-mode, which adds an entry for "\\.jl\\'"
  ;; to the auto-mode-alist.  so it's impossible to shadow it using the same key
  (setq auto-mode-alist (rassq-delete-all 'julia-mode auto-mode-alist))
  :mode "\\.jl\\'"
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode))


;; Function to switch on adpative-wrap-prefix-mode for visual-line-mode
;; when appropriate.
;; http://stackoverflow.com/questions/13559061/emacs-how-to-keep-the-indentation-level-of-a-very-long-wrapped-line
(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

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
              ("C-c [" . markdown-reftex-citation)
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
  :bind (:map js2-mode-map
	 ("C-c g" . grunt)))
;; (add-hook 'js2-mode-hook (lambda () (electric-indent-local-mode -1)))


;; Bind magit-status to C-c i
(use-package magit
  :ensure t
  :bind (("C-c i" . magit-status)))

(use-package magithub
  :ensure t
  :after magit
  :config (magithub-feature-autoinject t))

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
  :mode (("\\.jmd\\'" . poly-markdown-mode)
	 ("\\.Rmd" . poly-markdown+r-mode)))

;; (use-package poly-markdown
;;   :ensure polymode
;;   ;; poly-markdown-mode auto-detects chunk types.
;;   :mode (("\\.jmd\\'" . poly-markdown-mode)
;; 	 ("\\.Rmd" . poly-markdown+r-mode)))


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
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; mac-style bindings for new/close window (frame)
(global-set-key (kbd "H-n") 'make-frame)
(global-set-key (kbd "H-w") 'delete-frame)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ess-indent-with-fancy-comments nil)
 '(fci-rule-color "#073642")
 '(fill-column 80)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(js2-include-node-externs t)
 '(julia-repl-executable "/home/dave/build/julia/julia")
 '(magit-diff-use-overlays nil)
 '(markdown-enable-math t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("~/work/talkernet/talkernet.org" "~/work/advising/sten/sten.org" "~/code/experiments/animal-similarity/imaging-analysis/pres/writeup/animals.org" "~/work/notes/todo.org" "~/work/notes/projects.org" "~/work/notes/general.org" "~/work/notes/talks.org" "~/work/notes/unfuck.org" "~/work/notes/writing.org" "~/work/notes/julia.org")))
 '(org-directory "~/work/notes")
 '(package-selected-packages
   (quote
    (markdown-mode poly-markdown ace-jump-mode ace-window julia-repl yaml yaml-mode pkgbuild-mode expand-region multiple-cursors matlab-mode counsel flyspell-correct-ivy ivy ivy-bibtex swiper auctex stan-mode exec-path-from-shell adaptive-wrap web-mode wc-mode solarized-theme polymode org-bullets magit js2-mode ess auctex-latexmk)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(reb-re-syntax (quote string))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(smerge-command-prefix "\"\\C-cv\"")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :family "MesloLGS Nerd Font"))))
 '(markdown-comment-face ((t (:foreground "#586e75" :strike-through nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#839496" :height 1.3))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#839496" :height 1.1))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#839496"))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#839496"))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#839496"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#839496"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#839496"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#839496"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#22aa22")))))


(put 'upcase-region 'disabled nil)

;; smerge command prefix to C-c v
(setq smerge-command-prefix "\C-cv")
(put 'downcase-region 'disabled nil)

;; org mode prettification
(use-package org
  :ensure org-bullets
  :config
  (let* ((base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit default :weight bold :foreground ,base-font-color)))
    
    (custom-theme-set-faces 'user
                            `(org-level-1 ((t (,@headline :height 1.3))))
                            `(org-level-2 ((t (,@headline :height 1.1))))
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
  (add-hook 'org-mode-hook 'auto-fill-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)))

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
  (exec-path-from-shell-initialize))

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

