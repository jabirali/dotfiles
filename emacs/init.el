;;; ~/.config/emacs/init.el -*- outline-minor-mode: t; -*-

;;; Package management:
;; Emacs bundles a decent package manager (package.el) and a nice
;; interface for loading and configuring packages (use-package).
(use-package use-package
  :custom
  (native-comp-async-report-warnings-errors nil)
  (package-native-compile t)
  (use-package-always-demand t)
  (use-package-always-ensure t)
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;;; Core configuration:
(use-package emacs
  :custom
  (default-input-method 'TeX)
  (frame-title-format "%b")
  (inhibit-startup-message t)
  (line-spacing 0.15)
  (make-backup-files nil)
  (mouse-highlight nil)
  (outline-blank-line t)
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (tab-width 4) 
  (truncate-lines t)
  (use-short-answers t)
  (xterm-set-window-title t)
  :config
  (auto-save-mode -1)
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (server-mode 1)
  (when (display-graphic-p)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	(fringe-mode 16)))

;;; Global advice:
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Personal customizations of any Emacs theme that is loaded."
  (let ((bg0 (face-attribute 'default :background))
		(bg1 (face-attribute 'scroll-bar :background))
		(fg1 (face-attribute 'success :foreground))
		(fg2 (face-attribute 'mode-line :foreground)))

	;; Make the colorization of the tab bar, mode line, and dividers more minimal.
	(set-face-attribute 'tab-bar nil :foreground bg1 :background bg1 :box `(:line-width 6 :color ,bg1))
	(set-face-attribute 'tab-bar-tab nil :foreground fg1 :background bg1 :box `(:line-width 6 :color ,bg1))
	(set-face-attribute 'tab-bar-tab-inactive nil :foreground fg2 :background bg1 :box `(:line-width 6 :color ,bg1))

	(set-face-attribute 'mode-line nil :background bg1 :box `(:line-width 6 :color ,bg1))
	(set-face-attribute 'mode-line-inactive nil :background bg1 :box `(:line-width 6 :color ,bg1))

	(set-face-attribute 'fringe nil :foreground bg0 :background bg0)
	(set-face-attribute 'vertical-border nil :foreground bg1 :background bg1))

  ;; Make the Kitty theme the current Emacs theme.
  (shell-command
   (let* ((emacs-theme-name (symbol-name (car custom-enabled-themes)))
		  (kitty-theme-name (capitalize (replace-regexp-in-string "-" " " emacs-theme-name))))
	 (format "kitty +kitten themes %s" kitty-theme-name))))

;;; Vim keybindings:
(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after (evil org general)
  :hook
  (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :ensure nil
  :after evil-org
  :config
  (evil-org-agenda-set-keys))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-override-mode 1)

  ;; Prepare Spacemacs-like leader keymaps. Here, "gmap" and "lmap"
  ;; refers to a global map (leader) and local map (localleader).
  (general-create-definer gmap
	:keymaps 'override
	:states '(motion normal visual)
	:prefix "SPC")

  (general-create-definer lmap
	:keymaps 'override
	:states '(motion normal visual)
	:prefix ",")

  ;; Work around keyboard layout differences.
  (define-key key-translation-map (kbd "§") (kbd "`"))
  (define-key key-translation-map (kbd "±") (kbd "~"))

  ;; Fix terminal keys.
  (define-key key-translation-map (kbd "M-<return>") (kbd "M-RET"))

  ;; Map "SPC" to my custom "space menu" leader map.
  (gmap
	"SPC" '(execute-extended-command :which-key "cmd")

	"1" '(tab-bar-select-tab :which-key "1")          ; Tmux: C-b 1
	"2" '(tab-bar-select-tab :which-key "2")          ; Tmux: C-b 2
	"3" '(tab-bar-select-tab :which-key "3")          ; Tmux: C-b 3
	"4" '(tab-bar-select-tab :which-key "4")          ; Tmux: C-b 4
	"5" '(tab-bar-select-tab :which-key "5")          ; Tmux: C-b 5
	"6" '(tab-bar-select-tab :which-key "6")          ; Tmux: C-b 6
	"7" '(tab-bar-select-tab :which-key "7")          ; Tmux: C-b 7
	"8" '(tab-bar-select-tab :which-key "8")          ; Tmux: C-b 8
	"9" '(tab-bar-select-tab :which-key "9")          ; Tmux: C-b 9

	"a" '(org-agenda :which-key "agenda")             ; Emacs: C-c a
	"b" '(switch-to-buffer :which-key "buffer")       ; Emacs: C-x b
	"d" '(dired-jump :which-key "dired")              ; Emacs: C-x d
	"f" '(find-file :which-key "file")                ; Emacs: C-x C-f
	"g" '(magit :which-key "git")                     ; Emacs: C-x g
	"h" `(,help-map :which-key "help")                ; Emacs: C-h
	"i" '(imenu :which-key "imenu")                   ; Emacs: M-g i
	"j" '(bookmark-jump :which-key "jump")
	"k" '(kill-this-buffer :which-key "kill")         ; Emacs: C-x k
	"n" `(,narrow-map :which-key "narrow")            ; Emacs: C-x n
	"o" '(ace-window :which-key "other")              ; Emacs: C-x o
	"p" `(,project-prefix-map :which-key "project")   ; Emacs: C-x p
	"q" '(evil-window-delete :which-key "quit")       ; Vim: :q
	"r" '(recentf :which-key "recent")                ; Emacs: C-c r
	"s" '(save-buffer :which-key "save")              ; Emacs: C-x s
	"t" '(tab-bar-new-tab :which-key "tab")           ; Emacs: C-x t n
	"w" `(,evil-window-map :which-key "window")       ; Vim: C-w
	"y" '(clone-indirect-buffer-other-window :which-key "indirect"))

  ;; Map "C-c C-x" to ", x" for all letters "x". These are
  ;; generally keybindings defined by the current major mode,
  ;; and make a sensible set of default localleader bindings.
  (lmap
	"a" (general-key "C-c C-a")
	"b" (general-key "C-c C-b")
	"c" (general-key "C-c C-c")
	"d" (general-key "C-c C-d")
	"e" (general-key "C-c C-e")
	"f" (general-key "C-c C-f")
	"g" (general-key "C-c C-g")
	"h" (general-key "C-c C-h")
	"i" (general-key "C-c C-i")
	"j" (general-key "C-c C-j")
	"k" (general-key "C-c C-k")
	"l" (general-key "C-c C-l")
	"m" (general-key "C-c C-m")
	"n" (general-key "C-c C-n")
	"o" (general-key "C-c C-o")
	"p" (general-key "C-c C-p")
	"q" (general-key "C-c C-q")
	"r" (general-key "C-c C-r")
	"s" (general-key "C-c C-s")
	"t" (general-key "C-c C-t")
	"u" (general-key "C-c C-u")
	"v" (general-key "C-c C-v")
	"w" (general-key "C-c C-w")
	"x" (general-key "C-c C-x")
	"y" (general-key "C-c C-y")
	"z" (general-key "C-c C-z"))

  ;; Map "C-c ?" to ", ?" for all symbols "?". This includes some
  ;; major-mode keybindings and most minor-mode keybindings. One
  ;; exception: ", ," is mapped to "C-c C-c" for simplicity.
  (lmap
	"!"  (general-key "C-c !" )
	"\"" (general-key "C-c \"")
	"#"  (general-key "C-c #" )
	"$"  (general-key "C-c $" )
	"%"  (general-key "C-c %" )
	"&"  (general-key "C-c &" )
	"'"  (general-key "C-c '" )
	"("  (general-key "C-c (" )
	")"  (general-key "C-c )" )
	"*"  (general-key "C-c *" )
	"+"  (general-key "C-c +" )
	","  (general-key "C-c C-c" )
	"-"  (general-key "C-c -" )
	"."  (general-key "C-c ." )
	"/"  (general-key "C-c /" )
	":"  (general-key "C-c :" )
	";"  (general-key "C-c ;" )
	"<"  (general-key "C-c <" )
	"="  (general-key "C-c =" )
	">"  (general-key "C-c >" )
	"?"  (general-key "C-c ?" )
	"@"  (general-key "C-c @" )
	"["  (general-key "C-c [" )
	"\\" (general-key "C-c \\")
	"]"  (general-key "C-c ]" )
	"^"  (general-key "C-c ^" )
	"_"  (general-key "C-c _" )
	"`"  (general-key "C-c `" )
	"{"  (general-key "C-c {" )
	"|"  (general-key "C-c |" )
	"}"  (general-key "C-c }" )
	"~"  (general-key "C-c ~" )))

;;; Terminal support:
(use-package kkp
  :custom
  (kkp-super-modifier 'meta)
  :config
  (global-kkp-mode +1))

(use-package xclip
  :config
  (xclip-mode 1))

(use-package xt-mouse
  :ensure nil
  :config
  (xterm-mouse-mode t))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)
  :config
  (mouse-wheel-mode t))

(use-package evil-terminal-cursor-changer
  :after evil
  :config
  (evil-terminal-cursor-changer-activate))

;;; Modern interface:
(use-package doom-modeline
  :custom
  (doom-modeline-bar-width 0.1)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-icon nil)
  (doom-modeline-modal nil)
  (doom-modeline-position-line-format nil)
  (doom-modeline-time nil)
  (doom-modeline-workspace-name nil)
  :config
  (doom-modeline-mode 1))

(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs))
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-separator "  ")
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

(use-package modus-themes
  :custom
  (modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  :config
  (load-theme 'modus-vivendi-tinted t)
  :bind
  ("<f12>" . modus-themes-toggle))

(use-package vertico
  :config
  (vertico-mode 1)
  (vertico-mouse-mode 1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package ace-window)

;;; IDE features:
(use-package eglot
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  :config
  (defun +eglot-ensure-in-project ()
	"Run Eglot only if we're in a project."
	(if (project-current) (eglot-ensure))))

(use-package magit
  :bind
  (:map magit-status-mode-map ("SPC" . nil))
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; Integrate with Project.el.
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (keymap-set project-prefix-map "m" #'magit-project-status))

(use-package company
  :after eglot
  :hook (eglot-managed-mode . company-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

;;; Writing:
(use-package org
  :hook
  (org-mode . visual-line-mode)
  :custom
  (initial-major-mode 'org-mode)
  (initial-scratch-message "")
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	 (sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
  (org-adapt-indentation t)
  (org-agenda-files (list org-directory))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 'day)
  (org-hide-leading-stars t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-window-setup 'other-tab)
  (org-archive-location "::* Archive")
  (org-ctrl-k-protect-subtree t)
  (org-directory "~/Sync/Org")
  (org-fontify-quote-and-verse-blocks t)
  (org-highlight-latex-and-related '(native latex script entities))
  (org-image-actual-width '(400))
  (org-startup-folded 'fold)
  (org-tags-column -65)
  :config
  (defun +url-handler-zotero (link)
	"Open a zotero:// link in the Zotero desktop app."
	(start-process "zotero_open" nil "open" (concat "zotero:" link)))
  (org-link-set-parameters "zotero" :follow #'+url-handler-zotero))

(use-package org-download
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "assets")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d%H%M%S")
  :config
  (defun +org-download-file-format (filename)
	"Purely date-based naming of attachments."
	(concat
	 (format-time-string org-download-timestamp)
	 "."
	 (file-name-extension filename)))
  (setq org-download-file-format-function #'+org-download-file-format)
  (setq org-download-annotate-function (lambda (_link) ""))
  (org-download-enable)
  :bind (:map org-mode-map
			  ("M-S-v" . org-download-clipboard)))

(use-package org-babel
  :after org
  :ensure nil
  :no-require
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-results-keyword "results")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(use-package org-super-agenda
  :custom
  (org-super-agenda-groups '((:auto-parent t)))
  :config
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

(use-package ox-pandoc)

(use-package idle-org-agenda
  :after org-agenda
  :custom
  (idle-org-agenda-interval 3600)
  :config
  (idle-org-agenda-mode 1))

(use-package tex
  :ensure auctex
  :custom
  (font-latex-fontify-script nil)
  (TeX-auto-save t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (TeX-view-program-selection '((output-pdf "Skim")))
  :hook
  (TeX-mode . visual-line-mode)
  (TeX-mode . prettify-symbols-mode))

(use-package reftex
  :after tex
  :custom
  (reftex-cite-format 'bibtex)
  (reftex-enable-partial-scans t)
  (reftex-plug-into-AUCTeX t)
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t)
  :hook
  (TeX-mode . turn-on-reftex))

(use-package evil-tex
  :hook
  (LaTeX-mode . evil-tex-mode))

(use-package cdlatex
  :hook
  ((TeX-mode . turn-on-cdlatex)
   (org-mode . turn-on-org-cdlatex)))

;;; Programming:
(use-package python
  :after eglot
  :hook (python-mode . +eglot-ensure-in-project))

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package julia-mode)

(use-package gnuplot)

;;; File management:
(use-package dired
  :ensure nil
  :after (general)
  :custom
  (dired-listing-switches "-hlLgG --group-directories-first --time-style=long-iso")
  :config
  (mmap "^" 'dired-jump))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode 1))

(use-package openwith
  :config
  (setq openwith-associations
		'(("\\.\\(png\\|jpg\\|svg\\)$" "qlmanage -p" (file))
		  ("\\.\\(pdf\\|docx\\|xlsx\\|pptx\\)$" "open" (file))))
  (openwith-mode 1))

;;; Miscellaneous:
(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode))
