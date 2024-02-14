;; ~/.config/emacs/init.el
;;; Core:
(use-package use-package
  :custom
  (native-comp-async-report-warnings-errors nil)
  (package-native-compile t)
  (use-package-always-demand t)
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package emacs
  :custom
  (auto-save-default nil)
  (default-input-method 'TeX)
  (frame-title-format "%b")
  (inhibit-startup-message t)
  (initial-major-mode 'org-mode)
  (initial-scratch-message "")
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (tab-width 4)
  (truncate-lines t)
  (use-short-answers t)
  (xterm-set-window-title t)
  :bind
  ("C-\\" . activate-transient-input-method)
  ("<f5>" . sort-lines)
  ("H-:" . eval-expression)
  ("H-p" . execute-extended-command)
  :config
  ;; Don't indicate long or wrapped lines.
  (set-display-table-slot standard-display-table 'truncation ? )
  (set-display-table-slot standard-display-table 'wrap ? )
  ;; Make ISO and ANSI keyboards more similar.
  (define-key key-translation-map (kbd "§") (kbd "`"))
  (define-key key-translation-map (kbd "±") (kbd "~"))
  ;; Make some Meta keybindings more ergonomic.
  (define-key key-translation-map (kbd "H-<return>") (kbd "M-RET"))
  (define-key key-translation-map (kbd "H-<backspace>") (kbd "M-DEL"))
  ;; Disable the most annoying default modes.
  (blink-cursor-mode -1)
  (menu-bar-mode -1))

;;; Functions:
(defun +eglot-project-ensure ()
  "Enable Eglot iff the current buffer belongs to a project."
  (if (project-current) (eglot-ensure)))

(defun +theme-kitty (&rest _)
  "Synchronize the Kitty terminal theme and the Emacs theme."
  (shell-command
   (let* ((emacs-theme-name (symbol-name (car custom-enabled-themes)))
          (kitty-theme-name (capitalize (replace-regexp-in-string "-" " " emacs-theme-name))))
     (format "kitty +kitten themes %s" kitty-theme-name))))

(defun +theme-override (&rest _)
  "Override the current theme for a consistent and minimal look."
  (let ((bg0 (face-attribute 'default :background))
        (bg1 (face-attribute 'scroll-bar :background))
        (fg1 (face-attribute 'success :foreground))
        (fg2 (face-attribute 'mode-line :foreground)))
    (set-face-attribute 'tab-bar nil :foreground bg1 :background bg1 :box `(:line-width 6 :color ,bg1))
    (set-face-attribute 'tab-bar-tab nil :foreground fg1 :background bg1 :box `(:line-width 6 :color ,bg1))
    (set-face-attribute 'tab-bar-tab-inactive nil :foreground fg2 :background bg1 :box `(:line-width 6 :color ,bg1))
    (set-face-attribute 'mode-line nil :background bg1 :box `(:line-width 6 :color ,bg1))
    (set-face-attribute 'mode-line-inactive nil :background bg1 :box `(:line-width 6 :color ,bg1))
    (set-face-attribute 'fringe nil :foreground bg0 :background bg0)
    (set-face-attribute 'vertical-border nil :foreground bg1 :background bg1)))

(defun +url-handler-zotero (link)
  "Open a zotero:// link in the Zotero desktop app."
  (start-process "zotero_open" nil "open" (concat "zotero:" link)))

;;; Internal packages:
(use-package dired
  :custom
  (dired-listing-switches "-hlLgG --group-directories-first --time-style=long-iso"))

(use-package eglot
  :hook
  (python-mode . +eglot-project-ensure)
  :bind
  ("<f2>" . eglot-rename))

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil))

(use-package modus-themes
  :custom
  (modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  :config
  (advice-add 'load-theme :after #'+theme-override)
  (load-theme 'modus-vivendi-tinted t)
  :bind
  ("<f12>" . modus-themes-toggle))

(use-package mwheel
  :custom
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-progressive-speed nil)
  :config
  (mouse-wheel-mode 1))

(use-package org
  :hook
  (org-mode . auto-fill-mode)
  :custom
  (org-adapt-indentation t)
  (org-agenda-files (list org-directory))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-agenda-window-setup 'other-tab)
  (org-archive-location "::* Archive")
  (org-babel-results-keyword "results")
  (org-confirm-babel-evaluate nil)
  (org-ctrl-k-protect-subtree t)
  (org-directory "~/Sync/Org")
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-leading-stars t)
  (org-highlight-latex-and-related '(native latex script entities))
  (org-image-actual-width '(400))
  (org-return-follows-link t)
  (org-startup-folded 'fold)
  (org-tags-column -65)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  (org-link-set-parameters "zotero" :follow #'+url-handler-zotero))

(use-package recentf
  :config
  (recentf-mode 1))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package server
  :config
  (server-mode 1))

(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs))
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-separator "  ")
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  :bind
  ("H-[" . tab-bar-history-back)
  ("H-]" . tab-bar-history-forward)
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

(use-package xt-mouse
  :config
  (xterm-mouse-mode 1))

;;; External packages:
(use-package ace-window
  :ensure t
  :bind
  ("H-o" . ace-window))

(use-package cdlatex
  :ensure t
  :hook
  ((TeX-mode . turn-on-cdlatex)
   (org-mode . turn-on-org-cdlatex)))

(use-package company
  :ensure t
  :after eglot
  :hook (eglot-managed-mode . company-mode))

(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode 1))

(use-package doom-modeline
  :ensure t
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

(use-package evil
  :ensure t
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :after evil-org
  :config (evil-org-agenda-set-keys))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-terminal-cursor-changer
  :ensure t
  :after evil
  :config
  (evil-terminal-cursor-changer-activate))

(use-package evil-tex
  :ensure t
  :hook
  (LaTeX-mode . evil-tex-mode))

(use-package expand-region
  :ensure t)

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package format-all
  :ensure t
  :hook
  (eglot-managed-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Python" (isort) (ruff) (black)))))

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup t)
  (general-override-mode 1)
  (general-create-definer gmap
    :keymaps 'override
    :states '(motion normal visual)
    :prefix "SPC")
  (general-create-definer lmap
    :keymaps 'override
    :states '(motion normal visual)
    :prefix ","))

(use-package gnuplot
  :ensure t)

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode))

(use-package idle-org-agenda
  :ensure t
  :after org-agenda
  :custom
  (idle-org-agenda-interval 3600)
  :config
  (idle-org-agenda-mode 1))

(use-package iedit
  :ensure t)

(use-package julia-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind
  (:map magit-status-mode-map ("SPC" . nil))
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (keymap-set project-prefix-map "m" #'magit-project-status))

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . cdlatex-mode))

(use-package openwith
  :ensure t
  :config
  (setq openwith-associations
        '(("\\.\\(png\\|jpg\\|svg\\)$" "qlmanage -p" (file))
          ("\\.\\(pdf\\|docx\\|xlsx\\|pptx\\)$" "open" (file))))
  (openwith-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org-download
  :ensure t
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
              ("H-S-v" . org-download-clipboard)))

(use-package org-super-agenda
  :ensure t
  :custom
  (org-super-agenda-groups '((:auto-parent t)))
  :config
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

(use-package ox-pandoc
  :ensure t)

(use-package kkp
  :ensure t
  :custom
  (kkp-super-modifier 'hyper)
  :config
  (global-kkp-mode +1)
  (advice-add 'load-theme :after #'+theme-kitty))

(use-package reftex
  :ensure t
  :after tex
  :custom
  (reftex-cite-format 'bibtex)
  (reftex-enable-partial-scans t)
  (reftex-plug-into-AUCTeX t)
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t)
  :hook
  (TeX-mode . turn-on-reftex))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

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

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1)
  (vertico-mouse-mode 1))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;;; Keybindings:
(mmap                                           ; Motion map
  "^" 'dired-jump)

(vmap                                           ; Visual map
  "ii" 'er/expand-region)

(gmap                                           ; Space menu
  "SPC" '(execute-extended-command :which-key "cmd")
  "1" '(tab-bar-select-tab :which-key "1")
  "2" '(tab-bar-select-tab :which-key "2")
  "3" '(tab-bar-select-tab :which-key "3")
  "4" '(tab-bar-select-tab :which-key "4")
  "5" '(tab-bar-select-tab :which-key "5")
  "6" '(tab-bar-select-tab :which-key "6")
  "7" '(tab-bar-select-tab :which-key "7")
  "8" '(tab-bar-select-tab :which-key "8")
  "9" '(tab-bar-select-tab :which-key "9")
  "a" '(org-agenda :which-key "agenda")
  "b" '(switch-to-buffer :which-key "buffer")
  "d" '(dired-jump :which-key "dired")
  "f" '(find-file :which-key "file")
  "g" '(magit :which-key "git")
  "h" `(,help-map :which-key "help")
  "i" '(imenu :which-key "imenu")
  "j" '(bookmark-jump :which-key "jump")
  "k" '(kill-this-buffer :which-key "kill")
  "n" `(,narrow-map :which-key "narrow")
  "o" '(ace-window :which-key "other")
  "p" `(,project-prefix-map :which-key "project")
  "q" '(delete-window :which-key "quit window")
  "Q" '(tab-close :which-key "quit tab")
  "r" '(recentf :which-key "recent")
  "s" '(save-buffer :which-key "save")
  "t" '(tab-bar-new-tab :which-key "tab")
  "w" `(,evil-window-map :which-key "window")
  "y" '(clone-indirect-buffer-other-window :which-key "indirect"))

(lmap                                           ; Major modes
  "," (general-key "C-c C-c")
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

(lmap                                           ; Minor modes
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
  "~"  (general-key "C-c ~" ))

;;; Metadata:
;; Local Variables:
;;     comment-column: 48
;;     fill-column: 80
;;     indent-tabs-mode: nil
;;     outline-minor-mode: t
;;     eval: (outline-hide-body)
;; End:
