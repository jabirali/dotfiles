(use-package use-package
  :custom
  (native-comp-async-report-warnings-errors nil)
  (package-native-compile t)
  (use-package-always-demand t)
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package emacs
  :hook
  (prog-mode . hs-minor-mode)
  (text-mode . visual-line-mode)
  :custom
  (auto-save-default nil)
  (default-input-method 'TeX)
  (dired-listing-switches "-hlLgG --group-directories-first --time-style=long-iso")
  (fringes-outside-margins t)
  (inhibit-startup-message t)
  (initial-major-mode 'org-mode)
  (initial-scratch-message "")
  (line-spacing 0.15)
  (mac-command-modifier 'meta)
  (mac-option-modifier 'option)
  (make-backup-files nil)
  (mouse-highlight nil)
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (frame-title-format '("%b "
                        (:eval (let ((project (project-current))
                                     (remote (file-remote-p default-directory 'host)))
                                 (if remote
                                     (format "(%s)" (downcase remote))
                                   (if project
                                       (format "(%s)" (downcase (project-name project)))))))))
  (tab-width 4)
  (truncate-lines t)
  (use-short-answers t)
  (xterm-set-window-title t)
  :custom-face
  (default ((t (:family "JetBrains Mono NL" :height 140))))
  :bind
  ("C-\\" . activate-transient-input-method)
  ("<f5>" . sort-lines)
  :config
  ;; Don't indicate long or wrapped lines.
  (set-display-table-slot standard-display-table 'truncation ? )
  (set-display-table-slot standard-display-table 'wrap ? )
  ;; Make ISO and ANSI keyboards more similar.
  (define-key key-translation-map (kbd "§") (kbd "`"))
  (define-key key-translation-map (kbd "±") (kbd "~"))
  ;; Make some Meta keybindings more ergonomic.
  (define-key key-translation-map (kbd "M-<return>") (kbd "M-RET"))
  (define-key key-translation-map (kbd "M-<backspace>") (kbd "M-DEL"))
  ;; Turn on some useful default modes.
  (global-auto-revert-mode 1)
  (when (display-graphic-p)
    (pixel-scroll-precision-mode 1))
  ;; Disable the annoying default modes.
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (when (display-graphic-p)
    (fringe-mode 1)
    (tooltip-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(use-package server
  :custom
  (server-use-tcp t)
  (server-port 1337)
  :config
  (server-mode 1))

(use-package exec-path-from-shell
  :vc (:url "https://github.com/purcell/exec-path-from-shell" :rev "main")
  :config
  (exec-path-from-shell-initialize))

;; (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

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

(defun +org-find-file ()
  "Open one of my Org files (or create a new one)."
  (interactive)
  (let ((default-directory org-directory))
    (find-file (completing-read "Org: " (directory-files "." nil "\\.org$")))))

(defun +eglot-project-ensure ()
  "Enable Eglot iff the current buffer belongs to a project."
  (if (project-current) (eglot-ensure)))

(defun +theme-override (&rest _)
  "Override the current theme for a consistent and minimal look."
  (let ((bg0 (face-attribute 'default :background))
        (bg1 (face-attribute 'mode-line :background))
        (bg2 (face-attribute 'mode-line :background))
        (fg0 (face-attribute 'default :foreground))
        (fg1 (face-attribute 'mode-line :foreground))
        (fg2 (face-attribute 'mode-line-inactive :foreground)))
    (set-face-attribute 'tab-bar nil :foreground bg2 :background bg2 :box `(:line-width 6 :color ,bg2))
    (set-face-attribute 'tab-bar-tab nil :foreground fg1 :background bg2 :box `(:line-width 6 :color ,bg2))
    (set-face-attribute 'tab-bar-tab-inactive nil :foreground fg2 :background bg2 :box `(:line-width 6 :color ,bg2))
    (set-face-attribute 'mode-line nil :background bg1 :box `(:line-width 6 :color ,bg1))
    (set-face-attribute 'mode-line-inactive nil :background bg1 :box `(:line-width 6 :color ,bg1))
    (set-face-attribute 'fringe nil :foreground bg0 :background bg0)
    (set-face-attribute 'scroll-bar nil :foreground bg2 :background bg2)
    (set-face-attribute 'vertical-border nil :foreground bg1 :background bg1)))

(advice-add 'load-theme :after #'+theme-override)

(defun +url-handler-zotero (link)
  "Open a zotero:// link in the Zotero desktop app."
  (start-process "zotero_open" nil "open" (concat "zotero:" link)))

(use-package eglot
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  :hook
  (python-mode . +eglot-project-ensure)
  :bind
  ("<f2>" . eglot-rename))

;; (use-package eglot-booster
;;   :vc (:url "https://github.com/jdtsmith/eglot-booster.git" :rev "main")
;;   :after eglot
;;   :config (eglot-booster-mode))

(use-package python
  :config
  (when (executable-find "ipython")
    (setq-local python-shell-interpreter "ipython")
    (setq-local python-shell-prompt-detect-failure-warning nil)))

(use-package org
  :custom
  (org-adapt-indentation t)
  (org-agenda-files (list org-directory))
  (org-agenda-window-setup 'only-window)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-archive-location "::* Archive")
  (org-babel-results-keyword "results")
  (org-confirm-babel-evaluate nil)
  (org-ctrl-k-protect-subtree t)
  (org-directory "~/Sync/Org")
  (org-fontify-quote-and-verse-blocks t)
  (org-highlight-latex-and-related '(native latex script entities))
  (org-image-actual-width '(400))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-return-follows-link t)
  (org-startup-folded 'fold)
  (org-startup-indented t)
  (org-tags-column -65)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  (org-link-set-parameters "zotero" :follow #'+url-handler-zotero))

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
              ("M-V" . org-download-clipboard)))

(use-package org-super-agenda
  :ensure t
  :custom
  (org-super-agenda-groups '((:auto-parent t)))
  :config
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

(use-package idle-org-agenda
  :ensure t
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
  (TeX-view-program-selection '((output-pdf "Skim"))))

(use-package cdlatex
  :ensure t
  :hook
  ((TeX-mode . turn-on-cdlatex)
   (org-mode . turn-on-org-cdlatex)))

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

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . cdlatex-mode))

(use-package flyspell
  :custom
  (ispell-personal-dictionary (concat user-emacs-directory "ispell"))
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package mwheel
  :custom
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-progressive-speed nil)
  :config
  (mouse-wheel-mode 1))

(use-package recentf
  :config
  (recentf-mode 1))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs))
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-separator "  ")
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  :bind
  ("s-[" . tab-bar-history-back)
  ("s-]" . tab-bar-history-forward)
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

(use-package xt-mouse
  :config
  (xterm-mouse-mode 1))

(use-package persistent-scratch
  :after (org evil)
  :ensure t
  :config
  (persistent-scratch-autosave-mode 1))

(use-package ace-window
  :ensure t
  :config
  (defun +other-window-dwim ()
    "Select either the minibuffer or an arbitrary visible window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      (call-interactively #'ace-window)))
  :bind
  ("M-o" . +other-window-dwim))

(use-package adaptive-wrap
  :ensure
  :hook
  (LaTeX-mode . adaptive-wrap-prefix-mode))

;; (use-package company
;;   :ensure t
;;   :after eglot
;;   :hook (eglot-managed-mode . company-mode))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev "main")
  :custom
  (copilot-idle-delay 1)
  :hook
  (prog-mode . copilot-mode)
  :bind
  (:map copilot-mode-map
        ("M-RET" . copilot-accept-completion)
        ("M-n"   . copilot-next-completion)
        ("M-p"   . copilot-previous-completion)))

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

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))

;; (use-package spacemacs-theme
;;   :ensure t
;;   :custom
;;   (spacemacs-theme-org-height nil)
;;   :config
;;   (load-theme 'spacemacs-light t))

(use-package expand-region
  :bind*
  ("C-c RET" . er/expand-region)
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

(use-package gnuplot
  :ensure t)

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode))

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

;; (use-package matlab
;;   :ensure matlab-mode)

;; (use-package openwith
;;   :ensure t
;;   :config
;;   (setq openwith-associations
;;         '(("\\.\\(png\\|jpg\\|svg\\)$" "qlmanage -p" (file))
;;           ("\\.\\(pdf\\|docx\\|xlsx\\|pptx\\)$" "open" (file))))
;;   (openwith-mode 1))

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package outshine
  :ensure t
  :hook
  (prog-mode . outshine-mode))

(use-package ox-pandoc
  :ensure t)

;; (use-package kkp
;;   :ensure t
;;   :custom
;;   (kkp-super-modifier 'meta)
;;   :config
;;   (global-kkp-mode +1))

(use-package prescient
  :ensure t)

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

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

(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :config
  (vertico-prescient-mode 1))

;; (use-package vertico-posframe
;;   :ensure t
;;   :after vertico
;;   :custom
;;   (vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
;;   (vertico-posframe-width 70)
;;   (vertico-posframe-border-width 2)
;;   :config
;;   (vertico-posframe-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

;; (use-package xenops
;;   :ensure t
;;   :custom
;;   (xenops-image-width 350)
;;   :hook
;;   (org-mode . xenops-mode)
;;   (LaTeX-mode . xenops-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

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
