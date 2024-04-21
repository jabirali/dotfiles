(use-package use-package
  :custom
  (native-comp-async-report-warnings-errors nil)
  (package-native-compile t)
  (use-package-always-demand t)
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(recentf-mode 1)

(savehist-mode 1)

(setopt mac-command-modifier 'meta)
(setopt mac-option-modifier 'option)

(define-key key-translation-map (kbd "§") (kbd "`"))
(define-key key-translation-map (kbd "±") (kbd "~"))

(define-key key-translation-map (kbd "M-<return>") (kbd "M-RET"))
(define-key key-translation-map (kbd "M-<backspace>") (kbd "M-DEL"))

(setq default-input-method 'TeX)
(setq default-transient-input-method 'TeX)

;;(use-package kkp
;;  :ensure t
;;  :custom
;;  (kkp-super-modifier 'meta)
;;  :config
;;  (global-kkp-mode +1))

; (pixel-scroll-precision-mode 1)

(setopt mouse-wheel-follow-mouse t)
(setopt mouse-wheel-progressive-speed nil)
(mouse-wheel-mode 1)
(xterm-mouse-mode 1)

(setopt mouse-highlight nil)

(set-frame-parameter nil 'internal-border-width 0)
(setopt frame-resize-pixelwise t)

(use-package emacs
  :hook
  (prog-mode . hs-minor-mode)
  :custom
  (auto-save-default nil)
  (dired-listing-switches "-hlLgG --group-directories-first --time-style=long-iso")
  (fringes-outside-margins t)
  (inhibit-startup-message t)
  (initial-major-mode 'org-mode)
  (initial-scratch-message "")
  (line-spacing 0.15)
  (make-backup-files nil)
  (message-truncate-lines t)
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  ;; (frame-title-format '((:eval (if-let
  ;;                                  (remote (file-remote-p default-directory 'host))
  ;;                                  (format "%s: " (downcase remote))))
  ;;                       (:eval (if-let
  ;;                                  ((project (project-current)))
  ;;                                  (format "%s/" (downcase (project-name project)))))

  ;;                       "%b"))
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
;; Turn on some useful default modes.
(global-auto-revert-mode 1)
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

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(add-hook 'after-make-frame-functions 'contextual-menubar)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package python
  :custom
  (python-indent-guess-indent-offset t)  
  (python-indent-guess-indent-offset-verbose nil))

(use-package jupyter
  :ensure t
  :config
  (defun jabirali/jupyter-python ()
    (interactive)
    (jupyter-run-repl "python3" "py" t))
  :bind
  (:map python-mode-map
        ("C-c C-c" . jabirali/jupyter-python)))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

(use-package ultra-scroll-mac
  :ensure t
  :if (eq window-system 'mac)
  :init
  (setq scroll-conservatively 101)
  (setq scroll-margin 0) 
  :config
  (ultra-scroll-mac-mode 1))

(use-package evil
  :ensure t
  :custom
  (evil-respect-visual-line-mode t)
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

;;(evil-define-key* 'motion 'global ":" #'execute-extended-command)

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

(defun jabirali/science-definition-lookup ()
  "Look up a scientific definition using a ChatGPT wrapper."
  (interactive)
  (let* ((query (buffer-substring (region-beginning) (region-end)))
         (encoded-query (url-encode-url query))
         (search-url "https://chat.openai.com/g/g-Kihf3Sccx-science-definitions?q="))
    (browse-url (concat search-url encoded-query))))

(bind-key "<f12>" #'jabirali/science-definition-lookup)

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
    (set-face-attribute 'vertical-border nil :foreground bg1 :background bg1)
    (set-face-italic-p 'font-lock-comment-face nil)))

(advice-add 'load-theme :after #'+theme-override)

;; (use-package spacious-padding
;;   :ensure t
;;   :config
;;   (spacious-padding-mode 1))

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

(use-package format-all
  :ensure t
  :hook
  (eglot-managed-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Python" (isort) (ruff) (black)))))

(use-package org
  :custom
  (org-adapt-indentation nil)
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
     (sequence "WAIT(w)" "HOLD(h)" "READ(r)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
  :config
  (setopt org-latex-src-block-backend 'engraved)
  (setopt org-latex-engraved-theme 'ef-melissa-light)
  (setopt org-latex-packages-alist '(("" "microtype" t)))
  (setopt org-latex-hyperref-template "
\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},
 pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true}\n")
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

(use-package ox-pandoc
  :ensure t)

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
  :config
  (setopt markdown-fontify-code-blocks-natively t)
  (setopt markdown-enable-wiki-links t)
  (setopt markdown-enable-math t)
  :hook
  (markdown-mode . cdlatex-mode))

(use-package ispell
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-personal-dictionary (concat user-emacs-directory "ispell"))
  (setq ispell-dictionary "acamedic,bokmaal")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "acamedic,bokmaal"))

(use-package flyspell
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package adaptive-wrap
  :ensure t
  :hook
  (text-mode . visual-line-mode)
  (markdown-mode . adaptive-wrap-prefix-mode)
  (latex-mode . adaptive-wrap-prefix-mode))

(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs))
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-separator "  ")
  (tab-bar-show t)
  (tab-bar-tab-hints t)
  :bind
  ("s-[" . tab-bar-history-back)
  ("s-]" . tab-bar-history-forward)
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

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

(use-package company
  :ensure t
  :after eglot
  :bind (:map prog-mode-map ("<tab>" . company-indent-or-complete-common))
  :hook (eglot-managed-mode . company-mode))

;; (use-package corfu
;;   :ensure t
;;   :hook (prog-mode . corfu-mode)
;;   ;; :custom
;;   ;; (corfu-cycle t)
;;   ;; (corfu-auto t)
;;   ;; (corfu-auto-delay 0.1)
;;   ;; (corfu-quit-at-boundary t)
;;   ;; (corfu-quit-no-match t)
;;   :config
;;   (define-key corfu-map (kbd "C-n") 'corfu-next)
;;   (define-key corfu-map (kbd "C-p") 'corfu-previous))

;; (use-package copilot
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev "main")
;;   :custom
;;   (copilot-idle-delay 1)
;;   ;; :hook
;;   ;; (prog-mode . copilot-mode)
;;   :bind
;;   (:map copilot-mode-map
;;         ("M-RET" . copilot-accept-completion)
;;         ("M-n"   . copilot-next-completion)
;;         ("M-p"   . copilot-previous-completion)))

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
  (doom-modeline-env-enable-python nil)
  (doom-modeline-icon nil)
  (doom-modeline-modal nil)
  (doom-modeline-position-line-format nil)
  (doom-modeline-time nil)
  (doom-modeline-workspace-name nil)
  :config
  (doom-modeline-mode 1))

;; (use-package doom-themes				;
;;   :ensure t
;;   :config
;;   (load-theme 'doom-molokai t))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-melissa-light t))

;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kaolin-light t))

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

(set-face-italic-p 'italic nil)
