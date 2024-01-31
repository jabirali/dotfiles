(require 'package)
(setq package-user-dir "~/.cache/emacs/elpa")
(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package use-package
  :custom
  (use-package-always-demand t)
  (use-package-always-ensure t))

(use-package no-littering
  :init
  ;; Make cache files follow the XDG specification.
  (setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
  (setq no-littering-var-directory user-emacs-directory)
  (setq no-littering-etc-directory user-emacs-directory)
  :config
  ;; Move backup~ and #auto-save# files out of the way.
  (no-littering-theme-backups)
  ;; Move host-specific customization out of 'init.el'.
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file))

(use-package emacs
  :custom
  (default-input-method 'TeX)
  (inhibit-startup-message t)
  (line-spacing 0.15)
  (mouse-highlight nil)
  (outline-blank-line t)
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (tab-width 4) 
  (truncate-lines t)
  (use-short-answers t)
  :custom-face
  (default ((t (:family "JetBrains Mono NL" :height 140))))
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (fringe-mode 16)
  (recentf-mode 1)
  (savehist-mode 1))

(use-package server
  :config
  (unless (server-running-p)
    (server-mode 1)))

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
    (set-face-attribute 'vertical-border nil :foreground bg1 :background bg1)

    ;; Make the iTerm2 background color match the current theme.
    (send-string-to-terminal
     (format "\033]Ph%s\033\\"
             (substring (face-attribute 'default :background) 1)))))

(if (eq system-type 'darwin)
    (add-to-list 'exec-path "/opt/homebrew/opt/coreutils/libexec/gnubin"))

(defun +insert-date ()
  "Insert an ISO date stamp corresponding to today."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %A")))

(defun +find-projects ()
  (interactive)
  (project-remember-projects-under (expand-file-name "~/Sync/") t))

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
  :ensure 
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
    "w" `(,evil-window-map :which-key "window"))      ; Vim: C-w

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
  (frame-title-format "")
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs))
  (tab-bar-new-tab-choice "*scratch*")
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

(use-package ivy
  :custom
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package counsel
  :config
  (counsel-mode 1)
  :bind
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

(use-package ace-window)

(use-package org
  :hook
  (org-mode . visual-line-mode)
  :custom
  (initial-major-mode 'org-mode)
  (initial-scratch-message "")
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
  (org-agenda-files (list org-directory))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-agenda-window-setup 'other-tab)
  (org-archive-location "::* Archive")
  (org-ctrl-k-protect-subtree t)
  (org-directory "~/Sync/Org")
  (org-fontify-quote-and-verse-blocks t)
  (org-highlight-latex-and-related '(native latex script entities))
  (org-image-actual-width '(400))
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
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
              ("M-v" . org-download-clipboard)))

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

(use-package python
  :after (outline evil general)
  :config
  (defun +outline-python ()
    "Fold Python code like Org-mode headings."
    ;; Only fold definitions and decorators (not e.g. loops and conditions).
    (setq outline-regexp
          (rx (or
               (group (group (* space)) bow (or "class" "def") eow)
               (group (group (* space)) "@"))))
    ;; Org-mode-like keybindings for cycling through outline states.
    (evil-define-key 'motion 'local (kbd "<tab>")
      (general-predicate-dispatch nil (derived-mode-p  'prog-mode) 'outline-cycle))
    (evil-define-key 'motion 'local (kbd "<backtab>")
      (general-predicate-dispatch nil (derived-mode-p 'prog-mode) 'outline-cycle-buffer))
    ;; Enable the mode.
    (outline-minor-mode 1))
  :hook
  (python-mode . +outline-python))

(use-package dired
  :ensure nil
  :after (evil general)
  :custom
  (dired-listing-switches "-hlLgG --group-directories-first --time-style=long-iso")
  :config
  (mmap "^" 'dired-jump))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode 1))

(use-package gnuplot)

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode))

(use-package magit
  :bind
  (:map magit-status-mode-map ("SPC" . nil))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package openwith
  :config
  (setq openwith-associations
         '(("\\.\\(png\\|jpg\\|svg\\)$" "qlmanage -p" (file))
           ("\\.\\(pdf\\|docx\\|xlsx\\|pptx\\)$" "open" (file))))
  (openwith-mode 1))
