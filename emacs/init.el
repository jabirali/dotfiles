(require 'package)
(setq package-user-dir "~/.cache/emacs/elpa")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package use-package
  :demand t
  :custom
  (use-package-always-ensure t))

(use-package no-littering
  :demand t
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
  (fringe-mode -1)
  (recentf-mode 1)
  (savehist-mode 1))

(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-mode 1)))

(if (eq system-type 'darwin)
    (add-to-list 'exec-path "/opt/homebrew/opt/coreutils/libexec/gnubin"))

(defun +open-file (file &optional dir)
  "Create an interactive command for opening a given file.

If a directory is provided, we look for the file there."
  `(lambda ()
     (interactive)
     (if (null ,dir)
         (find-file (expand-file-name ,file))
                    (find-file (expand-file-name ,file ,dir)))))

(defun +close-window ()
  "Close window. If it's the last window, close the whole tab."
  (interactive)
  (if (one-window-p)
      (tab-bar-close-tab)
    (delete-window)
    (balance-windows)))

(defun +kill-buffer-and-close-window ()
  "Kill buffer and then close the currently active window."
  (interactive)
  (kill-buffer)
  (+close-window))

(defun +insert-date ()
  "Insert an ISO date stamp corresponding to today."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %A")))

(defun +find-projects ()
  (interactive)
  (project-remember-projects-under (expand-file-name "~/Sync/") t))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after (evil org general)
  :config
  (mmap :map org-mode-map "RET" 'org-open-at-point)
  :hook
  (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :ensure nil
  :after evil-org
  :config
  (evil-org-agenda-set-keys))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-override-mode 1)
  (general-create-definer +leader-map
    :keymaps 'override
    :states '(motion normal visual)
    :prefix "SPC")

  ;; Global leader mappings.
  (+leader-map
    ;; Important.
    "SPC" '(execute-extended-command :which-key "command")
    "TAB" '(ace-window :which-key "switch")
    "RET" '(scratch-buffer :which-key "scratch")

    ;; Existing keymaps.
    "h" `(,help-map :which-key "+help")

    ;; Common actions.
    "s" '(save-buffer :which-key "save")
    "t" '(tab-bar-new-tab :which-key "tab")
    "w" '(+close-window :which-key "close")
    "d" '(split-window-below :which-key "split")
    "q" '(+kill-buffer-and-close-window :which-key "quit")
    "Q" '(server-edit :which-key "done")
    "g" '(magit :which-key "git")
    "b" '(switch-to-buffer :which-key "buffer")
    "B" '(ibuffer :which-key "buffers")
    "a" '(org-agenda :which-key "agenda")

    ;; Bookmarks.
    "m" '(bookmark-set :which-key "set mark")
    "'" '(bookmark-jump :which-key "goto mark")

    ;; Reserved for major modes.
    "e" '(:ignore t :which-key "eval")

    ;; Tab switching.
    "1" '(tab-bar-select-tab :which-key "1")
    "2" '(tab-bar-select-tab :which-key "2")
    "3" '(tab-bar-select-tab :which-key "3")
    "4" '(tab-bar-select-tab :which-key "4")
    "5" '(tab-bar-select-tab :which-key "5")
    "6" '(tab-bar-select-tab :which-key "6")
    "7" '(tab-bar-select-tab :which-key "7")
    "8" '(tab-bar-select-tab :which-key "8")
    "9" '(tab-bar-select-tab :which-key "9")

    ;; Insert stuff.
    "i" '(:ignore t :which-key "insert")
    "id" '(+insert-date :which-key "date")

    ;; Open stuff.
    "o" '(:ignore t :which-key "open")
    "o ." `(,(+open-file "~/.config/emacs/README.org") :which-key "dotfile")
    "o d" '(dired-jump :which-key "directory")
    "o f" '(find-file :which-key "file")
    "o i" `(,(+open-file "inbox.org" 'org-directory) :which-key "inbox")
    "o j" `(,(+open-file "journal.org" 'org-directory) :which-key "journal")
    "o k" '(org-capture :which-key "capture")
    "o p" '(project-find-file :which-key "project")
    "o r" '(recentf :which-key "recent"))

  ;; Map "C-c C-x" to ", x" for all letters "x". These are
  ;; generally keybindings defined by the current major mode.
  (mmap :prefix "," :keymaps 'override
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
  ;; major-mode keybindings and most minor-mode keybindings.
  (mmap :prefix "," :keymaps 'override
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
    ","  (general-key "C-c ," )
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

(use-package tab-bar
  :custom
  (frame-title-format "")
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs))
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Fix `tab-bar-mode' after any theme has been loaded."
    (let ((bg  (face-attribute 'mode-line :background))
          (box (face-attribute 'mode-line :box)))
      (set-face-attribute 'tab-bar nil :background bg :box box)
      (set-face-attribute 'tab-bar-tab-inactive nil :background bg :box box)
      (set-face-attribute 'tab-bar-tab nil :background bg :box box :weight 'bold))))

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package doom-modeline
  :after doom-themes
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-icon nil)
  (doom-modeline-modal nil)
  (doom-modeline-position-line-format nil)
  (doom-modeline-time nil)
  (doom-modeline-workspace-name nil)
  :config
  (doom-modeline-mode 1))

(use-package vertico
  :config
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-reverse-mode 1))

(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless)))

(use-package consult
  :after vertico)

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

(use-package ace-window
  :bind
  ("M-o" . 'ace-window))

(use-package org
  :hook
  (org-mode . auto-fill-mode)
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
  (org-directory "~/Sync/Org")
  (org-agenda-files (list org-directory))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-agenda-window-setup 'other-tab)
  (org-archive-location "::* Archive")
  (org-ctrl-k-protect-subtree t)
  (org-fontify-quote-and-verse-blocks t)
  (org-image-actual-width '(400))
  (org-pretty-entities t)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-tags-column -65)
  (initial-major-mode 'org-mode)
  (initial-scratch-message "")
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

(use-package org-super-agenda
  :custom
  (org-super-agenda-groups '((:auto-parent t)))
  :config
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

(use-package idle-org-agenda
  :after org-agenda
  :custom
  (idle-org-agenda-interval 3600)
  :config
  (idle-org-agenda-mode 1))

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
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
