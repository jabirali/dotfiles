(require 'package)
(setq package-user-dir "~/.cache/emacs/elpa")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package use-package
  :config
  :custom
  (use-package-always-demand t)
  (use-package-always-ensure t))

(use-package no-littering
  :init
  (setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(use-package quelpa-use-package)

(use-package emacs
  :custom
  (inhibit-startup-message t)
  (use-short-answers t)
  (truncate-lines t)
  (line-spacing 0.15)
  (sentence-end-double-space nil)
  :custom-face
  (default ((t (:family "JetBrains Mono NL" :height 140))))
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (fringe-mode -1)
  (setq ring-bell-function 'ignore))

(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-format '(tab-bar-format-tabs))
  (frame-title-format "")
  :config
  (tab-bar-mode 1))

(use-package outline
  :custom
  (outline-blank-line t)
  :hook
  (prog-mode . outline-minor-mode))

(use-package recentf
  :config
  (recentf-mode 1))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package server
  :config
  (server-mode 1))

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

(use-package doom-themes
  :config
  (load-theme 'doom-snazzy t))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-icon nil)
  (doom-modeline-modal nil)
  (doom-modeline-position-line-format nil)
  (doom-modeline-time nil)
  (doom-modeline-workspace-name nil)
  :config
  (doom-modeline-mode))

(use-package spacious-padding
  :config
  (spacious-padding-mode)
  ;; Make the tab-bar match the mode-line.
  (set-face-attribute 'tab-bar nil
    :background (face-attribute 'mode-line :background)
    :box (face-attribute 'mode-line :box))
  (set-face-attribute 'tab-bar-tab-inactive nil
     :background (face-attribute 'mode-line :background)
     :box (face-attribute 'mode-line :box))
  (set-face-attribute 'tab-bar-tab nil
     :background (face-attribute 'mode-line :background)
     :box (face-attribute 'mode-line :box) :weight 'bold))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-reverse-mode))

(use-package consult
  :after vertico)

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)))

(use-package ace-window
  :bind
  ("M-o" . 'ace-window))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package evil
  :init
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after (evil org)
  :config
  (general-evil-define-key 'normal 'org-mode-map
    "RET" 'org-open-at-point)
  :hook (org-mode . evil-org-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package general
  :after evil
  :config
  (general-override-mode)
  (general-create-definer +leader-map
    :keymaps 'override
    :states '(normal visual)
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
    "o o" '(switch-to-buffer :which-key "buffer")
    "o ." `(,(+open-file "~/.config/emacs/README.org") :which-key "dotfile")
    "o a" '(org-agenda :which-key "agenda")
    "o d" '(dired-jump :which-key "directory")
    "o f" '(find-file :which-key "file")
    "o i" `(,(+open-file "inbox.org" 'org-directory) :which-key "inbox")
    "o j" `(,(+open-file "journal.org" 'org-directory) :which-key "journal")
    "o k" '(org-capture :which-key "capture")
    "o p" '(project-find-file :which-key "project")
    "o r" '(recentf :which-key "recent"))

  ;; Major-mode mappings.
  (+leader-map emacs-lisp-mode-map
              "e e" '(eval-buffer :which-key "buffer")
              "e d" '(eval-defun :which-key "defun")))

(use-package org
  :hook
  (org-mode . visual-line-mode)
  :bind
  (:map org-mode-map
        ("M-p" . org-priority)
        ("M-t" . org-set-tags-command))
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
  (org-directory "~/Sync/Org")
  (org-agenda-files (list org-directory))
  (org-archive-location "::* Archive")
  (org-ctrl-k-protect-subtree t)
  (org-image-actual-width '(400))
  (org-pretty-entities t)
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
  (org-download-timestamp "%Y%m%d%H%M%S")
  (org-download-screenshot-basename ".png")
  :config
  (setq org-download-annotate-function (lambda (_link) ""))
  (org-download-enable)
  :bind (:map org-mode-map
              ("M-v" . org-download-clipboard)))

(use-package persistent-scratch
  :after org
  :custom
  (initial-major-mode 'org-mode)
  (initial-scratch-message "#+TITLE: Scratch buffer\n")
 :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode))
