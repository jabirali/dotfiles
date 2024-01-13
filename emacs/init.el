;;; Custom functions:
;; These are various helper functions used throughout my config.
;; I define them here at the top so that they're available below.

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

;;; Package management:
;; Setup 'use-package' to automatically download MELPA packages.
(use-package use-package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  :custom
  (use-package-always-demand t)
  (use-package-always-ensure t))

;; Prevent packages from spamming my config directory with junk.
(use-package no-littering
  :init
  (setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

;;; Built-in packages:
(use-package emacs
  :custom
  (inhibit-startup-message t)
  (use-short-answers t)
  (truncate-lines t)
  (line-spacing 0.15)
  :custom-face
  (default ((t (:family "JetBrains Mono NL" :height 140))))
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (pixel-scroll-precision-mode 1)
  (setq ring-bell-function 'ignore))

(use-package tab-bar
  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  (display-time-mode 1))

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

;;; User interface:
(use-package doom-themes
  :config
  (load-theme 'doom-oksolar-light t))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-modal nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-workspace-name nil)
  :config
  (doom-modeline-mode))

(use-package spacious-padding
  :config
  (spacious-padding-mode))

;;; User experience:
(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :after vertico)

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)))

(use-package ace-window)

(use-package magit)

;;; Evil mode:
(use-package evil
  :init
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-respect-visual-line-mode t)
  :config
  (defun +nmap (key cmd)
    "Global normal-state mapping of key to command."
    (evil-define-key 'normal 'global (kbd key) cmd))
  (defun +imap (key cmd)
    "Global insert-state mapping of key to command."
    (evil-define-key 'insert 'global (kbd key) cmd))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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
    "SPC" 'execute-extended-command
    "TAB" 'ace-window

    ;; Existing keymaps.
    "h" help-map

    ;; Common actions.
    "s" 'save-buffer
    "t" 'tab-bar-new-tab
    "w" '+close-window
    "q" '+kill-buffer-and-close-window
    "g" 'magit

    ;; Tab switching.
    "1" 'tab-bar-select-tab
    "2" 'tab-bar-select-tab
    "3" 'tab-bar-select-tab
    "4" 'tab-bar-select-tab
    "5" 'tab-bar-select-tab
    "6" 'tab-bar-select-tab
    "7" 'tab-bar-select-tab
    "8" 'tab-bar-select-tab
    "9" 'tab-bar-select-tab

    ;; Open stuff.
    "o ." (+open-file user-init-file)
    "o a" 'org-agenda
    "o b" 'switch-to-buffer
    "o d" 'dired
    "o f" 'find-file
    "o i" (+open-file "inbox.org" 'org-directory)
    "o j" (+open-file "journal.org" 'org-directory)
    "o k" 'org-capture
    "o p" 'project-find-file
    "o r" 'recentf)

  ;; Major-mode mappings.
  (+leader-map emacs-lisp-mode-map
	      "e e" 'eval-buffer
	      "e d" 'eval-defun))

;;; Org mode
(use-package org
  :hook
  (org-mode . visual-line-mode)
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
  (org-directory "~/Sync/Org")
  (org-agenda-files (list org-directory))
  (org-ctrl-k-protect-subtree t)
  (org-auto-align-tags nil)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(400))
  (org-reverse-note-order t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-pretty-entities t))

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

(use-package org-modern
  :after org
  :custom
  (org-modern-list nil)
  (org-modern-star nil)
  :config
  (global-org-modern-mode))

;;; Inbox
;; This section is for newly added and not-yet-integrated elisp.
