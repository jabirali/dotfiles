;;; Custom keybindings:
;; These keybindings are setup *before* any `use-package' statements.
;; This ensures that they remain available even if I break my config.

(defun ⌘ (key cmd)
  "Bind the chord mod+key to the given interactive command.

The modifier key is automatically set to super (⌘) if Emacs is in
GUI mode, or meta (⌥) if Emacs is running in a terminal."
  (let* ((mod (if (display-graphic-p) "s-" "M-"))
	 (chord (concat mod key)))
    (global-set-key (kbd chord) cmd)))

;; Emacs-like keybindings.
(⌘ ":" 'eval-expression)
(⌘ ";" 'comment-line)
(⌘ "/" 'dabbrev-expand)
(⌘ "." 'xref-find-definitions)
(⌘ "b" 'switch-to-buffer)
(⌘ "B" 'ibuffer)
(⌘ "f" 'find-file)
(⌘ "d" 'dired)
(⌘ "e" 'eval-defun)
(⌘ "g" 'magit)
(⌘ "o" 'ace-window)
(⌘ "q" 'kill-buffer-and-window)
(⌘ "Q" 'kill-some-buffers)
(⌘ "r" 'consult-recent-file)
(⌘ "u" 'universal-argument)
(⌘ "x" 'execute-extended-command)

;; MacOS-like keybindings.
(⌘ "k" 'execute-extended-command)
(⌘ "p" 'project-find-file)
(⌘ "s" 'save-buffer)
(⌘ "S" 'save-some-buffers)
(⌘ "t" 'tab-bar-new-tab)
(⌘ "w" 'tab-bar-close-tab)
(⌘ "{" 'tab-bar-switch-to-prev-tab)
(⌘ "}" 'tab-bar-switch-to-next-tab)
(⌘ "[" 'tab-bar-history-back)
(⌘ "]" 'tab-bar-history-forward)
(⌘ "," 'baba/config)

;; Org-mode keybindings.
(⌘ "<return>" 'org-capture)
(⌘ "a" 'org-agenda)
(⌘ "i" 'baba/org-inbox)
(⌘ "j" 'baba/org-journal)

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;;; Package management:
;; Setup 'use-package' to automatically download MELPA packages.
(use-package use-package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  :custom
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
  (truncate-lines t)
  (line-spacing 0.15)
  :custom-face
  (default ((t (:family "JetBrains Mono NL" :height 140))))
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (pixel-scroll-precision-mode 1)
  (setq ring-bell-function 'ignore)

  (defun baba/config ()
      (interactive)
      (find-file (expand-file-name "~/.config/emacs/init.el"))))

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

;;; Aesthetics
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

;;; Keybindings
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

(use-package which-key
  :config
  (which-key-mode))

;;; Org mode
(use-package org
  :config
  (defun baba/org-inbox ()
    "Open my task management system."
    (interactive)
    (find-file (expand-file-name "inbox.org" org-directory)))
  (defun baba/org-journal ()
    "Open my daily research journal."
    (interactive)
    (find-file (expand-file-name "journal.org" org-directory)))
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

(use-package org-modern
  :after org
  :custom
  (org-modern-list nil)
  (org-modern-star nil)
  :config
  (global-org-modern-mode))

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
	      ("s-v" . org-download-clipboard)))

;;; Modern user experience
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

;;; Integrations
(use-package magit)

;;; Inbox
;; This section is for newly added and not-yet-integrated elisp.
