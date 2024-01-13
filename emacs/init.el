;;; Global keybindings:
;; These keybindings are setup *before* any `use-package' statements.
;; This ensures that they remain available even if I break my config.

(defun +bind (key cmd &optional no-prefix)
  "Bind the chord mod+key globally to a given interactive command.

The modifier key is automatically set to super (⌘) if Emacs is in
GUI mode, or meta (⌥) if Emacs is running in a terminal. If the
optional argument no-prefix is set to 't, don't use a modifier."
  (let* ((mod (if (display-graphic-p) "s-" "M-"))
	 (prefix (if (null no-prefix) mod ""))
	 (chord (concat prefix key)))
    (global-set-key (kbd chord) cmd)))

(defun +open (file &optional dir)
  "Create an interactive command for opening a given file.

If a directory is provided, we will look for the file there."
  `(lambda ()
     (interactive)
     (if (null ,dir)
	 (find-file (expand-file-name ,file))
		    (find-file (expand-file-name ,file ,dir)))))

;; Emacs-inspired keybindings. Most of these have built-in
;; equivalents bound to similar C-x ... or M-... keybindings.
(+bind ":" 'eval-expression)
(+bind "b" 'switch-to-buffer)
(+bind "B" 'ibuffer)
(+bind "d" 'dired)
(+bind "e" 'eval-defun)
(+bind "f" 'find-file)
(+bind "g" 'magit)
(+bind "q" 'kill-buffer-and-window)
(+bind "Q" 'kill-some-buffers)
(+bind "r" 'recentf)
(+bind "u" 'universal-argument)
(+bind "x" 'execute-extended-command)
(+bind "o" 'ace-window)

;; MacOS-inspired keybindings. Most are similar in e.g. browsers.
(+bind "s" 'save-buffer)
(+bind "S" 'save-some-buffers)
(+bind "w" 'tab-bar-close-tab)
(+bind "t" 'tab-bar-new-tab)
(+bind "{" 'tab-bar-switch-to-prev-tab)
(+bind "}" 'tab-bar-switch-to-next-tab)
(+bind "[" 'previous-buffer)
(+bind "]" 'next-buffer)
(+bind "," (+open user-init-file))

(+bind "<left>" 'tab-bar-history-back)
(+bind "<right>" 'tab-bar-history-forward)
(+bind "<down>" 'xref-find-definitions)
(+bind "<up>" 'xref-go-back)

;; Org-mode keybindings.
(+bind "a" 'org-agenda)
(+bind "i" (+open "inbox.org" 'org-directory))
(+bind "j" (+open "journal.org" 'org-directory))
(+bind "k" 'org-capture)

;; Personal keybindings.
(+bind "<return>" 'execute-extended-command)
(+bind ";" 'execute-extended-command)
(+bind "/" 'comment-line)
(+bind "y" 'package-upgrade-all)

;; Choice of modifiers.
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

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

;;; Inbox
;; This section is for newly added and not-yet-integrated elisp.
