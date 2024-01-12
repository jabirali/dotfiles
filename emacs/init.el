;; Use 'use-package' for package management
(use-package use-package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  :custom
  (use-package-always-ensure t))

;; Prevent config spam.
(use-package no-littering
  :init
  (setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

;; Load bundled modes.
(use-package emacs
  :config
  (set-frame-font "JetBrains Mono NL 14" nil t)
  (setq-default line-spacing 0.15)
  (setq inhibit-startup-message t)
  ;; UI modes.
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (pixel-scroll-precision-mode 1)
  ;; UX modes.
  (recentf-mode 1))

(use-package tab-bar
  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  :bind
  ;; Browser-like keybindings for tabs.
  ("M-{" . tab-bar-switch-to-prev-tab)
  ("M-}" . tab-bar-switch-to-next-tab)
  ("M-w" . tab-bar-close-tab)
  ("M-t" . tab-bar-new-tab)
  ;; Browser-like keybindings for history.
  ("M-[" . tab-bar-history-back)
  ("M-]" . tab-bar-history-forward))

;; Theming
(use-package doom-themes
  :config
  (load-theme 'doom-oksolar-light t))

(use-package doom-modeline)

(use-package spacious-padding
  :config
  (spacious-padding-mode))

;; Evil mode configuration
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Org mode
(use-package org
  :bind
  (:map org-mode-map
	("M-{" . nil)
	("M-}" . nil))
  :custom
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
	      ("M-p" . org-download-clipboard)))

;; Vertico, Consult, Marginalia, Orderless, and Corfu setup
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

;; (use-package corfu
;;   :init
;;   (global-corfu-mode))

;;; Spacemacs Lite.
(use-package which-key
  :config
  (which-key-mode))
  
(use-package general
  :config
  ;; Define a space leader key
  (general-auto-unbind-keys)
  (general-create-definer my-leader-def
    :states 'normal
    :keymaps 'override
    :prefix "SPC"
    ;:non-normal-prefix "ESC SPC"
    )

  ;; Example usage of the space leader key
  (my-leader-def
    "SPC" '(execute-extended-command :which-key "cmd")
    "TAB" '(switch-to-buffer :which-key "buffer")

    ";" '(eval-expression :which-key "lisp")

    "c" '(:ignore t :which-key "change")
    "ct" '(consult-theme :which-key "theme")

    "o" '(:ignore t :which-key "org")
    "oo" '(org-agenda :which-key "agenda")
    "oi" '(lambda () (interactive) (find-file-other-tab "~/Sync/Org/Inbox.org") :which-key "inbox")
    "oj" '(lambda () (interactive) (find-file-other-tab "~/Sync/Org/Journal.org") :which-key "journal")
    "ok" '(org-capture :which-key "capture")

    "i" '(consult-imenu :which-key "imenu")

    "h" '(:keymap help-map :which-key "help")

    "w" '(:keymap evil-window-map :which-key "win")

    "f" '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find")
    "fs" '(save-buffer :which-key "save")
    "fk" '(kill-this-buffer :which-key "kill")
    "fr" '(consult-recent-file :which-key "recent")

    "g" '(:ignore t :which-key "git")
    "gg" '(magit :which-key "status")

    "pi" 'package-install
    "pp" 'package-upgrade-all
    "pr" 'package-delete
    "pc" 'package-autoremove

    "u" '(universal-argument :which-key "unarg")

    "e"   '(eval-defun :which-key "eval")
    ))

;; Inbox
(use-package ace-window
  :bind
  ("M-o" . 'ace-window))

(use-package magit)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))

(setq org-agenda-files '("~/Sync/Org"))
