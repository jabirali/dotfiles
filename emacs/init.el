;; Use 'use-package' for package management
(use-package use-package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  :custom
  (use-package-always-ensure t))

;; General Emacs settings
(setq inhibit-startup-message t)

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

;; Eglot configuration with Pyright for Python files
;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
;;   :hook
;;   (python-mode . eglot-ensure))

;; Theme configuration
;; (use-package catppuccin-theme
;;   :config
;;   (load-theme 'catppuccin t))
  
;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-dark t))

;; (defun toggle-kaolin-theme ()
;;   "Toggle between Kaolin light and dark themes."
;;   (interactive)
;;   (if (eq (car custom-enabled-themes) 'kaolin-dark)
;;       (load-theme 'kaolin-light t)
;;     (load-theme 'kaolin-dark t)))

;; ;; Hotkey to toggle themes
;; (global-set-key (kbd "C-c t") 'toggle-kaolin-theme)

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
    "`" 'ace-window

    "l" '(eval-expression :which-key "lisp")

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
    ;; Actions

    "[" 'evil-tab-prev
    "]" 'evil-tab-next
    ; "`"   '(eshell :which-key "sh")
    "e"   '(eval-defun :which-key "eval")
    ))

(evil-define-key 'normal 'global "]t" 'tab-bar-switch-to-next-tab)
(evil-define-key 'normal 'global "[t" 'tab-bar-switch-to-prev-tab)


;; Misc
(set-frame-font "JetBrains Mono NL 14" nil t)
(tool-bar-mode +1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(recentf-mode 1)
(winner-mode 1)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode 1)
; (tab-bar-mode)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(setq org-agenda-files '("~/Sync/Org"))

;; Inbox
(use-package ace-window)
(use-package magit)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "WAIT(w)" "HOLD(h)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
(setq-default line-spacing 0.15)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit ace-window general which-key catppuccin-theme orderless marginalia consult vertico org-download org-modern evil-collection evil spacious-padding doom-modeline doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#FBF7EF")))
 '(header-line ((t :box (:line-width 4 :color "#eeeae3" :style nil))))
 '(header-line-highlight ((t :box (:color "#657377"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#FBF7EF")))
 '(mode-line ((t :box (:line-width 6 :color "#eeeae3" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#eeeae3" :style nil))))
 '(mode-line-highlight ((t :box (:color "#657377"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#f4f0e9" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#FBF7EF" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#F1E9D2" :style nil))))
 '(window-divider ((t :background "#FBF7EF" :foreground "#FBF7EF")))
 '(window-divider-first-pixel ((t :background "#FBF7EF" :foreground "#FBF7EF")))
 '(window-divider-last-pixel ((t :background "#FBF7EF" :foreground "#FBF7EF"))))
