;; Initialize package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; General Emacs settings
(setq inhibit-startup-message t)

;; Use 'use-package' for package management
(require 'use-package)
(setq use-package-always-ensure t)

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
(use-package org)

(use-package org-modern
  :after org
  :custom
  (org-modern-list nil)
  (org-modern-star nil)
  :config
  (global-org-modern-mode))


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
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  :hook
  (python-mode . eglot-ensure))

;; Theme configuration
(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t))
  
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
    "oi" '(lambda () (interactive) (find-file-other-tab "~/Desktop/Inbox.org") :which-key "inbox")
    "oj" '(lambda () (interactive) (find-file-other-tab "~/Desktop/Journal.org") :which-key "journal")
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

    "u" '(universal-argument :which-key "unarg")
    ;; Actions

    "[" 'evil-tab-prev
    "]" 'evil-tab-next
    ; "`"   '(eshell :which-key "sh")
    "e"   '(eval-defun :which-key "eval")
    ))

;; Misc
(set-frame-font "JetBrains Mono NL 14" nil t)
(tool-bar-mode +1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(recentf-mode 1)
(winner-mode 1)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode 1)
(tab-bar-mode)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

;; Inbox
(use-package ace-window)
(use-package magit)

;; End of init.el file


;; End of init.el file
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" default))
 '(package-selected-packages
   '(magit ace-window org-modern catppuccin-theme catppuccin which-key general kaolin-themes corfu orderless marginalia consult vertico evil-collection evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
