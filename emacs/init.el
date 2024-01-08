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
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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

;; Kaolin themes configuration
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t))

(defun toggle-kaolin-theme ()
  "Toggle between Kaolin light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'kaolin-dark)
      (load-theme 'kaolin-light t)
    (load-theme 'kaolin-dark t)))

;; Hotkey to toggle themes
(global-set-key (kbd "C-c t") 'toggle-kaolin-theme)

;;; Spacemacs Lite.
(use-package which-key
  :config
  (which-key-mode))
  
(use-package general
  :config
  ;; Define a space leader key
  (general-create-definer my-leader-def
    :states 'normal
    :keymaps 'override
    :prefix "SPC"
    ;:non-normal-prefix "ESC SPC"
    )

  ;; Example usage of the space leader key
  (my-leader-def
    ;; Top-level headers.
    "f" '(:ignore t :which-key "file")
    "b" '(:ignore t :which-key "buf")
    "o" '(:ignore t :which-key "org")

    ;; Buffers.
    "bb" '(switch-to-buffer :which-key "switch")
    "bk" '(kill-this-buffer :which-key "kill")

    ;; Files.
    "fs" '(save-buffer :which-key "save")
    "ff" '(find-file   :which-key "find")

    ;; Org-mode.
    "oa" '(org-agenda :which-key "agenda")

    ;; Actions.
    "SPC" '(execute-extended-command :which-key "cmd")
    "TAB" '(switch-to-buffer :which-key "buf")
    "`"   '(eshell :which-key "sh")
    ))

;; Rest of your configurations (Evil, Vertico, etc.)

;; End of init.el file


;; End of init.el file
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key general kaolin-themes corfu orderless marginalia consult vertico evil-collection evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
