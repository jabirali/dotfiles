;; This config is inspired by the blog post "Emacs from scratch", which
;; is available at https://blog.jft.rocks/emacs/emacs-from-scratch.html.
;; TODO:
;;  * Magit
;;  * Auctex (?)
;;  * Evil Collection (+Evil Goggles?)
;;  * Python mode that is good (goto def etc.)
;;  * Org-mode intro
;;
;; TODO:
;; https://realpython.com/emacs-the-best-python-editor/



;;------------------------------
;; Package management
;;------------------------------

;; Enable the package manager
(require 'package)
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Enable the ELPA repositories
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Install the `use-package` wrapper
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable the `use-package` wrapper
(require 'use-package)

;; Autoinstall missing packages
(setq use-package-always-ensure t)



;;------------------------------
;; User interface
;;------------------------------

;; Minimalist interface
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Turn on line numbers globally
(global-linum-mode t)

;; Font and frame size
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "mononoki-13"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

;; Fancy minimal modeline
(use-package mood-line
  :hook
  (after-init . mood-line-mode))




;;------------------------------
;; User experience
;;------------------------------

;; Answer questions with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable unneeded screens
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq initial-scratch-message "")

;; Don't litter with backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Helm (vs. ivy etc)
(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-list 150
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  :config
  (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
  (helm-mode 1))

;; Custom keybinding
(use-package general
  :config
  (general-override-mode)
  (general-define-key
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
     "SPC" '(other-window 1 :which-key "window")
     "RET" '(eshell :which-key "shell")
     "TAB" '(helm-mini :which-key "buffer")
     "p"   '(helm-projectile-switch-project :which-key "project")
     "f"   '(helm-projectile-find-file :which-key "file")
     "/"   '(helm-projectile-grep :which-key "search")
     "x"   '(helm-M-x :which-key "exec")
     "v"   '(split-window-right :which-key "split right")
     "h"   '(split-window-below :which-key "split bottom")
     "c"   '(delete-window :which-key "delete window")))

;; Projectile
(use-package projectile
  :init
  ;(setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

;; All The Icons
;(use-package all-the-icons :ensure t)

;; NeoTree
(use-package neotree
  :init
  (setq neo-theme 'arrow)) ;; (if (display-graphic-p) 'icons 'arrow)))





;;-------------------------------
;; Evil mode
;;-------------------------------

;; Vim editing keybindings
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Evil extension to more modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; More visual cues when you cut and paste
(use-package evil-goggles
  :config
  (evil-goggles-mode))
  ;(evil-goggles-use-diff-faces))

;; Make it easier to escape from weird places
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))



;;-------------------------------
;; Org mode
;;-------------------------------

;; Hide emphasis
(setq org-hide-emphasis-markers t)



;;-------------------------------
;; Python mode
;;-------------------------------

;; Turn on IDE features
(use-package elpy
  :config
  (elpy-enable)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))



;;-------------------------------
;; Latex mode
;;-------------------------------

(use-package latex
    :mode
    ("\\.tex\\'" . latex-mode)
    :config
    (setq-default TeX-master nil
                  TeX-PDF-mode t
                  TeX-engine 'luatex)
    (setq TeX-auto-save t
          TeX-save-query nil
          TeX-parse-self t
          TeX-show-compilation nil
          LaTeX-babel-hyphen nil))



;;-------------------------------
;; Optimization (final)
;;-------------------------------

;; Turn garbage collection on again
;(add-hook! 'emacs-startup-hook
;  (setq gc-cons-threshold 16777216
;        gc-cons-percentage 0.1))





;; Eshell
(defalias 'e 'find-file-other-window)
;(defalias 'o 'xdg-open)

;; Helm autocomp
(setq helm-use-frame-when-more-than-two-windows nil)
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map (kbd "TAB") 'helm-esh-pcomplete)))

;; Helm projectile
(use-package helm-projectile
  :config
  (helm-projectile-on))



;; Get rid of window border
(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border (face-background 'vertical-border))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup layout
(split-window-horizontally)
(find-file "~/TODO.org")
(other-window 1)
(eshell)
(other-window 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-projectile elpy evil-goggles evil-collection evil neotree projectile general which-key helm mood-line doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
