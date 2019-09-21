;; this config is inspired by the blog post "Emacs from scratch", which
;; is available at https://blog.jft.rocks/emacs/emacs-from-scratch.html.
;;
;; Outline:
;;  [ ] Setup auctex/pdftools for LaTeX usage
;;  [ ] Setup autocomment
;;  [ ] Clean up all sections
;; TODO:
;;  * Magit
;;  * Auctex (?)
;;  * Python mode that is good (goto def etc.) (dumbjump)
;;  * Org-mode intro
;;
;; TODO:
;; https://realpython.com/emacs-the-best-python-editor/
;;
;; Ivy:
;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/



;;------------------------------
;; Package management
;;------------------------------

;; Load and configure `package`, which is used to download
;; and install packages from the online ELPA repositories.
(require 'package)
(setq package-enable-at-startup nil
      package--init-file-ensured t
      package-archives
      '(("org"   . "http://orgmode.org/elpa/")
	("gnu"   . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Install and load `use-package`, which is used throughout
;; this config to load and configure other packages. Setting
;; the option `always-ensure` makes it autoinstall missing
;; packages, which is useful when moving to new computers.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)



;;------------------------------
;; User interface
;;------------------------------

;; Minimalist interface
(scroll-bar-mode  -1)
(tool-bar-mode    -1)
(tooltip-mode     -1)
(menu-bar-mode    -1)
(blink-cursor-mode 0)

;; Turn on line numbers globally
;(global-linum-mode t)

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

;; Interactive help on keyboard shortcuts
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Use Ivy/Councel/Swiper for autocompletion
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

; Use C-o to more efficiently select hit to open via Hydra
(use-package ivy-hydra)



;; Projectile
(use-package projectile
  :init
  ;(setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode 1))

;; All The Icons
;(use-package all-the-icons)
;(use-package all-the-icons-ivy
;  :config
;  (all-the-icons-ivy-setup))



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

;; Custom menu with keybindings
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
     ;"TAB" '(helm-mini :which-key "buffer")
     ;"p"   '(helm-projectile-switch-project :which-key "project")
     ;"f"   '(helm-projectile-find-file :which-key "file")
     ;"/"   '(helm-projectile-grep :which-key "search")
     ;"x"   '(helm-M-x :which-key "exec")
     "v"   '(split-window-right :which-key "split right")
     "h"   '(split-window-below :which-key "split bottom")
     "c"   '(delete-window :which-key "delete window")))

;; Make it easier to escape from weird places
;(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Swiper
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "/") 'swiper))
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "?") 'swiper-backward))


(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;(global-set-key (kbd "<f1> l") 'counsel-find-library)
;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c b") 'counsel-buffer-or-recentf)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)



;;-------------------------------
;; Org mode
;;-------------------------------

;; Use indentation
(add-hook 'org-mode-hook 'org-indent-mode)

;; Hide emphasis
(setq org-hide-emphasis-markers t)

;; Heading fancy bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Org file dir
(setq org-agenda-files '("~/Documents/Org"))

;; List bullets
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "━"))))))



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
    :ensure auctex
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
;(defalias 'o 'xdg-open)
(defalias 'e 'find-file)
;(use-package bash-completion
;  ; :if (ohai/is-exec "bash")
;  :commands bash-completion-dynamic-complete
;  :init
;  (add-hook 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete)
;  (setq fish-completion-fallback-on-bash-p t))
(use-package fish-completion
  ; :if (ohai/is-exec "fish")
  :config
  (global-fish-completion-mode))

;; Eshell
; Alternative: call complete-symbol, works in any mode with ivy.
(add-hook 'eshell-mode-hook
  (lambda () 
    (define-key eshell-mode-map (kbd "<tab>")
      (lambda () (interactive) (pcomplete-std-complete)))))
(defalias 'e 'find-file-other-window)
;(defalias 'o 'xdg-open)

; Ivy projectile (Counsel)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package counsel
  :bind*
  (("M-x" . counsel-M-x)
   ("C-c C-m" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;;;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)))



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



