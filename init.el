;; this config is inspired by the blog post "Emacs from scratch", which
;; is available at https://blog.jft.rocks/emacs/emacs-from-scratch.html.
;;
;; Outline:
;;  [ ] Setup auctex/pdftools for LaTeX usage
;;  [ ] Setup autocomment
;; TODO:
;;  * Auctex (?)
;;  * Python mode that is good (goto def etc.) (dumbjump)
;;  * Org-mode intro
;;
;; TODO:
;; https://realpython.com/emacs-the-best-python-editor/



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

;; Use a minimalist interface.
(scroll-bar-mode  -1)
(menu-bar-mode    -1)
(tool-bar-mode    -1)
(tooltip-mode     -1)
(blink-cursor-mode 0)

;; Set the font and frame size.
(add-to-list 'default-frame-alist '(font . "mononoki-13"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Select color scheme.
(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

;; Select modeline.
(use-package mood-line
  :hook
  (after-init . mood-line-mode))

;; Remove window border.
(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border (face-background 'vertical-border))

;; Disable unneeded screens.
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq initial-scratch-message "")

;; Show matching parentheses.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Show line numbers.
; (global-display-line-numbers-mode)

;; Disable the bell.
(setq ring-bell-function 'ignore)


;;------------------------------
;; User experience
;;------------------------------

;; Provide pop-up help when starting
;; typing chorded keyboard shortcuts.
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Autocompletion via the Ivy framework. To make full
;; use of the package, we also have to remap default
;; keyboard shortcuts to use Councel and Swiper.
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; Load the Counsel, which provides Ivy integration.
(use-package counsel
  :custom
  (counsel-find-file-ignore-regexp ".git"))

;; This package provides a more efficient mode for
;; selecting Ivy hits, available when yuo press C-o.
(use-package ivy-hydra)

;; Project management via Projectile. This package
;; remembers which version-controlled folders you
;; visited, and makes it easy to go back to those.
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-use-git-grep t)
  :config
  (projectile-mode 1))

;; Version control management via Magit. This package
;; makes it easier to do e.g. status/checkout/commit.
(use-package magit)

;; Answer questions with `y` or `n`.
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't litter with backup files.
(setq make-backup-files nil)
(setq auto-save-default nil)



;;-------------------------------
;; Keyboard shortcuts
;;-------------------------------

;; Provide Vim-like keybindings via the Evil framework.
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Integrate the Evil keybindings in more modes.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Provide visual cues for editing actions, such
;; as cut & paste, search & replace, and so on.
(use-package evil-goggles
  :config
  (evil-goggles-mode))

;; Provide a custom "leader-key menu" with my own bindings.
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
     "p"   '(projectile-command-map :which-key "projectile")
     "gs"  '(magit-status :which-key "git status")
     "gc"  '(magit-status :which-key "git commit")
     "/"   '(projectile-grep :which-key "grep")
     "u"   '(counsel-unicode-char :which-key "unicode")
     "v"   '(split-window-right :which-key "split right")
     "h"   '(split-window-below :which-key "split bottom")
     "c"   '(delete-window :which-key "delete window")))

;; Replace the default Evil forward/backward search
;; with Swiper, which integrates nicely with Ivy.
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "/") 'swiper)
  (define-key evil-motion-state-map (kbd "?") 'swiper-backward))

;; Replace some default Emacs bindings with Ivy.
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Replace some default Emacs bindings with Counsel.
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c b") 'counsel-buffer-or-recentf)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)

;; Make it easier to escape from weird places.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))



;;-------------------------------
;; Org mode
;;-------------------------------

;; Set default locations for notes.
(setq org-agenda-files '("~/Documents/Org"))

;; Indent subsections for readability.
(add-hook 'org-mode-hook 'org-indent-mode)

;; Hide markup around bold/italic/etc.
(setq org-hide-emphasis-markers t)

;; Symbols used to show lists.
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
;; Eshell mode
;;-------------------------------

;; Define aliases for common actions.
(defalias 'e 'find-file-other-window)
(defalias 'o 'xdg-open)

;; Use the fish shell for completion
;; if eshell doesn't know what to do.
(use-package fish-completion
  :config
  (global-fish-completion-mode))

;; Tab completion via standard means.
; Alternative: call complete-symbol, works in any mode with ivy.
(add-hook 'eshell-mode-hook
  (lambda () 
    (define-key eshell-mode-map (kbd "<tab>")
      (lambda () (interactive) (pcomplete-std-complete)))))



;;-------------------------------
;; Startup commands
;;-------------------------------

;; Horizontal two-split layout
(split-window-horizontally)

;; Open notes in the first split
(find-file "~/TODO.org")
(other-window 1)

;; Open shell in the other split
(eshell)
(other-window 1)
