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



;;-------------------------------
;; Optimization (initial)
;;-------------------------------

;; Disable garbage collection at init
;(setq gc-cons-threshold 402653184
;      gc-cons-percentage 0.6)



;; Minimalist interface
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Font and frame size
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "mononoki-13"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))



;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
; (defun remove-scratch-buffer ()
;   (if (get-buffer "*scratch*")
;       (kill-buffer "*scratch*")))
; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
; (setq-default message-log-max nil)
; (kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
;(add-hook 'minibuffer-exit-hook
;      '(lambda ()
;         (let ((buffer "*Completions*"))
;           (and (get-buffer buffer)
;                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
;(add-hook 'window-setup-hook 'delete-other-windows)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)


(global-linum-mode t) ;; enable line numbers globally

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil
      package--init-file-ensured t)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Fancy minimal modeline
(use-package mood-line
  :ensure t
  :hook
  (after-init . mood-line-mode))

;; Helm (vs. ivy etc)
(use-package helm
  :ensure t
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

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
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
  :ensure t
  :init
  ;(setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

;; All The Icons
;(use-package all-the-icons :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'arrow)) ;; (if (display-graphic-p) 'icons 'arrow)))





;;-------------------------------
;; Evil mode
;;-------------------------------

;; Vim editing keybindings
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Evil extension to more modes
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; More visual cues when you cut and paste
(use-package evil-goggles
  :ensure t
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

;; IDE features
(use-package elpy
  :ensure t
  :init
  (elpy-enable))



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

;; Helm autocomp
(setq helm-use-frame-when-more-than-two-windows nil)
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map (kbd "TAB") 'helm-esh-pcomplete)))

;; Helm projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup layout
(split-window-horizontally)
(find-file "~/TODO.org")
(other-window 1)
(eshell)
(other-window 1)
