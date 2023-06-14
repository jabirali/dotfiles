
;; Prevent spam.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Fix the outdated default aesthetics.
(fringe-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode 1)

(xterm-mouse-mode 1)

(setq-default cursor-type 'bar)
(set-frame-font "JetBrains Mono NL-13" nil t)


; (load-theme 'modus-operandi)		

;; Package repos.
(setq package-archives 
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Better default keybindings.



(global-set-key (kbd "s-<return>") 'ansi-term)
(global-set-key (kbd "s-w") 'kill-buffer-and-window)

(defun my/goto-def ()
  (interactive)
  (call-interactively 'xref-find-definitions)
  (recenter-top-bottom 0))
(global-set-key (kbd "M-<down>") 'my/goto-def)
					; (setq mac-command-modifier 'meta)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-catppuccin-dark-variant "macchiato")
  (load-theme 'doom-catppuccin))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

;; Useful for customization/scripting.
(use-package f
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode))

;; Automatically install and use tree-sitter.
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package pdf-tools
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; (use-package direnv
;;   :ensure t
;;   :config
;;   (direnv-mode 1))

(use-package org
  :ensure t
  :config
  (setq org-level-color-stars-only t)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)
  :hook
  ((org-mode . org-cdlatex-mode)
   (org-mode . org-indent-mode)
   (org-mode . visual-line-mode)))

(use-package org-babel
  :no-require
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (setq org-babel-default-header-args:python '((:python . "python3") (:results . "output")))
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-results-keyword "results"))


(use-package eglot
  :ensure t
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-prefer-doc-buffer t)
  (setq evil-lookup-func 'eldoc)
  :hook
  ((python-ts-mode . eglot-ensure)))

(use-package markdown-mode
  :ensure t)

(use-package evil
  :ensure t
  :custom
  ((evil-want-keybinding nil)
   (evil-want-C-u-scroll t)
   (evil-want-C-u-delete t)
   (evil-want-C-w-delete t))
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package outline
  :hook
  ((python-ts-mode . outline-minor-mode)
   (LaTeX-mode . outline-minor-mode)))

(use-package multi-vterm
  :ensure t)

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  :hook
  ((LaTeX-mode . cdlatex-mode)
   (LaTeX-mode . prettify-symbols-mode)))


(define-key evil-normal-state-map (kbd "SPC") 'execute-extended-command)

(global-set-key (kbd "M-f") 'find-file)
(global-set-key (kbd "M-b") 'ivy-switch-buffer)
(global-set-key (kbd "M-p") 'project-switch-project)



(defun my/select-and-open-pdf ()
  "Select a PDF file from the Zotero storage directory and open it in pdf-view mode."
  (interactive)
  (let* ((pdf-files (f-entries "~/Zotero/storage"
                               (lambda (f) (equal "pdf" (f-ext f)))
                               t))
         (selected-file (ivy-read "Select PDF: " pdf-files)))
    (when selected-file
      (find-file-other-window selected-file)
      (pdf-view-mode)
      (pdf-view-themed-minor-mode))))

(global-set-key (kbd "C-c z") 'my/select-and-open-pdf)


