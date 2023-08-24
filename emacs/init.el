
;; Prevent Emacs from spamming me.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq native-comp-async-report-warnings-errors nil)

(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
      
;; Fix the outdated default aesthetics.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(xterm-mouse-mode 1)
(pixel-scroll-precision-mode 1)

(setq-default
 left-margin-width 1
 right-margin-width 1
 cursor-type 'bar)

;; (set-frame-parameter nil 'alpha-background 50)
(set-frame-font "JetBrains Mono NL-13" nil t)

 
; (load-theme 'modus-operandi)		

;; Package repos.
(setq package-archives 
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Better default keybindings.
(global-set-key (kbd "M-o") 'other-window) 		; "C-x o". The default is unbound.
(global-set-key (kbd "M-p") 'ivy-switch-buffer)		; "C-x b". This is more like Sublime and VSCode.

; (global-set-key (kbd "M-DEL") 'kill-buffer)		; "C-x k". I've remapped M-DEL to C-w.



;; (defun my/goto-def ()
;;   (interactive)
;;   (call-interactively 'xref-find-definitions)
;;   (recenter-top-bottom 0))

;; (global-set-key (kbd "M-<down>") 'my/goto-def)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-catppuccin-dark-variant "macchiato")
  (load-theme 'doom-laserwave))

(use-package adaptive-wrap
  :ensure t
  :config
  (setq-default adaptive-wrap-extra-indent 1)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  (global-visual-line-mode +1))

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

(use-package windmove
  :ensure nil
  :config
  (windmove-mode 1))
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
   '((emacs-lisp . t)
     (python . t)))
  (setq org-babel-default-header-args:python '((:python . "python3") (:results . "output")))
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-results-keyword "results"))



(use-package eglot
  :ensure t
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-prefer-doc-buffer t)
  ;; (setq evil-lookup-func 'eldoc)
  :hook
  ((python-ts-mode . eglot-ensure)))

(use-package markdown-mode
  :ensure t)

;; (use-package evil
;;   :ensure t
;;   :custom
;;   ((evil-want-keybinding nil)
;;    (evil-want-C-u-scroll t)
;;    (evil-want-C-u-delete t)
;;    (evil-want-C-w-delete t))
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;; (use-package evil-goggles
;;   :ensure t)

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

(setq mac-option-modifier 'super
      mac-command-modifier 'meta)

;; (global-set-key (kbd "H-SPC") 'execute-extended-command)
;; (global-set-key (kbd "H-f") 'find-file)
;; (global-set-key (kbd "H-b") 'ivy-switch-buffer)
;; (global-set-key (kbd "H-p") 'project-switch-project)
;; (global-set-key (kbd "H-e") 'eval-last-sexp)

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


(defun my/C-w-dwim (&optional arg)
  "Kill either a region or the preceding word.
This essentially merges the default keybindings of Emacs and Bash.
With prefix arg N, delete backward to the start of the Nth word."
  (interactive "P")
  (cond ((use-region-p)
         (kill-region (region-beginning) (region-end)))
	(arg
         (backward-kill-word (prefix-numeric-value arg)))
        (t (backward-kill-word 1))))
    
(global-set-key (kbd "C-w") 'my/C-w-dwim)

;; Many commands set the mark automatically when you jump around in a
;; file; for instance, C-s / C-r / M-< / M-> all do this. You can also
;; set the mark manually using C-SPC C-SPC. It can therefore be useful
;; to have some keybindings to more easily navigate these marks. The
;; default (C-u C-SPC) only goes one direction through the mark ring,
;; and moreover is not so convenient to type cf. e.g. Sublime and Vim.
;; The bindings I chose here are similar to browser history navigation.

(defun my/mark-ring-backward ()
  "Retreat through the mark ring."
  (interactive)
  (pop-to-mark-command))

(defun my/mark-ring-forward ()
  "Advance through the mark ring."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "M-[") 'my/mark-ring-backward)
(global-set-key (kbd "M-]") 'my/mark-ring-forward)
