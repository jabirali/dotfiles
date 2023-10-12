(setq package-archives 
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq native-comp-async-report-warnings-errors nil)
(setq ring-bell-function 'ignore)

(setq auto-save-default nil)
(setq backup-directory-alist `(("." . "~/.cache/emacs/backup/")))
(setq backup-by-copying t)

(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(delete-selection-mode 1)
(context-menu-mode 1)
(xterm-mouse-mode 1)
; (pixel-scroll-precision-mode 1)

(blink-cursor-mode -1)
(setq-default cursor-type 'bar)

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

(set-frame-font "JetBrains Mono NL-13" nil t)
(use-package nerd-icons :ensure t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-`") 'switch-to-buffer)
(global-set-key (kbd "C-.") 'repeat)

(use-package evil
  :ensure t
  :defer .1
  :init
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-u-scroll t)
  :custom
  evil-disable-insert-state-bindings t
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode))

(use-package iedit
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode)
  :bind
  (("C-x C-r" . ivy-resume)))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-b" . counsel-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-g" . counsel-git)
   ("C-S-s"   . counsel-git-grep)
   :map help-map
   ("f" . counsel-describe-function)
   ("v" . counsel-describe-variable)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

;; (use-package swiper
;;   :ensure t
;;   :config
;;   (setq swiper-action-recenter t)
;;   :bind
;;   (("C-s" . 'swiper)))

(use-package ace-window
  :ensure t
  :bind
  (("M-o" . ace-window)))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dir "~/.config/emacs/snippets")
  (yas-global-mode 1))

(use-package eglot
  :ensure t
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-prefer-doc-buffer t))
  ;:hook
  ;((python-ts-mode . eglot-ensure)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox-light))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

(use-package org
  :ensure t
  :config
  (setq org-pretty-entities t)
  (setq org-startup-indented t))

  ;(setq org-pretty-entities-include-sub-superscripts nil)
  ;; :hook
  ;; ((org-mode . org-cdlatex-mode)
  ;;  (org-mode . visual-line-mode)))

(use-package org-babel
  :no-require
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (add-to-list 'org-babel-default-header-args '(:wrap . "results"))
  (setq org-babel-default-header-args:python '((:python . "python3") (:results . "output")))
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-results-keyword "results")
  (add-to-list 'org-latex-packages-alist '("" "eulervm" t)))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  :hook
  ((LaTeX-mode . cdlatex-mode)
   (LaTeX-mode . prettify-symbols-mode)))

(use-package xenops
  :ensure xenops
  :config
  (setq xenops-math-image-scale-factor 1.4)
  ; (setq xenops-reveal-on-entry t)
  :hook
  ((org-mode . xenops-mode)
   (latex-mode . xenops-mode)
   (LaTeX-mode . xenops-mode)))

(use-package markdown-mode
  :ensure t)

(defun my/goto-def ()
  (interactive)
  (call-interactively 'xref-find-definitions)
  (recenter-top-bottom 0))

(global-set-key (kbd "M-.") 'my/goto-def)

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

(defun my/scratch ()
  (interactive)
  (find-file (concat "~/Notes/Scratch/" (format-time-string "%Y%m%d%H%M%S.org"))))

(global-set-key (kbd "C-c c") 'my/scratch)

(use-package adaptive-wrap
    :ensure t
    :hook
    ((visual-line-mode . adaptive-wrap-prefix-mode)))

  ;; Useful for customization/scripting.
  (use-package f
    :ensure t)

  ;; Automatically install and use tree-sitter.
  ;; (use-package treesit-auto
  ;;   :config
  ;;   (setq treesit-auto-install 'prompt)
  ;;   (global-treesit-auto-mode))

  (use-package pdf-tools
    :ensure t)

  (use-package windmove
    :ensure nil
    :config
    (windmove-mode 1))

  (use-package outline
    :hook
    ((python-ts-mode . outline-minor-mode)
     (LaTeX-mode . outline-minor-mode)))

  (use-package multi-vterm
    :ensure t)

(define-prefix-command 'my-leader-map)
(global-set-key (kbd "C-SPC") 'my-leader-map)
(keymap-set evil-motion-state-map "SPC" 'my-leader-map)
(keymap-set evil-normal-state-map "SPC" 'my-leader-map)

(evil-define-key nil my-leader-map
    ;; add your bindings here:
    "SPC" 'switch-to-buffer
    "B"  'project-switch-to-buffer
    "pf" 'project-find-file
    "ps" 'project-shell-command
    "s"  'save-buffer
    ;; etc.
    )
