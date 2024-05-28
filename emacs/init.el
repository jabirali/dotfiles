;;; init.el --- Emacs configuration file.
;; This configuration is structured in a modular manner, so most of
;; the actual configuration can be found in the `lisp' subdirectory.
;; The exception is a few settings in `early-init.el' that must be
;; set before creating the first Emacs frame in order to be useful.

;; Add our custom modules in `~/.emacs.d/lisp' to the load path.
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Load my configuration layers.
(require 'init-pkg)
(require 'init-gui)
(require 'init-tui)
(require 'init-mac)

; (require 'keys-global)
(require 'keys-evil)
; (require 'keys-leader)

(require 'lang-python)
;; (require 'lang-org)
;; (require 'lang-tex)

;; Inbox: Settings that are yet to be integrated above.
(set-frame-font "Iosevka Comfy:size=14:weight=light" :frames t)

;; init-history
(global-auto-revert-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(tab-bar-history-mode 1)

;; (require 'init-theme)
(load-theme 'modus-vivendi)

;; keys-global
(bind-key "M-o" 'other-window)	
(bind-key "M-[" 'tab-bar-history-back)
(bind-key "M-]" 'tab-bar-history-forward)

   ;; (use-package emacs
   ;;   :custom
   ;;   (auto-save-default nil)
   ;;   (default-input-method 'TeX)
   ;;   (default-transient-input-method 'TeX)
   ;;   (dired-listing-switches "-hlLgG --group-directories-first --time-style=long-iso")
   ;;   (frame-title-format "GNU Emacs")
   ;;   (inhibit-startup-message t)
   ;;   (make-backup-files nil)
   ;;   (message-truncate-lines t)
   ;;   (truncate-lines t)
   ;;   (ring-bell-function 'ignore)
   ;;   (sentence-end-double-space nil)
   ;;   (tab-width 4)
   ;;   (tramp-remote-path '(tramp-own-remote-path tramp-default-remote-path))
   ;;   (use-short-answers t)
   ;;   :config
   ;;   ;; Don't indicate long or wrapped lines.
   ;;   (set-display-table-slot standard-display-table 'truncation ? )
   ;;   (set-display-table-slot standard-display-table 'wrap ? )
   ;;   ;; Make ANSI and ISO keyboards more similar.
   ;;   (define-key key-translation-map (kbd "§") (kbd "`"))
   ;;   (define-key key-translation-map (kbd "±") (kbd "~"))
   ;; (use-package gcmh
   ;;   ;; Garbage Collection Magic Hack
   ;;   :ensure t
   ;;   :config
   ;;   (gcmh-mode 1))
   ;; (use-package server
   ;;   ;; Provide Emacs as a service
   ;;   (interactive)
   ;;   (let* ((query (buffer-substring (region-beginning) (region-end)))
   ;; 		 (encoded-query (url-encode-url query))
   ;; 		 (search-url "https://chat.openai.com/g/g-Kihf3Sccx-science-definitions?q="))
   ;; 	(browse-url (concat search-url encoded-query))))
   ;; (defun +url-handler-zotero (link)
   ;;   "Open a zotero:// link in the Zotero desktop app."
   ;;   (start-process "zotero_open" nil "open" (concat "zotero:" link)))
   
   ;;; Packages:
   ;;   (defun +other-window-dwim ()
   ;; 	"Select either the minibuffer or an arbitrary visible window."
   ;; 	(interactive)
   ;; 	(if (active-minibuffer-window)
   ;; 		(select-window (active-minibuffer-window))
   ;; 	  (call-interactively #'ace-window)))
   ;;   :bind
   ;;   ("M-o" . +other-window-dwim))
   ;; (use-package comint						; Mother of all Emacs REPLs
   ;;   :custom
   ;;   (comint-prompt-read-only t)
   ;;   :bind (:map comint-mode-map
   ;; 			  ("C-c C-l" . comint-clear-buffer)))
   ;; (use-package company					; COMPlete ANYthing
   ;;   :ensure t
   ;;   :custom
   ;;   (company-idle-delay nil)
   ;;   :bind
   ;;   (:map company-mode-map
   ;; 		("TAB" . company-indent-or-complete-common))
   ;;   :hook
   ;;   (after-init . global-company-mode))
   ;; (use-package consult					; Improves misc commands
   ;;   :ensure t
   ;;   :bind
   ;;   ("C-c t t" . consult-theme))
   ;; (use-package diredfl					; File management with style
   ;;   :ensure t
   ;;   :after dired
   ;;   :config
   ;;   (diredfl-global-mode 1))
   ;; (use-package eglot					 	; The built-in LSP client
   ;;   :after project
   ;;   :custom
   ;;   (eldoc-echo-area-prefer-doc-buffer t)
   ;;   (eldoc-echo-area-use-multiline-p nil)
   ;;   :hook
   ;;   (python-mode . +eglot-project-ensure)
   ;;   :bind
   ;;   ("<f2>" . eglot-rename))
   ;; ;; (use-package eldoc-box)
   ;; ;; (use-package expand-region
   ;; ;;   :bind*
   ;; ;;   ("C-c RET" . er/expand-region)
   ;; ;;   :ensure t)
   ;; (use-package flymake-ruff
   ;;   :ensure t
   ;;   :hook
   ;;   (python-mode . flymake-mode)
   ;;   (python-mode . flymake-ruff-load))
   ;; (use-package flyspell
   ;;   :after ispell
   ;;   :hook
   ;;   (text-mode . flyspell-mode))
   ;; (use-package flyspell-correct
   ;;   :ensure t
   ;;   :after flyspell
   ;;   :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
   ;; ;; (use-package format-all
   ;; ;;   :ensure t
   ;; ;;   :hook
   ;; ;;   (prog-mode . format-all-mode)
   ;; ;;   (markdown-mode . format-all-mode)
   ;; ;;   (format-all-mode . format-all-ensure-formatter)
   ;; ;;   :config
   ;; ;;   (setq-default format-all-formatters
   ;; ;; 				'(("Python" (isort) (ruff) (black)))))
   ;; (use-package gnuplot
   ;;   :ensure t)
   ;; (use-package hl-todo
   ;;   :ensure t
   ;;   :hook
   ;;   (prog-mode . hl-todo-mode))
   ;; (use-package hideshow					; Folding of code files
   ;;   :hook
   ;;   (prog-mode . hs-minor-mode))
   ;; ;; (use-package iedit
   ;; ;;   :ensure t)
   ;; (use-package ispell
   ;;   ;; Remember to install Hunspell dictionaries into ~/Library/Spelling.
   ;;   ;; Download dictionaries from: https://github.com/wooorm/dictionaries
   ;;   :config
   ;;   (setq ispell-program-name "hunspell")
   ;;   (setq ispell-personal-dictionary (concat user-emacs-directory "ispell"))
   ;;   (setq ispell-dictionary "en_US,nb_NO")
   ;;   (ispell-set-spellchecker-params)
   ;;   (ispell-hunspell-add-multi-dic "en_US,nb_NO"))
   ;; ;; (use-package julia-mode
   ;; ;;   :ensure t)
   ;; (use-package magit
   ;;   :ensure t
   ;;   :bind
   ;;   (:map magit-status-mode-map ("SPC" . nil))
   ;;   :custom
   ;;   (magit-diff-refine-hunk 'all)
   ;;   :config
   ;;   (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
   ;;   (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
   ;;   (keymap-set project-prefix-map "m" #'magit-project-status))
   ;; (use-package markdown-mode
   ;;   :ensure t
   ;;   :after adaptive-wrap
   ;;   :custom
   ;;   (markdown-fontify-code-blocks-natively t)
   ;;   (markdown-enable-wiki-links t)
   ;;   (markdown-enable-math t)
   ;;   (initial-major-mode 'markdown-mode)
   ;;   (initial-scratch-message "")
   ;;   :config
   ;;   (kill-buffer "*scratch*")
   ;;   (scratch-buffer))
   ;; (use-package matlab
   ;;   :ensure matlab-mode)
   ;;   :config
   ;; (use-package vertico
   ;;   :ensure t
   ;;   :config
   ;;   (vertico-mode 1)
   ;;   (vertico-mouse-mode 1))
   ;; (use-package vertico-directory
   ;;   :after vertico
   ;;   :bind (:map vertico-map
   ;; 			  ("RET"   . vertico-directory-enter)
   ;; 			  ("DEL"   . vertico-directory-delete-char)
   ;; 			  ("M-DEL" . vertico-directory-delete-word))
   ;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
   ;; (use-package vertico-prescient
   ;;   :ensure t
   ;;   :after (vertico prescient)
   ;;   :config
   ;;   (vertico-prescient-mode 1))
   ;; (use-package which-key
   ;;   :ensure t
   ;;   :config
   ;;   (which-key-mode 1))
   ;; (use-package yasnippet
   ;;   :ensure t
   ;;   :config
   ;;   (yas-global-mode 1))
   
   ;; ;;; Keybindings:
   ;; (bind-key "<f5>" #'sort-lines)
   ;; (bind-key "<f12>" #'+science-definition-lookup)
   

(provide 'init)
;;; init.el ends here
