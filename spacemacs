;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.



(defun dotspacemacs/layers ()
  "Select the layers and packages to use in Spacemacs."

  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     (auto-completion)
     (emacs-lisp)
     (git)
     (html)
     (ivy)
     (latex :variables latex-enable-auto-fill nil)
     (markdown)
     (org)
     (pdf)
     (python)
     (shell :variables shell-default-shell 'eshell)
     (version-control :variables version-control-diff-side 'left)
    )
   dotspacemacs-additional-packages
   '(
     gruvbox-theme
    )
   dotspacemacs-excluded-packages
   '(
     vi-tilde-fringe
    )
   dotspacemacs-frozen-packages '()
   dotspacemacs-install-packages 'used-only))



(defun dotspacemacs/init ()
  "Customize the settings recognized by Spacemacs."

  (setq-default
   dotspacemacs-enable-emacs-pdumper nil
   dotspacemacs-emacs-pdumper-executable-file "emacs"
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-use-spacelpa t
   dotspacemacs-verify-spacelpa-archives t
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'vim
   dotspacemacs-startup-banner 'nil
   dotspacemacs-startup-lists '((todos . 5) (recents . 5) (projects . 5))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-new-empty-buffer-major-mode 'text-mode
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-initial-scratch-message nil
   dotspacemacs-themes '(gruvbox-dark-medium gruvbox-light-soft)
   dotspacemacs-mode-line-theme '(spacemacs :separator arrow :separator-scale 1.5)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Input" :size 13.0 :weight normal :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-undecorated-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server nil
   dotspacemacs-server-socket-dir nil
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-frame-title-format "%t"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup 'all
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil))



(defun dotspacemacs/user-init ()
  "Custom code run before `dotspacemacs/layers'."

  ;; Aesthetic revision all themes.
  (add-hook
   'spacemacs-post-theme-change-hook
   (lambda ()
     ;; Do not highlight the current line number.
     (set-face-background 'line-number (face-background 'default))
     (set-face-background 'line-number-current-line (face-background 'line-number))
     (set-face-foreground 'line-number-current-line (face-foreground 'line-number))
     ;; Use Ubuntu/Yaru colors for window borders.
     (set-face-background 'vertical-border "#1d1d1d")
     (set-face-foreground 'vertical-border "#1d1d1d"))))



(defun dotspacemacs/user-config ()
  "Custom code run after `dotspacemacs/layers'."

  ;; Navigation settings:
  ;; Disable line-highlighting and line-wrapping. However, keep the current
  ;; line vertically centered, to provide more context and smooth scrolling.
  (spacemacs/toggle-highlight-current-line-globally-off)
  (spacemacs/toggle-centered-point-globally-on)
  (spacemacs/toggle-truncate-lines-on)
  (set-default 'truncate-lines t)

  ;; Spaceline settings:
  ;; Define a minimalist modeline, where unnecessary information is removed,
  ;; and sections don't move around more than necessary as I switch windows.
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-default)
  (setq spaceline-workspace-numbers-unicode nil)
  (setq spaceline-window-numbers-unicode nil)
  (spaceline-compile
  '(((persp-name workspace-number window-number) :fallback evil-state :face highlight-face :priority 100)
    ((buffer-id buffer-modified) :priority 90)
    ((process) :when active))
  '(which-function
    ((anzu) :priority 95)
    ((selection-info) :priority 95)
    ((global) :when active)
    ((auto-compile) :priority 85)
    ((flycheck-error flycheck-warning flycheck-info) :when active :priority 85)
    ((version-control) :when active :priority 80)))

  ;; File management:
  ;; Use ISO dates, hide hidden files, show directories first, follow symlinks.
  (setq dired-listing-switches "-lGh1v --time-style=long-iso --group-directories-first")
  (setq vc-follow-symlinks t)

  ;; Org-mode settings:
  ;; I prefer using indentation to see what text belongs to which section.
  ;; Also, bullet points should use consistent bullet types at all levels.
  (setq org-startup-indented t)
  (setq org-bullets-bullet-list '("•"))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  ;; Pdf-tools settings:
  ;; I want PDFs opened within Emacs to use the same colorscheme as
  ;; Emacs itself, and want to use this mode to view all TeX output.
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))

  ;; Keybindings:
  ;; It is more useful to navigate horizontally than vertically with H/L,
  ;; at least when using the centered-point and truncate-lines settings.
  (define-key key-translation-map (kbd "H") (kbd "zH"))
  (define-key key-translation-map (kbd "L") (kbd "zL"))

  ;; Startup:
  ;; This sets the default buffers to open in every Emacs session.
  (find-file-other-window "~/Notes/TODO.org")
  (other-window)
  (evil-goto-first-line))



(defun dotspacemacs/user-load ()
  "Custom code run during config dumps.")



(defun dotspacemacs/user-env ()
  "Custom environment variables."

  ;; Load from ~/.spacemacs.env
  (spacemacs/load-spacemacs-env))



;;------------------------------------------------------------
;; Autogenerated content below here
;;------------------------------------------------------------

(defun dotspacemacs/emacs-custom-settings ()
  "Autogenerated from M-x customize."
  (custom-set-variables
   '(pdf-tools-enabled-modes
     (quote
      (pdf-history-minor-mode pdf-isearch-minor-mode pdf-links-minor-mode pdf-misc-minor-mode pdf-outline-minor-mode pdf-misc-size-indication-minor-mode pdf-misc-menu-bar-minor-mode pdf-annot-minor-mode pdf-sync-minor-mode pdf-misc-context-menu-minor-mode pdf-cache-prefetch-minor-mode pdf-view-auto-slice-minor-mode pdf-occur-global-minor-mode))))
  (custom-set-faces))
