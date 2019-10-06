;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     auto-completion
     emacs-lisp
     git
     (version-control :variables version-control-diff-side 'left)
     ivy
     latex
     pdf
     python
     markdown
     org
     (shell :variables
            shell-default-shell 'eshell
            shell-default-position 'right)
     ;; spell-checking
     ;; syntax-checking
     )
   dotspacemacs-additional-packages '(gruvbox-theme)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(vi-tilde-fringe)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
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
   dotspacemacs-startup-lists '((todos . 5)
                                (recents . 5)
                                (projects . 5))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-new-empty-buffer-major-mode 'text-mode
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-initial-scratch-message nil
   dotspacemacs-themes '(gruvbox-dark-medium
                         gruvbox-light-medium)
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

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; Prevent Emacs from littering my .spacemacs file.
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Common
  (spacemacs/toggle-highlight-current-line-globally-off)
  (spacemacs/toggle-centered-point-globally-on)
  (set-default 'truncate-lines t)
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)
  (setq vc-follow-symlinks t)

  ;; Dired
  (setq dired-listing-switches "-lGh1v --time-style=long-iso --group-directories-first")

  ;; Org
  (setq org-startup-indented t)
  (setq org-bullets-bullet-list '("•"))

  ;; Python
  (add-hook 'python-mode-hook (lambda () (setq tab-width 4) (setq indent-tabs-mode t)))

  ;; Modeline
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

  ;; Theme
  (set-face-background 'line-number (face-background 'default))
  (set-face-foreground 'line-number-current-line (face-foreground 'line-number))
  (set-face-background 'line-number-current-line (face-background 'line-number))
  (set-face-background 'vertical-border "#1d1d1d")
  (set-face-foreground 'vertical-border "#1d1d1d")

  ;; Startup
  (find-file-other-window "~/Notes/TODO.org")
  (other-window)
  (evil-goto-first-line))
