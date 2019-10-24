;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;------------------------------------------------------------
;; File: Spacemacs config
;; Path: ~/.spacemacs
;;------------------------------------------------------------


(defun dotspacemacs/layers ()
  "Select the layers and packages to use in Spacemacs."

  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     (auto-completion
       :variables
         auto-completion-idle-delay nil
         auto-completion-enable-sort-by-usage t)
     (emacs-lisp)
     (git)
     (html)
     (ivy
       :variables
         ivy-height 20
         ivy-enable-advanced-buffer-information t)
     (latex
       :variables
         latex-enable-auto-fill nil
         latex-enable-folding nil
         latex-enable-magic nil
         font-latex-fontify-script nil
         font-latex-fontify-sectioning 'color)
     (markdown)
     (org
       :variables
         org-startup-indented t
         org-pretty-entities t
         org-hide-emphasis-markers t
         org-bullets-bullet-list '("•")
         org-projectile-file "TODO.org"
         org-todo-keywords '((sequence "TODO" "INIT" "|" "WAIT" "DONE")))
     (pdf
       :variables
         TeX-view-program-selection '((output-pdf "PDF Tools"))
         pdf-view-bounding-box-margin 0.06
         pdf-tools-enabled-modes
          '(pdf-history-minor-mode
            pdf-isearch-minor-mode
            pdf-links-minor-mode
            pdf-misc-minor-mode
            pdf-outline-minor-mode
            pdf-misc-size-indication-minor-mode
            pdf-misc-menu-bar-minor-mode
            pdf-annot-minor-mode
            pdf-sync-minor-mode
            pdf-misc-context-menu-minor-mode
            pdf-cache-prefetch-minor-mode
            pdf-view-auto-slice-minor-mode
            pdf-occur-global-minor-mode))
     (perl5)
     (python)
     (ranger)
     (shell
       :variables
         shell-default-shell 'vterm
         shell-default-height 40
         shell-enable-smart-eshell t
         eshell-banner-message ""
         eshell-history-size 1024
         eshell-destroy-buffer-when-process-dies t)
     (shell-scripts)
     (spell-checking
       :variables
         spell-checking-enable-by-default nil
         spell-checking-enable-auto-dictionary t)
     (syntax-checking
       :variables
         syntax-checking-enable-tooltips nil
         flycheck-check-syntax-automatically '(mode-enabled save)
         flycheck-indication-mode nil)
     (version-control
       :variables
         version-control-diff-side 'right
         vc-follow-symlinks t)
     (yaml)
    )
   dotspacemacs-additional-packages
   '(
     esh-autosuggest
     fish-completion
     gruvbox-theme
     wolfram
    )
   dotspacemacs-excluded-packages
   '(
     vi-tilde-fringe
     treemacs
     treemacs-evil
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
   dotspacemacs-mode-line-theme '(spacemacs :separator arrow :separator-scale 1.667)
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
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server t
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

  ;; Don't autowrite settings to my config.
  (setq custom-file "~/.emacs.d/custom.el")

  ;; Aesthetic revision of all themes.
  (add-hook 'spacemacs-post-theme-change-hook 'baba/customize-theme))


(defun dotspacemacs/user-config ()
  "Custom code run after `dotspacemacs/layers'."

  ;; Disable line-highlighting. It's distracting in buffers that can have
  ;; varying line height (e.g. org-mode with inline equations or images),
  ;; and it doesn't provide much that cursor highlighting doesn't already.
  (spacemacs/toggle-highlight-current-line-globally-off)

  ;; Turn centered-cursor-mode on everywhere. This keeps the current line
  ;; centered. This results in smoother vertical scrolling, removes the
  ;; need to move view and cursor separately, and automatically provides
  ;; the maximum possible context for the code we're currently editing.
  (spacemacs/toggle-centered-point-globally-on)

  ;; Use line truncation. For lines slightly wider than the current window,
  ;; I find it more annoying to look at wrapped lines than to scroll right.
  (spacemacs/toggle-truncate-lines-on)
  (set-default 'truncate-lines t)

  ;; It is more useful to navigate horizontally than vertically with H/L,
  ;; at least when using the centered-point and truncate-lines settings.
  (define-key evil-motion-state-map (kbd "H") 'evil-scroll-left)
  (define-key evil-motion-state-map (kbd "L") 'evil-scroll-right)

  ;; Load more advanced customization defined below.
  (baba/customize-modeline)
  (baba/customize-readers)
  (baba/customize-shells)
  (baba/customize-layouts)
  (baba/leader-keys)

  ;; Turn on appropriate linters for all programming modes.
  (add-hook 'prog-mode-hook (lambda () (flycheck-mode 1)))

  ;(define-key evil-motion-state-map (kbd "C-f") 'evil-avy-goto-word-1)

  ;; Minor tweaks that simply don't fit in anywhere else.
  (setq dired-listing-switches "-lGh1v --time-style=long-iso --group-directories-first")
  (setq wolfram-alpha-app-id (getenv "WOLFRAM_ID"))

  ;; This sets the default buffers to open in every Emacs session.
  (find-file-other-window "~/Notes/TODO.org")
  (other-window 1)
  (evil-goto-first-line))


(defun dotspacemacs/user-load ()
  "Custom code run during config dumps.")


(defun dotspacemacs/user-env ()
  "Custom environment variables."

  ;; Load from ~/.spacemacs.env
  (spacemacs/load-spacemacs-env))


(defun baba/customize-theme ()
  "Customize the graphical interface.
Some of these modifications make Emacs look more like a minimalist tiling
window manager, others make it fit better with the Ubuntu default theme."

  ;; Use Ubuntu/Yaru colors for window separators.
  (set-face-foreground 'window-divider "#1d1d1d")
  (set-face-foreground 'window-divider-first-pixel "#1d1d1d")
  (set-face-foreground 'window-divider-last-pixel "#1d1d1d")
  (set-face-background 'vertical-border "#1d1d1d")
  (set-face-foreground 'vertical-border "#1d1d1d")

  ;; Turn on window separators.
  (setq window-divider-default-places t)
  (setq window-divider-default-right-width 8)
  (setq window-divider-default-bottom-width 8)
  (window-divider-mode 1)

  ;; Increase line spacing.
  (setq-default line-spacing 2)
  (setq-default line-spacing 2)

  ;; Customize fringes and margins.
  (setq-default fringes-outside-margins t
                left-fringe-width 3
                right-fringe-width 3
                left-margin-width 1
                right-margin-width 0)

  ;; Disable fringe arrows on long lines.
  (setf (cdr (assq 'truncation   fringe-indicator-alist)) '(nil nil))
  (setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil)))


(defun baba/customize-modeline ()
  "Define a minimalist modeline via Spaceline.
This function removes unnecessary information from the default setup,
and tries to minimize the section movement during window switching."

  ;; Define a segment for showing the version-control status which
  ;; is simpler (only one character) than the default one. This is
  ;; done by eliminating information that is most of the time not
  ;; needed (what version control system and branch we are using),
  ;; and reducing the representation of the rest to one character.
  (spaceline-define-segment version-control-simple
    (when vc-mode
      (powerline-raw
       (when (buffer-file-name)
         (pcase (vc-state (buffer-file-name))
           (`up-to-date " ")
           (`ignored " ")
           (`edited "~")
           (`added "+")
           (`removed "-")
           (`needs-merge "!")
           (`needs-update "!")
           (_ "?"))))))

  ;; Disable coloring of the modeline based on the current Evil state.
  ;; The cursor coloring is sufficient to indicate state, and having the
  ;; modeline flash in colors every time I switch state is distracting.
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-default)

  ;; Disable Unicode numbers for workspace and window numbers. The Unicode
  ;; numbers look decent, but they're too small, causing legibility issues.
  (setq spaceline-workspace-numbers-unicode nil)
  (setq spaceline-window-numbers-unicode nil)

  ;; Compile the Spaceline. This setup contains essentially only the window
  ;; and workspace numbers, buffer name and position, version control status,
  ;; and flycheck status. Search and selection generates extra sections, but
  ;; only in places that don't cause the existing sections to bounce around.
  (spaceline-compile
    '(((window-number) :face highlight-face :priority 100)
      ((buffer-id version-control-simple) :priority 90)
      ((process) :when active))
    '(((which-function) :when active)
      ((anzu) :when active :priority 95)
      ((selection-info) :when active :priority 95)
      ((global) :when active)
      ((auto-compile) :when active :priority 85)
      ((flycheck-error flycheck-warning flycheck-info) :when active :priority 85)
      ((line-column) :when active :priority 50)
      ((workspace-number) :when active :face highlight-face :priority 100))))


(defun baba/customize-readers ()
  "Customize the modes used for reading documents."

  ;; Recolor PDF buffers to match surrounding color theme by default.
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

  ;; Prevent ugly borders around the recolored PDF buffers;
  ;; this fixes a bug in the combination pdf-view + evil.
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-evilified-state-cursor) (list nil))
              (setq left-fringe-width 1 right-fringe-width 1
                    left-margin-width 0 right-margin-width 0))))


(defun baba/customize-shells ()
  "Customize the modes used to interact with terminals and shells."

  ;; When spawning a `vterm', we can define a proper normal-mode, where
  ;; hjkl are autotranslated to arrow keys, and JK are autotranslated
  ;; to page-up/page-down. This makes any terminal application that
  ;; understands arrow keys (fish, htop, etc.) useful from an Evil
  ;; normal-mode, as one can drop to normal mode for navigation.
  (evil-define-key 'normal vterm-mode-map (kbd "h") 'vterm-send-left)
  (evil-define-key 'normal vterm-mode-map (kbd "j") 'vterm-send-down)
  (evil-define-key 'normal vterm-mode-map (kbd "k") 'vterm-send-up)
  (evil-define-key 'normal vterm-mode-map (kbd "l") 'vterm-send-right)
  (evil-define-key 'normal vterm-mode-map (kbd "J") 'vterm-send-next)
  (evil-define-key 'normal vterm-mode-map (kbd "K") 'vterm-send-prior)

  ;; TODO: Figure out which of these to keep.
  ;(when (and (executable-find "fish")
  ;           (require 'fish-completion nil t))
  ;  (global-fish-completion-mode))
  ;
  ;(setq ivy-display-functions-alist nil)
  ;(add-hook 'eshell-mode-hook 'fish-completion-mode)
  ;(add-hook 'eshell-mode-hook 'esh-autosuggest-mode)
  ;(add-hook 'eshell-post-command-hook 'evil-normal-state)

  ;(evil-define-key 'insert 'vterm-mode-map
  ;  (kbd "C-h") 'vterm-send-left
  ;  (kbd "C-j") 'vterm-send-down
  ;  (kbd "C-k") 'vterm-send-up
  ;  (kbd "C-l") 'vterm-send-right

  ;; Define aliases for use in Eshell.
  (defalias 'v 'eshell-exec-visual))


(defun baba/customize-layouts ()
  "Customize the behavior of persp-mode layouts, eyebrowse workspaces, etc."

  ;; What do do when opening a new workspace. An eshell is convenient
  ;; because I can `cd' around, including to `/ssh:' paths, and then
  ;; do an appropriate `find-file', `projectile', or `magit' command.
  (setq eyebrowse-new-workspace 'eshell)

  ;; Keybindings for switching to a workspace.
  (define-key evil-motion-state-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key evil-motion-state-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key evil-motion-state-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key evil-motion-state-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (define-key evil-motion-state-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (define-key evil-motion-state-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
  (define-key evil-motion-state-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
  (define-key evil-motion-state-map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
  (define-key evil-motion-state-map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
  (define-key evil-motion-state-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)

  ;; Keybindings for making and removing workspace.
  (define-key evil-motion-state-map (kbd "M-RET") 'eyebrowse-create-window-config)
  (define-key evil-motion-state-map (kbd "C-w")   'eyebrowse-close-window-config))


(defun baba/leader-keys ()
  "Populate the private leader-key menu (SPC o)."

  ;; TODO: add an entry for "oo" to my own TODO list.
  (spacemacs/set-leader-keys "ow" 'wolfram-alpha)
  (spacemacs/set-leader-keys "os" 'counsel-search))
