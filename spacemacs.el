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
     (bibtex
       :variables
         org-ref-pdf-directory "~/Refs/"
         org-ref-default-bibliography '("~/Refs/index.bib")
         org-ref-bibliography-notes "~/Refs/index.org")
     (csv)
     (deft
       :variables
         deft-directory "~/Notes/deft/"
         deft-auto-save-interval 60
         deft-strip-summary-regexp "\\(\n.*\\|^#\s*\\)")
     (emacs-lisp)
     (git
       :variables
         magit-diff-refine-hunk t)
     (html)
     (helm
       :variables
         helm-use-frame-when-more-than-two-windows nil
         helm-show-completion-display-function #'helm-show-completion-default-display-function
         helm-always-two-windows nil
         helm-split-window-inside-p t
         helm-ff-skip-git-ignored-files t
         helm-display-header-line nil)
     (latex
       :variables
         latex-enable-auto-fill nil
         latex-enable-folding nil
         latex-enable-magic nil
         font-latex-fontify-script nil
         font-latex-fontify-sectioning 'color)
     (markdown)
     (mu4e
       :variables
          mu4e-maildir "~/Mail"
          mu4e-sent-folder "/Sent"
          mu4e-trash-folder "/Trash"
          mu4e-drafts-folder "/Drafts"
          mu4e-refile-folder "/Archive"
          mu4e-headers-fields '((:date . 20) (:from . 30) (:to . 30) (:thread-subject))
          mu4e-headers-date-format "%Y-%m-%d %H:%M"
          mu4e-headers-show-threads nil
          mu4e-get-mail-command "offlineimap"
          mu4e-spacemacs-layout-name "@mail"
          mu4e-spacemacs-layout-binding "m"
          mu4e-update-interval 300
          mu4e-compose-signature-auto-include nil
          org-mu4e-link-query-in-headers-mode t
          mu4e-use-fancy-chars nil
          mu4e-view-show-images t
          mu4e-view-show-addresses t
          mu4e-split-view 'vertical
          mu4e-headers-visible-columns 80)
     (org
       :variables
         org-startup-indented t
         org-image-actual-width '(300)
         org-pretty-entities t
         org-hide-emphasis-markers t
         org-catch-invisible-edits 'smart
         org-want-todo-bindings t
         org-bullets-bullet-list '("•")
         org-projectile-file "TODO.org"
         org-directory "~/Notes"
         org-default-notes-file "~/Notes/TODO.org"
         org-agenda-files '("~/Notes/TODO.org")
         org-attach-directory "~/Notes/data/"
         org-download-method 'attach
         org-todo-keywords
          '((sequence "TODO(t)" "INIT(i)" "|" "DONE(d!)")
            (sequence "WAIT(w@/!)" "|" "STOP(c@)")))
     (pdf
       :variables
         TeX-view-program-selection '((output-pdf "PDF Tools"))
         pdf-view-resize-factor 1.1)
     (perl5)
     (python)
     (ranger
       :variables
         ranger-override-dired t)
     (shell
       :variables
         shell-default-shell 'eshell
         shell-default-height 40
         eshell-banner-message ""
         eshell-history-size 1024
         eshell-destroy-buffer-when-process-dies t
         tramp-default-method "ssh")
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
     (evil-smartparens)
     (fish-completion)
     (helm-fish-completion :location (recipe :fetcher github :repo "emacs-helm/helm-fish-completion"))
     (gruvbox-theme)
     (ob-async)
    )
   dotspacemacs-excluded-packages
   '(
     (vi-tilde-fringe)
     (treemacs)
     (treemacs-evil)
     (org-brain)
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
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-default-layout-name "main"
   dotspacemacs-display-default-layout t
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
   dotspacemacs-show-transient-state-title nil
   dotspacemacs-show-transient-state-color-guide nil
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode t
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

  ;; Since I bind `SPC 0' to switch workspace, it's very convenient
  ;; to let 0 be default workspace. This way, `SPC 0 0' goes back to
  ;; default, while workspace #1 correspond to project/task #1, etc.
  (setq eyebrowse-default-workspace-slot 0)

  ;; Aesthetic revision of all themes.
  (add-hook 'spacemacs-post-theme-change-hook 'baba/customize-theme))

(defun dotspacemacs/user-config ()
  "Custom code run after `dotspacemacs/layers'."

  ;; Disable line-highlighting. It's distracting in buffers that can have
  ;; varying line height (e.g. org-mode with inline equations or images),
  ;; and it doesn't provide much that cursor highlighting doesn't already.
  (spacemacs/toggle-highlight-current-line-globally-off)

  ;; Use line truncation. For lines slightly wider than the current window,
  ;; I find it more annoying to look at wrapped lines than to scroll right.
  (spacemacs/toggle-truncate-lines-on)
  (set-default 'truncate-lines t)

  ;; Use 4-space tabs when reading tabbed code, since that's more common.
  (setq-default tab-width 4)

  ;; Since F11 does fullscreen, F12 should do writeroom-mode.
  (global-set-key (kbd "<f12>") 'writeroom-mode)

  ;; Load more advanced customization defined below.
  (baba/customize-evil)
  (baba/customize-modeline)
  (baba/customize-readers)
  (baba/customize-text)
  (baba/customize-prog)
  (baba/customize-eshell)
  (baba/customize-layouts)
  (baba/customize-leaders)

  ;; Enable "Secret Service" (FreeDesktop.org password storage).
  (require 'secrets)
  (setq auth-sources '("secrets:session" "secrets:Login"))

  ;; Minor tweaks that simply don't fit in anywhere else.
  ;(add-hook 'deft-mode-hook
  ;          (lambda ()
  ;            (setq-local evil-insert-state-cursor '(nil (bar . 0)))
  ;            (setq-local evil-normal-state-cursor '(nil (bar . 0)))
  ;            (hl-line-mode)))
  (setq dired-listing-switches "-lGh1v --time-style=long-iso --group-directories-first"))

(defun dotspacemacs/user-load ()
  "Custom code run during config dumps.")

(defun dotspacemacs/user-env ()
  "Custom environment variables."

  ;; Load from ~/.spacemacs.env
  (spacemacs/load-spacemacs-env)

  ;; Extract Emacs variables
  (setq wolfram-alpha-app-id (getenv "WOLFRAM_ID")))

(defun baba/customize-evil ()
  "Customize the evil-mode behavior.
This function modifies the default Evil behavior to what I find more natural
and ergonomic, including easier code folding and automatic view navigation."

  ;; Turn centered-cursor-mode on everywhere. This keeps the current line
  ;; centered. This results in smoother vertical scrolling, removes the
  ;; need to move view and cursor separately, and automatically provides
  ;; the maximum possible context for the code we're currently editing.
  (spacemacs/toggle-centered-point-globally-on)

  ;; Use ESC to escape from basically anything.
  (bind-key "<escape>" 'isearch-cancel isearch-mode-map)
  (define-key minibuffer-local-map (kbd "ESC") 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map (kbd "ESC") 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map (kbd "ESC") 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map (kbd "ESC") 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map (kbd "ESC") 'abort-recursive-edit)
  (with-eval-after-load 'helm
    (bind-key "<escape>" 'helm-keyboard-quit helm-map)
    (bind-key "<escape>" 'helm-keyboard-quit helm-comp-read-map))

  ;; Make up/down operate in screen lines instead of logical lines.
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; It is more useful to navigate horizontally than vertically with H/L,
  ;; at least when using the centered-point and truncate-lines settings.
  (evil-global-set-key 'motion (kbd "H") 'evil-scroll-left)
  (evil-global-set-key 'motion (kbd "L") 'evil-scroll-right)

  ;; After getting used to these in org-mode, I want them everywhere...
  (evil-global-set-key 'normal (kbd "M-h") 'evil-shift-left-line)
  (evil-global-set-key 'normal (kbd "M-l") 'evil-shift-right-line)
  (evil-global-set-key 'normal (kbd "M-k") 'move-text-up)
  (evil-global-set-key 'normal (kbd "M-j") 'move-text-down)

  (evil-global-set-key 'visual (kbd "M-h") 'evil-shift-left)
  (evil-global-set-key 'visual (kbd "M-l") 'evil-shift-right)
  (evil-global-set-key 'visual (kbd "M-k") 'move-text-region-up)
  (evil-global-set-key 'visual (kbd "M-j") 'move-text-region-down)

  ;; Follow the lead of org-mode, and use TAB and S-TAB to fold everywhere.
  ;; By default, TAB either does autoindent (which can be done with `=') or
  ;; autocomplete (which I only want in insert mode), so I prefer folding.
  (evil-global-set-key 'normal (kbd "<tab>") 'evil-toggle-fold)
  (evil-global-set-key 'normal (kbd "<backtab>") 'evil-close-folds)

  ;; Smartparens seems like it might be a good idea. Let's do more of that.
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

  ;; Make a function that can be used to run Eshell in a smarter way.
  (defun baba/eshell-dwim ()
    "Open an eshell, and do so via projectile if possible."
    (interactive)
    (if (projectile-project-p)
        (projectile-run-eshell)
      (eshell)))

  ;; Make it easy to use Helm to pick file names for commands.
  (with-eval-after-load 'helm
    (evil-global-set-key 'insert (kbd "<C-i>") 'helm-find-files)
    (define-key eshell-mode-map (kbd "<C-i>") 'helm-find-files)
    (define-key helm-map (kbd "<C-i>") 'helm-ff-run-complete-fn-at-point))

  ;; Make S a shortcut for opening a shell. This is already the default
  ;; keybinding in Deer/Ranger, and doesn't do anything useful in Evil.
  (evil-global-set-key 'normal (kbd "S") 'baba/eshell-dwim)
  (with-eval-after-load 'ranger
    (define-key ranger-normal-mode-map (kbd "S") 'baba/eshell-dwim))
  (with-eval-after-load 'pdf-tools
    (define-key pdf-view-mode-map (kbd "S") 'baba/eshell-dwim))

  ;; I always want to jump specifically to mark, not to the line of mark.
  (evil-global-set-key 'motion (kbd "'")  'evil-goto-mark))

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
  ;; NOTE: Breaks magit images...
  ; (setq-default line-spacing 2)

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

  ;; Print PDF in the usual way.
  (setq pdf-misc-print-programm "/usr/bin/lpr"
        pdf-misc-print-programm-args (quote ("-o media=a4" "-o fitplot")))

  ;; Recolor PDF buffers to match surrounding color theme by default.
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

  ;; Prevent ugly borders around the recolored PDF buffers;
  ;; this fixes a bug in the combination pdf-view + evil.
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-evilified-state-cursor) (list nil))
              (setq left-fringe-width 1 right-fringe-width 1
                    left-margin-width 0 right-margin-width 0))))

(defun baba/customize-text ()
  "Customize plaintext buffers by adding appropriate hooks."

  ;; Do word wrapping at fill column in visual-line-mode.
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

  ;; Break long lines on word boundaries.
  (add-hook 'magit-diff-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'help-mode-hook #'visual-line-mode))

(defun baba/customize-prog ()
  "Customize programming buffers by adding appropriate hooks."

  (defun baba/prog-enable-minors ()
    "Enable linting and folding in all prog-mode buffers."

    (flycheck-mode)
    (hs-minor-mode)
    (evil-close-folds))

  (add-hook 'prog-mode-hook 'baba/prog-enable-minors))

(defun baba/customize-eshell ()
  "Customize the behavior of the Emacs shell."

  ;; Workaround for Eshell bug related to `eshell-mode-map':
  ;; start a temporary Eshell instance while configuring...
  (eshell)

  ;; Load a package for smart autocompletion via Helm and Fish.
  (require 'helm-fish-completion nil t)
  (setq helm-esh-pcomplete-build-source-fn
        #'helm-fish-completion-make-eshell-source)

  ;; Define aliases for use in Eshell.
  (defalias 'v 'eshell-exec-visual)
  (defalias 'o 'browse-url-xdg-open)
  (defalias 'g 'magit-status-here)

  ;; Customize the prompt to use in Eshell.
  (setq eshell-prompt-regexp "^❯ "
        eshell-prompt-function
        (lambda ()
          (concat
           "\n"
           (if (file-remote-p default-directory)
               (with-parsed-tramp-file-name default-directory nil
                 (concat
                  (propertize (concat "/" method ":") 'face '(bold :foreground "#cc241d"))
                  (if (string= host (system-name))
                      (propertize ":" 'face '(bold :foreground "#cc241d"))
                    (propertize (concat host ":") 'face '(bold :foreground "#d79921")))
                  (propertize (concat localname "\n") 'face '(bold :foreground "#83a598"))))
             (unless (or (string= default-directory "~/")
                         (string= default-directory (concat (getenv "HOME") "/")))
               (propertize (concat default-directory "\n") 'face '(bold :foreground "#83a598"))))
           (propertize "❯ " :foreground "default"))))

  ;; Do what `eshell-previous-prompt' should be doing by default.
  (defun eshell-above-prompt ()
    "Jump to the prompt above in `eshell'."
    (interactive)
    (evil-previous-line)
    (eshell-previous-prompt 1)
    (evil-end-of-line)
    (evil-beginning-of-line))

  ;; Customize keybindings. Unfortunately, Eshell does something weird, and only defines
  ;; its keymap upon buffer creation. It therefore has to be configured via hooks...
  (add-hook
   'eshell-mode-hook
   (lambda ()
     ;; Motions between prompts (replaces the Plan9 "smart shell" feature).
     (evil-define-key 'normal eshell-mode-map (kbd "[") 'eshell-above-prompt)
     (evil-define-key 'normal eshell-mode-map (kbd "]") 'eshell-next-prompt)

     ;; A few terminal commands don't do their own line wrapping, and end up
     ;; writing 800-character lines instead of 80-character lines. Moreover, I
     ;; occasionally write long commands due to long path names, in which case
     ;; I also prefer not navigating horizontally. This fixes those issues by
     ;; breaking long lines at word boundaries, which is suitable for shells.
     (visual-line-mode)

     ;; Turn off centered-point mode in shells. It's not that useful there...
     (spacemacs/toggle-centered-point-off)))

  ;; Remove the temporary Eshell buffer after configuration.
  (kill-buffer))

(defun baba/customize-layouts ()
  "Customize the behavior of persp-mode layouts, eyebrowse workspaces, etc."

  ;; Place dotfiles in a separate autogenerated layout, to prevent them from
  ;; cluttering my projectile layouts when I'm just doing short edits.
  (ignore-errors
    (persp-def-auto-persp "conf"
                          :file-name (concat (getenv "HOME") "/[.][^.].*")
                          :parameters '((dont-save-to-file . t))
                          :switch 'frame))

  ;; What do do when opening a new workspace. An eshell is convenient
  ;; because I can `cd' around, including to `/ssh:' paths, and then
  ;; do an appropriate `find-file', `projectile', or `magit' command.
  (setq eyebrowse-new-workspace 'eshell)

  ;; Typing `SPC l w' is inconvenient for such a useful command as the
  ;; `eyebrowse' transient state, at least when using workspaces a lot.
  ;; Let's just map `SPC 0' to it instead, since I don't use `treemacs'.
  (spacemacs/set-leader-keys "0" 'spacemacs/workspaces-transient-state/body))

(defun baba/customize-leaders ()
  "Populate the private leader-key menu (SPC o)."

  (defun baba/note-goto ()
    (interactive)
    (persp-switch "note")
    (spacemacs/window-split-double-columns)
    (find-file org-default-notes-file)
    (other-window 1)
    (spacemacs/deft)
    (deft-filter-clear)
    (other-window 1))

  ;(defun baba/note-deft ()
  ;  (interactive)
  ;  (baba/note-goto)
  ;  (helm-find-files-1 deft-directory))

  ;; Define the private leader keys.
  (spacemacs/set-leader-keys "oc" 'org-capture)
  (spacemacs/set-leader-keys "oo" 'baba/note-goto)
  (spacemacs/set-leader-keys "os" 'helm-surfraw))
