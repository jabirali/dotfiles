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
         bibtex-completion-pdf-field "file"
         bibtex-completion-bibliography '("~/Refs/index.bib")
         org-ref-default-bibliography '("~/Refs/index.bib")
         org-ref-default-ref-type "cref")
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
     (imenu-list)
     (latex
       :variables
         latex-enable-auto-fill nil
         latex-enable-folding t
         latex-enable-magic nil
         font-latex-fontify-script nil
         font-latex-fontify-sectioning 'color)
     (markdown)
     (mu4e
       :variables
         ;; Identity
         user-full-name "Jabir Ali Ouassou"
         user-mail-address "jabirali@switzerlandmail.ch"
         mail-user-agent 'mu4e-user-agent
         ;; Downloading mail
         mu4e-maildir "~/Mail"
         mu4e-sent-folder "/Sent"
         mu4e-trash-folder "/Trash"
         mu4e-drafts-folder "/Drafts"
         mu4e-refile-folder "/Archive"
         mu4e-get-mail-command "offlineimap"
         mu4e-update-interval 900
         ;; Uploading mail
         sendmail-program "/usr/bin/msmtp"
         send-mail-function 'smtpmail-send-it
         message-sendmail-f-is-evil t
         message-sendmail-extra-arguments '("--read-envelope-from")
         message-send-mail-function 'message-send-mail-with-sendmail
         ;; Shortcuts
         mu4e-maildir-shortcuts '(("/INBOX"       . ?i)
                                  ("/Archive"     . ?a)
                                  ("/Accounts"    . ?c)
                                  ("/Documents"   . ?d)
                                  ("/Receipts"    . ?r)
                                  ("/Notes"       . ?n)
                                  ("/Sent"        . ?s))
         ;; User interface
         mu4e-enable-mode-line t
         mu4e-headers-show-threads nil
         mu4e-use-fancy-chars nil
         mu4e-headers-date-format "%Y-%m-%d %H:%M"
         mu4e-headers-fields '((:date . 20) (:from . 30) (:to . 30) (:thread-subject))
         mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND maildir:/INBOX"
         ;mu4e-split-view 'vertical
         mu4e-headers-visible-columns 80
         mu4e-view-show-addresses t
         mu4e-view-show-images t
         mu4e-compose-in-new-frame t)
     (octave)
     (org
       :variables
         org-ellipsis " +"
         org-mu4e-link-query-in-headers-mode t
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
         org-download-image-dir "~/Notes/pics"
         org-download-method 'directory
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
     (adaptive-wrap)
     (fish-completion)
     (helm-fish-completion :location (recipe :fetcher github :repo "emacs-helm/helm-fish-completion"))
     (gruvbox-theme)
     (ob-async)
     (org-msg)
    )
   dotspacemacs-excluded-packages
   '(
     (persp-mode)
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

  ;; Make Y consistent with C and D (operate until the end of line).
  (setq evil-want-Y-yank-to-eol t)

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

  ;; Use 4-space tabs when reading tabbed code, since that's more common...
  (setq-default tab-width 4)

  ;; Since F11 does fullscreen, F12 should do writeroom-mode.
  (global-set-key (kbd "<f12>") 'writeroom-mode)

  ;; Check syntax continuously in all programming modes.
  (add-hook 'prog-mode-hook 'flycheck-mode)

  ;; Clean code folding via Outline minor mode.
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'text-mode-hook 'outline-minor-mode)
  (add-hook 'eshell-mode-hook 'outline-minor-mode)

  ;; Show all headings but no content in Outline mode.
  (add-hook 'outline-minor-mode-hook
            (defun baba/outline-overview ()
              "Show only outline headings."
              (outline-show-all)
              (outline-hide-body)))

  (add-hook 'python-mode-hook
            (defun baba/outline-python ()
              "Fold only definitions in Python."
              (setq outline-regexp
                    (rx (or
                         ;; Definitions
                         (group (group (* space)) bow (or "class" "def") eow)

                         ;; Decorators
                         (group (group (* space)) "@"))))
              (baba/outline-overview)))

  (add-hook 'octave-mode-hook
            (defun baba/outline-matlab ()
              "Fold definitions in Matlab."
              (setq outline-regexp
                    (rx (or
                         (group
                          (group (* space))
                          bow
                          (or "classdef" "function" "properties" "methods")
                          eow))))
              (baba/outline-overview)))

  (add-hook 'f90-mode-hook
            (defun baba/outline-fortran ()
              "Fold definitions in Fortran."
              (setq outline-regexp
                    (rx (or
                         ;; Module and interface blocks.
                         (group (group (* space)) (or "module" "interface"))

                         ;; Procedures and type definitions.
                         (group
                          (group (* space))
                          (*? (or "pure " "impure " "elemental "))
                          (or "function" "subroutine" "interface" "type" "type,")
                          (group (+ space))))))
                    (baba/outline-overview)))

  ;; Customize the distracting folding markers.
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) org-ellipsis))))

  ;; LaTeX buffers use additional folding. However, by default I have to
  ;; do that manually; let's instead autofold on init and after edits.
  ;; While we're at it, let's also fix the weird TeX folding colors.
  (add-hook 'LaTeX-mode-hook
            (defun baba/TeX-fold-auto ()
              (TeX-fold-mode 1)
              (set-face-foreground 'TeX-fold-folded-face "#ebdbb2")
              (add-hook 'find-file-hook 'TeX-fold-buffer)
              (add-hook 'evil-insert-state-exit-hook 'TeX-fold-paragraph)))

  ;; Load more advanced customization defined below.
  (baba/customize-evil)
  (baba/customize-modeline)
  (baba/customize-readers)
  (baba/customize-eshell)
  (baba/customize-text)
  (baba/customize-layouts)
  (baba/customize-leaders)

  ;; Enable "Secret Service" (FreeDesktop.org password storage).
  (require 'secrets)
  (setq auth-sources '("secrets:session" "secrets:Login"))

  ;; Minor tweaks that simply don't fit in anywhere else.
  (setq dired-listing-switches "-lGh1v --time-style=long-iso --group-directories-first")
  ;(add-hook 'dired-mode-hook 'org-download-enable)

  ;; Setup HTML mail composition.
  ;(require 'org-msg)
  ;(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil"
  ;      org-msg-startup "hidestars indent inlineimages")
  ;(org-msg-mode)
  )

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
  ;(define-key evil-motion-state-map "j" 'evil-next-visual-line)
  ;(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;(define-key evil-visual-state-map "j" 'evil-next-visual-line)
  ;(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; It is more useful to navigate horizontally than vertically with H/L,
  ;; at least when using the centered-point and truncate-lines settings.
  (evil-global-set-key 'motion (kbd "H") 'evil-scroll-left)
  (evil-global-set-key 'motion (kbd "L") 'evil-scroll-right)

  ;; After getting used to these in Org-mode, I want them everywhere...
  (evil-global-set-key 'insert (kbd "M-h") 'evil-shift-left-line)
  (evil-global-set-key 'insert (kbd "M-l") 'evil-shift-right-line)

  (evil-global-set-key 'normal (kbd "M-h") 'evil-shift-left-line)
  (evil-global-set-key 'normal (kbd "M-l") 'evil-shift-right-line)
  (evil-global-set-key 'normal (kbd "M-k") 'move-text-up)
  (evil-global-set-key 'normal (kbd "M-j") 'move-text-down)

  (evil-global-set-key 'visual (kbd "M-h") 'evil-shift-left)
  (evil-global-set-key 'visual (kbd "M-l") 'evil-shift-right)
  (evil-global-set-key 'visual (kbd "M-k") 'move-text-region-up)
  (evil-global-set-key 'visual (kbd "M-j") 'move-text-region-down)

  ;; Code folding via Outline minor mode.
  (defun baba/outline-toggle-p ()
    "Check the current code folding state."
    (interactive)
    (save-excursion
      (end-of-line)
      (outline-invisible-p)))

  (defun baba/outline-toggle-buffer ()
    "Toggle code folding throughout the buffer."
    (interactive)
    (if (baba/outline-toggle-p)
        (progn
          (outline-show-all)
          (recenter 0))
      (outline-hide-body)
      (recenter)))

  (defun baba/outline-toggle-point ()
    "Toggle code folding at the current point."
    (interactive)
    (if (baba/outline-toggle-p)
        (progn
          (outline-show-entry)
          (recenter 0))
      (outline-hide-entry)
      (recenter)))

  (evil-global-set-key 'normal (kbd "<backtab>") 'baba/outline-toggle-buffer)
  (evil-global-set-key 'normal (kbd "<tab>") 'baba/outline-toggle-point)

  ;; Tab completion or indentation via Company and Helm.
  (defun baba/complete-or-indent ()
    "Snippet expansion, tab completion, or indentation."
    (interactive)
    (if (looking-back "^\\s-*")
        (funcall indent-line-function)
      (helm-company)))

  (evil-global-set-key 'insert (kbd "<tab>") 'baba/complete-or-indent)
  (evil-global-set-key 'insert (kbd "<backtab>") 'yas-expand)

  ;; I always want to jump specifically to mark, not to the line of mark.
  (evil-global-set-key 'motion (kbd "'") 'evil-goto-mark)

  ;; Make it easy to use Helm to pick file names for commands.
  (with-eval-after-load 'helm
    (evil-global-set-key 'insert (kbd "<C-i>") 'helm-find-files)
    (define-key eshell-mode-map (kbd "<C-i>") 'helm-find-files)
    (define-key helm-map (kbd "<C-i>") 'helm-ff-run-complete-fn-at-point)))

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

  ;; Bundle modes for centering, indenting, wrapping, etc.
  (add-hook 'visual-line-mode-hook
            (defun baba/visual-line-bundle ()
              "Toggle extra minor modes that work well with visual-line-mode."
              (interactive)
              (letf ((writeroom-maximize-window nil)
                     (writeroom-mode-line t))
                (adaptive-wrap-prefix-mode 'visual-line-mode)
                (visual-fill-column-mode 'visual-line-mode)
                (writeroom-mode 'visual-line-mode))))

  ;; Enable these bundled minor modes where it makes sense.
  (add-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'magit-diff-mode-hook #'visual-line-mode)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (add-hook 'help-mode-hook #'visual-line-mode))

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

  ;; Customize the prompt to use in Eshell. This includes
  ;; what outline-minor-mode folding pattern to use.
  (setq eshell-prompt-regexp "^❯ "
        outline-regexp "❯"
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
     (setq outline-regexp "❯")
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

  ;; What do do when opening a new workspace. An eshell is convenient
  ;; because I can `cd' around, including to `/ssh:' paths, and then
  ;; do an appropriate `find-file', `projectile', or `magit' command.
  (setq eyebrowse-new-workspace 'eshell)

  ;; Typing `SPC l w' is inconvenient for such a useful command as the
  ;; `eyebrowse' transient state, at least when using workspaces a lot.
  (global-set-key (kbd "<f1>") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "<f2>") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "<f3>") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "<f4>") 'eyebrowse-switch-to-window-config-4)
  (global-set-key (kbd "<f5>") 'eyebrowse-switch-to-window-config-5)
  (global-set-key (kbd "<f6>") 'eyebrowse-switch-to-window-config-6)
  (global-set-key (kbd "<f7>") 'eyebrowse-switch-to-window-config-7)
  (global-set-key (kbd "<f8>") 'eyebrowse-switch-to-window-config-8))

(defun baba/customize-leaders ()
  "Populate the private leader-key menu (SPC o)."

  ;; Define the private leader keys.
  (spacemacs/set-leader-keys "os" 'helm-surfraw))
