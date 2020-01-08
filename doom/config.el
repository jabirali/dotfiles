;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

;; This file contains my private Doom configuration. Possibly useful macros
;; here include `load!' to load *.el files, `use-package' for loading and
;; configuring packages, `after!' to run code after a certain package has
;; loaded, `add-load-path!' to extend where Emacs looks for packages, and
;; finally `map!' for modifying or extending the default keybindings.


;;; Accounts and locations:
;; This section defines the user accounts and system folders relevant for Emacs.
;; This includes mail accounts, mail folders, notes folders, and bibliographies.

;; User identity. This is used by a lot of utilities in Emacs:
;; mail, encryption, file templates, code snippets, and so on.
(setq user-full-name "Jabir Ali Ouassou"
      user-mail-address "jabirali@switzerlandmail.ch")

;; Document folders.
(setq org-directory "~/Documents/Notes/"
      deft-directory "~/Documents/Notes/"
      org-agenda-files (list (concat org-directory (system-name) ".org")))

;; Bibliographies.
(setq reftex-default-bibliography "~/.zotero/library.bib"
      bibtex-completion-bibliography '("~/.zotero/library.bib")
      org-ref-default-bibliography '("~/.zotero/library.bib"))

;; Mail directories.
(setq mu4e-maildir "~/.mail"
      mu4e-sent-folder "/Personal/Sent"
      mu4e-trash-folder "/Personal/Trash"
      mu4e-drafts-folder "/Personal/Drafts"
      mu4e-refile-folder "/Personal/Archive")

;; Download folders.
(setq mu4e-attachment-dir "~/Downloads")


;;; System integration:
;; This section defines (i) how Emacs interacts with the surrounding operating
;; system, and (ii) how the different parts of Emacs interact with each other.

;; Access the system keyring via the FreeDesktop.org "Secret Service API".
(use-package! secrets
  :config
  (setq auth-sources '("secrets:session" "secrets:Login")))

;; Handle email via the internal program mu4e.
(setq mail-user-agent 'mu4e-user-agent)

;; Fetch email via the external program offlineimap.
(setq +mu4e-backend 'offlineimap)

;; Send email via the external program msmtp.
(after! mu4e
  (setq sendmail-program "/usr/bin/msmtp"
        message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-f-is-evil t))

;; If fish is available on the system, use that as the default shell. Also,
;; do enable fish-based completion in shell and eshell buffers via company.
;; TODO: Remove the executable-find statement to cut down startup time.
(let ((path (executable-find "fish")))
  (when path
    (setq explicit-shell-file-name path)
    (with-eval-after-load 'shell
      (add-hook 'shell-mode-hook (lambda () (setq comint-process-echoes t))))
    (use-package! company-fish
      :config
      (add-to-list 'company-backends 'company-fish)
      (add-hook 'shell-mode-hook 'company-mode)
      (add-hook 'eshell-mode-hook 'company-mode))))

;; Automatically enable Python virtual environments.
(after! projectile
  (defun pyvenv-autoload ()
    "Automatically activates pyvenv version if .venv directory exists."
    (let ((path (concat (projectile-project-root) ".venv")))
      (when path
        (pyvenv-activate path))))

  (add-hook! python-mode 'pyvenv-autoload))

;; Preferred previewers when working in latex.
(setq +latex-viewers '(pdf-tools zathura evince sumatrapdf okular skim))

;; Ensure that synctex works and the pdf is updated.
(after! latex
  (setq TeX-source-correlate-start-server t)
  (add-hook! 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; Recognize the linked pdfs in bibtex files generated by Zotero.
(setq bibtex-completion-pdf-field "file")

;; Enable bibtex citations in org-mode via an ivy interface.
(use-package! org-ref
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-get-pdf-filename-function #'org-ref-get-pdf-filename-helm-bibtex))

;; Print pdfs via the standard Unix tools.
(setq pdf-misc-print-programm "/usr/bin/lpr"
      pdf-misc-print-programm-args '("-o media=a4" "-o fitplot"))

;; Use the deft filename as note title, and the first line as a summary.
(setq deft-use-filename-as-title t
      deft-strip-summary-regexp "\\(\n.*\\|^#\s*\\)")


;;; User interface:
;; This section contains the settings for the graphical user interface.
;; This includes all aesthetic settings controlling colors, fonts, etc.

;; Select what fonts to use for the gui.
(setq doom-font (font-spec :family "Iosevka SS09" :size 19)
      doom-variable-pitch-font (font-spec :family "sans" :size 19))

;; Select what colors to use for the gui.
(setq doom-theme 'doom-one)

;; Don't highlight the current line number.
(custom-set-faces!
  `(line-number-current-line :background ,(doom-color 'bg)
                             :foreground ,(face-foreground 'line-number)))

;; Come to the dark side; i.e. invert the colors used to read pdf documents.
(add-hook! 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
(setq pdf-view-midnight-colors `(cons ,(doom-color 'bg) ,(doom-color 'fg)))

;; Customize how latex buffers should be formatted.
(after! latex
  (setq font-latex-fontify-script nil))

;; Default to an undecorated maximized frame. (I mean, if you basically live
;; in Emacs, why wouldn't you want it to get your entire screen immediately?)
(add-to-list 'default-frame-alist '(fullscreen  . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

;; Increase the spacing between windows.
(setq window-divider-default-right-width 4
      window-divider-default-bottom-width 4)

;; Disable help mouse-overs. They're usually annoying and not very helpful.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; Remove the ugly and distracting underlines from all hyperlinks.
(custom-set-faces! '(link :underline nil))

;; Remove highlighting from the mu4e command hints.
(custom-set-faces! `(mu4e-highlight-face :inherit 'mu4e-link-face))

;; Prettify mu4e metadata.
(setq mu4e-use-fancy-chars t
      mu4e-headers-date-format "%Y-%m-%d"
      mu4e-headers-fields '((:date . 12) (:from . 20) (:to . 20) (:thread-subject)))

;; Use regular bullets instead of weird circles and flowers, resize to reasonable
;; defaults if no width has been manually set, and don't show all the org markup.
(setq org-bullets-bullet-list '("⏵")
      org-ellipsis " ▼ "
      org-hide-emphasis-markers t
      org-image-actual-width '(400))


;;; Keyboard shortcuts:
;; This section defines custom keyboard shortcuts for Doom Emacs.

;; Maildir shortcuts in mu4e.
(setq mu4e-maildir-shortcuts
      '(("/Personal/INBOX"       . ?i)
        ("/Personal/Archive"     . ?a)
        ("/Personal/Accounts"    . ?c)
        ("/Personal/Documents"   . ?d)
        ("/Personal/Receipts"    . ?r)
        ("/Personal/Notes"       . ?n)
        ("/Personal/Sent"        . ?s)))

;; Integrate Smartparens into the Evil bindings.
(use-package! evil-smartparens
  :after evil
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;; Use a Spacemacs'esque local leader key.
(setq doom-localleader-key ",")

;; Use the usual C-u/C-d keybindings to navigate pdfs.
(map!
 :map pdf-view-mode-map
 :m "C-u" 'pdf-view-scroll-down-or-previous-page
 :m "C-d" 'pdf-view-scroll-up-or-next-page)

;; TeX commands.
(map!
 :map LaTeX-mode-map
 :m "RET" #'TeX-view
 :localleader
 :desc "Compile" "c" #'TeX-command-run-all
 :desc "Fold"    "z" #'TeX-fold-buffer)
