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
      deft-directory "~/Documents/Notes/")

;; Bibliographies.
(setq reftex-default-bibliography "~/.zotero/library.bib"
      bibtex-completion-bibliography '("~/.zotero/library.bib")
      org-ref-default-bibliography '("~/.zotero/library.bib"))

;; Mail directories.
(setq mu4e-maildir "~/Mail"
      mu4e-sent-folder "/Sent"
      mu4e-trash-folder "/Trash"
      mu4e-drafts-folder "/Drafts"
      mu4e-refile-folder "/Archive")


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
(setq sendmail-program "/usr/bin/msmtp"
      send-mail-function 'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail)

;; Enable fish completion in shell/eshell buffers via company.
(use-package! company-fish
  :config
  (when (executable-find "fish")
    (add-to-list 'company-backends 'company-fish)
    (add-hook 'shell-mode-hook 'company-mode)
    (add-hook 'eshell-mode-hook 'company-mode)))

;; Preferred previewers when working in latex.
(setq +latex-viewers '(pdf-tools zathura evince sumatrapdf okular skim))

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
(setq doom-theme 'doom-nord)

;; Don't highlight the current line number. It breaks in Org-mode when using
;; heading-based indentation, and highlights too much in the Gruvbox theme.
(custom-set-faces!
  `(line-number-current-line :background ,(doom-color 'bg)
                             :foreground ,(face-foreground 'line-number)))

;; Come to the dark side; i.e. invert the colors used to read pdf documents.
(add-hook! 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
(setq pdf-view-midnight-colors `(cons ,(doom-color 'bg) ,(doom-color 'fg)))

;; Default to an undecorated maximized frame. (I mean, if you basically live
;; in Emacs, why wouldn't you want it to get your entire screen immediately?)
(add-to-list 'default-frame-alist '(fullscreen  . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

;; Disable help mouse-overs. They're usually annoying and not very helpful.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; For org headings, use regular bullets instead of weird circles and flowers.
(setq org-bullets-bullet-list '("⏵"))

;; For org images, resize to a reasonable default width if none has been set.
(setq org-image-actual-width '(400))


;;; Keyboard shortcuts:
;; This section defines custom keyboard shortcuts for Doom Emacs.

;; Maildir shortcuts in mu4e.
(setq mu4e-maildir-shortcuts
      '(("/INBOX"       . ?i)
        ("/Archive"     . ?a)
        ("/Accounts"    . ?c)
        ("/Documents"   . ?d)
        ("/Receipts"    . ?r)
        ("/Notes"       . ?n)
        ("/Sent"        . ?s)))

;; Integrate Smartparens into the Evil bindings.
(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;; After getting used to these bindings in Org-mode, I want them everywhere...
; (map!
;  :n  "<backtab>" '+fold/close-all
;  :ni "M-h" 'evil-shift-left-line
;  :v  "M-h" 'evil-visual-dedent
;  :ni "M-l" 'evil-shift-right-line
;  :v  "M-l" 'evil-visual-indent)
