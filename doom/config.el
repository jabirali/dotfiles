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
(setq org-directory "~/projects/notes"
      org-agenda-files '("~/projects/notes")
      +org-capture-todo-file "~/projects/notes/personal.org")

;; Bibliographies.
(setq reftex-default-bibliography "~/.zotero/library.bib"
      bibtex-completion-bibliography '("~/.zotero/library.bib")
      org-ref-default-bibliography '("~/.zotero/library.bib"))

;; Download folders.
(setq mu4e-attachment-dir "~/tmp")

;; Mail directories.
(setq mu4e-maildir "~/.mail"
      mu4e-sent-folder "/Personal/Sent"
      mu4e-trash-folder "/Personal/Trash"
      mu4e-drafts-folder "/Personal/Drafts"
      mu4e-refile-folder "/Personal/Archive")

;; Mail bookmarks.
(after! mu4e
  (setq mu4e-bookmarks
        `( ,(make-mu4e-bookmark
             :name  "Inbox"
             :query "maildir:/Personal/INBOX AND NOT subject:NOTE*"
             :key ?i)
           ,(make-mu4e-bookmark
             :name "Todos"
             :query "maildir:/Personal/INBOX AND subject:NOTE*"
             :key ?t)
           ,(make-mu4e-bookmark
             :name "Notes"
             :query "maildir:/Personal/Notes"
             :key ?n)
           ,(make-mu4e-bookmark
             :name "Files"
             :query "maildir:/Personal/Archive AND flag:attach"
             :key ?f))))


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

;; Change which external programs are used by Org-mode links.
(after! org
  (add-to-list 'org-file-apps '("\\.xls?x\\'" . "gnumeric %s")))

;; If fish is available on the system, use that as the default shell in `vterm'.
;; Also, specify what prompt I use, so that Emacs knows how to fold and jump.
(setq vterm-shell "fish")
(setq-hook! 'vterm-mode-hook outline-regexp ".*❯")

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

;; Enable extra syntax checkers.
(add-hook 'lsp-after-initialize-hook
          (lambda ()
            (flycheck-add-next-checker 'lsp 'python-flake8)))


;;; User interface:
;; This section contains the settings for the graphical user interface.
;; This includes all aesthetic settings controlling colors, fonts, etc.

;; Select what fonts to use for the gui.
(setq doom-font (font-spec :family "Iosevka SS09" :size 19)
      doom-variable-pitch-font (font-spec :family "sans" :size 19))

;; Select what colors to use for the gui.
(setq doom-theme  'doom-moonlight)

;; Disable line numbers. They look nice in regular buffers, but they look bad in
;; terminals, look strange in buffers with variable line height, and cause issues
;; with smooth scrolling in Emacs (much more responsive without them present).
(setq display-line-numbers-type nil)

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

;; Minimalist modeline.
(after! doom-modeline
  ;; Disable unwanted modeline details.
  (size-indication-mode 0)

  ;; Define a new default modeline.
  (doom-modeline-def-modeline 'babaline
    '(bar workspace-name buffer-info remote-host)
    '(matches debug checker))

  ;; Actually use the new settings.
  (add-hook 'doom-modeline-mode-hook
    (lambda ()
      (doom-modeline-set-modeline 'babaline 'default))))

;; Disable help mouse-overs. They're usually annoying and not very helpful.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; Remove the ugly and distracting underlines from all hyperlinks.
(custom-set-faces! '(link :underline nil))

;; Remove highlighting from the mu4e command hints.
(custom-set-faces! `(mu4e-highlight-face :inherit 'mu4e-link-face))

;; Prettify and minimize the mu4e metadata.
(after! mu4e
  (setq mu4e-use-fancy-chars t
        mu4e-headers-show-threads nil
        mu4e-headers-include-related nil
        mu4e-headers-date-format "%Y-%m-%d"
        mu4e-headers-fields '((:date . 12) (:from . 20) (:to . 20) (:thread-subject))))

;; Setup the LSP frontend.
(setq lsp-ui-sideline-enable nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-file-watchers nil)

;; Use regular bullets instead of weird circles and flowers, resize to reasonable
;; defaults if no width has been manually set, and don't show all the org markup.
(setq org-bullets-bullet-list '("*")
      org-ellipsis " * "
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-image-actual-width '(400))

;; Get rid of unneccessary fringe symbols.
(setq vi-tilde-fringe-bitmap-array [0])

;; Customize the Ivy popup frame.
(after! ivy
  (setq ivy-posframe-width (- (frame-width) 1)
        ivy-posframe-height 16
        ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-top-center))))

;; Move around continuously.
(after! ivy
  (use-package! centered-cursor-mode
    :init
    (require 'centered-cursor-mode)
    (global-centered-cursor-mode +1)))


;;; Keyboard shortcuts:
;; This section defines custom keyboard shortcuts for Doom Emacs.

;; I love unimpaired-style bindings, I just miss window navigation.
(map!
 :prefix "["
 :mnv "q" 'evil-window-prev
 :prefix "]"
 :mnv "q" 'evil-window-next)

;; Make an application menu containing some "missing defaults".
(map!
 :leader
 "K" 'man
 :prefix "o"
 "m" 'mu4e
 "v" 'pyvenv-workon
 "n" '+default/browse-notes)

;; Maildir shortcuts in mu4e.
(setq mu4e-maildir-shortcuts
      '(("/Personal/INBOX"       . ?i)
        ("/Personal/Archive"     . ?a)
        ("/Personal/Accounts"    . ?c)
        ("/Personal/Documents"   . ?d)
        ("/Personal/Receipts"    . ?r)
        ("/Personal/Notes"       . ?n)
        ("/Personal/Sent"        . ?s)))

;; This makes it more convenient to edit e.g. M-: commands, since you
;; can paste there using Vim keyboard shortcuts, etc. However, making
;; it usable requires redefining the Ivy minibuffer map (C-hjkl, etc.)
;; (setq evil-want-minibuffer t)

;; Enable some Evil extra bindings.
(setq evil-magit-want-horizontal-movement t)

;; It is more useful to navigate horizontally than vertically
;; with H/L, at least when using truncate lines in e.g. LaTeX.
;; I often revert a buffer, but rarely ever use replace-mode.
;; Finally, C-r is not the most logical or ergonomic redo key.
(map!
 :nv "R" 'revert-buffer
 :nv "U" 'undo-tree-redo
 :m  "H" 'evil-scroll-left
 :m  "L" 'evil-scroll-right)

;; Use a Spacemacs'esque local leader key.
(setq doom-localleader-key ",")

;; Use the usual C-u/C-d keybindings to navigate pdfs.
(map!
 :map pdf-view-mode-map
 :m "C-u" 'pdf-view-scroll-down-or-previous-page
 :m "C-d" 'pdf-view-scroll-up-or-next-page)

;; I don't need to separate between jumping to mark and line of mark.
;; I also don't use `...` in shell, as (...) or $(...) does the same.
;; Let's instead do a Quake-style terminal that pops up on backtick.
(map! :mnv "`" '+vterm/toggle)
(map! :map vterm-mode-map "`" '+vterm/toggle)

;; Terminal keybindings.
(after! vterm
  (map!
   :map vterm-mode-map
   ;; Navigate between prompts.
   :m "[[" 'outline-previous-heading
   :m "]]" 'outline-next-heading
   ;; Enable terminal control keys.
   :i "C-c" 'vterm-send-C-c
   :i "C-z" 'vterm-send-C-z
   ;; Doom-to-Emacs key translation.
   :i "C-h" 'vterm-send-left
   :i "C-j" 'vterm-send-down
   :i "C-k" 'vterm-send-up
   :i "C-l" 'vterm-send-right))

;; TeX commands.
(map!
 :map LaTeX-mode-map
 :m "RET" #'TeX-view
 :localleader
 :desc "Compile" "c" #'TeX-command-run-all
 :desc "Fold"    "z" #'TeX-fold-buffer)


;;; Code folding:
;; This code was copied over from my heavily customized Spacemacs setup, and has
;; not yet been adjusted to fit well in Doom. TODO: Find a good folding solution.

;; Enable outline folding in more modes.
(add-hook! 'vterm-mode-hook 'outline-minor-mode)
(add-hook! 'LaTeX-mode-hook 'outline-minor-mode)

;; Define advice to make the Doom Emacs `fold' module work as expected.
;; In particular, expanding an entry shouldn't expand its entire subtree,
;; and closing all folds should close top-level outline sections as well.
;; Finally, global operations with `+fold/*-all' should recenter cursor.

(defun +baba/fold-close-outline (&optional level)
  "Close Outline folds after running `+fold/close-all' without prefix."
  (unless (or (integerp level) (derived-mode-p 'org-mode))
    (outline-hide-body)))

(defun +baba/fold-close-subtree (fun &rest args)
  "Close Hideshow subtree after opening an entry with `+fold/open'."
  (interactive)
  (apply fun args)
  (when (+fold--hideshow-fold-p)
    (+fold-from-eol (hs-hide-level 0))))

(advice-add '+fold/open :around '+baba/fold-close-subtree)
(advice-add '+fold/close-all :after '+baba/fold-close-outline)
(advice-add '+fold/close-all :after 'evil-scroll-line-to-center)
(advice-add '+fold/open-all :after 'evil-scroll-line-to-center)

;; (add-hook 'octave-mode-hook
;;           (defun baba/outline-matlab ()
;;             "Fold definitions in Matlab."
;;             (setq outline-regexp
;;                   (rx (or
;;                        (group
;;                         (group (* space))
;;                         bow
;;                         (or "classdef" "function" "properties" "methods")
;;                         eow))))
;;             (baba/outline-overview)))

;; (add-hook 'f90-mode-hook
;;           (defun baba/outline-fortran ()
;;             "Fold definitions in Fortran."
;;             (setq outline-regexp
;;                   (rx (or
;;                        ;; Module and interface blocks.
;;                        (group (group (* space)) (or "module" "interface"))

;;                        ;; Procedures and type definitions.
;;                        (group
;;                         (group (* space))
;;                         (*? (or "pure " "impure " "elemental "))
;;                         (or "function" "subroutine" "interface" "type" "type,")
;;                         (group (+ space))))))
;;             (baba/outline-overview)))

;; ;; Customize the distracting folding markers.
;; (set-display-table-slot
;;  standard-display-table
;;  'selective-display
;;  (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
;;    (vconcat (mapcar (lambda (c) (+ face-offset c)) org-ellipsis))))
