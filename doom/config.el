;;; -*- lexical-binding: t; -*-
;;; config.el -- custom configuration for doom emacs

(setq user-full-name "Jabir Ali Ouassou"
      user-mail-address "jabirali@switzerlandmail.ch")

(setq org-directory "~/iCloud/Org/"
      org-agenda-files '("~/iCloud/Org/")
      +org-capture-todo-file "~/iCloud/Org/Inbox.org")

(setq reftex-default-bibliography "~/Library/Zotero/Library.bib"
      bibtex-completion-bibliography '("~/Library/Zotero/Library.bib")
      org-ref-default-bibliography '("~/Library/Zotero/Library.bib"))

(setq doom-theme 'doom-one
      doom-font "Monaco-13"
      doom-variable-pitch-font "Monaco-13")

(add-to-list 'default-frame-alist '(fullscreen  . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

(setq display-line-numbers-type t)

(map!
 "s-[" 'evil-window-prev
 "s-]" 'evil-window-next
 "s-{" '+workspace/switch-left
 "s-}" '+workspace/switch-right
 "s-d" 'evil-window-vsplit
 "s-D" 'evil-window-split)
