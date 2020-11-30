;;; -*- lexical-binding: t; -*-
;;; config.el -- custom configuration for doom emacs

(setq user-full-name "Jabir Ali Ouassou"
      user-mail-address "jabirali@switzerlandmail.ch")

(setq org-directory "~/iCloud/Notes/"
      org-roam-directory "~/iCloud/Notes/"
      org-agenda-files '("~/iCloud/Notes/Journal/" "~/iCloud/Notes/Projects/")
      org-roam-dailies-directory "~/iCloud/Notes/Journal/"
      org-roam-index-file "~/iCloud/Notes/index.org")

(setq reftex-default-bibliography "~/Library/Zotero/Library.bib"
      bibtex-completion-bibliography '("~/Library/Zotero/Library.bib")
      org-ref-default-bibliography '("~/Library/Zotero/Library.bib"))

(setq doom-theme 'doom-one
      doom-font "Monaco-12"
      doom-variable-pitch-font "Monaco-12")

(setq display-line-numbers-type t)

(add-to-list 'default-frame-alist '(fullscreen  . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

(map!
 "s-[" 'evil-window-prev
 "s-]" 'evil-window-next
 "s-{" '+workspace/switch-left
 "s-}" '+workspace/switch-right
 "s-d" 'evil-window-vsplit
 "s-D" 'evil-window-split)

(map!
 "s-<up>"    'evil-window-up
 "s-<down>"  'evil-window-down
 "s-<left>"  'evil-window-left
 "s-<right>" 'evil-window-right)

(setq org-roam-capture-templates
      '(("c" "Concept note" plain
         (function org-roam-capture--get-point)
         "%?"
         :file-name "Brain/%<%Y%m%d%H%M%S>"
         :head "#+title: ${title}\n\n"
         :unnarrowed t)
        ("l" "Literature note" plain
         (function org-roam-capture--get-point)
         "%?"
         :file-name "Brain/%<%Y%m%d%H%M%S>"
         :head "#+title: ${title}\n\n* Reference\n* Summary\n* Details\n"
         :unnarrowed t)
        ("p" "Project note" plain
         (function org-roam-capture--get-point)
         "%?"
         :file-name "Projects/%<%Y%m%d%H%M%S>"
         :head "#+title: ${title}\n\n* Motivation\n* Objective\n* Tasks\n* Resources\n"
         :unnarrowed t)))
