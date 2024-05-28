;;; lang-org.el --- Note taking and task management

;;; Code:
;; Make the outline look like a "tree".
(setopt org-startup-indented t)

;; Prettify e.g. TeX math expressions.
(setopt org-pretty-entities t)
(setopt org-pretty-entities-include-sub-superscripts nil)

;; Press RET to follow links (like VimWiki etc.).
(setopt org-return-follows-link t)

;; Personal Wiki.
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n n" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(provide 'lang-org)
;;; lang-org.el ends here
