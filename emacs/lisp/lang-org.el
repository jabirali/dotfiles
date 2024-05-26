
   ;; (use-package org						; Note-taking on steroids
   ;;   :custom
   ;;   (org-adapt-indentation nil)
   ;;   (org-agenda-files (list NOTES))
   ;;   (org-agenda-skip-deadline-if-done t)
   ;;   (org-agenda-skip-scheduled-if-done t)
   ;;   (org-agenda-span 'day)
   ;;   (org-agenda-start-on-weekday nil)
   ;;   (org-agenda-window-setup 'only-window)
   ;;   (org-archive-location "::* Archive")
   ;;   (org-babel-results-keyword "results")
   ;;   (org-confirm-babel-evaluate nil)
   ;;   (org-ctrl-k-protect-subtree t)
   ;;   (org-directory NOTES)
   ;;   (org-fontify-quote-and-verse-blocks t)
   ;;   (org-highlight-latex-and-related '(native latex script entities))
   ;;   (org-image-actual-width '(400))
   ;;   (org-latex-engraved-theme 'ef-melissa-light)
   ;;   (org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true}\n")
   ;;   (org-latex-packages-alist '(("" "microtype" t)))
   ;;   (org-latex-src-block-backend 'engraved)
   ;;   (org-pretty-entities t)
   ;;   (org-pretty-entities-include-sub-superscripts nil)
   ;;   (org-return-follows-link t)
   ;;   (org-startup-folded 'fold)
   ;;   (org-startup-indented t)
   ;;   (org-tags-column -65)
   ;;   (org-todo-keywords
   ;;    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
   ;; 	 (sequence "WAIT(w)" "HOLD(h)" "READ(r)" "IDEA(*)" "|" "NOTE(-)" "STOP(s)")))
   ;;   :config
   ;;   (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
   ;;   (org-link-set-parameters "zotero" :follow #'+url-handler-zotero))
   ;; (use-package org-download				; Drag-and-drop into notes
   ;;   :ensure t
   ;;   :after org
   ;;   :custom
   ;;   (org-download-method 'directory)
   ;;   (org-download-image-dir "assets")
   ;;   (org-download-heading-lvl nil)
   ;;   (org-download-timestamp "%Y%m%d%H%M%S")
   ;;   :config
   ;;   (defun +org-download-file-format (filename)
   ;; 	"Purely date-based naming of attachments."
   ;; 	(concat
   ;; 	 (format-time-string org-download-timestamp)
   ;; 	 "."
   ;; 	 (file-name-extension filename)))
   ;;   (setq org-download-file-format-function #'+org-download-file-format)
   ;;   (setq org-download-annotate-function (lambda (_link) ""))
   ;;   (org-download-enable)
   ;;   :bind (:map org-mode-map
   ;; 			  ("M-V" . org-download-clipboard)))
   ;; (use-package org-super-agenda			; Sort the agenda by project
   ;;   :ensure t
   ;;   :after org
   ;;   :custom
   ;;   (org-super-agenda-groups '((:auto-parent t)))
   ;;   :config
   ;;   (setq org-super-agenda-header-map (make-sparse-keymap))
   ;;   (org-super-agenda-mode 1))
   ;; (use-package ox-pandoc					; Org-export to anything
   ;;   :ensure t)
   ;; (use-package prescient
   ;;   :ensure t)
   ;; (use-package project)
   ;; (use-package python
   ;;   :config
   ;;   (defun +run-ipython ()
   ;; 	"Run IPython in a way that is TRAMP-compatible."
   ;; 	(interactive)
   ;; 	(run-python (format "%s --simple-prompt --classic" (executable-find "ipython" t)) :show t))
   ;;   :custom
   ;;   (python-indent-guess-indent-offset t)
   ;;   (python-indent-guess-indent-offset-verbose nil))
   ;; ;; (use-package reftex
   ;; ;;   :ensure t
   ;; ;;   :after tex
   ;; ;;   :custom
   ;; ;;   (reftex-cite-format 'bibtex)
   ;; ;;   (reftex-enable-partial-scans t)
   ;; ;;   (reftex-plug-into-AUCTeX t)
   ;; ;;   (reftex-save-parse-info t)
   ;; ;;   (reftex-use-multiple-selection-buffers t)
   ;; ;;   :hook
   ;; ;;   (TeX-mode . turn-on-reftex))

   ;; (use-package idle-org-agenda
   ;;   :ensure t
   ;;   :after org
   ;;   :custom
   ;;   (idle-org-agenda-interval 3600)
   ;;   :config
   ;;   (idle-org-agenda-mode 1))
