
   ;; (use-package adaptive-wrap				; Vim-like line wrapping
   ;;   :ensure t
   ;;   :hook
   ;;   (text-mode . visual-line-mode)
   ;;   (markdown-mode . adaptive-wrap-prefix-mode)
   ;;   (latex-mode . adaptive-wrap-prefix-mode))

   ;; (use-package cdlatex					; Speedrunning LaTeX
   ;;   :ensure t
   ;;   :hook
   ;;   ((TeX-mode . turn-on-cdlatex)
   ;;    (org-mode . turn-on-org-cdlatex)))

   ;; (use-package tex
   ;;   :ensure auctex
   ;;   :custom
   ;;   (font-latex-fontify-script nil)
   ;;   (TeX-auto-save t)
   ;;   (TeX-source-correlate-method 'synctex)
   ;;   (TeX-source-correlate-mode t)
   ;;   (TeX-source-correlate-start-server t)
   ;;   (TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
   ;;   (TeX-view-program-selection '((output-pdf "Skim"))))
