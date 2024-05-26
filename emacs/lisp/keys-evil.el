;;; keys-evil.el --- Vim keybindings.

;;; Code:
;; Use Vim-like keybindings everywhere.
(use-package evil
  :ensure t
  :init
  (setopt evil-want-C-i-jump nil)
  (setopt evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;; Use the modern Emacs implementation of redo.
(setopt evil-undo-system 'undo-redo)


;; (use-package evil						; Vim for the Emacs OS
;;   :ensure t
;;   :custom
;;   (evil-respect-visual-line-mode t)
;;   (evil-want-integration t)
;;   (evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-motion-state-map (kbd "SPC") nil)
;;   (define-key evil-motion-state-map (kbd "RET") nil)
;;   (define-key evil-motion-state-map (kbd "TAB") nil))
;; (use-package evil-collection
;;   :ensure t
;;   :after evil
;;   :config
;;   (evil-collection-init))
;; (use-package evil-org
;;   :ensure t
;;   :after (evil org)
;;   :hook (org-mode . evil-org-mode))
;; (use-package evil-org-agenda
;;   :after evil-org
;;   :config (evil-org-agenda-set-keys))
;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))
;; (use-package evil-tex
;;   :ensure t
;;   :hook
;;   (LaTeX-mode . evil-tex-mode))

(provide 'keys-evil)
;;; keys-evil.el ends here
