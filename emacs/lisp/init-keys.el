;;; init-keys.el --- Global keybindings

;;; Code:
;; More ergonomic window switching.
(bind-key "M-o" 'other-window)	

;; Browser-like keybindings for history navigation.
(bind-key "M-[" 'tab-bar-history-back)
(bind-key "M-]" 'tab-bar-history-forward)

;; Use Vim-like keybindings.
(use-package evil
  :ensure t
  :init
  (setopt evil-want-C-i-jump nil)
  (setopt evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;; Use the Emacs implementation of redo.
(setopt evil-undo-system 'undo-redo)


(provide 'init-keys)
;;; init-keys.el ends here
