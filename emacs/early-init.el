;;; ~/.config/emacs/early-init.el

;; Don't litter my init.el with the customization cache.
(setq custom-file (concat user-emacs-directory "custom.el"))
(ignore-errors (load custom-file))

;; Customize how new frames and windows look by default.
(setq-default left-margin-width 2 right-margin-width 2)
