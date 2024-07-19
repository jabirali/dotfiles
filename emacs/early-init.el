(setq custom-file (concat user-emacs-directory "custom.el"))
(ignore-errors (load custom-file))

(setq-default left-margin-width 2
              right-margin-width 2)

(add-to-list 'default-frame-alist '(internal-border-width . 0))

(setq gc-cons-threshold most-positive-fixnum)
