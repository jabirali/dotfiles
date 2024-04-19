(setq custom-file (concat user-emacs-directory "custom.el"))
(ignore-errors (load custom-file))

(setq-default left-margin-width 2 right-margin-width 2)

(setopt default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . light))

(setq gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 1024 1024))
