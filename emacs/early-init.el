;;; ~/.config/emacs/early-init.el
;; Prevent dumping the `customize' cache into my `init.el'.
(setq custom-file (concat user-emacs-directory "custom.el"))
(ignore-errors (load custom-file))

;; Make Emacs snappy again.
(setq gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 1024 1024))

;; Add some margins.
(setq-default left-margin-width 2
              right-margin-width 2)
