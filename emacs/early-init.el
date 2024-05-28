;;; early-init.el --- Initialization before "starting" Emacs.

;; Prevent the `customize' cache from leaking into `init.el'.
(setq custom-file (concat user-emacs-directory "custom.el"))
(ignore-errors (load custom-file))

;; Make Emacs start quickly and act snappy.
(setq gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 1024 1024))

;; Disable window decorations, but keep MacOS rounded corners.
;; (add-to-list 'default-frame-alist '(undecorated-round . t))

(provide 'early-init)
;;; early-init.el ends here
