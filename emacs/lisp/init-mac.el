;;; init-mac.el --- Configuration settings specific to MacOS.

;;; Code:
;; Choose which modifier keys to use when running in a GUI.
(setopt ns-function-modifier 'hyper)
(setopt ns-control-modifier 'control)
(setopt ns-option-modifier 'alt)
(setopt ns-command-modifier 'meta)

;; Ensure that a transparent title bar remains readable when you
;; switch between light and dark modes. (Doesn't fit `tab-bar'.)
;; (use-package ns-auto-titlebar
;;  :config
;;  (ns-auto-titlebar-mode 1))

;; Fix $PATH when launching MacOS GUI frames without a terminal.
;; (package-vc-install "https://github.com/purcell/exec-path-from-shell")
;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

;; Alternative to `pixel-scroll-precision-mode'. (Requires Emacs Mac port.)
;; (package-vc-install "https://github.com/jdtsmith/ultra-scroll-mac.git")
;; (use-package ultra-scroll-mac
;;   :ensure t
;;   :if (eq window-system 'mac)
;;   :init
;;   (setq scroll-conservatively 101)
;;   (setq scroll-margin 0)
;;   :config
;;   (ultra-scroll-mac-mode 1))

(provide 'init-mac)
;;; init-mac.el ends here
