;;; init-tui.el --- Setup the Terminal User Interface (TUI)

;;; Code:

;; Enable mouse support in the terminal.
;; (xterm-mouse-mode 1)

;; Let the terminal show frame title.
;; (setopt xterm-set-window-title t)

;; Make the tab bar a bit less noisy.
(setopt tab-bar-separator " ")

;; Enable extra keys in the Kitty terminal (super, hyper, etc.).
;; (use-package kkp)

(provide 'init-tui)
;;; init-tui.el ends here
