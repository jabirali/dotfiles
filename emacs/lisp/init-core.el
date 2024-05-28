;;; init-core.el --- Core emacs functionality

;;; Code:
;; Notice when files change on disk (e.g. due to Git actions).
(global-auto-revert-mode 1)

;; Remember which files I visit.
(recentf-mode 1)

;; Remember which commands I run.
(savehist-mode 1)

;; Undo/redo splits and jumps.
(tab-bar-history-mode 1)

;; Disable welcome information.
(setopt inhibit-startup-screen t)
(setopt initial-scratch-message nil)
(fset 'display-startup-echo-area-message 'ignore)

(provide 'init-core)
;;; init-core.el ends here
