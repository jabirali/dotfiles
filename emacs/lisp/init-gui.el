;;; init-gui.el --- Setup the Graphical User Interface (GUI).

;;; Code:
;; Turn off distractions.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; Turn on tab support.
(tab-bar-mode 1)

;; Make it easier to switch directly to a tab.
(setopt tab-bar-tab-hints t)

;; Resize pixel-by-pixel instead of char-by-char. Nice for tiling.
(setopt frame-resize-pixelwise t)

;; Modern mouse scrolling. Useful with a high-precision trackpad.
;; (There is also `ultra-scroll-mac', but only for the Mac port.)
(pixel-scroll-precision-mode 1)

;; This seems to reduce stuttering when scrolling?
(setopt fast-but-imprecise-scrolling t)

;; Add some breathing room around various interface elements.  Note
;; the minimal border width and fringe, since that makes e.g. the
(use-package spacious-padding
  :config
  (setopt spacious-padding-widths
	  '(:internal-border-width 0
	    :fringe-width 0
	    :tab-width 6
	    :mode-line-width 6))
  (spacious-padding-mode 1))

;; Add some breathing room around the window contents themselves.
;; The settings don't take effect until we `set-window-buffer'.
(setq-default left-margin-width 2
	      right-margin-width 2
	      line-spacing 0.15)
(set-window-buffer nil (current-buffer))

(provide 'init-gui)
;;; init-gui.el ends here
