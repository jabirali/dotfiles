;;; lang-python.el --- Initialize `python-mode' and friends.

;; The interactive features of IPython are not as important inside
;; Emacs. However, it makes it easy to e.g. autoload Python modules,
;; and it is a prerequisite for inline plotting via `comint-mime'.
(when (executable-find "ipython3")
  (setq python-shell-interpreter "ipython3"
	python-shell-interpreter-args "--simple-prompt --classic"))

;; Show Matplotlib plots in Inferior Python buffers. The SVG format
;; ensures that the plots look crisp also on Retina / hiDPI screens.
(use-package comint-mime
  :ensure t
  :config
  (setopt comint-mime-prefer-svg t)
  :hook
  (inferior-python-mode . comint-mime-setup))

;; Provide Jupyter-like code cells for `python-mode', as well as
;; providing some support for reading/writing *.ipynb notebooks.
(use-package code-cells
  :ensure t
  :after python
  :bind* (:map code-cells-mode-map
	       ("M-p" . code-cells-backward-cell)
	       ("M-n" . code-cells-forward-cell)
	       ("M-RET" . code-cells-eval))
  :hook (python-mode . code-cells-mode)
  :config
  (set-face-attribute 'code-cells-header-line nil :overline nil :underline nil))

(provide 'lang-python)
;;; lang-python.el ends here
