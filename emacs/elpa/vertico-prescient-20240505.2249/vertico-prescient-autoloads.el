;;; vertico-prescient-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from vertico-prescient.el

(defvar vertico-prescient-mode nil "\
Non-nil if Vertico-Prescient mode is enabled.
See the `vertico-prescient-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-prescient-mode'.")
(custom-autoload 'vertico-prescient-mode "vertico-prescient" nil)
(autoload 'vertico-prescient-mode "vertico-prescient" "\
Minor mode to use prescient.el in Vertico menus.

This mode will:
- if `vertico-prescient-override-sorting' is non-nil,
  configure `vertico-sort-override-function' and set
 `vertico-prescient-enable-filtering' to t

- if `vertico-prescient-enable-filtering' is non-nil,
  configure `vertico-sort-function'

- if `vertico-prescient-enable-filtering' is non-nil:
  - bind `prescient-toggle-map' to `M-s' in `vertico-map'
  - change `completion-styles' to `vertico-prescient-completion-styles'
  - apply `vertico-prescient-completion-category-overrides'
    to `completion-category-overrides'
  - set `completion-category-defaults' to nil

- insert code for remembering candidates into `minibuffer-exit-hook'
  and `post-command-hook'.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Prescient mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-prescient-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "vertico-prescient" '("vertico-prescient-"))

;;; End of scraped data

(provide 'vertico-prescient-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; vertico-prescient-autoloads.el ends here
