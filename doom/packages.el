;;; ~/.config/doom/packages.el -*- no-byte-compile: t; -*-

;; This file defines what additional packages to install. Declare them with the
;; `package!' macro, then run 'doom refresh' on the command line afterwards. To
;; fetch packages from standard MELPA/ELPA repos, just use `package!' directly;
;; to install directly from another repo, add the straight.el `:recipe' keyword.
;; It's also possible to use `:disable' keyword to disable built-in packages.

(package! company-fish :recipe (:host github :repo "CeleritasCelery/company-fish"))
(package! evil-smartparens)
(package! org-ref)

(package! hl-line :disable t)
(package! vi-tilde-fringe :disable t)
;(package! mu4e-maildirs-extension :disable t)
