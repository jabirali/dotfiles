#+begin_src elisp :tangle packages.el
;;; -*- no-byte-compile: t; -*-


;; This file defines what additional packages to install. Declare them with the
;; `package!' macro, then run 'doom refresh' on the command line afterwards. To
;; fetch packages from standard MELPA/ELPA repos, just use `package!' directly;
;; to install directly from another repo, add the straight.el `:recipe' keyword.
;; It's also possible to use `:disable' keyword to disable built-in packages.
(package! hl-line :disable t)
