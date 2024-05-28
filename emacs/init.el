;;; init.el --- Emacs configuration file.
;; This configuration is structured in a modular manner, so most of
;; the actual configuration can be found in the `lisp' subdirectory.
;; The exception is a few settings in `early-init.el' that must be
;; set before creating the first Emacs frame in order to be useful.

;; Add our custom modules in `~/.emacs.d/lisp' to the load path.
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Load my configuration layers.
(require 'init-pkg)
(require 'init-gui)
(require 'init-tui)
(require 'init-mac)

(require 'init-core)
(require 'init-look)
(require 'init-keys)

(require 'lang-python)
(require 'lang-org)

;; Benchmark numbers.
(message " Emacs started in %s " (emacs-init-time))

(provide 'init)
;;; init.el ends here
