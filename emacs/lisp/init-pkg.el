;;; init-pkg.el --- Package management.

;; Load the built-in package manager.
(require 'package)

;; Add the community package repo.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Compile packages on installation.
(setopt package-native-compile t)

;; Don't spam my *Messages* with other people's mistakes.
(setopt native-comp-async-report-warnings-errors nil)

;; Okay, let's go!
(package-initialize)

(provide 'init-pkg)
;;; init-pkg.el ends here.
