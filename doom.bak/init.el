;;; ~/.config/doom/init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'SPC h d h' for the documentation, and run 'doom sync' after updates.
;; You can also press 'gd' on a module name to jump to its source directory.

(doom!
 :input

 :checkers
 (spell)
 (syntax)

 :completion
 (company)
 (ivy)

 :ui
 (doom)
 (doom-dashboard)
 (doom-quit)
 (hl-todo)
 (hydra)
 (modeline)
 (nav-flash)
 (ophints)
 (popup +all +defaults)
 (vc-gutter)
 (vi-tilde-fringe)
 (window-select +numbers)
 (workspaces)
 (zen)

 :editor
 (evil +everywhere)
 (file-templates)
 (fold)
 (format +onsave)
 (multiple-cursors)
 (rotate-text)
 (snippets)

 :emacs
 (dired +ranger)
 (electric)
 (ibuffer)
 (vc)

 :term
 (eshell)
 (vterm)

 :tools
 (editorconfig)
 (eval +overlay)
 (lookup +docsets)
 (magit)
 (pdf)

 :lang
 (cc)
 (data)
 (emacs-lisp)
 (julia)
 (latex +latexmk)
 (markdown)
 (org +dragndrop +present)
 (python)
 (sh)

 :email
 (mu4e)

 :app
 (calendar)

 :config
 (default +bindings +smartparens))