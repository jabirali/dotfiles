;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load.
;; You can use 'K' and 'gd' to inspect a module or flag, and should run 'doom
;; sync' after modifying this file to ensure that Doom Emacs is up to date.

(doom! :os
       (:if IS-MAC macos)

       :completion
       (company)
       (ivy)

       :ui
       (doom)
       (doom-dashboard)
       (doom-quit)
       (hl-todo)
       (indent-guides)
       (modeline)
       (nav-flash)
       (ophints)
       (popup +all +defaults)
       (tabs)
       (vc-gutter)
       (vi-tilde-fringe)
       (workspaces)

       :editor
       (evil +everywhere)
       (file-templates)
       (fold)
       (format +onsave)
       (snippets)

       :emacs
       (dired +ranger +icons)
       (electric)
       (undo)
       (vc)

       :term
       (eshell)
       (vterm)

       :checkers
       (syntax)
       (spell)

       :tools
       (direnv)
       (editorconfig)
       (eval +overlay)
       (lookup +docsets)
       (lsp +eglot)
       (magit)
       (pdf)

       :lang
       (org)
       (markdown)
       (emacs-lisp)
       (cc +lsp)
       (julia +lsp)
       (python +lsp)
       (latex +lsp +latexmk)
       (sh +lsp +fish)
       (data)

       :email
       (mu4e)

       :config
       (literate)
       (default +bindings +smartparens))
