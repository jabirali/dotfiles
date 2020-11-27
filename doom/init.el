;;; -*- lexical-binding: t; -*-
;;; init.el -- high-level package management for doom emacs

(doom!
 :input

 :completion
 (company)
 (ivy +icons)

 :ui
 (doom)
 (doom-dashboard)
 (doom-quit)
 (hl-todo)
 (indent-guides)
 (modeline)
 (ophints)
 (popup +all +defaults)
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

 :checkers
 (syntax)
 (spell +everywhere)

 :term
 (eshell)
 (vterm)

 :tools
 (direnv)
 (editorconfig)
 (eval +overlay)
 (lookup +docsets)
 (lsp +eglot)
 (magit)
 (pdf)

 :os
 (:if IS-MAC macos)

 :lang
 (org)
 (markdown)
 (latex +latexmk)
 (python +lsp +pyright)
 (emacs-lisp)
 (sh +fish)
 (data)

 :email
 (mu4e)

 :app

 :config
 (literate)
 (default +bindings +smartparens))
