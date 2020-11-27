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
 (modeline)
 (ophints)
 (popup +all +defaults)
 (workspaces)
 (hl-todo)
 (vc-gutter)

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
