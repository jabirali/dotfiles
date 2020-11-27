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
 (fold)
 (format +onsave)
 (file-templates)
 (snippets)
 :emacs
 (dired +ranger +icons)
 (vc)
 (electric)
 (undo)
 :checkers
 (syntax)
 (spell +everywhere)
 :term
 (eshell)
 (vterm)
 :tools
 (direnv)
 (editorconfig)
 (magit)
 (lookup +docsets)
 (eval +overlay)
 (lsp)
 (pdf)
 :os
 (:if IS-MAC macos)
 :lang
 (org)
 (markdown)
 (latex +latexmk)
 (emacs-lisp)
 (sh +fish)
 (python +lsp +pyright)
 (julia +lsp)
 :email
 :app
 :config
 (literate)
 (default +bindings +smartparens))
