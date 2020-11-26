;;; -*- lexical-binding: t; -*-
;;; init.el -- high-level package management for doom emacs

(doom!

:os (:if IS-MAC macos)

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

       :term
       (eshell)
       (vterm)

       :checkers
       (syntax)
       (spell +everywhere)

       :tools
       (direnv)
       (editorconfig)
       (eval +overlay)
       (lookup +docsets)
       (lsp +eglot)
       (magit)
       (pdf)

       :lang
       ;;
       (markdown)
       (org)
       (latex +lsp +latexmk)

       (emacs-lisp)
       (python +lsp +pyright)
       (sh +lsp +fish)

       ;; CSV
       (data)

       :email
       (mu4e)

       :config
       (literate)
       (default +bindings +smartparens)

)
