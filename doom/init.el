;;; -*- lexical-binding: t; -*-
;;; init.el -- high-level package management for doom emacs

(doom!

:os (:if IS-MAC macos)

:config
(literate)

:lang
(org)
(markdown)
(latex +latexmk)

:lang
(emacs-lisp)
(sh +fish)

:lang
(python +lsp +pyright)

:lang
(data)

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

:email
(mu4e)

:config
(default +bindings +smartparens)

)
