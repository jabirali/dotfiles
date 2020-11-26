;;; -*- lexical-binding: t; -*-
;;; init.el -- high-level package management for doom emacs

;; This file controls what Doom modules are enabled and what order they load.
;; You can use 'K' and 'gd' to inspect a module or flag, and should run 'doom
;; sync' after modifying this file to ensure that Doom Emacs is up to date.

(doom! :os
       (:if IS-MAC macos)

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
       (default +bindings +smartparens))
