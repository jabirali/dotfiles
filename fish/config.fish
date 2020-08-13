# vim: foldmethod=marker

# Fish configuration file. Note that much of my interesting modifications have
# been exported to separate plugins, which are now hosted on github.com/jabirali.

# Fish package manager.
if not functions -q fisher
	curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
	fish -c fisher
end

# ANSI colors.
set fish_color_autosuggestion normal
set fish_color_command brwhite --bold
set fish_color_comment brred
set fish_color_end brmagenta
set fish_color_error brred
set fish_color_escape bryellow --bold
set fish_color_normal normal
set fish_color_operator bryellow
set fish_color_param normal
set fish_color_quote yellow
set fish_color_redirection green

# Integrate with `fzf`.
fzf_key_bindings

# Integrate with `direnv`.
eval (direnv hook fish)
__direnv_export_eval

# Plugin settings.
set projector_dir ~/Code
set projector_cmd edit +GFiles
set zotfile_root ~/.local/share/zotero

# Common path options.
set -gx PATH ~/.local/bin $PATH

# Aliases for common actions.
alias exa    'exa --git-ignore --group-directories-first --time-style=long-iso'
alias mkvenv 'echo layout_python >> .envrc; direnv allow'
alias pytest 'fd \'.py$\' | entr pytest'
alias wget   'wget -e robots=off'
alias boox   'bluetooth-sendto --device=22:22:3B:D5:62:2C'

# Abbreviations.
# * These are located in `abbrfile`, and handled by my plugin `fish-abbrfile`.

# Extensions.
# * These are located in `fishfile`, and handled by the package manager `fisher`.
