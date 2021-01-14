# vim: foldmethod=marker

# Fish configuration file. Note that much of my interesting modifications have
# been exported to separate plugins, which are now hosted on github.com/jabirali.
# Abbreviations are stored in `abbrfile` (handled by my plugin `fish-abbrfile`),
# and plugins are located in `fishfile` (handled by a package manager `fisher`).

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
# fzf_key_bindings

# Integrate with `direnv`.
eval (direnv hook fish)
__direnv_export_eval

# Plugin settings.
set projector_dir ~/Code
set projector_cmd edit +GFiles
set zotfile_root ~/snap/zotero-snap

# Common path options.
set -gx PATH ~/.config/bin ~/.config/emacs/bin ~/.local/bin $PATH

# Aliases for common actions.
alias exa    'exa --group-directories-first --time-style=long-iso'
alias mkvenv 'echo layout_python >> .envrc; direnv allow'
alias pytest 'fd \'.py$\' | entr pytest'
alias wget   'wget -e robots=off'
alias boox   'bluetooth-sendto --device=22:22:3B:D5:62:2C'
set -g fish_user_paths "/usr/local/opt/binutils/bin" $fish_user_paths

# Activate miniconda.
eval /usr/local/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
