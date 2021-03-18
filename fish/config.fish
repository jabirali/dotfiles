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

# Fish syntax highlighting
#theme_gruvbox dark medium

# set -g fish_color_autosuggestion '555'  'brblack'
# set -g fish_color_cancel -r
# set -g fish_color_command --bold
# set -g fish_color_comment red
# set -g fish_color_cwd green
# set -g fish_color_cwd_root red
# set -g fish_color_end brmagenta
# set -g fish_color_error brred
# set -g fish_color_escape 'bryellow'  '--bold'
# set -g fish_color_history_current --bold
# set -g fish_color_host normal
# set -g fish_color_match --background=brblue
# set -g fish_color_normal normal
# set -g fish_color_operator bryellow
# set -g fish_color_param cyan
# set -g fish_color_quote yellow
# set -g fish_color_redirection brblue
# set -g fish_color_search_match 'bryellow'  '--background=brblack'
# set -g fish_color_selection 'white'  '--bold'  '--background=brblack'
# set -g fish_color_user brgreen
# set -g fish_color_valid_path --underline

# Integrate with `fzf`.
# fzf_key_bindings

# Integrate with `direnv`.
#eval (direnv hook fish)
#__direnv_export_eval

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

# Activate starship.
starship init fish | source
