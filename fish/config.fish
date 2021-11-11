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
alias bat    'bat -p --theme ansi'
set -g fish_user_paths "/usr/local/opt/binutils/bin" $fish_user_paths

# Activate miniconda.
# eval /usr/local/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source

# Activate starship.
# starship init fish | source

# Monokai colors.
set fish_color_normal normal
set fish_color_command yellow --bold
set fish_color_quote gray
set fish_color_redirection red
set fish_color_end cyan
set fish_color_error red
set fish_color_param normal
set fish_color_comment gray
set fish_color_match F8F8F2
set fish_color_search_match --background=49483E
set fish_color_operator red
set fish_color_escape 66D9EF
set fish_color_cwd 66D9EF
set fish_pager_color_prefix F8F8F2
set fish_pager_color_completion 75715E
set fish_pager_color_description 49483E
set fish_pager_color_progress F8F8F2
set fish_pager_color_secondary F8F8F2


fish_add_path /usr/local/bin
fish_add_path /Library/TeX/texbin

zoxide init fish --cmd cd | source

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /usr/local/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

