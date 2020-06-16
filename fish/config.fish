# vim: foldmethod=marker

# Fish configuration file. Note that much of my interesting modifications have
# been exported to separate plugins, which are now hosted on github.com/jabirali.

# Fish package manager.
if not functions -q fisher
	curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
	fish -c fisher
end

# Theme settings.
# set -x FZF_DEFAULT_OPTS '--color=bg+:#fdf6e3,bg:#fdf6e3,spinner:#2aa198,hl:#268bd2,fg:#657b83,header:#268bd2,info:#b58900,border:#d7d7af,pointer:#2aa198,marker:#2aa198,fg+:#073642,prompt:#b58900,hl+:#268bd2 --layout=reverse'

# ANSI colors.
set fish_color_autosuggestion normal
set fish_color_command blue
set fish_color_comment brred
set fish_color_end brmagenta
set fish_color_error brred
set fish_color_escape bryellow --bold
set fish_color_normal normal
set fish_color_operator bryellow
set fish_color_param normal
set fish_color_quote yellow
set fish_color_redirection green
set fish_color_user brgreen

# Plugin settings.
set projector_dir ~/Code
set projector_cmd edit +GFiles
set zotfile_root ~/snap/zotero-snap
set expressvpn_relink on

# Aliases for common actions.
alias exa    'exa --git-ignore --group-directories-first --time-style=long-iso'
alias mkvenv 'echo layout_python >> .envrc; direnv allow'
alias pytest 'fd \'.py$\' | entr pytest'
alias wget   'wget -e robots=off'
alias boox   'bluetooth-sendto --device=22:22:3B:D5:62:2C'

# Integrate external tools.
fzf_key_bindings
eval (direnv hook fish)
__direnv_export_eval

function venv-exec --description 'Run command in virtualenv'
	source "$HOME/.cache/virtualenvs/$argv[1]/bin/activate.fish"
	eval $argv[2..-1]
	deactivate
end

prefer 'exa -T'   'lsd --tree' 'tree'
prefer 'exa -l'   'lsd -l'     'ls -l'  'll'
prefer 'exa -la'  'lsd -la'    'ls -la' 'la'
prefer 'exa'      'lsd'        'ls'     'l'
prefer 'bat -p'   'batcat -p'  'cat'
prefer 'fd'       'fdfind'     'find'
prefer 'rg'       'ag'         'grep'
prefer 'htop'     'top'
prefer 'parallel' 'xargs'
