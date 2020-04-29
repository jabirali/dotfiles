# ~/.config/fish/config.fish 
# vim: foldmethod=marker

# The most interesting parts of my `config.fish` has been exported as separate 
# Fish plugins on github.com/jabirali/fish-*, which can be installed via `fisher`.
# The specific list of Fish plugins that I use are registered in my `fishfile`.

# Fish package manager.
if not functions -q fisher
	curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
	fish -c fisher
end

# System settings.
set -x LANG 'en_US'
set -x LC_ALL 'en_DK.UTF-8'
set -x NNN_TRASH 1
set -x NNN_USE_EDITOR 1
set -x POETRY_VIRTUALENVS_PATH ~/.virtualenvs
set -x DOTNET_CLI_TELEMETRY_OPTOUT 1

# Theme settings.
set -x BAT_THEME 'ansi-light'
set -x FZF_DEFAULT_OPTS '--color=bg+:-1,bg:-1,spinner:#2aa198,hl:#268bd2,fg:#657b83,header:#268bd2,info:#b58900,border:#d7d7af,pointer:#2aa198,marker:#2aa198,fg+:#073642,prompt:#b58900,hl+:#268bd2 --layout=reverse'
set -x NNN_CONTEXT_COLORS 5555

# ANSI colors.
set fish_color_autosuggestion normal
set fish_color_command blue
set fish_color_comment brred
set fish_color_end brmagenta
set fish_color_error brred
set fish_color_escape bryellow --bold
set fish_color_normal normal
set fish_color_operator bryellow
set fish_color_param black
set fish_color_quote yellow
set fish_color_redirection green
set fish_color_user brgreen

# Plugin settings.
set -x projector_dir ~/Documents
set -x zotfile_root ~/snap/zotero-snap

# Aliases for common actions.
alias 'exa'    'exa --git-ignore --group-directories-first --time-style=long-iso'
alias 'mkvenv' 'python3 -m venv ~/.virtualenvs/(basename (pwd))'
alias 'open'   'xdg-open'
alias 'pytest' 'fd \'.py$\' | entr pytest'
alias 'wget'   'wget -e robots=off'

# Abbreviate common commands.
abbr -ga 'a' 'sudo apt'
abbr -ga 'g' 'git'
abbr -ga 'o' 'open'
abbr -ga 's' 'sudo snap'
abbr -ga 'u' 'update'
abbr -ga 'v' 'mkvenv'
abbr -ga 'z' 'zotfile'

# Fuzzy-finder integration.
fzf_key_bindings
