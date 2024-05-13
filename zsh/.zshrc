# ~/.zshrc: Configuration for interactive shells.

# Load plugins.
source $ANTIDOTE_HOME/antidote/antidote.zsh
antidote load

# Prompt settings
prompt belak

# Set custom options
setopt autocd
zstyle ':completion:*' menu select

# Shell integrations
hook() {
	# Integrate external executables into Zsh when available.
	if [ -x "$(command -v $1)" ]; then
		eval "$( $@ )"
	fi
}

hook fzf --zsh
hook conda shell.zsh hook
hook direnv hook zsh

# Default arguments
alias ls="ls --color --human-readable --dereference --group-directories-first --time-style=long-iso --hyperlink=auto"
alias grep="grep --color"
alias bat="bat -p"
alias exa='exa --group-directories-first --time-style=long-iso'
alias ipython='TERM=linux ipython'  # ANSI colors
alias matlab='matlab -nosplash -nodesktop'  # CLI mode
alias wget='wget -e robots=off'  # Web scraping

# Intuitive commands
alias jobs="squeue -u jabirali"
alias hours="cost -d"

# Command abbreviations
alias e="${=EDITOR}"
alias o="open"
alias p="qlmanage -p"

alias l="ls"
alias ll='ls -l'
alias la="ls -la"

alias bb="brew sync"
alias bc="brew clean"
alias be="e ~/.config/Brewfile"
alias bi="brew install"
alias br="brew remove"
alias bu="brew upgrade"
alias bs="brew search"

alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gcm="git commit -m"
alias gca="git commit -am"
alias gd="git diff"
alias gdc="git diff --cached"
alias gf="git fetch"
alias gm="git merge"
alias gp="git pull"
alias gP="git push"
alias gs="git status"
alias gr="git reset"
