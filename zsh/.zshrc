# ~/.zshrc: Configuration for interactive shells.

# Load plugins.
source /opt/homebrew/opt/antidote/share/antidote/antidote.zsh
antidote load
prompt belak

# Set custom options
setopt autocd
zstyle ':completion:*' menu select

# Integrations
eval "$(fzf --zsh 2>/dev/null)"
eval "$(conda shell.zsh hook 2>/dev/null)"

# Default arguments
alias ls="ls --color --human-readable --dereference --group-directories-first --time-style=long-iso --hyperlink=auto"
alias grep="grep --color"
alias bat="bat -p"
alias exa='exa --group-directories-first --time-style=long-iso'
alias ipython='TERM=linux ipython'  # ANSI colors
alias matlab='matlab -nosplash -nodesktop'  # CLI mode
alias wget='wget -e robots=off'  # Web scraping

# Command abbreviations
alias e="${=EDITOR}"
alias o="open"
alias p="qlmanage -p"

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

# Message of the day
if [ -e ~/.motd ]; then
    cat ~/.motd
fi