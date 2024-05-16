# ~/.zshrc: Configuration for interactive shells.

# Load plugins.
source $ANTIDOTE_HOME/antidote/antidote.zsh
antidote load

# Custom settings.
# prompt belak
bindkey -e
setopt autocd
zstyle ':completion:*' menu select

# Prompt.
function precmd {
    # Whitespace after command output.
    print;

    # Reset the PROMPT.
    PROMPT=''
    RPROMPT=''

    # Are we in an SSH session?
    if [ -n "$SSH_CLIENT" ]; then
        PROMPT+='%F{green}%n@%m%f '
    fi

    # Which folder are we in?
    PROMPT+='%F{blue}%~%f '

    # Are we in Git?
    if git rev-parse --is-inside-work-tree &>/dev/null; then
        RPROMPT+='%F{red}git %f'
    fi

    # Is Direnv active?
    if [ -n "$DIRENV_DIR" ]; then
        RPROMPT+='%F{red}env %f'
    fi

    # Two-line prompt.
    PROMPT+=$'\n'

    # Prompt character.
    PROMPT+="❯ "
}

function preexec {
    # Whitespace before command output.
    print;
}

# Shell integrations
function hook {
	# Run a command to integrate with external tools,
	# but only on systems where the command exists.
	if which $1 &>/dev/null; then
		eval "$($@)"
	fi
}

hook fzf --zsh
hook conda shell.zsh hook
hook direnv hook zsh

# Default arguments
alias bat="bat -p"
alias exa='exa --group-directories-first --time-style=long-iso'
alias grep="grep --color"
alias ipython='TERM=linux ipython'  # ANSI colors
alias less="less -R"
alias ls="ls --color --human-readable --dereference --group-directories-first --time-style=long-iso --hyperlink=auto"
alias matlab='matlab -nosplash -nodesktop'  # CLI mode
alias wget='wget -e robots=off'  # Web scraping

# Intuitive commands
alias queue="squeue -u jabirali"
alias hours="cost -d"

# Command abbreviations
alias e="${=EDITOR}"
alias f="tail -n 1024 -f"
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

function dot {
    # Dotfile management: Execute a Git command in ~/.config using a subshell.
    (
        cd ~/.config
        $@
    )
}

function git-edit {
    # Edit a Git file using FZF.
    git rev-parse --is-inside-work-tree 1>/dev/null || return
    file="$(git ls-files | fzf)"
    if [ -e "$file" ]; then
        e "$file"
    fi
}

alias da="dot git add"
alias db="dot git branch"
alias dc="dot git commit"
alias dd="dot git diff"
alias de="dot git-edit"
alias dD="dot git diff --cached"
alias df="dot git fetch"
alias dm="dot git merge"
alias dp="dot git pull"
alias dP="dot git push"
alias dr="source ~/.zshrc"
alias ds="dot git status"
alias dz="dot git stash"

alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gd="git diff"
alias ge="git-edit"
alias gD="git diff --cached"
alias gf="git fetch"
alias gm="git merge"
alias gp="git pull"
alias gP="git push"
alias gs="git status"
alias gz="git stash"

# Named directories
hash -d c=~/Code
hash -d d=~/Code/config
