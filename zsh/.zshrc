# Modules
autoload -Uz compinit && compinit
autoload -U colors && colors

# Zsh config
HISTFILE=~/.cache/zsh/history
HISTSIZE=1000000
SAVEHIST=10000

setopt hist_ignore_dups
setopt hist_ignore_space
setopt share_history

setopt autocd
setopt extendedglob
setopt nomatch

# Keybindings
bindkey -e

# System path
path+=~/.config/bin
path+=/opt/conda/bin
path+=/opt/intel/bin
path+=/opt/homebrew/bin
path+=/opt/homebrew/opt/coreutils/libexec/gnubin
path+=/opt/homebrew/opt/grep/libexec/gnubin
path+=/opt/homebrew/Caskroom/miniconda/base/bin
path+=/Library/TeX/texbin
path+=/Applications/Matlab.app/bin

# Environment variables
if [ -z "$SSH_CLIENT" ]; then
	EDITOR="subl -nw"
	PROMPT="%{$fg[purple]%}%~ $ %{$reset_color%}"
else
	EDITOR="rmate -w"
	PROMPT="%{$fg[cyan]%}%~ $ %{$reset_color%}"
fi

# Aliases
alias e="${=EDITOR}"
alias ls="ls --color"
alias grep="grep --color"

# Integrations
eval "$(fzf --zsh 2>/dev/null)"
eval "$(conda shell.zsh hook 2>/dev/null)"

# Message of the day
if [ -e ~/.motd ]; then
    cat ~/.motd
fi

# Environment variables
export MPLBACKEND="module://itermplot"
export ITERMPLOT=rv
