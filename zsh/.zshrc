# Modules
autoload -Uz compinit && compinit
autoload -U colors && colors

# Zsh config
HISTFILE=~/.cache/zsh/history
HISTSIZE=1000000
SAVEHIST=10000
PROMPT="%{$fg[blue]%}%~ $ %{$reset_color%}"

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

# Editor choice
if [ -z "$SSH_CLIENT" ]; then
	# Sublime Text locally
	export EDITOR="subl -nw"
else
	# Sublime Text over SSH
	export EDITOR="rmate -w"
fi
alias e="${=EDITOR}"

# Aliases
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
