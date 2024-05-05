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

# Environment
export PATH=~/.config/bin:~/.emacs.d/bin:/opt/homebrew/Caskroom/miniconda/base/bin:/opt/homebrew/opt/gnupg@2.2/bin:/opt/homebrew/opt/coreutils/libexec/gnubin:/opt/homebrew/opt/grep/libexec/gnubin:~/.luarocks/bin:/usr/local/bin:/opt/homebrew/bin:/opt/homebrew/Caskroom/miniconda/base/bin/:/opt/conda/bin:/Library/TeX/texbin:/Applications/Matlab.app/bin:$PATH

if [ -z "$SSH_CLIENT" ]; then
	export EDITOR="subl -nw"
else
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
