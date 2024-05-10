# ~/.zprofile: Configuration for login shells.

# Command path
path=(
	# Personal scripts
	~/.config/bin

	# Python environments
	/opt/intel/bin
	/opt/conda/bin

	# HomeBrew commands
	/opt/homebrew/bin
	/opt/homebrew/opt/coreutils/libexec/gnubin
	/opt/homebrew/opt/grep/libexec/gnubin

	# MacOS applications
	/Library/TeX/texbin
	/Applications/Matlab.app/bin

	# System defaults
	$path
)

# Editor choice
if [ -z "$SSH_CLIENT" ]; then
	export EDITOR="subl -nw"
else
	export EDITOR="rmate -w"
fi

# Plugin manager
export ANTIDOTE_HOME=~/.cache/zsh/plugins
if [[ ! -d $ANTIDOTE_HOME/antidote ]]; then
    git clone https://github.com/mattmc3/antidote $ANTIDOTE_HOME/antidote
fi

# Application settings
export BAT_STYLE="plain"
export BAT_THEME="ansi"
export COLORTERM="truecolor"
export DIRENV_LOG_FORMAT=
export MPLBACKEND="module://itermplot"

# Message of the day
if [ -e ~/.motd ]; then
    cat ~/.motd
fi