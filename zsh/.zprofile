# ~/.zprofile: Configuration for login shells.

# Command path
path=(
	# Personal
	~/.config/bin

	# Python environments
	/opt/intel/bin
	/opt/conda/bin

	# HomeBrew commands
	/opt/homebrew/bin

	# GNU environment
	/opt/homebrew/opt/coreutils/libexec/gnubin
	/opt/homebrew/opt/grep/libexec/gnubin

	# MacOS applications
	/Library/TeX/texbin
	/Applications/Matlab.app/bin

	# System defaults
	$path
)

# System settings
export COLORTERM="truecolor"
export EDITOR="vim"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# Application settings
export BAT_STYLE="plain"
export BAT_THEME="ansi"
export DIRENV_LOG_FORMAT=
export MPLBACKEND="module://itermplot"

# Message of the day
if [ -e ~/.motd ]; then
	cat ~/.motd
fi

# Install plugin manager
export ANTIDOTE_HOME=~/.cache/zsh/plugins
if [[ ! -d $ANTIDOTE_HOME/antidote ]]; then
	git clone https://github.com/mattmc3/antidote $ANTIDOTE_HOME/antidote
fi
