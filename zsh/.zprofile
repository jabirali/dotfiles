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
	# Local session
	export EDITOR="subl -nw"
else
	# Remote session
	export EDITOR="rmate -w"
fi

# Application settings
export BAT_STYLE="plain"
export BAT_THEME="ansi"
export COLORTERM="truecolor"
export DIRENV_LOG_FORMAT=
export MPLBACKEND="module://itermplot"
