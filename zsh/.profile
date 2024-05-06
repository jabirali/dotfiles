# ~/.profile: Settings used by all POSIX-like shells.
#             Note that Zsh is configured to read this.

# Settings that depend on SSH status.
if [ -z "$SSH_CLIENT" ]; then
	EDITOR="subl -nw"
else
	EDITOR="rmate -w"
fi

# Update $PATH.
PATH="$HOME/.config/bin:$PATH"

# Export to subshells.
export EDITOR PATH
