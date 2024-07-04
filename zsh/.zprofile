# ~/.zprofile: Configuration for login shells.

# Environment variables. Sourcing .zshenv shouldn't be necessary, but it is
# on MacOS and some Linux distros due to what they place in /etc/zprofile.
source ~/.zshenv

# Message of the day
if [ -e ~/.motd ]; then
	cat ~/.motd
fi

# Install plugin manager
export ANTIDOTE_HOME=~/.cache/zsh/plugins
if [[ ! -d $ANTIDOTE_HOME/antidote ]]; then
	git clone https://github.com/mattmc3/antidote $ANTIDOTE_HOME/antidote
fi

# Setting PATH for Python 3.12
# The original version is saved in .zprofile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.12/bin:${PATH}"
export PATH
