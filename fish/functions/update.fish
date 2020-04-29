#!/usr/bin/env fish

function update -d 'Update all command-line tools'
	echo -e "\n\e[1m:: Neovim plugins\e[0m"
	nvim +PlugStatus +only +PlugInstall +PlugUpdate +PlugUpgrade +UpdateRemotePlugins +qa
	
	echo -e "\n\e[1m:: Fish plugins\e[0m"
	fisher
	
	echo -e "\n\e[1m:: Tmux plugins\e[0m"
	echo -e 'Installing plugins!\n'
	~/.tmux/plugins/tpm/bin/install_plugins
	echo
	~/.tmux/plugins/tpm/bin/update_plugins all
end
