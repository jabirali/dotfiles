#!/usr/bin/env fish

function ssh-fzf -d 'Connect via SSH'
	while true
		ssh ( sed -ne 's/^\s*Host \([^*].*\)/\1/p' ~/.ssh/config \
			| fzf --prompt "SSH> " --preview="echo -e '\e[31m# Workspaces\e[0m'; ssh {} tmux list-windows 2>/dev/null || echo 'None.'") \
				  -tt "fish -c \"tmux attach || tmux\" || tmux attach || tmux || fish || bash"
	end
end
