#!/usr/bin/env fish

function vpn -d 'Connect to VPN'
	# Disconnect from current server.
	expressvpn disconnect
	
	# Select an ExpressVPN server via FZF.
	set line (expressvpn list all 1>| sed -e '1,/^---/ d' | fzf --prompt='ExpressVPN> ')
	
	# If a choice was made, extract name.
	if [ -n "$line" ]
		set name (echo $line | sed -e 's/\(\S*\).*/\1/')
	else
		return 1
	end
	
	# Connect to the chosen server.
	expressvpn connect $name
end
