function xdg-open-with
	# Check what app to use for opening this file type.
	set app \
	(
		fdfind . {/usr,$HOME/.local}/share/applications/ -e desktop |\
		sed 's|.*/||'                                               |\
		sort -du                                                    |\
		fzf --preview 'cat {}'
	)
	
	# Change the default application and open file.
	if [ -n "$app" ]
		xdg-mime default $app (xdg-mime query filetype $argv)
		xdg-open $argv
	end
end

