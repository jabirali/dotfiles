vim9script

if exists('$TMUX')
  g:fzf_layout = { 'tmux': '-p90%,60%' }
else
  g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6 } }
endif

# Keybindings for the FZF fuzzy finder. These bindings use <C-s>
# as a prefix; this makes sense for 'search', is not used by Vim
# by default, and works across both normal and terminal modes.

# Function for automating mappings.
def MapSearch(key: string, cmd: string)
	execute 'nnoremap <C-s>' .. key .. ' <C-w>:' .. cmd .. '<cr>'
	execute 'tnoremap <C-s>' .. key .. ' <C-w>:' .. cmd .. '<cr>'
enddef

# Commands for custom FZF calls.
command Dotfiles call fzf#run(fzf#wrap({'source': 'git -C ~/.config ls-files | sed s_^_~/.config/_'}))
command Projects call fzf#run(fzf#wrap({'source': 'find ~/Code -name .git | sed s/\.git$//', 'sink': 'cd'}))
command Zotero call fzf#run(fzf#wrap({'source': 'find ~/Zotero -iname "*.pdf"', 'sink': '!open'}))

# Perform the keymapping.
call MapSearch('s', 'GitFiles')
call MapSearch('a', 'Rg')
call MapSearch('b', 'Buffers')
call MapSearch('d', 'Dotfiles')
call MapSearch('f', 'Files')
call MapSearch('h', 'Helptags')
call MapSearch('l', 'BLines')
call MapSearch('p', 'Projects')
call MapSearch('w', 'WikiFzfPages')
call MapSearch('z', 'Zotero')
