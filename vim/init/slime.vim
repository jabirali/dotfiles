vim9script

g:slime_target = "vimterminal"

def g:SlimeMap()
	nmap <buffer> <C-cr> vih:SlimeSend<cr>
	nmap <buffer> <S-cr> vih:SlimeSend<cr>]h
enddef
