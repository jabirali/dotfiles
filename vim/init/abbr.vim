vim9script

def g:Eatchar(pat: string): string
	# Eat one character from the typeahead buffer, and return an
	# empty string as the replacement. This can be used to avoid
	# extra spaces after expanding the built-in vim abbreivations.
	getchar(0)
	return ''
enddef

def MathAbbr(input: string, output: string)
	execute 'iabbrev ;' .. input .. ' ' .. output .. '<c-r>=Eatchar(''\s'')<cr>'
enddef

MathAbbr('a', 'α')
MathAbbr('b', 'β')
MathAbbr('c', 'ξ')
MathAbbr('d', 'δ')
MathAbbr('e', 'ε')
MathAbbr('f', 'φ')
MathAbbr('g', 'γ')
MathAbbr('h', 'θ')
MathAbbr('i', 'ι')
MathAbbr('j', 'ϊ') # TODO
MathAbbr('k', 'κ')
MathAbbr('l', 'λ')
MathAbbr('m', 'μ')
MathAbbr('n', 'ν')
MathAbbr('o', 'ο')
MathAbbr('p', 'π')
MathAbbr('q', 'ψ')
MathAbbr('r', 'ρ')
MathAbbr('s', 'σ')
MathAbbr('t', 'τ')
MathAbbr('u', 'υ') # TODO
MathAbbr('v', 'ϋ') # TODO
MathAbbr('w', 'ω')
MathAbbr('x', 'χ')
MathAbbr('y', 'η')
MathAbbr('z', 'ζ')

MathAbbr('A', 'Α') # TODO
MathAbbr('B', 'Β') # TODO
MathAbbr('C', 'Ξ')
MathAbbr('D', 'Δ')
MathAbbr('E', 'Ε') # TODO
MathAbbr('F', 'Φ')
MathAbbr('G', 'Γ')
MathAbbr('H', 'Θ')
MathAbbr('I', 'Ι') # TODO
MathAbbr('J', 'Ϊ') # TODO
MathAbbr('K', 'Κ') # TODO
MathAbbr('L', 'Λ')
MathAbbr('M', 'Μ') # TODO
MathAbbr('N', 'Ν') # TODO
MathAbbr('O', 'Ο') # TODO
MathAbbr('P', 'Π')
MathAbbr('Q', 'Ψ')
MathAbbr('R', 'Ρ') # TODO
MathAbbr('S', 'Σ')
MathAbbr('T', 'Τ') # TODO
MathAbbr('U', 'Υ') # TODO
MathAbbr('V', 'Ϋ') # TODO
MathAbbr('W', 'Ω')
MathAbbr('X', 'Χ') # TODO
MathAbbr('Y', 'Η') # TODO
MathAbbr('Z', 'Ζ') # TODO

# TODO: Numbers.

