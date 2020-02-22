#------------------------------------------------------------
# File: Latexmk config
# Path: ~/.config/latexmk/latexmkrc
#------------------------------------------------------------

# Always use normalized ISBN13 codes.
$biber='biber --isbn13 --isbn-normalise %O %S';

# Compile to PDF via LuaLaTeX by default.
$pdf_mode=4;

# Trick LatexMk into always using LuaLaTeX,
# regardless of what e.g. Emacs requests.
$lualatex='lualatex -file-line-error -synctex=1 %O %S';
#$pdflatex=$lualatex;

# Continuous compilation by default.
$preview_continuous_mode=1;
