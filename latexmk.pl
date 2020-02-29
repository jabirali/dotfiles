#------------------------------------------------------------
# File: Latexmk config
# Path: ~/.config/latexmk/latexmkrc
#------------------------------------------------------------

# Always use normalized ISBN13 codes.
$biber='biber --isbn13 --isbn-normalise %O %S';

# Compile to PDF via LuaLaTeX by default.
$pdf_mode=4;

# Always compile with SyncTeX on.
$lualatex='lualatex -file-line-error -shell-escape -synctex=1 %O %S';
$pdflatex='pdflatex -file-line-error -shell-escape -synctex=1 %O %S';

# Continuous compilation by default.
$preview_continuous_mode=1;
