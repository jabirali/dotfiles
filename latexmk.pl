#------------------------------------------------------------
# File: Latexmk config
# Path: ~/.config/latexmk/latexmkrc
#------------------------------------------------------------

$biber='biber --isbn13 --isbn-normalise %O %S';
$pdf_mode=4;
$pdf_previewer="start evince %O %S";
$preview_continuous_mode=1;