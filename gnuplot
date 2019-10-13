#------------------------------------------------------------
# File: Gnuplot config
# Path: ~/.gnuplot
#------------------------------------------------------------

# Terminal
set terminal qt enhanced font 'Libertinus Serif,24' size 800,600
set size ratio 0.75
set encoding utf8
set minus

# Horizontal legends above the figure
set key horizontal center above maxrows 1 maxcols 6 samplen 1

# Outwards-pointing axis ticks
set tics front out nomirror scale 0.667

# Border color and thickness
set style line 10 lc rgb '#000000' lt 1 lw 1.5
set border front ls 10 lw 1.5

# High resolution for analytical plots
set samples   1024
set isosample 128,128

# Axis settings
set autoscale xy
set cbtics nomirror


################################################################################
# Definition of macros and functions
################################################################################

# Macros
set macros
contour = "set pm3d map interp 0,0 corners2color c1"
 
# Constants
i = {0.0,1.0}
 
# Physics
fermi(x)  = 1/( exp(x) + 1)
bose(x)   = x > 0 ? 1/( exp(x) - 1) : NaN
bcs(x, s) = abs(x) > 1 ? real(abs(x+s*i)/sqrt((x+s*i)**2-1)) : real(-i * ((x+s*i)/sqrt(1-(x+s*i)**2)))

# Calculus
d(y)   = ($0 == 0) ? (y1 = y, 1/0) : (y2 = y1, y1 = y, y1-y2)
sum(y) = ($0 == 0) ? (r = 0, 0) : (r = y+r, r)



################################################################################
# Definition of colors
################################################################################

# Set1 curve colors (taken from ColorBrewer2)
set style data lines
set style line  1 lc rgb '#00000000' pt 7 ps 1 lw 3 # black
set style line  2 lc rgb '#00bf1617' pt 7 ps 1 lw 3 # red
set style line  3 lc rgb '#001673bf' pt 7 ps 1 lw 3 # blue
set style line  4 lc rgb '#004DAF4A' pt 7 ps 1 lw 3 # green
set style line  5 lc rgb '#00984EA3' pt 7 ps 1 lw 3 # purple
set style line  6 lc rgb '#00FF7F00' pt 7 ps 1 lw 3 # orange
set style line  7 lc rgb '#00FFFF33' pt 7 ps 1 lw 3 # yellow
set style line  8 lc rgb '#00A65628' pt 7 ps 1 lw 3 # brown
set style line  9 lc rgb '#00F781BF' pt 7 ps 1 lw 3 # pink
set style line 11 lc rgb '#99000000' pt 7 ps 1 lw 3 # black
set style line 12 lc rgb '#99bf1617' pt 7 ps 1 lw 3 # red
set style line 13 lc rgb '#991673bf' pt 7 ps 1 lw 3 # blue
set style line 14 lc rgb '#994DAF4A' pt 7 ps 1 lw 3 # green
set style line 15 lc rgb '#99984EA3' pt 7 ps 1 lw 3 # purple
set style line 16 lc rgb '#99FF7F00' pt 7 ps 1 lw 3 # orange
set style line 17 lc rgb '#99FFFF33' pt 7 ps 1 lw 3 # yellow
set style line 18 lc rgb '#99A65628' pt 7 ps 1 lw 3 # brown
set style line 19 lc rgb '#99F781BF' pt 7 ps 1 lw 3 # pink

# Modify colors to use for contours [Viridis]
set palette defined (\
  0   0.267004 0.004874 0.329415,\
  1   0.268510 0.009605 0.335427,\
  2   0.269944 0.014625 0.341379,\
  3   0.271305 0.019942 0.347269,\
  4   0.272594 0.025563 0.353093,\
  5   0.273809 0.031497 0.358853,\
  6   0.274952 0.037752 0.364543,\
  7   0.276022 0.044167 0.370164,\
  8   0.277018 0.050344 0.375715,\
  9   0.277941 0.056324 0.381191,\
  10  0.278791 0.062145 0.386592,\
  11  0.279566 0.067836 0.391917,\
  12  0.280267 0.073417 0.397163,\
  13  0.280894 0.078907 0.402329,\
  14  0.281446 0.084320 0.407414,\
  15  0.281924 0.089666 0.412415,\
  16  0.282327 0.094955 0.417331,\
  17  0.282656 0.100196 0.422160,\
  18  0.282910 0.105393 0.426902,\
  19  0.283091 0.110553 0.431554,\
  20  0.283197 0.115680 0.436115,\
  21  0.283229 0.120777 0.440584,\
  22  0.283187 0.125848 0.444960,\
  23  0.283072 0.130895 0.449241,\
  24  0.282884 0.135920 0.453427,\
  25  0.282623 0.140926 0.457517,\
  26  0.282290 0.145912 0.461510,\
  27  0.281887 0.150881 0.465405,\
  28  0.281412 0.155834 0.469201,\
  29  0.280868 0.160771 0.472899,\
  30  0.280255 0.165693 0.476498,\
  31  0.279574 0.170599 0.479997,\
  32  0.278826 0.175490 0.483397,\
  33  0.278012 0.180367 0.486697,\
  34  0.277134 0.185228 0.489898,\
  35  0.276194 0.190074 0.493001,\
  36  0.275191 0.194905 0.496005,\
  37  0.274128 0.199721 0.498911,\
  38  0.273006 0.204520 0.501721,\
  39  0.271828 0.209303 0.504434,\
  40  0.270595 0.214069 0.507052,\
  41  0.269308 0.218818 0.509577,\
  42  0.267968 0.223549 0.512008,\
  43  0.266580 0.228262 0.514349,\
  44  0.265145 0.232956 0.516599,\
  45  0.263663 0.237631 0.518762,\
  46  0.262138 0.242286 0.520837,\
  47  0.260571 0.246922 0.522828,\
  48  0.258965 0.251537 0.524736,\
  49  0.257322 0.256130 0.526563,\
  50  0.255645 0.260703 0.528312,\
  51  0.253935 0.265254 0.529983,\
  52  0.252194 0.269783 0.531579,\
  53  0.250425 0.274290 0.533103,\
  54  0.248629 0.278775 0.534556,\
  55  0.246811 0.283237 0.535941,\
  56  0.244972 0.287675 0.537260,\
  57  0.243113 0.292092 0.538516,\
  58  0.241237 0.296485 0.539709,\
  59  0.239346 0.300855 0.540844,\
  60  0.237441 0.305202 0.541921,\
  61  0.235526 0.309527 0.542944,\
  62  0.233603 0.313828 0.543914,\
  63  0.231674 0.318106 0.544834,\
  64  0.229739 0.322361 0.545706,\
  65  0.227802 0.326594 0.546532,\
  66  0.225863 0.330805 0.547314,\
  67  0.223925 0.334994 0.548053,\
  68  0.221989 0.339161 0.548752,\
  69  0.220057 0.343307 0.549413,\
  70  0.218130 0.347432 0.550038,\
  71  0.216210 0.351535 0.550627,\
  72  0.214298 0.355619 0.551184,\
  73  0.212395 0.359683 0.551710,\
  74  0.210503 0.363727 0.552206,\
  75  0.208623 0.367752 0.552675,\
  76  0.206756 0.371758 0.553117,\
  77  0.204903 0.375746 0.553533,\
  78  0.203063 0.379716 0.553925,\
  79  0.201239 0.383670 0.554294,\
  80  0.199430 0.387607 0.554642,\
  81  0.197636 0.391528 0.554969,\
  82  0.195860 0.395433 0.555276,\
  83  0.194100 0.399323 0.555565,\
  84  0.192357 0.403199 0.555836,\
  85  0.190631 0.407061 0.556089,\
  86  0.188923 0.410910 0.556326,\
  87  0.187231 0.414746 0.556547,\
  88  0.185556 0.418570 0.556753,\
  89  0.183898 0.422383 0.556944,\
  90  0.182256 0.426184 0.557120,\
  91  0.180629 0.429975 0.557282,\
  92  0.179019 0.433756 0.557430,\
  93  0.177423 0.437527 0.557565,\
  94  0.175841 0.441290 0.557685,\
  95  0.174274 0.445044 0.557792,\
  96  0.172719 0.448791 0.557885,\
  97  0.171176 0.452530 0.557965,\
  98  0.169646 0.456262 0.558030,\
  99  0.168126 0.459988 0.558082,\
  100 0.166617 0.463708 0.558119,\
  101 0.165117 0.467423 0.558141,\
  102 0.163625 0.471133 0.558148,\
  103 0.162142 0.474838 0.558140,\
  104 0.160665 0.478540 0.558115,\
  105 0.159194 0.482237 0.558073,\
  106 0.157729 0.485932 0.558013,\
  107 0.156270 0.489624 0.557936,\
  108 0.154815 0.493313 0.557840,\
  109 0.153364 0.497000 0.557724,\
  110 0.151918 0.500685 0.557587,\
  111 0.150476 0.504369 0.557430,\
  112 0.149039 0.508051 0.557250,\
  113 0.147607 0.511733 0.557049,\
  114 0.146180 0.515413 0.556823,\
  115 0.144759 0.519093 0.556572,\
  116 0.143343 0.522773 0.556295,\
  117 0.141935 0.526453 0.555991,\
  118 0.140536 0.530132 0.555659,\
  119 0.139147 0.533812 0.555298,\
  120 0.137770 0.537492 0.554906,\
  121 0.136408 0.541173 0.554483,\
  122 0.135066 0.544853 0.554029,\
  123 0.133743 0.548535 0.553541,\
  124 0.132444 0.552216 0.553018,\
  125 0.131172 0.555899 0.552459,\
  126 0.129933 0.559582 0.551864,\
  127 0.128729 0.563265 0.551229,\
  128 0.127568 0.566949 0.550556,\
  129 0.126453 0.570633 0.549841,\
  130 0.125394 0.574318 0.549086,\
  131 0.124395 0.578002 0.548287,\
  132 0.123463 0.581687 0.547445,\
  133 0.122606 0.585371 0.546557,\
  134 0.121831 0.589055 0.545623,\
  135 0.121148 0.592739 0.544641,\
  136 0.120565 0.596422 0.543611,\
  137 0.120092 0.600104 0.542530,\
  138 0.119738 0.603785 0.541400,\
  139 0.119512 0.607464 0.540218,\
  140 0.119423 0.611141 0.538982,\
  141 0.119483 0.614817 0.537692,\
  142 0.119699 0.618490 0.536347,\
  143 0.120081 0.622161 0.534946,\
  144 0.120638 0.625828 0.533488,\
  145 0.121380 0.629492 0.531973,\
  146 0.122312 0.633153 0.530398,\
  147 0.123444 0.636809 0.528763,\
  148 0.124780 0.640461 0.527068,\
  149 0.126326 0.644107 0.525311,\
  150 0.128087 0.647749 0.523491,\
  151 0.130067 0.651384 0.521608,\
  152 0.132268 0.655014 0.519661,\
  153 0.134692 0.658636 0.517649,\
  154 0.137339 0.662252 0.515571,\
  155 0.140210 0.665859 0.513427,\
  156 0.143303 0.669459 0.511215,\
  157 0.146616 0.673050 0.508936,\
  158 0.150148 0.676631 0.506589,\
  159 0.153894 0.680203 0.504172,\
  160 0.157851 0.683765 0.501686,\
  161 0.162016 0.687316 0.499129,\
  162 0.166383 0.690856 0.496502,\
  163 0.170948 0.694384 0.493803,\
  164 0.175707 0.697900 0.491033,\
  165 0.180653 0.701402 0.488189,\
  166 0.185783 0.704891 0.485273,\
  167 0.191090 0.708366 0.482284,\
  168 0.196571 0.711827 0.479221,\
  169 0.202219 0.715272 0.476084,\
  170 0.208030 0.718701 0.472873,\
  171 0.214000 0.722114 0.469588,\
  172 0.220124 0.725509 0.466226,\
  173 0.226397 0.728888 0.462789,\
  174 0.232815 0.732247 0.459277,\
  175 0.239374 0.735588 0.455688,\
  176 0.246070 0.738910 0.452024,\
  177 0.252899 0.742211 0.448284,\
  178 0.259857 0.745492 0.444467,\
  179 0.266941 0.748751 0.440573,\
  180 0.274149 0.751988 0.436601,\
  181 0.281477 0.755203 0.432552,\
  182 0.288921 0.758394 0.428426,\
  183 0.296479 0.761561 0.424223,\
  184 0.304148 0.764704 0.419943,\
  185 0.311925 0.767822 0.415586,\
  186 0.319809 0.770914 0.411152,\
  187 0.327796 0.773980 0.406640,\
  188 0.335885 0.777018 0.402049,\
  189 0.344074 0.780029 0.397381,\
  190 0.352360 0.783011 0.392636,\
  191 0.360741 0.785964 0.387814,\
  192 0.369214 0.788888 0.382914,\
  193 0.377779 0.791781 0.377939,\
  194 0.386433 0.794644 0.372886,\
  195 0.395174 0.797475 0.367757,\
  196 0.404001 0.800275 0.362552,\
  197 0.412913 0.803041 0.357269,\
  198 0.421908 0.805774 0.351910,\
  199 0.430983 0.808473 0.346476,\
  200 0.440137 0.811138 0.340967,\
  201 0.449368 0.813768 0.335384,\
  202 0.458674 0.816363 0.329727,\
  203 0.468053 0.818921 0.323998,\
  204 0.477504 0.821444 0.318195,\
  205 0.487026 0.823929 0.312321,\
  206 0.496615 0.826376 0.306377,\
  207 0.506271 0.828786 0.300362,\
  208 0.515992 0.831158 0.294279,\
  209 0.525776 0.833491 0.288127,\
  210 0.535621 0.835785 0.281908,\
  211 0.545524 0.838039 0.275626,\
  212 0.555484 0.840254 0.269281,\
  213 0.565498 0.842430 0.262877,\
  214 0.575563 0.844566 0.256415,\
  215 0.585678 0.846661 0.249897,\
  216 0.595839 0.848717 0.243329,\
  217 0.606045 0.850733 0.236712,\
  218 0.616293 0.852709 0.230052,\
  219 0.626579 0.854645 0.223353,\
  220 0.636902 0.856542 0.216620,\
  221 0.647257 0.858400 0.209861,\
  222 0.657642 0.860219 0.203082,\
  223 0.668054 0.861999 0.196293,\
  224 0.678489 0.863742 0.189503,\
  225 0.688944 0.865448 0.182725,\
  226 0.699415 0.867117 0.175971,\
  227 0.709898 0.868751 0.169257,\
  228 0.720391 0.870350 0.162603,\
  229 0.730889 0.871916 0.156029,\
  230 0.741388 0.873449 0.149561,\
  231 0.751884 0.874951 0.143228,\
  232 0.762373 0.876424 0.137064,\
  233 0.772852 0.877868 0.131109,\
  234 0.783315 0.879285 0.125405,\
  235 0.793760 0.880678 0.120005,\
  236 0.804182 0.882046 0.114965,\
  237 0.814576 0.883393 0.110347,\
  238 0.824940 0.884720 0.106217,\
  239 0.835270 0.886029 0.102646,\
  240 0.845561 0.887322 0.099702,\
  241 0.855810 0.888601 0.097452,\
  242 0.866013 0.889868 0.095953,\
  243 0.876168 0.891125 0.095250,\
  244 0.886271 0.892374 0.095374,\
  245 0.896320 0.893616 0.096335,\
  246 0.906311 0.894855 0.098125,\
  247 0.916242 0.896091 0.100717,\
  248 0.926106 0.897330 0.104071,\
  249 0.935904 0.898570 0.108131,\
  250 0.945636 0.899815 0.112838,\
  251 0.955300 0.901065 0.118128,\
  252 0.964894 0.902323 0.123941,\
  253 0.974417 0.903590 0.130215,\
  254 0.983868 0.904867 0.136897,\
  255 0.993248 0.906157 0.143936)
