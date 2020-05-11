CRAN Package Check Results for Solaris warns about "It is not known that wchar_t is Unicode on this platform".
This package is about using Unicode glyphs to make board game graphics and it doesn't make sense to avoid these glyphs. 
However by default the functions use only Unicode glyphs found in typical R "sans" fonts (i.e. Arial, DejaVu Sans, Arimo) but some of these glyphs like the French suits Hearts, Diamonds, Clubs, and Spades are not Latin-1 glyphs.

**Test environments**

* local (linux, R 3.6.3) 
* win-builder (windows, R devel) 
* appveyor (windows, R devel) 
* appveyor (windows, R release) 
* travis-ci (OSX, R release) 
* travis-ci (linux, R devel) 
* travis-ci (linux, R release) 

**R CMD check --as-cran results**

Status: OK
