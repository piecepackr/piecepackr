Configurable Piecepack PDF Maker
--------------------------------

This is an R package and some executable Rscripts designed to make a pdf of piecepack components with the intention that they either be printed on label paper and mounted on components or printed on paperboard and folded/glued together in order to make a `piecepack <http://www.ludism.org/ppwiki/HomePage>`_.  

.. warning:: This package is currently in an alpha state.  This means the API is incomplete and likely to change.

Install
-------

You'll need to install some system requirements::

    $ sudo apt install ghostscript pdfsam rake r-base 

You'll also need to install the development version of grImport2 package and this package::

    $ sudo R
    > install.packages("devtools")
    > devtools::install_github("sjp/grImport2")
    > devtools::install_github("trevorld/piecepack")

You'll also need some decent fonts installed on your system with the Unicode symbols you'd like to use in your piecepack.  

.. warning:: Not all fonts can be freely distributed!  Be careful with which ones you use!

The software currently assumes one has installed:

* `Symbola <http://www.fontspace.com/unicode-fonts-for-ancient-scripts/symbola>`_ good coverage of the Symbol block of the Unicode Standard.
* `Noto Sans Cham <https://www.google.com/get/noto/>`_ contains the rare "Cham Punctuation Spiral".

How to use
----------

Although the API is in flux you run a working example that builds a "dual piecepacks" demo by running::

    $ rake dual

Besides building a "dual piecepacks" pdf this command will also print the command-line commands needed to build the pdf.  It runs the command-line programs `exec/configure_piecepack`, `exec/make_piecepack`, and `exec/arrange_piecepacks`.  You can view sample configuration files for individual piecepacks in the `configurations` folder.

Licence
-------

This software package and the piecepack pdf's created by it are released under a Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0).  You can see file LICENSE for more info.  This license is compatible with version 3 of the Gnu Public License (GPL-3).
