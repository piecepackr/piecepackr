Piecepack PDF Label Maker
-------------------------

This is an R package and some Rscripts designed to make a pdf of a customized piecepack components with the intention that they be printed on label paper and mounted on components in order to make a piecepack.  It is currently in alpha.

Install
-------

You'll also want to install some system requirements::

    $ sudo apt install pdfsam rake r-base 

You'll also need to install the grImport2 package and this package::

    $ sudo R
    > install.packages("devtools")
    > devtools::install_github("sjp/grImport2")
    > devtools::install_github("trevorld/piecepack")

You'll also need some decent fonts installed on your system with the Unicode symbols you'd like to use in your piecepack.  

.. warning:: Not all fonts can be freely distributed!  Be careful with which ones you use!

Some libre font suggestions:

* `Symbola <http://www.fontspace.com/unicode-fonts-for-ancient-scripts/symbola>`_ good coverage of the Symbol block of the Unicode Standard.
* `Noto Sans Cham <https://www.google.com/get/noto/>`_ contains the rare "Cham Punctuation Spiral".

Licence
-------

This software package and the piecepack pdf's created by it are released under a Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0).  You can see file LICENSE for more info.  This license is compatible with version 3 of the Gnu Public License (GPL-3).

