Configurable Piecepack PDF Maker
--------------------------------

This is an R package and some executable Rscripts designed to make a pdf of piecepack components with the intention that they either be printed on label paper and mounted on components or printed on paperboard and folded/glued together in order to make a `piecepack <http://www.ludism.org/ppwiki/HomePage>`_.  

.. warning:: This package is currently in an alpha state.  This means the API is incomplete and likely to change.

Install
-------

You'll need to install some system requirements::

    $ sudo apt install ghostscript pdfsam poppler-utils rake r-base 

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

One uses the ``exec/make_piecepack`` command to make a single PnP pdf of a piecepack deck.  One can arrange several PnP pdf's using the ``exec/arrange_piecepack`` command.  The ``exec/make_piecepack`` command requires JSON configuration either provided as Standard Input to the program or as a file.  You can view sample configuration files for several demo piecepacks in the ``configurations`` folder.  The ``exec/configure_piecepack`` can be used to generate configuration files or you can manually modify a pre-existing one.  Although the API is in flux you can build the demo files and see the command-line calls used to build them by running::

    $ rake demo_name

Where ``demo_name`` is either

default
    This will build a non-configured piecepack (the default type built by this software).  `Resulting pdf <https://www.dropbox.com/s/7k1nrhc0sgwm0e3/default_demo.pdf?dl=0>`_.
dual
    This will build the six piecepacks in the `"dual piecepacks" <http://www.ludism.org/ppwiki/DualPiecepacks>`_ proof-of-concept.  `Resulting pdf <https://www.dropbox.com/s/iezcku9rktvuk6r/dual_demo.pdf?dl=0>`_.
orthodox
    This will build a piecepack-suited piecepack that complies with the `Anatomy of a Piecepack <http://www.piecepack.org/Anatomy.html>`_ standard as well as a matching 2-color french-suited piecepack (aka a "Playing Cards" expansion).  `Resulting pdf <https://www.dropbox.com/s/derdlo3j8sdeoox/orthodox_demo.pdf?dl=0>`_.

Configuration options
---------------------

`exec/configure_piecepack --help <https://github.com/trevorld/piecepack/blob/master/configurations/configure_piecepack_options.txt>`_

Licence
-------

This software package and the piecepack pdf's created by it are released under a Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0).  You can see file LICENSE for more info.  This license is compatible with version 3 of the Gnu Public License (GPL-3).
