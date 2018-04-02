Configurable Piecepack PDF Maker
--------------------------------

This is an R package and some executable Rscripts designed to make a pdf of piecepack components with the intention that they either be printed on label paper and mounted on components or printed on paperboard and folded/glued together in order to make a `piecepack <http://www.ludism.org/ppwiki/HomePage>`_.  

.. warning:: This package is currently in an alpha state.  This means the API is incomplete and likely to change.

Installation
------------

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

One uses the ``exec/make_piecepack`` command to make a single PnP pdf of a piecepack deck.  One can arrange several PnP pdf's using the ``exec/arrange_piecepacks`` command.  The ``exec/make_piecepack`` command requires JSON configuration either provided as standard input to the program or as a file.  You can view sample configuration files for several demo piecepacks in the ``configurations`` folder.  The ``exec/configure_piecepack`` can be used to generate suitable JSON configuration files or you can manually modify a pre-existing one.  Although the API is in flux you can currently build the demo files and see the command-line calls used to build them by running::

    $ rake demo_name

Where ``demo_name`` is either:

chinese_zodiac
~~~~~~~~~~~~~~

Build a `Chinese Zodiac demo piecepack pdf <https://www.dropbox.com/s/eu5uxwk6hcihy53/chinese_zodiac_demo.pdf?dl=0>`_.  This is two 5-suited piecepacks (Wood, Fire, Earth, Metal, and Water) each using a different six animals taken from the Chinese zodiac as ranks.

default
~~~~~~~

Build a `default demo piecepack pdf <https://www.dropbox.com/s/7k1nrhc0sgwm0e3/default_demo.pdf?dl=0>`_.  This is the default type of piecepack built by this software if the user does no configuration.  

dual
~~~~

Build a `dual piecepacks demo pdf <https://www.dropbox.com/s/iezcku9rktvuk6r/dual_demo.pdf?dl=0>`_ which includes the six piecepacks in the `"dual piecepacks" <http://www.ludism.org/ppwiki/DualPiecepacks>`_ proof-of-concept. 

orthodox
~~~~~~~~

Build an `"orthodox piecepacks" demo pdf <https://www.dropbox.com/s/derdlo3j8sdeoox/orthodox_demo.pdf?dl=0>`_.  It includes a piecepack-suited piecepack that complies with the `Anatomy of a Piecepack <http://www.piecepack.org/Anatomy.html>`_ standard as well as a matching 2-color french-suited piecepack (aka a "Playing Cards" expansion).  The "chip" accessory has been configured to be more convenient for labeling paper pyramids to make "piecepack pyramids".

sixpack
~~~~~~~

Build a `"sixpack demo pdf <https://www.dropbox.com/s/nr60w36885dgudz/sixpack_demo.pdf?dl=0>`_.  It includes two red/black `Sixpack <http://www.ludism.org/ppwiki/SixPack>`_ suited piecepacks (the second in an "orthodox" scheme) as well as dual multicolor sixpacks.

Executable options
------------------

* `exec/configure_piecepack --help <https://github.com/trevorld/piecepack/blob/master/configurations/configure_piecepack_options.txt>`_
* `exec/make_piecepack --help <https://github.com/trevorld/piecepack/blob/master/configurations/make_piecepack_options.txt>`_
* `exec/arrange_piecepacks --help <https://github.com/trevorld/piecepack/blob/master/configurations/arrange_piecepacks_options.txt>`_

Licence
-------

This software package and the piecepack pdf's created by it are released under a Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0).  You can see file LICENSE for more info.  This license is compatible with version 3 of the Gnu Public License (GPL-3).

Frequently Asked Questions
--------------------------

What are the "chips" accessories that shows up on the accesories page of the print-and-play pdf supposed to be used for?
    The "chips" are a customizable accessory that can aid in playing certain types of games.  Some possible uses:

    1.  One option (and source of the name "chip") is to mount them on suit-colored poker chips.  By default both sides will show suit and direction and one side will also show a rank. In such a configuration it could be used to replace piecepack pyramids in a subset of games like Alien City or Ice Floe, could be used to add more pieces in games like checkers/go, could be used to reduce abstraction in chess (i.e. each side's pieces could be distinguished by color), etc. 
    2. A second option would be to mount them on pyramids (i.e. paste rank side on one face of the pyramid and suit side on another face of the pyramid) to get something equivalent to `piecepack pyramids <http://www.ludism.org/ppwiki/PiecepackPyramids>`_.  A classic configuration for this purpose would be "``--rank_symbols.chip_face='A,B,C,D,E,F' --use_ace_as_ace.chip_face --directional_mark_symbols.chip_face=,,,, --directional_mark_symbols.chip_back=,,,,``".
    3. A third option would be to produce the equivalent of the "piecepack stones" accessory (i.e. from the `Sensible Expansions proposal <http://www.ludism.org/ppwiki/SensibleExpansions>`_).  A good configuration for this purpose would be  "``suit_symbols.chip_back=,,,, --directional_mark_colors.chip_back=grey,grey,grey,grey,grey --uninvert_colors.chip_back``". 
    4. A fourth option would be to produce the equivalent of the "suit (star) coin" accessory (i.e. from the `JCD piecepack <http://www.piecepack.org/JCD.html>`_).  A good configuration for this purpose would be "``--use_suit_as_ace.chip_face --invert_colors.chip_face``".  
    5. A fifth option if paired with another deck with six extra ranks would be to mount the chip faces on a large d12 to make a "dozenal piecepack die" for each suit.  The suits could then also go on a d12 to make a "dozenal suit die" especially if there are in fact a dozen suits.

What is the purpose of the "hex lines" that can be configured onto the tiles by the ``hexline_colors`` option?
    It you use the tiles to build a hex board the hexlines will visually show four of the six hexagon cell sides.

What are the possible color options?
    You can specify colors either by `RGB hex color codes <http://www.color-hex.com/>`_ or `R color strings <http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf>`_. 
