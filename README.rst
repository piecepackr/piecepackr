piecepackr: Board Game Graphics in R
====================================

.. image:: https://travis-ci.org/trevorld/piecepackr.png?branch=master
    :target: https://travis-ci.org/trevorld/piecepackr
    :alt: Build Status

.. image:: https://ci.appveyor.com/api/projects/status/github/trevorld/piecepackr?branch=master&svg=true 
    :target: https://ci.appveyor.com/project/trevorld/piecepackr
    :alt: AppVeyor Build Status

.. image:: https://img.shields.io/codecov/c/github/trevorld/piecepackr/master.svg
    :target: https://codecov.io/github/trevorld/piecepackr?branch=master
    :alt: Coverage Status

.. image:: http://www.repostatus.org/badges/latest/wip.svg
   :alt: Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.
   :target: http://www.repostatus.org/#wip

``piecepackr`` is an R_ package designed to make configurable board game graphics.  It can be used with the grid_ R package to make board game diagrams, board game animations, and custom `Print & Play layouts`_.    By default it is configured to make piecepack_ game diagrams, animations, and "Print & Play" layouts but can be configured to make graphics for other game systems.

.. image:: https://trevorldavis.com/piecepackr/images/knitr/pnp-previews.gif
   :alt: Previews of demo piecepack configurations

.. _piecepack: http://www.ludism.org/ppwiki/HomePage

.. _grid: https://www.rdocumentation.org/packages/grid

.. _R: https://www.r-project.org/

.. _Print & Play layouts: https://trevorldavis.com/piecepackr/pages/print-and-play-pdfs.html

.. _man pages: https://rdrr.io/github/trevorld/piecepackr/man/

.. contents::

Installation
------------

To install the development version of ``piecepackr`` use the following commands in R_:

.. code:: r

   install.packages("remotes")
   remotes::install_github("trevorld/piecepackr")

The default piecepackr configuration should work out on the box on most modern OSes including Windows without the user needing to mess with their system fonts.  However if you wish to use advanced piecepackr configurations you'll need to install additional Unicode fonts and Windows users are highly recommended to use and install piecepackr on "Ubuntu on Bash on Windows" if planning on using Unicode symbols from multiple fonts.  The following bash commands will give you a good selection of fonts (Noto, Quivira, and Dejavu) on Ubuntu:

.. code:: bash

    sudo apt install fonts-dejavu fonts-noto 
    fonts_dir=${XDG_DATA_HOME:="$HOME/.local/share"}/fonts
    curl -O http://www.quivira-font.com/files/Quivira.otf
    mv Quivira.otf $fonts_dir/
    curl -O https://noto-website-2.storage.googleapis.com/pkgs/NotoEmoji-unhinted.zip
    unzip NotoEmoji-unhinted.zip NotoEmoji-Regular.ttf
    mv NotoEmoji-Regular.ttf $fonts_dir/
    rm NotoEmoji-unhinted.zip

**Note**  ``piecpackr`` works best if the version of R installed was compiled with support for Cairo and fortunately this is typically the case.  One can confirm if this is true via R's ``capabilities`` function:

.. code:: r

   > capabilities("cairo")
   cairo
    TRUE

Also although most users won't need them ``piecpackr`` contains utility functions that depend on the system dependencies ``ghostscript`` and ``poppler-utils``:

1. ``save_print_and_play`` will embed additional metadata into the pdf if ``ghostscript`` is available.
2. ``get_embedded_font`` (a debugging helper function) needs ``pdffonts`` (usually found in ``poppler-utils``)

You can install these utilities on Ubuntu with

.. code:: bash

    sudo apt install ghostscript poppler-utils

API Intro
---------

``grid.piece`` is the core function that can be used used to draw any piecepack_ component:

.. code:: r

   library("piecpackr")
   grid.piece("tile_face", s=1:4, r=1:4, x=inch(seq(1, 7, by=2)))

.. image:: https://trevorldavis.com/piecepackr/images/knitr/docs-intro-dc1-1.svg
   :alt: 3 of Hearts Tile Face

One can use `lists to configure <https://trevorldavis.com/piecepackr/configuration-lists.html>`_ the appearance of the piecepack graphics drawn by ``grid.piece``:

.. code:: r

    dark_colorscheme <- list(suit_color="darkred,black,darkgreen,darkblue,black",
                         invert_colors.suited=TRUE)
    piecepack_suits <- list(suit_text="\U0001f31e,\U0001f31c,\U0001f451,\u269c,\uaa5c", # 🌞,🌜,👑,⚜,꩜
                        suit_fontfamily="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham",
                        suit_scale="0.6,0.7,0.75,0.9,0.9")
    traditional_ranks <- list(use_suit_as_ace=TRUE, rank_text=",a,2,3,4,5")
    cfg <- c(piecepack_suits, dark_colorscheme, traditional_ranks)
    grid.piece("tile_face", s=1:4, r=1:4, x=inch(seq(1, 7, by=2)))

.. image:: https://trevorldavis.com/piecepackr/images/knitr/docs-intro-dc1t-1.svg
   :alt: 3 of Suns Tile Face

``make_pnp`` makes a "Print & Play" pdf of a configured piecepack, ``make_images`` makes individual images of each piecepack component:

.. code:: r

   make_pnp(cfg, "my_piecepack.pdf", size="letter")
   make_images(cfg)

A slightly longer `intro to piecepackr's API <https://trevorldavis.com/piecepackr/intro-to-piecepackrs-api.html>`_ plus several `piecepackr demos <https://trevorldavis.com/piecepackr/category/demos.html>`_ and other `piecpackr docs <https://trevorldavis.com/piecepackr/category/docs.html>`_ are available at piecepackr's `companion website <https://trevorldavis.com/piecepackr/>`_ as well as some pre-configured `Print & Play PDFs <https://trevorldavis.com/piecepackr/pages/print-and-play-pdfs.html>`_.  More API documentation is also available in the package's `man pages`_.

Licence
-------

This software package and the images created by it are released under a Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0).  You can see file ``LICENSE.md`` for more info.  This license is compatible with version 3 of the GNU Public License (GPL-3).

Frequently Asked Questions
--------------------------

How should I Print & Play my piecepack?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Print-and-Play pdf's produced by the ``make_pnp`` function are designed to be used in three different ways:

- Print single-sided on label paper, cut out the labels, and apply to components (in the material of your choice).  
- Print single-sided on paper(board), apply adhesive to the back, fold over in half "hot-dog-style", and cut out the components.  One will need to to some additional folding and application of adhesive/tape in order to construct the dice and pawns.  One can build more dice/pawns/pawn belts if you cut them out *before* folding the paper(board) in half but if you don't do so you should still have all the "standard" piecepack components.
- Print double-sided on paper(board) and cut out the components.  One will need to do some additional folding and application of adhesive/tape in order to construct the dice and pawns.

The `Piecepack Wiki <www.ludism.org/ppwiki>`_ has a page on `making piecepacks <http://www.ludism.org/ppwiki/MakingPiecepacks>`_. The BoardGameGeek `Print-and-Play Wiki <https://boardgamegeek.com/wiki/page/Print_and_Play_Games#>`_ also has lots of good info like how to `quickly make coins uisng an arch punch <https://boardgamegeek.com/thread/507240/making-circular-tokens-and-counters-arch-punch>`_.  

**Warning:**  Generally it is advisable to uncheck 'fit to size' when printing PDF files otherwise your components maybe re-sized by the printer.

What are the dimensions of the components?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Although one can use the API to make layouts with components of different sizes the default print-and-play pdf's draw components of the following size which (except for the pawns and non-standard "pawn belts") matches the traditional `Mesomorph piecepack dimensions <http://www.piecepack.org/Anatomy.html>`_ if one uses the default component shapes:

- tiles (default "rect") are drawn into a 2" by 2" square 
- coins (default "circle") are drawn into a ¾" by ¾" square
- dice (default "rect") faces are drawn into a ½" by ½" square
- pawn sides (default "halma") are drawn into a ½" by ⅞" rectangle
- "pawn belts" (default "rect") are drawn into a ¾π" by ½" rectangle
- "pawn saucers" (default "circle") are drawn into a ⅞" by ⅞" square
       
Components are drawn into rectangular drawing spaces (which are always squares except for pawn components).  The program allows one to customize piecepack component shapes.  If a components shape is ``rect`` it will fill up the entire rectangular drawing space, if it is a ``circle`` then the rectangular drawing space will be circumscribed around the circle.  If a components shape is a ``convex#`` or ``concave#``  where ``#`` is the number of exterior vertices then the rectangular drawing space will be circumscribed around a circle that will be circumscribed around that convex/concave polygon.  The rectangular drawing space also is circumscribed around the special ``halma``, ``kite``, and ``pyramid`` shapes.

**Warning:**  Generally it is advisable to uncheck 'fit to size' when printing PDF files otherwise your components maybe re-sized by the printer.

What are the possible color options?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can specify colors either by `RGB hex color codes <http://www.color-hex.com/>`_ or `R color strings <http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf>`_.  "transparent" is a color option which does what you'd expect it to (if used for something other than the background color will render the element effectively invisible).  **Warning:** you shouldn't mix "transparent" backgrounds with the ``invert_colors`` options.

I have some images I want to use as suit/rank/directional mark symbols, how can I use them with this program?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You'll need to take them and put them into a font.  `FontForge <https://fontforge.github.io/en-US/>`_ is a popular open-source program suitable for this task.  `fontcustom <https://github.com/FontCustom/fontcustom>`_ is a popular command-line wrapper around FontForge.  You may need to convert your images from one format to another format first.  To guarantee dispatch by ``fontconfig`` you might want to put the symbols in a part of the "Private Use Area" of Unicode not used by any other fonts on your system.  If you do that you won't need to specify your font otherwise you'll need to configure the ``suit_symbols_font``, ``rank_symbols_font``, and/or ``dm_font`` options.

Why does the package sometimes use a different font then the one I instructed it to use for a particular symbol?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The program uses ``Cairo`` which uses ``fontconfig`` to select fonts.  ``fontconfig`` picks what it thinks is the 'best' font and sometimes it annoyingly decides that the font to use for a particular symbol is not the one you asked it to use.  (although sometimes the symbol it chooses instead still looks nice in which case maybe you shouldn't sweat it).  It is hard but not impossible to `configure which fonts <https://eev.ee/blog/2015/05/20/i-stared-into-the-fontconfig-and-the-fontconfig-stared-back-at-me/>`_ are dispatched by fontconfig.  A perhaps easier way to guarantee your symbols will be dispatched would be to either make a new font and re-assign the symbols to code points in the Unicode "Private Use Area" that aren't used by any other font on your system or to simply temporarily move (or permanently delete) from your system font folders the undesired fonts that ``fontconfig`` chooses over your requested fonts::

    # temporarily force fontconfig to use Noto Emoji instead of Noto Color Emoji in my piecepacks on Ubuntu 18.04
    $ sudo mv /usr/share/fonts/truetype/noto/NotoColorEmoji.ttf ~/
    ## Make some piecepacks
    $ sudo mv ~/NotoColorEmoji.ttf /usr/share/fonts/truetype/noto/

Also as a sanity check use the command-line tool ``fc-match`` to make sure you specified your font correctly in the first place (i.e. ``fc-match "Noto Sans"`` on my system returns "Noto Sans" but ``fc-match "Sans Noto"`` returns "DejaVu Sans" and not "Noto Sans" as one may have expected).    To help determine which fonts are actually being embedded you can use the ``get_embedded_font`` function:

.. code:: r

    get_embedded_font(c('Noto Sans Symbols2', 'Noto Emoji', 'sans'), c('♥', '♠', '♣', '♦', '🌞' ,'🌜' ,'꩜'))

::

           requested_font            embedded_font char
    1  Noto Sans Symbols2 NotoSansSymbols2-Regular    ♥
    2  Noto Sans Symbols2 NotoSansSymbols2-Regular    ♠
    3  Noto Sans Symbols2 NotoSansSymbols2-Regular    ♣
    4  Noto Sans Symbols2 NotoSansSymbols2-Regular    ♦
    5  Noto Sans Symbols2                NotoEmoji    🌞
    6  Noto Sans Symbols2                NotoEmoji    🌜
    7  Noto Sans Symbols2     NotoSansCham-Regular    ꩜
    8          Noto Emoji                NotoEmoji    ♥
    9          Noto Emoji                NotoEmoji    ♠
    10         Noto Emoji                NotoEmoji    ♣
    11         Noto Emoji                NotoEmoji    ♦
    12         Noto Emoji                NotoEmoji    🌞
    13         Noto Emoji                NotoEmoji    🌜
    14         Noto Emoji     NotoSansCham-Regular    ꩜
    15               sans                    Arimo    ♥
    16               sans                    Arimo    ♠
    17               sans                    Arimo    ♣
    18               sans                    Arimo    ♦
    19               sans                NotoEmoji    🌞
    20               sans                NotoEmoji    🌜
    21               sans     NotoSansCham-Regular    ꩜

How do I use this package in piecepack rulesets?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two main ways that this package could be used to help make piecepack rulesets:

1) The ``make_images`` function makes individual images of components.  By default it makes them in the pdf, png, and svg formats with rotations of 0, 90, 180, and 270 degrees but with configuration can also make them in the bmp, jpeg, tiff, and ps formats and other rotations.  These can be directly inserted into your ruleset or even used to build diagrams with the aid of a graphics editor program.  An example filename (and directory) is ``pdf/components/orthodox1/tile_face_s1_r5_t180.pdf`` where ``orthodox1`` is the configuration used to build that image, ``tile`` is the component, ``face`` is the side, ``s1`` indicates it was the first suit, ``r5`` indicates it was the 5th rank, ``t180`` indicates it was rotated 180 degrees, and ``pdf`` indicates it is a pdf image.
2) This R package can be directly used with the ``grid`` graphics library in R to make diagrams.  Here is a link to a `shogi diagram making example <https://github.com/trevorld/piecepack_rules/blob/master/R/make_shogi_diagrams.R>`_.  The important function for diagram drawing exported by the ``piecepack`` R package is ``grid.piece`` which draws piecepack components to the graphics device.  One can also use this package to `make animations <https://trevorldavis.com/piecepackr/animations.html>`__:

.. image:: https://www.trevorldavis.com//piecepackr/images/knitr/tictactoe.gif
   :alt: Example animation of using piecepackr to create piecepack game diagrams
