piecepackr: A Piecepack Graphics R Package
==========================================

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

``piecepackr`` is an R package designed to make configurable `piecepack <http://www.ludism.org/ppwiki/HomePage>`_ graphics.  The API can be used with the ``grid`` R package to make piecepack diagrams (i.e. for inclusion in rulesets) as well as a customized `"Print & Play  <https://boardgamegeek.com/wiki/page/Print_and_Play_Games#>`_ layouts.  

.. contents::

.. _`Demo descriptions`:

Demos
-----

.. image:: https://www.trevorldavis.com/share/piecepack/animation.gif
   :alt: Previews of selected demo piecepack configurations

Some `piecepackr demos <https://trevorldavis.com/piecepackr/category/demos.html>`_ are available at the `piecepackr companion website <https://trevorldavis.com/piecepackr/>`_.

Installation
------------

Short instructions using Rakefile on Ubuntu (16.04+) including "Bash on Ubuntu on Windows (10)"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The below commands clones the ``piecepackr`` R package github repo, installs a bunch of system dependencies, fonts, and R packages (often using ``sudo``) and then installs the ``piecepackr`` R package.  Should set up everything needed to build all the demos on a recent version of Ubuntu (i.e. more things are installed than what a strictly minimal install would need).  These short instructions have been lightly tested on both Ubuntu Linux as well as `Bash on Ubuntu on Windows <https://www.microsoft.com/en-us/store/p/ubuntu/9nblggh4msv6>`_:

.. code:: bash

    sudo apt install -y git rake 
    git clone https://github.com/trevorld/piecepackr
    cd piecepackr
    rake apt_install_dependencies sudo= yes=
    rake install sudo=

To update a previously cloned repo to the newest version and re-install the piecepackr R package:

.. code:: bash

    cd piecepackr
    git pull
    rake install sudo=

Detailed instructions
~~~~~~~~~~~~~~~~~~~~~

System dependencies
+++++++++++++++++++

#. `R <https://cran.r-project.org/>`_ compiled with support for Cairo plus several R packages file available on CRAN which are usually installed for you by R when you install the ``piecepackr`` R package.
#. Unicode font(s) (installed where Cairo can find them) that (altogether) have all your required glyphs
#. `ghostscript <https://www.ghostscript.com/>`_ (not needed if you won't be using the ``collect_pnp`` function)
#. `poppler-utils (aka xpdf-utils)  <https://poppler.freedesktop.org/>`_ (not needed if you won't be using the ``get_embedded_font`` function)

System suggestions
++++++++++++++++++

#. `Rake - Ruby Make <https://github.com/ruby/rake>`_ (needed for running demos and other developer build commands)
#. Several `Noto <https://www.google.com/get/noto/>`_ fonts (in particular "Noto Sans", "Noto Sans Symbols", "Noto Sans Symbols2", "Noto Emoji", "Noto Sans Cham", "Noto Sans CJK SC")
#. `Quivira <http://quivira-font.com/>`_ font
#. `DejaVu Sans <https://dejavu-fonts.github.io/>`_ font

Installation Notes
++++++++++++++++++

This package is developed and tested on Ubuntu Linux.  Instructions are given below for installation on Ubuntu Linux but installing on another \*nix OS should be a straightforward substitution of the ``apt`` package manager with your OS's preferred package manager like ``brew`` for OSX (you may also need to tweak the package names to match what is in your repos and to manually install some software/fonts not in your repos).  The system dependencies/suggestions are all *theoretically* installable on Windows but it is likely easier on recent versions of Windows to `install and run an Ubuntu terminal <https://www.microsoft.com/en-us/store/p/ubuntu/9nblggh4msv6>`_ or to (freely) run Ubuntu in a virtual machine or possibly even in a ``chroot``.  

You'll need to install some system requirements to use this R package:

.. code:: bash

    sudo apt install -y ghostscript poppler-utils r-base 

The ``ghostscript`` system requirement can be dropped if you do not plan on using the ``collect_pnp`` function to collect several print-and-play pdf's into one pdf (with previews at the beginning).  The ``poppler-utils`` system requirement can be dropped if you do not plan on using ``get_embedded_font`` function to help figure out which fonts ``cairo_pdf`` actually embeds into output pdf's. 

You'll also need to install the development version of the ``piecepackr`` R package and its R package dependencies.  These can easily be installed with help of the ``install`` or ``install_github`` functions from the ``devtools`` package:

.. code:: bash

    sudo apt install -y libcurl4-openssl-dev libssl-dev # system dependencies to install devtools's R package dependencies
    sudo Rscript -e "install.packages('devtools', repos='https://cran.rstudio.com/')" 
    sudo Rscript -e "devtools::install_github('trevorld/piecepackr')"

Although the `default piecepackr configuration <https://trevorldavis.com/piecepackr/default-demo.html>`_ should work out of the box without out the user messing with their system fonts if you want to try reproducing some of the fancier `piecepackr demos`_ you may need to install several additional fonts:

.. code:: bash

    sudo apt install fonts-dejavu fonts-noto 
    fonts_dir=${XDG_DATA_HOME:="$HOME/.local/share"}/fonts
    curl -O http://www.quivira-font.com/files/Quivira.otf
    mv Quivira.otf $fonts_dir/
    curl -O https://noto-website-2.storage.googleapis.com/pkgs/NotoEmoji-unhinted.zip
    unzip NotoEmoji-unhinted.zip NotoEmoji-Regular.ttf
    mv NotoEmoji-Regular.ttf $fonts_dir/
    rm NotoEmoji-unhinted.zip

..    $ curl -O http://www.chessvariants.com/d.font/chess1.ttf
..    $ mv chess1.ttf $fonts_dir/ChessUtrecht.ttf

If you have an older version of Ubuntu you may need to manually install additional `Noto fonts <https://www.google.com/get/noto/>`_ if you want to run the demos.

**Warning**: This program embeds (subsets of) fonts into the print-and-play pdf's.  Not all fonts can be legally distributed this way!  Be careful with which ones you use!  The DejaVu, Noto and Quivira fonts used in the demos are legal to embed into CC-BY-SA-4.0 licensed print-and-play pdf's as are all fonts licensed under the SIL Open Font License (OFL).

If you want to help further **develop** the ``piecepackr`` R package you'll also need to install the suggested packages so you can run the unit tests and re-build the documentation and you may want to:

.. code:: bash

    sudo rake # To be able to use utility commands in the Rakefile
    sudo apt install -y libxml2-dev libcairo2-dev # system dependencies for roxygen2 and gdtools
    sudo Rscript -e "devtools::install(dependencies=\"Suggests\", upgrade_dependencies=FALSE)"

Windows Notes
+++++++++++++

Although Windows users are highly recommended to install ``piecepackr`` on "Ubuntu on Bash On Windows" ``piecepackr`` will natively run in Windows with a few caveats.  `Issue #70 <https://github.com/trevorld/piecepackr/issues/70>`_ contains some native Windows installation notes using `Chocolately <https://chocolately.org/install>`_ in a Powershell. The default configuration has been carefully chosen to work with the default Windows font (Arial) and configurations also seem to work when all piecepack symbols come from a single (properly installed) font such as Quivira or DejaVu.  However you may run into problems with piecepack configurations combining symbols from multiple fonts (such as grabbing glyphs from multple Noto Sans fonts in the "orthodox" demos).  

Piecepack configuration
-----------------------

One can override the piecepackr defaults by manually creating/modifying a list of configuration options.  

This program uses the abstraction that every piecepack component has a "component_side" name (like ``belt_face``), a suit, a rank, a primary symbol, a directional mark symbol, and embellishments like border lines, grid lines, hex lines, checkers, and ribbons.  On top of the normal "suited" piecepack suits this program also recognizes an extra "unsuit" suit which is used to configure "neutral" components like tile backs and coin faces.  Although the primary and directional mark symbols can be configured directly they are often configured indirectly by specifying various "suit" and "rank" symbol configurations.

The configurations in this program "cascade" (sort of like in "Cascading Style Sheets").  A style configuration has the following format::

    style_name(.suit)(.rank)(.component)

The configuration "cascade" priorities are as follows:

#. Direct styles have priority over indirect styles e.g. ``dm_symbols`` has priority over ``suit_symbols.tile_face`` for which symbol is used in the corner of the tile face and in turn ``suit_symbols_font`` has priority over ``font`` for which fonts are used on the coin back.  This is because indirect styles are only used to a generate reasonable default if a direct style cannot be found.
#. Then if there is a tie ``.component_side`` has priority over ``.component`` which has priority over no component specification e.g. ``dm_symbols.saucer_back`` has priority over ``dm_symbols.saucer`` which has priority over just ``dm_symbols``.
#. Then if there is still a tie ``.r#`` has priority over no rank specification e.g. ``invert_colors.r1`` has priority over ``invert_colors``.
#. Then if there is still a tie ``.s#`` has priority over ``.suited`` / ``.unsuited`` which has priority over no suit specification e.g. ``invert_colors.s2`` has priority over ``invert_colors.suited`` has priority over just ``invert_colors``.

Configurations are *often* allowed to be comma-separated to be able to specify different values for different suits or ranks e.g. ``background_colors=white`` or ``background_colors=pink,grey,grey,pink,white`` (note how the last [5th] element specifies that the "unsuit" background color should be "white").  

Licence
-------

This software package and the piecepack pdf's created by it are released under a Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0).  You can see file LICENSE.md for more info.  This license is compatible with version 3 of the Gnu Public License (GPL-3).

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

Although one can use the API to make layouts with components of different sizes the default print-and-play pdf's draw components of the following size which (except for the pawns and non-standard "pawn belts" and "chips") matches the traditional `Mesomorph piecepack dimensions <http://www.piecepack.org/Anatomy.html>`_ if one uses the default component shapes:

- tiles (default "rect") are drawn into a 2" by 2" square 
- coins (default "circle") are drawn into a ¾" by ¾" square
- dice (default "rect") faces are drawn into a ½" by ½" square
- pawn sides (default "halma") are drawn into a ½" by ⅞" rectangle
- "pawn belts" (default "rect") are drawn into a 2" by ½" rectangle
- "pawn saucers" (default "circle") are drawn into a ⅞" by ⅞" square
- "chips" (default "circle") are drawn into a ⅝" by ⅝" square
       
Components are drawn into rectangular drawing spaces (which are always squares except for pawn components).  The program allows one to customize piecepack component shapes.  If a components shape is ``rect`` it will fill up the entire rectangular drawing space, if it is a ``circle`` then the rectangular drawing space will be circumscribed around the circle.  If a components shape is a ``convex#`` or ``concave#``  where ``#`` is the number of exterior vertices then the rectangular drawing space will be circumscribed around a circle that will be circumscribed around that convex/concave polygon.  The rectangular drawing space also is circumscribed around the special ``halma``, ``kite``, and ``pyramid`` shapes.

**Warning:**  Generally it is advisable to uncheck 'fit to size' when printing PDF files otherwise your components maybe re-sized by the printer.

What are the possible color options?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can specify colors either by `RGB hex color codes <http://www.color-hex.com/>`_ or `R color strings <http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf>`_.  "transparent" is a color option which does what you'd expect it to (if used for something other than the background color will render the element effectively invisible).  **Warning:** you shouldn't mix "transparent" backgrounds with the ``invert_colors`` options.

I have some images I want to use as suit/rank/directional mark symbols, how can I use them with this program?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You'll need to take them and put them into a font.  `FontForge <https://fontforge.github.io/en-US/>`_ is a popular open-source program suitable for this task.  `fontcustom <https://github.com/FontCustom/fontcustom>`_ is a popular command-line wrapper around FontForge.  You may need to convert your images from one format to another format first.  To guarantee dispatch by ``fontconfig`` you might want to put the symbols in a part of the "Private Use Area" of Unicode not used by any other fonts on your system.  If you do that you won't need to specify your font otherwise you'll need to configure the ``suit_symbols_font``, ``rank_symbols_font``, and/or ``dm_symbols_font`` options.

What is the purpose of the "hex lines" that can be configured onto the tiles by the ``hexline_colors`` option?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It you use the tiles to build a hex board the hexlines will visually show four of the six hexagon cell sides.

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
2) This R package can be directly used with the ``grid`` graphics library in R to make diagrams.  Here is a link to a `shogi diagram making example <https://github.com/trevorld/piecepack_rules/blob/master/R/make_shogi_diagrams.R>`_.  The important functions for diagram drawing exported by the ``piecepack`` R package are ``load_configurations`` used to load various piecepack configurations and ``draw_component`` which draws piecepack components to the graphics device.  Below is an animation demonstrating how to use the ``piecpackr`` and ``grid`` API's to make a simple tic-tac-toe game diagram (the animation itself was also made with the help of the `animation <https://cran.r-project.org/web/packages/animation/index.html>`_ package):

.. image:: https://www.trevorldavis.com/share/piecepack/tictactoe.gif
   :alt: Example animation of using piecepackr to create piecepack game diagrams
