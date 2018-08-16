Piecepack Graphics R Package
============================

.. image:: https://travis-ci.org/trevorld/piecepack.png?branch=master
    :target: https://travis-ci.org/trevorld/piecepack
    :alt: Build Status

.. image:: https://ci.appveyor.com/api/projects/status/github/trevorld/piecepack?branch=master&svg=true 
    :target: https://ci.appveyor.com/project/trevorld/piecepack
    :alt: AppVeyor Build Status

.. image:: https://img.shields.io/codecov/c/github/trevorld/piecepack/master.svg
    :target: https://codecov.io/github/trevorld/piecepack?branch=master
    :alt: Coverage Status

.. image:: http://www.repostatus.org/badges/latest/wip.svg
   :alt: Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.
   :target: http://www.repostatus.org/#wip

This is an R package designed to make configurable piecepack graphics.  It includes some executable Rscripts designed to make a `"Print & Play" <https://boardgamegeek.com/wiki/page/Print_and_Play_Games#>`_ pdf of `piecepack <http://www.ludism.org/ppwiki/HomePage>`_ components as well as an Rscript to build images of individual components.  The API can also be used with the ``grid`` R package to make piecepack diagrams (i.e. for inclusion in rulesets) or even to make a custom Print & Play layout.

.. contents::

Installation
------------

Short instructions using Rakefile on Ubuntu (16.04+) including "Bash on Ubuntu on Windows (10)"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The below commands clones repo and installs a bunch of system dependencies, fonts, and R packages (often using ``sudo``) but doesn't add executable Rscripts to ``$PATH``.  Should sets up everything needed to build all the demos on a recent version of Ubuntu (i.e. more things are installed then what a strictly minimal install would need).::

    $ sudo apt install git rake
    $ git clone https://github.com/trevorld/piecepack
    $ cd piecepack
    $ rake install_dependencies_ubuntu
    $ rake install

To update previously cloned repo to newest version and re-install R package::

    $ cd piecepack
    $ git pull
    $ rake install

Detailed instructions
~~~~~~~~~~~~~~~~~~~~~

System dependencies
+++++++++++++++++++

#. `R <https://cran.r-project.org/>`_ compiled with support for Cairo plus several R packages file available on CRAN which are usually installed for you when you install the ``piecepack`` R package using ``devtools::install()``.
#. Unicode font(s) (installed where Cairo can find them) that (altogether) have all your required glyphs

System suggestions
++++++++++++++++++

#. ``gs`` (part of `ghostscript <https://www.ghostscript.com/>`_) (needed for ``collect_piecepacks`` executable)
#. ``pdfinfo`` (part of `poppler-utils <https://poppler.freedesktop.org/>`_) (needed for ``collect_piecepacks`` executable)
#. `Rake - Ruby Make <https://github.com/ruby/rake>`_ (needed for running demos and other developer build commands)
#. Several `Noto <https://www.google.com/get/noto/>`_ fonts (in particular "Noto Sans", "Noto Sans Symbols", "Noto Sans Symbols2", "Noto Emoji", "Noto Sans Cham", "Noto Sans CJK SC")
#. `Quivira <http://quivira-font.com/>`_ font
#. `DejaVu Sans <https://dejavu-fonts.github.io/>`_ font

Installation Notes
++++++++++++++++++

This package is developed and tested on Ubuntu Linux.  Instructions are given below for installation on Ubuntu Linux but installing on another \*nix OS should be a straightforward substitution of the ``apt`` package manager with your OS's preferred package manager like ``brew`` for OSX (you may also need to tweak the package names to match what is in your repos and to manually install some software/fonts not in your repos).  The system dependencies/suggestions are all *theoretically* installable on Windows but it is likely easier on recent versions of Windows to `install and run an Ubuntu terminal <https://www.microsoft.com/en-us/store/p/ubuntu/9nblggh4msv6>`_ or to (freely) run Ubuntu in a virtual machine or possibly even in a ``chroot``.  

You'll need to install some system requirements to use this R package and its executables::

    $ sudo apt install ghostscript poppler-utils r-base 

The ``ghostscript`` and ``poppler-utils`` system requirements can be dropped if you do not plan on using the ``collect_piecepacks`` executable to collect several print-and-play pdf's into one pdf (with previews at the beginning).  You'll also need to install the development version of the ``piecepack`` R package and its R package dependencies.  These can easily be installed with help of the ``install`` function from the ``devtools`` package ::

    $ sudo apt install libcurl4-openssl-dev libssl-dev # system dependencies to install devtools packages
    $ sudo Rscript -e "install.packages(\"devtools\", repos=\"https://cran.rstudio.com/\")"' 
    $ sudo Rscript -e "devtools::install_github(\"trevorld/piecepack\")"

R does not add executables in an installed R package to a user's path.  If you plan on using the Rscript executables included with this package (in the ``exec`` folder) you can either:

1. Find where R installed them and either use them directly (perhaps with help of an 'alias' or 'symbolic link') or add that directory to your ``$PATH``.  The location is system dependent but on my computer they are located in ``/usr/local/lib/R/site-library/piecepack/exec/``. 
2. Download them from github, mark them executable (if necessary), and if desired manually add them to your path (perhaps by creating a symbolic link pointing to them in ``$HOME/bin/``).  Simple but you may need to re-download them again if you ever upgrade the underlying R package.  If you clone the entire repo you can download the newest versions using ``git pull``::

    $ git clone https://github.com/trevorld/piecepack # done only once
    $ cd piecepack # executables are in the exec folder
    $ git pull # downloads any updates to the executables
    $ sudo Rscript -e "devtools::install(quiet=TRUE, upgrade_dependencies=FALSE)" # re-install R package

3. You can use a simple shell script wrapper like `Rbin <https://github.com/trevorld/Rbin>`_ to access them::

    $ Rbin piecepack configure_piecepack [options]
    $ Rbin piecepack make_piecepack [options]
    $ Rbin piecepack make_preview [options]
    $ Rbin piecepack collect_piecepacks [options]

   If using ``Rbin`` you may want to create some aliases in your ``.bashrc`` file so it appears that the Rscript executables are on your path::

    alias configure_piecepack="Rbin piecepack configure_piecepack"
    alias make_piecepack="Rbin piecepack make_piecepack"
    alias make_preview="Rbin piecepack make_preview"
    alias collect_piecepacks="Rbin piecepack collect_piecepacks"

If you want to run the demos you'll also need to clone the git repository and you'll need ``rake``  and several fonts:: 

    $ git clone https://github.com/trevorld/piecepack
    $ sudo apt install fonts-dejavu fonts-noto rake
    $ fonts_dir=${XDG_DATA_HOME:="$HOME/.local/share"}/fonts
    $ curl -O http://www.quivira-font.com/files/Quivira.otf
    $ mv Quivira.otf $fonts_dir/
    $ curl -O https://noto-website-2.storage.googleapis.com/pkgs/NotoEmoji-unhinted.zip
    $ unzip NotoEmoji-unhinted.zip NotoEmoji-Regular.ttf
    $ mv NotoEmoji-Regular.ttf $fonts_dir/
    $ rm NotoEmoji-unhinted.zip

..    $ curl -O http://www.chessvariants.com/d.font/chess1.ttf
..    $ mv chess1.ttf $fonts_dir/ChessUtrecht.ttf

Since rake runs the demos locally in the cloned repo directory you don't need to worry about whether the Rscript executables are on your path or not when running a demo. If you want to upgrade to the newest version of the package you'll need to run ``$ git pull; rake install`` to download the newest versions of the Rscript executables and the demo-building ``Rakefile`` and to then re-install the ``piecepack`` R package.  If you have an older version of Ubuntu you may need to manually install additional `Noto fonts <https://www.google.com/get/noto/>`_ if you want to run the demos.

If you don't install the above fonts then you might need to install some additional fonts onto your system in order to cover all the symbols you'd like to use in your piecepack.  **Warning**: This program embeds (subsets of) fonts into the print-and-play pdf's.  Not all fonts can be legally distributed this way!  Be careful with which ones you use!  The DejaVu, Noto and Quivira fonts used in the demos are legal to embed into CC-BY-SA-4.0 licensed print-and-play pdf's as are all fonts licensed under the SIL Open Font License (OFL).

If you want to help **develop** the ``piecepack`` R package you'll also need to install the suggested packages so you can run the unit tests and re-build the documentation::

    $ sudo apt install libxml2-dev libcairo2-dev
    $ sudo Rscript -e "devtools::install(dependencies=\"Suggests\", upgrade_dependencies=FALSE)"


How to use executable Rscripts
------------------------------

One uses the ``make_pnp_piecepack`` executable to make a single print-and-play pdf of a piecepack deck.  One uses the ``make_piecepack_preview`` executable to make a svg preview of a piecepack deck.  One can collect several print-and-play pdf's and previews using the ``collect_pnp_piecepacks`` executable.  The ``make_piecepack_images`` executable makes individual images of piecepack components.  The ``make_pnp_piecepack``, ``make_piecepack_images``, and ``make_piecepack_preview`` executables requires JSON configuration either provided as standard input to the program or as a file.  You can view sample configuration files for several demo piecepacks in the ``configurations`` folder.  The ``configure_piecepack`` executable can be used to generate suitable JSON configuration files or you can manually modify a pre-existing one.  Although the API is in flux you can currently build the demo files and see the command-line calls used to build them by running::

    $ rake demo_name

Where ``demo_name`` is either:

#. ``all`` (makes each of the following demos)
#. ``chess``
#. ``chinese_zodiac``
#. ``crown_and_anchor``
#. ``default``
#. ``dual``
#. ``hex``
#. ``orthodox``
#. ``rainbow_deck``
#. ``reversi``
#. ``sixpack``

Executable options
------------------

* `configure_piecepack --help <https://github.com/trevorld/piecepack/blob/master/txt/configure_piecepack_options.txt>`_
* `make_pnp_piecepack --help <https://github.com/trevorld/piecepack/blob/master/txt/make_pnp_piecepack_options.txt>`_
* `make_piecepack_images --help <https://github.com/trevorld/piecepack/blob/master/txt/make_piecepack_images_options.txt>`_
* `make_piecepack_preview --help <https://github.com/trevorld/piecepack/blob/master/txt/make_piecepack_preview_options.txt>`_
* `collect_pnp_piecepacks --help <https://github.com/trevorld/piecepack/blob/master/txt/collect_pnp_piecepacks_options.txt>`_

.. _`Demo descriptions`:

Demos
-----

chess
~~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/zksjzil99efjn3r/chess_demo.pdf?dl=0>`__ of some "chess-ranked" piecepacks.  These are piecepacks that replace the six traditional piecepack ranks with the six FIDE chess ranks: ♟,♞,♝,♜,♛,♚.  This decreases the abstraction needed to play several variants of chess but does increases the abstraction needed to play some piecepack games (since one needs to mentally convert chess ranks to N,A,2,3,4,5 or 0,1,2,3,4,5).  Some of the decks have checkered tile faces or tile backs and various color schemes to facilitate playing various classic abstract games like Chess, Checkers, Backgammon, and Reversi.  **Warning**: checkered tile backs results in a *non-conforming* piecepack since it leaks information about the direction of the tile face.  The last two pairs of demo chess sets are matched using a "mirrored" color scheme.  Besides reversi this type of scheme is great for playing chess games like `Bughouse chess <https://en.wikipedia.org/wiki/Bughouse_chess>`_ since each side's pieces will all be of the same color.  The last pair of chess-ranked piecepacks altogether has 12 * 6 = 72 coins (perfect for Reversi).  An interesting accessory for several of these chess-ranked piecepacks could be Daniel Wilcox and Nathan Morse's `The King's Caste <https://boardgamegeek.com/boardgame/38417/kings-caste>`_ Tarot deck.

chinese_zodiac
~~~~~~~~~~~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/eu5uxwk6hcihy53/chinese_zodiac_demo.pdf?dl=0>`__ of some "Chinese Zodiac" piecepacks.  These are a pair of 5-suited piecepacks (Wood, Fire, Earth, Metal, and Water) each using a different six animals taken from the `Chinese zodiac <https://en.wikipedia.org/wiki/Chinese_zodiac>`_ as ranks.

crown_and_anchor
~~~~~~~~~~~~~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/pir2aau09yl11h5/crown_and_anchor_demo.pdf?dl=0>`__ of some "Crown and anchor" suited piecepacks.  `"Crown and anchor" <https://en.wikipedia.org/wiki/Crown_and_Anchor>`_ is a classic public domain dice game that uses the following six suits: ♥,♦,♣,♠,♚,⚓.  These six suits were also used by the `"Empire Deck" <https://boardgamegeek.com/boardgame/24869/empire-deck>`_ of playing cards. Jonathan C. Dietrich's classic `JCD Piecepack <http://www.piecepack.org/JCD.html>`_ replaced the Fleur-de-lis (Arms) with Anchors to allow compatibility with these suits.  This demo builds two six-suited piecepack decks using the Crown and anchor suits (one in a classic red/black and another multicolored) and four four-suited piecepack decks using the JCD piecepack suits (one monoscale, one red/black, one classic multicolored, and one in an alternative multicolored scheme).

default
~~~~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/7k1nrhc0sgwm0e3/default_demo.pdf?dl=0>`__ of the default type of piecepack built by this software if the user does no configuration (except configure for the use of the "Noto Sans" family of fonts and its filename).  It currently builds a 4-suited piecepack using `multicolored french-suits <https://en.wikipedia.org/wiki/Four-color_deck>`_.

dual
~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/iezcku9rktvuk6r/dual_demo.pdf?dl=0>`__ of the six piecepacks in the `"dual piecepacks" <http://www.ludism.org/ppwiki/DualPiecepacks>`_ proof-of-concept: one piecepack-suited piecepack, one `latin-suited <https://en.wikipedia.org/wiki/Suit_(cards)#Origin_and_development_of_the_Latin_suits>`_ piecepack (inverted color scheme), two french-suited piecepacks (one dark color scheme, one light color scheme), and two `swiss-suited <https://en.wikipedia.org/wiki/Suit_(cards)#Invention_of_the_Germanic_suits>`_ piecepacks (one dark grayscale color scheme, one light grayscale color scheme).  One could use the piecepack-suited, latin-suited, and one of the french-suited piecepacks to build a "trial hoardpack".

"Dual piecepacks" are eight piecepack **suits** with the following properties:

* The eight **suits** suits can be "easily" visually distinguished
* The eight suits can be "easily" visually split into two separate **groups** of four suits
* Each "suit" in a group can be "easily" visually **linked** with exactly one suit in the other group 

This gives one the following nice properties:

* One can play games requiring one piecepack deck plus an expansion piecepack deck by treating the eight **suits** as separate suits
* One can play games requiring two piecepack decks by treating each pair of **linked** suits as the same suit
* One can play games that are "SixPack" friendly by taking three suits from each visually distinct **group**. One can scale this down to games that are "Playing Cards Expansion" friendly or even scale up to four-grouped-suits versus four-grouped-suits friendly games (like Canadian checkers or Bughouse chess).
* One can play entirely new games provided by the extra layer of relationships. Proof-of-concept new game is `Dual Piecepacks Poker <http://www.ludism.org/ppwiki/DualPiecepacksPoker>`_. 

It is possible to construct three piecepacks where each pair of piecepack decks are "dual piecepacks" (e.g. piecepack-suited + inverted latin-suited + light french-suited). This could be called a "trial `HoardPack <http://www.ludism.org/ppwiki/HoardPack>`_" (apparently "trial" is the proper "three" analogue to "dual"). 

hex
~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/2q7k2kfaung4f6l/hex_demo.pdf?dl=0>`__ of piecepack designs friendly for building and playing games on a hex board.  First deck has hex lines on the tile faces matching the suit color and second deck has grey hex lines on both tiles faces/backs.  If you build a "hex" layout with tiles that have hex lines then the hex lines should show four out of the six "hex" edges.  Third and fourth decks are inspired by the Hexpack_ by Daniel Wilcox and Nathan Morse and have hex-shaped tiles and triangular coins.  The third deck has the traditionally "red" french suits have a pink background and the traditionally "black" suits have a grey background: three different background colors (pink, grey, white) facilitate building certain types of `hexagonal boards <https://en.wikipedia.org/wiki/Hexagonal_chess>`_.  **Warning:** the hexagonal tiles produced by this program are a little bit smaller than those suggested by the Hexpack_ standard (i.e. instead of a hexagon circumscribed around a 2" diameter circle we have a 2" diameter circle circumscribed around the hexagon), note this does mean that these hexagons can fit entirely onto 2" by 2" square tiles.

.. _Hexpack: http://www.hexpack.org/

orthodox
~~~~~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/derdlo3j8sdeoox/orthodox_demo.pdf?dl=0>`__ of a piecepack-suited piecepack that complies with the `Anatomy of a Piecepack <http://www.piecepack.org/Anatomy.html>`_ standard as well as a matching 2-color french-suited piecepack (aka a "Playing Cards" expansion).  The "chip" accessory has been configured to be more convenient for labeling paper pyramids to make "piecepack pyramids".

rainbow_deck
~~~~~~~~~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/dcxrrmcqtfass2r/rainbow_deck_demo.pdf?dl=0>`__ of a pair of "Rainbow Deck" suited piecepacks.  It builds two 6-suited piecepacks with the suits ♥,★,♣,♦,♛,♠: one in a "dark" multicolored scheme and another in a "light" multicolored scheme.  The `Rainbow Deck (RD) <https://boardgamegeek.com/boardgame/59655/rainbow-deck>`_ is a cardgame system by Chen Changcai.

reversi
~~~~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/rgxkdwqwwkd5jbk/reversi_demo.pdf?dl=0>`__ of several piecepacks with color schemes configured to easily distinguish between the back and face of the coins, tiles, and "chips" accessories (and in some decks the suit dice and suit-rank dice) to facilitate the playing of games like `Reversi <http://www.piecepack.org/rules/Reversi.pdf>`_.  It contains a piecepack-suited piecepack with brown "suited" background, an `ACS-elements-suited <http://www.scs.illinois.edu/~mainzv/HIST/Logo/logo.php>`_ piecepack with black "suited" background, dual printer-friendly grayscale sixpacks, and two "mirrored" color scheme six-suited piecepacks where one has a red "suited" background and black "unsuited" background and the other one has a black "suited" background and black "unsuited" background.  Besides reversi a pair of "mirrored" color scheme piecepacks are great for playing games like `Bughouse chess <https://en.wikipedia.org/wiki/Bughouse_chess>`_ and `Backgammon <https://en.wikipedia.org/wiki/Backgammon>`_ since each side's pieces will all be of the same color.  Each of the last two pairs of reversi-friendly piecepacks altogether has 12 * 6 = 72 coins (perfect for Reversi).

sixpack
~~~~~~~

A demo `print-and-play pdf <https://www.dropbox.com/s/nr60w36885dgudz/sixpack_demo.pdf?dl=0>`__ of some Sixpacks.  The Sixpack is a six-suited piecepack deck using the following suits: ♥,♠,♣,♦,🌞,🌜.  The demo includes two red/black `Sixpack <http://www.ludism.org/ppwiki/SixPack>`_ suited piecepacks (the second in an "orthodox" scheme) as well as dual multicolor sixpacks.

Licence
-------

This software package and the piecepack pdf's created by it are released under a Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0).  You can see file LICENSE for more info.  This license is compatible with version 3 of the Gnu Public License (GPL-3).

Frequently Asked Questions
--------------------------

How should I Print & Play my piecepack?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Print-and-Play pdf's produced by the ``make_piecepack`` executable are designed to be used in three different ways:

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
- "pawn belts" (default "rect") are drawn into a 1½" by ½" rectangle
- "pawn saucers" (default "circle") are drawn into a ⅞" by ⅞" square
- "chips" (default "circle") are drawn into a ⅝" by ⅝" square
       
Components are drawn into rectangular drawing spaces (which are always squares except for pawn components).  The program allows one to customize piecepack component shapes.  If a components shape is ``rect`` it will fill up the entire rectangular drawing space, if it is a ``circle`` then the rectangular drawing space will be circumscribed around the circle.  If a components shape is a ``star`` or a regular polygon specified by its number of sides then the rectangular drawing space will be circumscribed around a circle that will be circumscribed around that regular polygon (or ``star``).  The rectangular drawing space also is circumscribed around the special ``halma`` and ``kite`` shapes.

**Warning:**  Generally it is advisable to uncheck 'fit to size' when printing PDF files otherwise your components maybe re-sized by the printer.

What are the possible color options?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can specify colors either by `RGB hex color codes <http://www.color-hex.com/>`_ or `R color strings <http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf>`_.  "transparent" is a color option which does what you'd expect it to (if used for something other than the background color will render the element effectively invisible).  **Warning:** you shouldn't mix "transparent" backgrounds with the ``invert_colors`` options.

I have some images I want to use as suit/rank/directional mark symbols, how can I use them with this program?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You'll need to take them and put them into a font.  `FontForge <https://fontforge.github.io/en-US/>`_ is a popular open-source program suitable for this task.  `fontcustom <https://github.com/FontCustom/fontcustom>`_ is a popular command-line wrapper around FontForge.  You may need to convert your images from one format to another format first.  To guarantee dispatch by ``fontconfig`` you might want to put the symbols in a part of the "Private Use Area" of Unicode not used by any other fonts on your system.  If you do that you won't need to specify your font otherwise you'll need to configure the ``suit_symbols_font``, ``rank_symbols_font``, and/or ``dm_symbols_font`` options.

What are the "chips" accessories that shows up on the accesories page of the print-and-play pdf supposed to be used for?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The "chips" are a customizable accessory that can aid in playing certain types of games.  Some possible uses:

-  One option (and source of the name "chip") is to mount them on suit-colored poker chips.  By default both sides will show suit and direction and one side will also show a rank. In such a configuration it could be used to replace piecepack pyramids in a subset of games like Alien City or Ice Floe, could be used to add more pieces in games like checkers/go, could be used to reduce abstraction in chess (i.e. each side's pieces could be distinguished by color), etc. 
-  A second option would be to mount them on pyramids to make `piecepack pyramids <http://www.ludism.org/ppwiki/PiecepackPyramids>`_.  A classic configuration for this purpose would be ``--rank_symbols.chip_face='A,B,C,D,E,F' --use_ace_as_ace.chip_face --dm_symbols.chip= --shape.chip=kite``.
-  A third option would be to produce the "piecepack stones" accessory (i.e. from the `Sensible Expansions proposal <http://www.ludism.org/ppwiki/SensibleExpansions>`_).  A good configuration for this purpose would be  ``--suit_symbols.chip_back= --dm_colors.chip_back=grey --dm_symbols=■ --uninvert_colors.chip_back  --shape.chip=rect``. 
-  A fourth option would be to produce a "suit (star) coin" accessory (i.e. from the `JCD piecepack <http://www.piecepack.org/JCD.html>`_).  A good configuration for this purpose would be ``--use_suit_as_ace.chip_face --invert_colors.chip_face --shape.chip=star --rank_symbols_scale.chip_face=0.7 --dm_symbols_scale.chip=0.7 --suit_symbols_scale.chip_back=0.7``.  
-  A fifth option if paired with another deck with six extra ranks would be to mount the chip faces on a large d12 to make a "dozenal piecepack die" for each suit.  The suits could then also go on a d12 to make a "dozenal suit die" especially if there are in fact a dozen suits.  A good configuration for this purpose would be "``--shape.chip=5``.
-  A sixth option would be to make "hexpack triangular chits" (i.e. from the `Hexpack`_).  A good configuration for this purpose would be ``--shape.chip=3 --dm_theta.chip=-90 --dm_symbols_scale.chip=0.7 --suit_colors.chip_back=``.

What is the purpose of the "hex lines" that can be configured onto the tiles by the ``hexline_colors`` option?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It you use the tiles to build a hex board the hexlines will visually show four of the six hexagon cell sides.

Why does the package sometimes use a different font then the one I instructed it to use for a particular symbol?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The program uses ``Cairo`` which uses ``fontconfig`` to select fonts.  ``fontconfig`` picks what it thinks is the 'best' font and sometimes it annoyingly decides that the font to use for a particular symbol is not the one you asked it to use (i.e. this sometimes happens to me in my demos but since the decks still look nice with the font it chooses I decided at some point not to waste anymore time banging my head on messing around with ``fontconfig`` configuration files trying to override ``fontconfig``).  Also as a sanity check use the command-line tool ``fc-match`` to make sure you specified your font correctly in the first place (i.e. ``fc-match "Noto Sans"`` on my system returns "Noto Sans" but ``fc-match "Sans Noto"`` returns "DejaVu Sans" and not "Noto Sans").  If this happens and you really care about it then the only way to guarantee your symbols will be dispatched would be to either make a new font and re-assign the symbols to code points in the Unicode "Private Use Area" that aren't used by any other font on your system or to delete from your system the fonts that ``fontconfig`` chooses over your font.

How do I use this package in piecepack rulesets?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two main ways that this package could be used to help make piecepack rulesets:

1) The ``make_piecepack_images`` executable makes individual images of components.  By default it makes them in the pdf, png, and svg formats with rotations of 0, 90, 180, and 270 degrees but with configuration can also make them in the bmp, jpeg, tiff, and ps formats and other rotations.  These can be directly inserted into your ruleset or even used to build diagrams with the aid of a graphics editor program.  An example filename (and directory) is ``pdf/components/orthodox1/tile_face_s1_r5_t180.pdf`` where ``orthodox1`` is the configuration used to build that image, ``tile`` is the component, ``face`` is the side, ``s1`` indicates it was the first suit, ``r5`` indicates it was the 5th rank, ``t180`` indicates it was rotated 180 degrees, and ``pdf`` indicates it is a pdf image.
2) This R package can be directly used with the ``grid`` graphics library in R to make diagrams.  Here is a link to a `shogi diagram making example <https://github.com/trevorld/piecepack_rules/blob/master/R/make_shogi_diagrams.R>`_.  The important functions for diagram drawing exported by the ``piecepack`` R package are ``read_configuration`` used to read in a JSON configuration file with the relevant piecepack configuration and ``draw_component`` which draws piecepack components to the graphics device. 
