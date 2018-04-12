Piecepack Graphics R Package
----------------------------

This is an R package designed to make configurable piecepack graphics.  It includes some executable Rscripts designed to make a `"Print & Play" <https://boardgamegeek.com/wiki/page/Print_and_Play_Games#>`_ pdf of `piecepack <http://www.ludism.org/ppwiki/HomePage>`_ components.

**Warning**: This package is currently in an alpha state.  This means the API is incomplete and likely to change.

.. contents::

Installation
------------

You'll need to install some system requirements::

    $ sudo apt install ghostscript pdfsam poppler-utils r-base 

The ``ghostscript``, ``pdfsam``, and ``poppler-utils`` system requirements can be dropped if you do not plan on using the ``exec/collect_piecepacks`` executable to collect several print-and-play pdf's into one pdf (with previews at the beginning).  You'll also need to install the development version of ``grImport2`` R package as well as the ``piecepack`` R package itself::

    $ sudo R
    > install.packages("devtools")
    > devtools::install_github("sjp/grImport2")
    > devtools::install_github("trevorld/piecepack")

If you want to run the demos you'll need ``rake``  and several fonts::

    $ sudo apt install fonts-dejavu fonts-noto rake
    $ fonts_dir=${XDG_DATA_HOME:="$HOME/.local/share"}/fonts
    $ curl -O http://www.quivira-font.com/files/Quivira.otf
    $ cp Quivira.otf $fonts_dir/
    $ curl -O https://noto-website-2.storage.googleapis.com/pkgs/NotoEmoji-unhinted.zip
    $ unzip NotoEmoji-unhinted.zip NotoEmoji-Regular.ttf
    $ cp NotoEmoji-Regular.ttf $fonts_dir/
    $ rm NotoEmoji-unhinted.zip

..    $ curl -O http://www.chessvariants.com/d.font/chess1.ttf
..    $ cp chess1.ttf $fonts_dir/ChessUtrecht.ttf

If you don't install the above fonts then you might need to install some additional fonts onto your system in order to cover all the symbols you'd like to use in your piecepack.  If you have an older version of Ubuntu you may need to manually install additional `Noto fonts <https://www.google.com/get/noto/>`_.

**Warning**: This program embeds (subsets of) fonts into the print-and-play pdf's.  Not all fonts can be legally distributed this way!  Be careful with which ones you use!  The DejaVu, Noto and Quivira fonts used in the demos are legal to embed into CC-BY-SA-4.0 licensed print-and-play pdf's as are all fonts licensed under the SIL Open Font License (OFL).

How to use executable Rscripts
------------------------------

One uses the ``exec/make_piecepack`` command to make a single print-and-play pdf of a piecepack deck.  One uses the ``exec/make_preview`` command to make a svg preview of a piecepack deck.  One can collect several print-and-play pdf's and previews using the ``exec/collect_piecepacks`` command.  The ``exec/make_piecepack`` and ``exec/make_preview`` commands requires JSON configuration either provided as standard input to the program or as a file.  You can view sample configuration files for several demo piecepacks in the ``configurations`` folder.  The ``exec/configure_piecepack`` can be used to generate suitable JSON configuration files or you can manually modify a pre-existing one.  Although the API is in flux you can currently build the demo files and see the command-line calls used to build them by running::

    $ rake demo_name

Where ``demo_name`` is either:

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

* `exec/configure_piecepack --help <https://github.com/trevorld/piecepack/blob/master/txt/configure_piecepack_options.txt>`_
* `exec/make_piecepack --help <https://github.com/trevorld/piecepack/blob/master/txt/make_piecepack_options.txt>`_
* `exec/make_preview --help <https://github.com/trevorld/piecepack/blob/master/txt/make_preview_options.txt>`_
* `exec/collect_piecepacks --help <https://github.com/trevorld/piecepack/blob/master/txt/collect_piecepacks_options.txt>`_

Demo descriptions
-----------------

chess
~~~~~

A demo `"Chess ranked" piecepack pdf <https://www.dropbox.com/s/zksjzil99efjn3r/chess_demo.pdf?dl=0>`_.  These are piecepacks that replace the six traditional piecepack ranks with the six FIDE chess ranks: ♞,♟,♝,♜,♛,♚.  This decreases the abstraction needed to play several variants of chess but does increases the abstraction needed to play some piecepack games (since one needs to mentally convert chess ranks to N,A,2,3,4,5 or 0,1,2,3,4,5).  Some of the decks have checkered tile faces or tile backs and various color schemes to facilitate playing various classic abstract games like Chess, Checkers, Backgammon, and Reversi.  **Warning**: checkered tile backs results in a *non-conforming* piecepack since it leaks information about the direction of the tile face.  The last two pairs of demo chess sets are matched using a "mirrored" color scheme.  Besides reversi this type of scheme is great for playing chess games like `Bughouse chess <https://en.wikipedia.org/wiki/Bughouse_chess>`_ since each side's pieces will all be of the same color.  The last pair of chess-ranked piecepacks altogether has 12 * 6 = 72 coins (perfect for Reversi).

chinese_zodiac
~~~~~~~~~~~~~~

A demo `"Chinese Zodiac" piecepack pdf <https://www.dropbox.com/s/eu5uxwk6hcihy53/chinese_zodiac_demo.pdf?dl=0>`_.  This is two 5-suited piecepacks (Wood, Fire, Earth, Metal, and Water) each using a different six animals taken from the `Chinese zodiac <https://en.wikipedia.org/wiki/Chinese_zodiac>`_ as ranks.

crown_and_anchor
~~~~~~~~~~~~~~~~

A demo `"Crown and anchor" piecepack pdf <https://www.dropbox.com/s/pir2aau09yl11h5/crown_and_anchor_demo.pdf?dl=0>`_.  `"Crown and anchor" <https://en.wikipedia.org/wiki/Crown_and_Anchor>`_ is a classic public domain dice game that uses the following six suits: ♥,♦,♣,♠,♚,⚓.  These six suits were also used by the `"Empire Deck" <https://boardgamegeek.com/boardgame/24869/empire-deck>`_ of playing cards. Jonathan C. Dietrich's classic `JCD Piecepack <http://www.piecepack.org/JCD.html>`_ replaced the Fleur-de-lis (Arms) with Anchors to allow compatibility with these suits.  This demo builds two six-suited piecepack decks using the Crown and anchor suits (one in a classic red/black and another multicolored) and four four-suited piecepack decks using the JCD piecepack suits (one monoscale, one red/black, one classic multicolored, and one in an alternative multicolored scheme).

default
~~~~~~~

A demo `"default" piecepack pdf <https://www.dropbox.com/s/7k1nrhc0sgwm0e3/default_demo.pdf?dl=0>`_.  This is the default type of piecepack built by this software if the user does no configuration (except configure for the use of the "Noto Sans" family of fonts and its filename).  It currently builds a 4-suited piecepack using `multicolored french-suits <https://en.wikipedia.org/wiki/Four-color_deck>`_.

dual
~~~~

A demo `"dual piecepacks" pdf <https://www.dropbox.com/s/iezcku9rktvuk6r/dual_demo.pdf?dl=0>`_ which includes the six piecepacks in the `"dual piecepacks" <http://www.ludism.org/ppwiki/DualPiecepacks>`_ proof-of-concept: one piecepack-suited piecepack, one `latin-suited <https://en.wikipedia.org/wiki/Suit_(cards)#Origin_and_development_of_the_Latin_suits>`_ piecepack (inverted color scheme), two french-suited piecepacks (one dark color scheme, one light color scheme), and two `swiss-suited <https://en.wikipedia.org/wiki/Suit_(cards)#Invention_of_the_Germanic_suits>`_ piecepacks (one dark grayscale color scheme, one light grayscale color scheme).  One could use the piecepack-suited, latin-suited, and one of the french-suited piecepacks to build a "trial hoardpack".

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

A demo `"hex-friendly piecepacks" pdf <https://www.dropbox.com/s/2q7k2kfaung4f6l/hex_demo.pdf?dl=0>`_ of piecepack designs friendly for building and playing games on a hex board.  First deck has hex lines on the tile faces matching the suit color and second decks has grey hex lines on both tiles faces/backs.  If you build a "hex" layout with tiles that have hex lines then the hex lines should show four out of the six "hex" edges.  Third and fourth decks are inspired by the `Hexpack <http://www.hexpack.org/>`_ by Daniel Wilcox and Nathan Morse and have hex-shaped tiles and triangular coins.

orthodox
~~~~~~~~

A demo `"orthodox piecepacks" pdf <https://www.dropbox.com/s/derdlo3j8sdeoox/orthodox_demo.pdf?dl=0>`_.  It includes a piecepack-suited piecepack that complies with the `Anatomy of a Piecepack <http://www.piecepack.org/Anatomy.html>`_ standard as well as a matching 2-color french-suited piecepack (aka a "Playing Cards" expansion).  The "chip" accessory has been configured to be more convenient for labeling paper pyramids to make "piecepack pyramids".

rainbow_deck
~~~~~~~~~~~~

A demo `"Rainbow Deck suited piecepacks" pdf <https://www.dropbox.com/s/dcxrrmcqtfass2r/rainbow_deck_demo.pdf?dl=0>`_.  It builds two 6-suited piecepacks with the suits ♥,★,♣,♦,♛,♠: one in a "dark" multicolored scheme and another in a "light" multicolored scheme.  The `Rainbow Deck (RD) <https://boardgamegeek.com/boardgame/59655/rainbow-deck>`_ is a cardgame system by Chen Changcai.

reversi
~~~~~~~

A demo `"Reversi-friendly piecepacks" pdf <https://www.dropbox.com/s/rgxkdwqwwkd5jbk/reversi_demo.pdf?dl=0>`_.  It contains several piecepacks with color schemes configured to easily distinguish between the back and face of the coins, tiles, and "chips" accessories (and in some decks the suit dice and suit-rank dice) to facilitate the playing of games like `Reversi <http://www.piecepack.org/rules/Reversi.pdf>`_.  It contains a piecepack-suited piecepack with brown "suited" background, an `ACS-elements-suited <http://www.scs.illinois.edu/~mainzv/HIST/Logo/logo.php>`_ piecepack with black "suited" background, dual printer-friendly grayscale sixpacks, and two "mirrored" color scheme six-suited piecepacks where one has a red "suited" background and black "unsuited" background and the other one has a black "suited" background and black "unsuited" background.  Besides reversi a pair of "mirrored" color scheme piecepacks are great for playing games like `Bughouse chess <https://en.wikipedia.org/wiki/Bughouse_chess>`_ and `Backgammon <https://en.wikipedia.org/wiki/Backgammon>`_ since each side's pieces will all be of the same color.  Each of the last two pairs of reversi-friendly piecepacks altogether has 12 * 6 = 72 coins (perfect for Reversi).


sixpack
~~~~~~~

A demo `"Sixpack" pdf <https://www.dropbox.com/s/nr60w36885dgudz/sixpack_demo.pdf?dl=0>`_.  The Sixpack is a six-suited piecepack deck using the following suits: ♥,♠,♣,♦,🌞,🌜.  The demo includes two red/black `Sixpack <http://www.ludism.org/ppwiki/SixPack>`_ suited piecepacks (the second in an "orthodox" scheme) as well as dual multicolor sixpacks.

Licence
-------

This software package and the piecepack pdf's created by it are released under a Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0).  You can see file LICENSE for more info.  This license is compatible with version 3 of the Gnu Public License (GPL-3).

Frequently Asked Questions
--------------------------

How should I Print & Play my piecepack?
    The Print-and-Play pdf's produced by the ``exec/make_piecepack`` command are designed to be used in three different ways:

    1. Print single-sided on label paper, cut out the labels, and apply to components (in the material of your choice).  
    2. Print single-sided on paper(board), apply adhesive to the back, fold over in half "hot-dog-style", and cut out the components.  One will need to to some additional folding and application of adhesive/tape in order to construct the dice and pawns.  One can build more dice/pawns/pawn belts if you cut them out *before* folding the paper(board) in half but if you don't do so you should still have all the "standard" piecepack components.
    3. Print double-sided on paper(board) and cut out the components.  One will need to do some additional folding and application of adhesive/tape in order to construct the dice and pawns.

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
    You can specify colors either by `RGB hex color codes <http://www.color-hex.com/>`_ or `R color strings <http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf>`_.  "transparent" is a color option which does what you'd expect it to (if used for something other than the background color will render the element effectively invisible).
