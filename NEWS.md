piecepackr 1.3.1
================

New features
------------

* Graphic functions now support more game pieces:
  - ``board_face``, ``board_back`` for representing boards (#153).
  - ``card_face``, ``card_back`` for representing cards (#124).
  - ``bit_face``, ``bit_back`` for representing miscellaneous game pieces (#155).
* Function ``game_systems`` now returns more game systems:
  - ``dice``: Normal six-sided pipped dice in six color schemes (#166).
  - ``checkers1``, ``checkers2``: Checkered boards and checkers.  ``checkers1`` has 1" cells and ``checkers2`` has 2" cells (#168).
  - ``dominoes``, ``dominoes_black``, ``dominoes_blue``, ``dominoes_green``, ``dominoes_red``, ``dominoes_white``, ``dominoes_yellow``: Traditional Double-12 pipped dominoes in various color schemes (#159).  
  - ``playing_cards``, ``playing_cards_colored``, ``playing_cards_tarot`` (#170): 
    + ``playing cards`` is a traditional (French-suited) deck of (Poker) playing cards.
    + ``playing_cards_colored`` has five suits: red hearts, black spades, green clubs, blue diamonds, and yellow stars.
    + ``playing_cards_tarot`` is a (French Bourgeois) tarot deck with four suits (hearts, spades, clubs, diamonds) with 14 ranks (including knights) plus 22 trump cards (1-21 plus an "excuse").
* New function ``save_piece_obj`` for writing Waveform OBJ files (and associated MTL files and png textures) (#189, #190).
  Note support for dice and pawns are currently rather crude.
* New function ``piece3d`` for making graphics with the ``rgl`` package (#76).
* New function ``piece`` for making ``rayrender`` objects.

Bug fixes and minor improvements
--------------------------------

* Configuration lists now support an "oval" shape (#182).
* Improved drawing of oblique projection edges for "concave" polygons (stars) 
  and non-regular "convex" polygons (#174).
* Improved drawing of pyramids in the 3D oblique projection (#172).
* Fixes bug in ``op_transform`` when sorting tile faces rotated in various directions (#163).
* ``op_transform`` now accepts new argument ``pt_thickness``for improved handling of stacked pyramid (tops).
* The ``edge_color`` for dice, pawns, and pyramids now defaults to the background color of ``piece_side``.
* The ``edge_color`` for matchsticks now defaults to the background color of the ``matchstick_back``.
* Font sizes and locations of piecepack matchsticks in ``game_systems`` made more "standard".
* ``pmap_piece`` now supports an ``.f`` function to specify the function to apply to the rows of the data frame.
  By default it uses the (backwards-compatible) ``pieceGrob``.
* ``pp_cfg()$get_depth()`` now has better depth calculation for pyramid faces (representing laid down pyramids).
* ``pp_cfg`` now supports a new ``op_grob_fn`` style indicating which function to draw pieces with 
  when drawing with a 3D oblique projection in ``grid``.  The older ``shadow_fn`` style alternative is now deprecated.
* ``grid.piece`` (and ``pieceGrob``) now support a ``scale`` and ``alpha`` argument (#201).

Deprecated features
-------------------

The following ``pp_cfg`` R6 class public method is now deprecated:
    * ``get_pictureGrob``.  Use ``get_grob(piece_type, suit, rank, type="picture")`` instead.
The following ``pp_cfg`` "style" is now deprecated:
    * ``shadow_fn``.  Use the new ``op_grob_fn`` instead.

Breaking changes
----------------

* The function ``game_systems`` no longer returns an ``icehouse_pieces`` configuration.
  Configurations for Looney Pyramids can be found at https://github.com/piecepackr/piecenikr

piecepackr 1.2.1
================

New features
------------

* New function ``picturePieceGrobFn`` which returns a "grob" function that imports graphics
  from files found in its ``directory`` argument (#152).
* New function ``game_systems`` which returns a list of configuration objects
  for multiple game systems (#157):

  - ``dual_piecepacks_expansion``: A companion piecepack with a special suit scheme.
                 See https://trevorldavis.com/piecepackr/dual-piecepacks-pnp.html.
  - ``hexpack``: A hexagonal extrapolation of the piecepack designed by Nathan Morse and Daniel Wilcox.
                  See https://boardgamegeek.com/boardgameexpansion/35424/hexpack.
  - ``piecepack``: A public domain game system invented by James "Kyle" Droscha.
     See http://www.ludism.org/ppwiki.
     Configuration also contains the following piecepack accessories:

       + ``piecepack matchstick``s: A public domain accessory developed by Dan Burkey.
                                   See http://www.ludism.org/ppwiki/PiecepackMatchsticks.
       + ``piecepack pyramids``: A public domain accessory developed by Tim Schutz.
                                See http://www.ludism.org/ppwiki/PiecepackPyramids.
       + ``piecepack saucers``: A public domain accessory developed by Karol M. Boyle at Mesomorph Games.
                                See https://web.archive.org/web/20190719155827/http://www.piecepack.org/Accessories.html
  - ``playing_cards_expansion``: A piecepack with the standard "French" playing card suits.
                                  See http://www.ludism.org/ppwiki/PlayingCardsExpansion.
  - ``subpack``: A "mini" piecepack.  Designed to be used with the ``piecepack`` to make piecepack
                 "stackpack" diagrams.  See http://www.ludism.org/ppwiki/StackPack.
   
* Helper functions ``to_subpack`` and ``to_hexpack`` which given a piecepack configuration
  attempts to generate an appropriate matching (piecepack stackpack) subpack and hexpack (#161).
* New function ``file2grob`` that imports a given image file as a grob.

Bug fixes and minor improvements
--------------------------------

* Guesses made by ``op_transform`` for whether two pieces
  overlap are now more accurate (#150).
* Fixes bug in ``op_transform`` when computing bounding box for shapes 
  when width and height are not equal.
* Fixes bug in ``op_transform`` for inferring width/height of matchsticks 
  and shape/depth of pyramids.
* Fixes bug in printing ``pp_cfg`` objects when a custom grob function had been set.
* Fixes bug in setting individual suit colors with configurations list styles like ``suit_color.s2="white"``
* ``pp_cfg`` objects' ``get_suit_color`` function is now vectorized 
  (and by default now returns the suit colors for each suit).

piecepackr 1.1.1
================

* New helper function ``op_transform`` and new ``pmap_piece`` argument ``trans``
  to facilitate making graphics with an oblique projection (#138).
* Fixes bug when drawing non-regular-dimensioned "convex" shapes in an
  oblique 3D projection (#149)

piecepackr 1.0.2
================

* Initial "stable" release.

New features
------------

* ``save_print_and_play`` function now supports adding piecepack pyramids (#37), matchsticks (#69), and subpacks (aka "travel" piecepacks) (#129) to the print-and-play layout.
* Now supports drawing most pieces with a simple 3D oblique projection (#76).
* New ``pp_cfg`` function (and R6 class) which on-the-fly builds a cache to speed up component drawing (#112, #122). 
  Can also signal to rule diagram functions whether the piecepack lacks certain components (#130).
* Now exports several additional utility functions for those who would like to use their own custom draw function (#116).  See ``help("pp_utils")``, ``help("grob_fn_helpers")``, and ``help("grid.piece")`` for more info.

Breaking changes
----------------

* ``component_opt`` color names are now ``_color`` instead of ``_col`` to better match "configuration lists".
* ``draw_component`` has been renamed ``grid.piece`` (and many of its arguments vectorized) and
  ``draw_components`` has been renamed ``pmap_piece``(#123).
* ``make_images`` has been renamed ``save_piece_images`` and ``make_pnp`` has been renamed ``save_print_and_play``.
* The function arguments ``component_side``, ``i_r``, and ``i_s`` have been 
  renamed ``piece_side``, ``rank``, and ``suit``.
* Lots of configuration list style names were changed (#95, #121, #140).
* All executable Rscripts have been removed (#113).
* ``make_collection`` and ``make_preview`` functions removed.
* ``load_configurations`` and ``read_configuration`` functions removed.
* ``make_pnp`` print-and-play layouts have been radically re-configured and renamed to ``save_print_and_play`` (#54).
* Non-standard ``chip`` component no longer supported by piecepackr (#114)
* 'hexlines' and 'checkers' no longer supported by base piecepackr configuration (#115).  
  NB. Can still add checkers and hexlines via a custom drawing function.
* ``make_pnp`` now gets deck title from the configuration element ``title`` (#118)
* ``ribbons`` are now ``mat`` (as in picture frame 'matting') (#117)
* Default pawn "belt" width is now ¾π" (#105).  
* Default pawn "saucer" width is now ¾" and they now have a 'mat' coloring on the edge (#105)
* ``draw_preview`` function has been removed.  Can now do ``grid.piece("preview_layout", cfg=cfg)``.
* Demos moved to companion website: https://trevorldavis.com/piecepackr

Bug fixes and minor improvements
--------------------------------

* Bug in ``get_embedded_font`` function fixed.
* ``make_pnp`` function now supports the A5 page size and is more A4 page size friendly (#54).
* Can now specify ``fontface`` (#121) as well as ``width``, ``height``, and ``depth`` (#106) in configuration lists.
* Removed some package dependencies.
* ``pmap_piece`` now supports ``angle=NA`` and ``grid.piece`` no longer draws different output
  for components like ``tile_back`` if you specify a suit or rank (#120).
* "configuration lists" now support ``credit``, ``copyright``, and ``description`` fields  
  which add extra info to the new "credit", "copyright", and "description" sections of the print-and-play layout.  
* "configuration lists" now support ``coin_arrangement`` (#136).
*  Also if ``ghostscript`` installed will automatically embed metadata in the print-and-play pdf (#93).
* "configuration lists" now support ``border_lex`` and ``gridline_lex`` to allow customizing the width of the border and grid lines.
* ``save_piece_images``'s ``format`` argument is now vectorized.  
* ``save_print_and_play`` can now save a print-and-play "svg" and "ps" file (besides "pdf").
* ``grid.piece`` no longer throws a warning when ``use_pictureGrob==TRUE`` and ``angle!=0`` (#148).

piecepackr 0.11.0
=================

New features
------------

* Can draw ``pyramid_face``, ``pyramid_left``, ``pyramid_left``, ``pyramid_right``, ``pyramid_top``, and ``pyramid_layout`` components  (progress on #37).
* Can draw ``matchstick_face`` and ``matchstick_back`` components (progress on #69)
* Can draw ``pawn_layout``, ``die_layoutLF``, ``die_layoutRF``, ``suitdie_layoutLF``, ``suitdie_layoutRF`',
   ``suitrankdie_layoutLF``, and ``suitrankdie_layoutRF`` components (#101)

piecepackr 0.10.0
=================

* Generalized star shape to ``concave#`` and ``#`` shape to ``convex#`` shape (#100).  Added "pyramid" triangle shape (progress on #37).
* New ``draw_components`` function allows one to draw several piecepack components specified in a data frame (#96).
* New ``load_configurations`` function allows one to load in several JSON configuration files and/or internal piecepackr configurations in a layered (cascading) manner (#81).
* Made several non-reverse-compatible API changes to configuration and piecepack image making functions and scripts.
* Increased the width of a pawn belt from 1.5" to 2" (so can fit around a 5/8" diameter pawn).

* Changed the "light"-french-suited symbols in the "dual" demo from "black" symbols to "white" symbols.  Thanks Ron-Hale Evans for the suggestion.
* Changed default color scheme 

piecepackr 0.9.0
================

* Added ``get_embedded_font`` function and executable (#80).
* Dropped ``pdfjoin`` from list of system dependencies (#77).
* Added ability to configure by individual suits and/or ranks (#44).

* Added Yellow Crowns demo (#53)

piecepackr 0.8.0
================

* Added ``make_piecepack_images`` executable (#48).  Renamed other executables and functions in API. 
* Shrunk the pawn down to ½" by ⅞" and made them directional by default (#50).
* Exported component drawing function ``draw_component`` (#3, #58).
* Allow styles by general component (#57) and open up primary symbol theta and r (#51).
* Document exported functions in Rd pages.
* Option to configure piecepack dice arrangement (#13)

* Expanded Rainbow Deck demo (#64).

* Fixed bugs in "orthodox" pawns (#62).

piecepackr 0.7.1
================

* Added ``--border_color`` configuration option and fix bug in PnP border colors (#42).
* Added ``--header_font``, ``--dm_r``, ``--dm_r.*``, and ``--suit_colors.*`` options.
* Added (or improved) ``--checker_colors``, ``--gridline_colors``, and ``--hexline_colors`` options.
* Renamed ``--background_color`` option to ``--background_colors`` which can now vary by suit (#52)
* Added ``--shape``, ``--shape_theta``, ``--shape.*``, and ``--shape_theta.*`` options.
* Add new and rename existing pdf metadata options in ``exec/collect_piecepacks``.
* Improved "preview" layout.
* Added additional suit die to accessories pages.
* Fix bug in the "suit-rank" die for 5-suited piecepacks (#41).

* Added Crown and Anchor suited piecepacks demo (#46).
* Added Rainbow Deck suited piecepack demo (#47).
* Added Reversi-friendly piecepack demo (#21).
* Added Chess-ranked piecepack demo (#24).
* Added Hex-friendly piecepack demo.

* Fix bugs in "orthodox" demo (#43). 
* Tweaked configuration in "dual" demo so first two decks are more compatible with other decks.
* Tweaked fonts in all demos.
* Added Simplified Hanzi decks to Chinese zodiac demo.
* Tweaked "chip" shape in orthodox and Chineze zodiac demos.
* Tweaked background colors in 3rd hex demo.
* Tweaked background colors in 3rd crown-and-anchor demo.

piecepackr 0.7.0
================

* Improved print-and-play layout (#35).
* Added "Chinese zodiac" and "sixpack" demos to pre-existing "default", "dual", and "orthodox" demos (#38).
