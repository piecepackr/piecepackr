piecepackr 0.12.0
=================

New features
------------

* New ``pp_cfg`` function (and S3 class) allows one to add a pre-computed cache of component opt to configuration lists (#112). 
* ``make_pnp`` function now supports adding piecepack pyramids (#37) and matchsticks (#69) to the print-and-play layout.
* Now exports several additional utility functions for those who would like to use their own custom draw function (#116).  See ``help("pp_utils")``, ``help("draw_fn_helpers")``, and ``help("draw_component")`` for more info.

Breaking changes
----------------

* Lots of configuration list style names were changed (#95, #121).
* All executable Rscripts have been removed (#113).
* ``make_collection`` and ``make_preview`` functions removed.
* ``load_configurations`` and ``read_configuration`` functions removed.
* ``make_pnp`` print-and-play layouts have been radically re-configured (#54).
* Non-standard ``chip`` component no longer supported by piecepackr (#114)
* 'hexlines' and 'checkers' no longer supported by base piecepackr configuration (#115).  
  NB. Can still add checkers and hexlines via a custom drawing function.
* ``make_pnp`` now gets deck title from the configuration element ``title`` (#118)
* ``ribbons`` are now ``mat`` (as in picture frame 'matting') (#117)
* Default pawn "belt" width is now ¾π" (#105).  
* Default pawn "saucer" width is now ¾" and they now have a 'mat' coloring on the edge (#105)

Bug fixes and minor improvements
--------------------------------

* Bug in ``get_embedded_font`` function fixed.
* ``make_pnp`` function now supports the A5 page size and is more A4 page size friendly (#54).
* Missing ``popViewport()`` added to ``draw_preview``.
* Can now specify ``fontface`` (#121) as well as ``width`` and ``height`` (#106) in configuration lists.
* Removed some package dependencies.
* ``draw_components`` now supports ``angle=NA`` and ``draw_component`` no longer draws different output
  for components like ``tile_back`` if you specify a suit or rank (#120).

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
