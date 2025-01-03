piecepackr 1.14.0 (development)
===============================

Breaking changes
----------------

* Some features which were deprecated by v1.13.1 (2023-03-23) have been removed:

  + `get_embedded_font()` no longer falls back on the use of the command-line utility `pdffonts`
    if the suggested R package `{pdftools}` is not installed.
    Please install the suggested R package `{pdftools}` to use `get_embedded_font()`.

* Newly vectorized `save_piece_obj()` now returns a data frame (instead of a named list)
  with (the same) "obj", "mtl", and "png" names.
  Each row contains the file paths for a Wavefront ".obj" file and its associated ".mtl" and ".png" files.
  Each piece may now be represented by one or several ".obj" files.
  Previously `save_piece_obj()` was not vectorized and returned a list with names "obj", "mtl", and "png"
  with the file paths for a single Wavefront ".obj" file and its associated ".mtl" and ".png" files and it used
  to throw an error instead of saving multiple ".obj" files for certain "composite" pieces
  (like the "joystick" pawn or "reversi" disc).

New features
------------

* `crosshairGrob()` / `grid.crosshair()` create/draw "crosshair" grobs.
  Intended for use in creating print-and-play layouts.

  + `segmentsCrosshairGrob()` simply wraps `grid::segmentsGrob()`.
  + `squaresCrosshairGrob()` wraps `grid::rectGrob()` and alternates
  black/white squares for visibility on both light and dark backgrounds.

* `install_ppverse()` installs non-CRAN packages
  from the [piecepackr universe](https://piecepackr.r-universe.dev/builds).
  `pkgs_ppverse()` returns a character vector of R packages
  in the piecepackr universe (#351).

Bug fixes and minor improvements
--------------------------------

* Although angles can continue to be numeric vectors of (usually) degrees they can now also
  be `affiner::angle()` vectors (#350):

  + The `angle` argument of:

    - `AA_to_R()`
    - `cropmarkGrob()` / `grid.cropmark()`
    - `crosshairGrob()` / `grid.crosshair()`
    - `geom_piece()`
    - `piece()`
    - `piece3d()`
    - `pieceGrob()` / `grid.piece()`
    - `piece_mesh()`
    - `R_x()`, `R_y()`, and `R_z()`
    - `save_piece_obj()`, `save_ellipsoid_obj()`, and `save_peg_doll_obj()`

  + The `op_angle` argument of:

    - `aabb_piece()`
    - `geom_piece()`
    - `pieceGrob()` / `grid.piece()`
    - `op_transform()`

  + The `t` argument of `to_x()`, `to_y()`, `to_degrees()`, and `to_radians()`.
  + The `theta` argument of `pp_shape()`.

* `cropmarkGrob()` / `grid.cropmark()` no longer ignores `name`, `gp`, and `vp` arguments (#340).
* `cropmarkGrob()` / `grid.cropmark()` is now vectorized in most of its arguments.
* `save_piece_obj()` improvements (#347):

  + Most arguments are now vectorized and `save_piece_obj()` now returns a data frame with "obj", "mtl", and "png" names.
  + Each row contains the file paths for a Wavefront ".obj" file and its associated ".mtl" and ".png" files.
  + Each piece may now be represented by one or several ".obj" files.  In particular instead of throwing an error
    we now generate (multiple) ".obj" files to represent "composite" pieces like the "joystick" pawns and "reversi" discs.

piecepackr 1.13.11
==================

* Bumps version of `{rayrender}` in the `DESCRIPTION` to v0.34.3 to avoid a bug
  introduced in v0.34.2 that affected `piece()` (#344).

piecepackr 1.13.10
==================

* Bumps version of `{rayvertex}` in the `DESCRIPTION` to v0.10.4 to avoid a bug
  introduced in v0.10.3 that affected `piece_mesh()` (#339).

piecepackr 1.13.8
=================

* Wraps `get_embedded_font()` example with `try()` and skips test on CRAN.

  - This prevents R CMD check ERRORS on CRAN machines with buggy versions of `cairo` installed (#333).
  - In particular `cairo` versions 1.17.4 to 1.17.8 may be buggy (#334).
    We'll now also raise a warning if a user tries to use `get_embedded_font()` with these `cairo` versions.
    These warnings have class `"piecepackr_buggy_cairo"` and can also be suppressed by setting
    `options(piecepackr.check.cairo = FALSE)`.

piecepackr 1.13.6
=================

* Tweak documentation examples to make CRAN happy (#332).

piecepackr 1.13.3
=================

Bug fixes and minor improvements
--------------------------------

* `is_color_invisible()` now accepts `col` values of length greater than one.
* `basicPieceGrob()` no longer shrinks its background and gridlines grobs if
  they would overlap with a transparent mat.

piecepackr 1.13.1
=================

Breaking changes
----------------

* `pp_shape("kite")` with default `theta` and `radius` arguments now returns a slightly different "kite" shape.
  Explicitly set `radius = 0.25` i.e. `pp_shape("kite", radius = 0.25)` to get the old "kite" shape
  (previously the "kite" shape couldn't be altered at all by the `theta` and `radius` arguments but with this release it can).
* The suggested package `{xmpdf}` is now required to embed bookmarks and XMP/documentation info metadata into
  the pdf files generated by `save_print_and_play()`.
  Please install the suggested R package `{xmpdf}` and depending on what you metadata you want to embed
  either the `ghostscript`, `pdftk-java`, and/or `exiftool` command-line tools.
  If you use `save_print_and_play()` to create a pdf file and don't have these suggested software installed
  you'll get message informing you that `{piecepackr}` was unable to embed certain metadata.
  These messages have class `"piecepackr_embed_metadata"` and can also be suppressed by setting
  `options(piecepackr.metadata.inform = FALSE)`.
* Some features which were deprecated by v1.10.3 (2022-03-22) have been removed:

  - The `use_pictureGrob` argument of `grid.piece()` / `pieceGrob()` which was deprecated in v1.8.1 (2021-08-11) has been removed.
    Use `type = "picture"` instead of `use_pictureGrob = TRUE`.
    The `...` has also been moved sooner to where `use_pictureGrob` used to be in the argument order so
    the arguments `width`, `height`, `depth`, `op_scale`, `op_angle`, `default.units`, `envir`,
    `name`, `gp`, and `vp` must now always be named arguments (instead of positional arguments).
  - The `pp_cfg()` R6 class public method `get_shadow_fn()` which was deprecated in v1.10.1 (2022-02-22)
    has been removed. The public method `get_op_grob()` returns the complete oblique projection grob.
  - `animate_pieces()`'s `annotate` argument must now always be set as a named argument
    (or rely on its default value).
    Setting it positionally was deprecated in v1.10.3 (2022-03-22).

Deprecated features
-------------------

* The use of the command-line utility `pdffonts` (usually found in ``poppler-utils``)
  in `get_embedded_font()` is now deprecated.
  Please install the suggested R package `{pdftools}`
  (we now preferably use its `pdf_fonts()` function instead).

New features
------------

* The following enhancements to the configurations returned by `game_systems()`:

  - New configuration ``dice_d4`` which provide four-sided dice in six color schemes (color controlled by their suit).
    Tetrahedrons with the rank as a numeral at the top point (#286).
  - New configuration ``dice_numeral`` which provide six-sided dice with numerals instead of pips in six color schemes (#311).
  - New configuration ``dice_d8`` which provide eight-sided dice in six color schemes (color controlled by their suit).
    Octahedrons with the rank as a numeral at the top face (#312).
  - New configuration ``dice_d10`` which provide ten-sided dice in six color schemes (color controlled by their suit).
    Pentagonal trapezohedrons with the rank as a numeral at the top face.
    The rank of ten is represented by a zero (#315).
  - New configuration ``dice_d10_percentile`` which provide ten-sided dice in six color schemes (color controlled by their suit).
    Pentagonal trapezohedrons with the rank as a numeral followed by a zero at the top face.
    The rank of ten is represented by a zero (#322).
  - New configuration ``dice_d12`` which provide twelve-sided dice in six color schemes (color controlled by their suit).
    Dodecahedrons with the rank as a numeral at the top face (#316).
  - New configuration ``dice_d20`` which provide twenty-sided dice in six color schemes (color controlled by their suit).
    Icosahedrons with the rank as a numeral at the top face (#317).
  - New configurations ``dominoes_chinese`` and ``dominoes_chinese_black`` which provide [Chinese dominoes](https://en.wikipedia.org/wiki/Chinese_dominoes) and [Asian-style dice](https://en.wikipedia.org/wiki/Dice#Arrangement).
  ``dominoes_chinese`` has a white background with black and red pips while ``dominoes_chinese_black`` has a black background with white and red pips (#308).

* `save_print_and_play()` now uses `{xmpdf}` to embed bookmarks, documentation info, and XMP metadata into
  the pdf files generated by `save_print_and_play()`.

  + The suggested package `{xmpdf}` is now required to embed metadata.
  + If available it will continue to use `ghostscript` to embed bookmarks and documentation info but
    can now also fallback to using `pdftk` to embed bookmarks and documentation info.
  + Can now use `exiftool` to embed XMP metadata including document license information.
  + `save_print_and_play()` will now inform users if unable to embed metadata into the generated pdf file.
    These messages have class `"piecepackr_embed_metadata"` and can also be suppressed by setting
    `options(piecepackr.metadata.inform = FALSE)`.

Bug fixes and minor improvements
--------------------------------

* Fixes a misrendering bug when using the new affine transformation feature that affected a small subset of game piece faces (#309).
* Border lines in oblique projected pyramids are now correctly made thicker/thinner when `scale` is different from `1` (#310).
* `pp_shape()`'s `gridlines()` and `shape()` methods takes new argument `mat_width` which can be used to shrink them
  (to not overlap/extend past a target "mat" effect) for most shapes that support a "mat" effect
  (but not "halma" or "roundrect" shapes which will quietly ignore the `mat_width` argument).
  This is now used in the default grob functions.
* The "kite" shape returned by `pp_shape("kite")` can now be adjusted by its `theta` and `radius` arguments.
* The following enhancements to the configurations returned by `game_systems()`:

  - The dice in ``chess1`` and ``chess2`` now have larger chess symbols (#318).
  - In ``dual_piecepacks_expansion``:

    + The border color for "Moons" dice and pawns is now "black" instead of "grey40".
    + The "Moons" suit color is now "black" instead of "grey30" for "board\_face" pieces.

  - In ``piecepack``

    + The "Moons" suit color is now "black" instead of "grey30" except for "bit" and "matchstick" pieces.
    + The "Moons" suit color is now "grey30" instead of "black" for "bit" pieces.

  - In ``piecepack_inverted``:

    + Pieces no longer have a "mat" effect except for the "tile\_back"
    + The edges of "coin" and "tile" pieces are now "white".
    + The "coin\_face" and "card\_back" backgrounds are now "grey30" instead of "black".
    + The "Moons" "board\_face" and "matchstick" suit colors are now "black" instead of "grey30".

  - In ``playing_cards_expansion``:

    + The "Clubs" suit color is now "grey30" for "matchstick" pieces.
    + The "Clubs" and "Spades" suit color is now "grey30" for "bit" pieces.

piecepackr 1.12.2
=================

Bug fixes and minor improvements
--------------------------------

* In `save_print_and_play()` we fix rotation of pawn layout
  when `bleed = FALSE` and `size` is not "4x6".
* The default function used to create game pieces for use with the `{rayrender}`
  graphics system (via the `piece()` function) has been updated to handle
  some breaking changes in the arguments for `rayrender::obj_model()` (#304).
* `piece_mesh()` has been updated to handle some breaking changes in `{rayvertex}` (#307).

piecepackr 1.12.0
=================

New features
------------

* `save_print_and_play()` now supports new argument `size_bleed` to add an additional "bleed zone" to
  the edges of the print-and-play output `size`:

  + A list with names "top", "right", "bottom", "left"
    containing numeric values indicating the inches "bleed" to add to
    the `size` of the print-and-play layout.
  + The default `NULL` means no such bleed added to "letter", "A4", and "A5"
    `size` layouts and a small bleed added to "4x6" `size` layouts
    (1/16" to top/bottom and 3/32" to left/right).
    Light prototyping at my local photo print service indicates adding such a bleed delivers
    better results for 4x6 photo prints but your results my vary.
  + Multiply millimeters by `0.0393700787` to convert to inches.
  + We currently don't support an asymmetric left/right bleed combined with `arrangement = "double-sided"`.
  + Not to be confused with the `bleed` argument which if `TRUE` switches to an alternative layout
    which adds a bleed zone and crop marks around the game pieces.

Bug fixes and minor improvements
--------------------------------

* Default resolution when generating bitmap images with `save_print_and_play()` is now 300 pixels per inch.
* Fixes the "hexpack" configuration returned by `game_systems()`.

piecepackr 1.11.1
=================

Breaking changes
----------------

* Some tweaks to the "bleed" zone feature introduced in v1.10.1:

  + In `grid.piece()` / `pieceGrob()` if `bleed = TRUE` and `width` or `height` is not `NA`
    then `width` / `height` is now interpreted as the size of the piece *before* "bleed" zone is added.
  + A non-unitary `scale` factor will now be applied to piece dimensions *before* "bleed" zone is added.
  + If one of the "named slots" of the `{grid}` "grob" returned by a custom
    `grob_with_bleed_fn` function is `bleed` then `pieceGrob()` / `grid.piece()`
    will adjust that grob's `bleed` slot to the requested "bleed" zone margins.

* There were some tweaks on how custom function style elements
  (i.e. `grob_fn`, `op_grob_fn`, `grob_with_bleed_fn`,
  `obj_fn`, `rayrender_fn`, `rayvertex_fn`, `rgl_fn`)
  in configurations lists are internally called by `{piecepackr}`.
  If you use custom *function* style elements it is possible you may be affected:

  + The arguments of custom functions are now called by name (via `do.call()`)
    instead of positionally.
    If your custom function uses the same argument names
    as any of `{piecepackr}`'s internal custom functions then there should be no problem.
    The order of arguments no longer matters but the names must match the
    names `{piecepackr}` expects.

  + If one of the "named slots" of the `{grid}` "grob" returned by a custom
    `grob_fn` function is `border` then `pieceGrob()` / `grid.piece()` will
    adjust that named slot to `FALSE`
    and manually draw new border lines whenever distorting the grob via the
    affine transformation feature (#277).

  + If one of the "named slots" of the `{grid}` "grob" returned by a custom
    `grob_fn` function is `flip` then `pieceGrob()` / `grid.piece()` will
    adjust that named slot to `TRUE` and draw it horizontally flipped
    (via affine transformation feature) when drawing the "flip" side.
    Currently most relevant when drawing on-their-side pyramids viewed from their bottom.

  + If one of the "named slots" of the `{grid}` "grob" returned by a custom
    `grob_fn`, `op_grob_fn`, or `grob_with_bleed_fn` function is `scale` then
    `pieceGrob()` / `grid.piece()` will adjust that named slot rather than
    adjusting that grob's `gp` slot's `cex` and `lex` values when `scale != 1`.
    If your "grob" doesn't have a named slot `scale` you shouldn't be affected.

* A couple `pp_cfg()` R6 class active bindings which were deprecated in v1.7.1 (2021-03-25)
  have been removed:

  + ``cache_shadow``.  Use the new ``cache_op_fn`` instead.
  + ``i_unsuit``.  Add one to the ``n_suits`` field instead.

New features
------------

* `cropmarkGrob()` / `grid.cropmark()` create/draw "crop mark" grobs (#262).
  Intended for use in creating print-and-play layouts.

* `save_print_and_play()` supports new argument `bleed` (#259).
  If `bleed = TRUE` we provide a new print-and-play layout with
  1/8" bleed zones and "crop marks" indicating where to cut the pieces.
  Currently this feature only supports `pieces = "piecepack"`, does
  not support `size = "4x6"`, and requires more paper than the
  more compact legacy `bleed = FALSE` layout.

* `geom_piece()`, `grid.piece()`, `pieceGrob()`,
  `pp_cfg()$get_grob()`, and `pp_cfg()$get_op_grob()` now support
  argument `type = "transformation"` which uses
  the new affine transformation feature introduced in R 4.2.

  This should be a faster alternative to the existing "picture" and "raster" types when
  wishing to "faithfully" draw game pieces outside their "normal" (viewport) sizes
  and/or dilate them in x/y directions
  but will only work in select graphic devices in R 4.2 (or later).
  You can tell if the active graphic device supports the affine transformation feature
  with `isTRUE(dev.capabilities()$transformations)`.

* `grid.piece()` / `pieceGrob()` now has support for
  drawing two-sided tokens stood up on one of their sides
  i.e. ("top", "left", "right", or "base") in an oblique projection (#272).

  `grid.piece()` / pieceGrob()` now also draws more sides of
  die and pyramid pieces when drawn in an oblique projection (#173, #257).

  Fully rendering the visible sides of the pieces requires the new "affine transformation" feature
  which is only supported in select graphic devices in R 4.2 (most notably the "cairo" family of devices).
  If this feature is not detected
  we will output an `inform()` `message()` of class `"piecepackr_affine_transformation"`
  and fall back to either a `grImport2::pictureGrob()` or `grid::polygonGrob()` alternative.
  These messages may be suppressed by setting `options(piecepackr.at.inform = FALSE)`.

* ``pp_cfg()``'s `die_arrangement` "style" now supports comma-separated strings
  of six integers optionally followed by a `^`, `<`, `v`, `>` which can be
  used to completely customize the arrangement of faces on six-sided dice (#175).
  The `die_arrangement` "style" continues to also support the values `"counter_up"`,
  `"opposites_sum_to_5"`, and `"counter_down"` (default) which provide aliases
  for three popular arrangements.

* The following enhancements to the configurations returned by `game_systems()`:

  - New configuration ``dice_fudge`` which provide [Fudge dice](https://en.wikipedia.org/wiki/Fudge_(role-playing_game_system)#Fudge_dice)
    in six color schemes (#287).

Bug fixes and minor improvements
--------------------------------

* The `op_angle`, `op_scale`, `alpha`, and `scale` arguments of
  `pieceGrob()` / `grid.piece()` are now vectorized.
* The `op_scale` threshold when `pieceGrob()` / `grid.piece()` switches from "oblique projection"
  mode to "orthographic projection" mode has been lowered from `0.01` to `0.0001`.
* If `isTRUE(capabilities("cairo"))` then `pp_cfg()$get_raster()`
  now always uses `png(type = "cairo")`.
* `grid::grobCoords()` now returns slightly better values for dice, pyramids, and convex
  two-sided tokens when projected in an oblique projection by `pieceGrob()` / `grid.piece()` (#285).
* `save_print_and_play()`'s `pieces` argument now defaults to `NULL`.  If the `size` / `bleed`
  arguments support the "matchsticks" and "pyramids" pieces it defaults to
  `c("piecepack", "pyramids", "matchsticks")` (as before) and if they
  do not suport those pieces it defaults to just "piecepack".
  We are now more selective of which piecepack credits to include based on
  which components are in the `pieces` argument.
* `save_print_and_play()` now shuffles tile back directions (#103).
  If a user makes tiles by double-sided printing or folding over the "gutter" and the tile
  backs are not perfectly symmetric then the tile backs will now leak less information
  about the "direction" of the tile faces.
* The default "bleed" function (as used by `pp_cfg()$get_grob_with_bleed()` and `pieceGrob(bleed=TRUE)`)
  now better extends "mat" and "gridlines" from the default "grob" function (#288).
* `animate_piece()` now correctly handles `n_pauses != 1`.
* If the `file` argument of `animate_piece()` ends in ".bmp", ".jpg", ".jpeg", ".png", or ".tiff"
  we will now save individual images of the animation frames.
  `file` must have a "C integer format" in the filename.
* Fixes bug generating textures in `save_piece_obj()` if `options("piecepackr.op_scale")` was
  set to a positive number (#293).

piecepackr 1.10.3
=================

Breaking changes
----------------

* Upcoming breaking changes in R 4.2 for `grid::grobCoords()` and `grid::grobPoints()`
  required changes being made in `{piecepackr}` as well.
  However `{piecepackr}` users probably won't need to update any of their code due to these changes (#270).:

  + `{piecepackr}` exports new S3 `grobCoords()` / `grobPoints()` methods that
    works for grob objects returned by `pieceGrob()` and `pp_cfg()$get_grob()` (for `type = "normal"`)
    as well as grob objects returned by `pmap_piece()`.
    They return lists of lists with (x,y) coordinates of the polygons that bound the game pieces
    in the format expected for `grid::grobCoords()` methods according to the R version number.
  + Instead of `grobPoints.piece()` we now export the S3 method `grobPoints.pp_grobCoords()` and
    the grob objects returned by `pieceGrob()` and `pp_cfg()$get_grob()` (for `type = "normal"`) now
    inherit the additional class "pp_grobCoords".

Bug fixes
---------

* Final page in "4x6" `size` layout produced by `save_print_and_play()`
  is no longer incorrectly rotated from landscape to portrait mode (#269).

Deprecated features
-------------------

* Using `animate_piece()`'s `annotate` argument unnamed is now deprecated.
  Instead either use its `annotate` argument named e.g. `annotate = "cartesian"`
  or instead don't provide this argument (and let it use its default `TRUE` value).
  In a future version this argument will be moved behind the `...`.

piecepackr 1.10.1
=================

Breaking changes
----------------

* `save_print_and_play()` no longer automatically assumes that the
  generated print-and-play file should automatically be released under the CC BY-SA 4.0 license.
  To continue to release under the CC BY-SA 4.0 license you should set the `cfg` argument's
  `spdx_id` style/binding to "CC-BY-SA-4.0" (i.e. `cfg$spdx_id = "CC-BY-SA-4.0")
  before calling `save_print_and_play()`.

* Several features that were deprecated in 2020 have been removed:

  + The following utility functions have been removed:

    - `checkersGrob(c, s, t, n)`, use `pp_shape(s, t)$checkers(n, gp=gpar(fill=c))` instead.
    - `concaveGrobFn(n, t, r)`, use `pp_shape(paste0("concave", n), t, r)$shape` instead.
    - `convexGrobFn(n, t)`, use `pp_shape(paste0("convex", n), t)$shape` instead.
    - `get_shape_grob_fn(s, t, r, b)`, use `pp_shape(s, t, r, b)$shape` instead.
    - `gridlinesGrob(c, s, t, l, n)`, use `pp_shape(s, t)$gridlines(n, gp=gpar(col=c, lex=l))` instead.
    - `halmaGrob(...)`, use `pp_shape("halma")$shape(...)` instead.
    - `hexlinesGrob(c, s, n)`, use `pp_shape(s)$hexlines(n, gp=gpar(col=c))` instead.
    - `kiteGrob(...)`, use `pp_shape("kite")$shape(...)` instead.
    - `matGrob(c, s, t, mw, n)`, use `pp_shape(s, t)$mat(mw, n, gp=gpar(fill=c))` instead.
    - `pyramidGrob(...)`, use `pp_shape("pyramid")$shape(...)` instead.

  + The following ``pp_cfg()`` R6 class public method has been removed:

    - ``get_pictureGrob()``, use ``get_grob(piece_type, suit, rank, type="picture")`` instead.

  + The following ``pp_cfg()`` "style" has been removed:

    - `shadow_fn`, use `op_grob_fn` to indicate a custom function for drawing in an oblique projection.

New features
------------

* `animate_piece()` can be used to create board game animations
  using either the suggested package `{animation}` or `{gifski}` (#251).
  Adding transition effects between moves (i.e. `n_transitions > 0`)
  requires the suggested package `{tweenr}`.

* `scale_x_piece()` and `scale_y_piece()` are wrappers around
  `ggplot2::scale_x_continuous()` and `ggplot2::scale_y_continuous()`
  with better defaults for board game diagrams.
  The functions returned by `label_letter()` labels with letters,
 `label_counting()` labels with counting numbers,
  and `breaks_counting()` generates breaks at the counting numbers
  to more easily generate (i.e. chess) algebraic notation coordinates
  as commonly used in board game diagrams (#252).

* The following enhancements to the configurations returned by `game_systems()`:

  + New `piecepack_inverted` configuration (#256).
    The standard piecepack with its color scheme inverted.
    Intended to aid in highlighting special pieces in diagrams.

  + New `reversi` configuration (#258).
    `"board_face"` provides lined boards with colored backgrounds.
    `"board_back"` provides checkered boards.
    `"bit_face"` / `"bit_back"` provides circular game tokens with differently colored sides:
    red paired with green, black paired with white, and blue paired with yellow.

* Improvements to `save_print_and_play()`:

  + Now supports a `dev` argument and `dev.args` argument
    that allows one to customize which graphic device (and its arguments) is
    used to generate the print-and-play files.

  + Now natively supports bitmap file formats "bmp", "jpeg", "png", and "tiff".
    but `output_filename` will need to have a "C integer format" in the string
    (e.g. `"piecepack%02d.png"`) so it can generate multiple "pages".

  + Now supports `size` "4x6" to create a piecepack print-and-play layout for
    4x6 (photo print) size documents (#162).

    - Currently only supports `pieces = "piecepack"`.
      Will throw an error if you try to create
      (piecepack) "pyramids", "matchsticks", or "subpack" print-and-play layouts.
    - For 4x6 photo prints will probably want to use a bitmap file format e.g. set
      `output_filename = "piecepack%02d.png"`.

  + The license of the generated print-and-play file is now customized by
    the `cfg` argument's `spdx_id` style/binding (#235).
    `save_print_and_play()` will then create a license section with
    that license's "full name", a URL, and in case of a Creative Commons license
    add a "button mark" badge.
    If left `NULL` we will now omit the license section but print a message
    saying we omitted it (unless `quietly = TRUE`).

  + New argument `quietly`.
    Unless `quietly = FALSE` will now `message()` about missing metadata.

  + The "title" page was tweaked and improved.  In particular it should
    now be able to handle any license with a [SPDX Identifier](https://spdx.org/licenses/).

* Improvements to support adding a "bleed" zone around pieces:

  * `pieceGrob()` and `grid.piece()` now support a `bleed` argument.

    + If `bleed = TRUE` we add a "bleed" zone around the piece.
    + `width` and `height` should be the width and height of the piece plus bleed zone.
      If `width` or `height` is `NA` we will add 1/8 inch bleeds (about 3.18 mm).
    + `bleed = TRUE` is incompatible with `op_scale > 0`.

  * `pp_cfg()` now supports a `grob_with_bleed_fn` style that can be
    used to set a custom function for drawing the piece with bleed.
    The default `grob_with_bleed_fn` function tries to guess a
    good solid bleed color and draws it around the base piece.

* `pp_cfg()` supports a new `spdx_id` "style" and an associated active binding to update it.
  This is meant to refer to the [SPDX Identifier](https://spdx.org/licenses/) for
  the graphical design license.
  `save_print_and_play()` can now use this field to customize the License info
  for the generated print-and-play layout (#235).

* `spdx_license_list` is a dataset for the [SPDX License List](https://spdx.org/licenses/).
  It is intended as an aid for developers writing print-and-play layout generators
  or board game ruleset generators.

Bug fixes and minor improvements
--------------------------------

* `render_piece()` should now handle its `dev.args` argument properly.
* `pmap_piece()` now simply returns `list()` if its `.l` argument has zero length
  or number of rows (instead of throwing an error).
* The "saucer" pieces in `game_systems()`'s piecepack configurations have been improved.
  The suit text on the backs are better sized and the suit text has been removed from the faces.
* The "joystick" or "peg-doll" pawns now have support for `piece_side = "pawn_base"` with `pieceGrob()`.

Deprecated features
-------------------

The following ``pp_cfg()`` R6 class public method has been deprecated:

* ``get_shadow_fn()``, use ``get_op_grob()`` to get complete oblique projection **grob**.

piecepackr 1.9.2
================

Bug fixes and minor improvements
--------------------------------

* We now throw a more helpful error message if one uses
  `save_piece_obj()` on a "joystick" pawn (returned
  by some `game_systems()` configurations if `pawn = "joystick"`).
  In general we cannot save Wavefront OBJ files for "composite" pieces
  such as "joystick" pawns although "peg-doll" pawns are an exception.

piecepackr 1.9.1
================

New features
------------

* New `piece_mesh()` function that creates `{rayvertex}` objects (#247).

  + Requires suggested package `{rayvertex}`.
  + Can further customize how `piece_mesh()` creates `{rayvertex}` object
    by setting `rayvertex_fn` in `pp_cfg()` configuration list.
  + `piece_mesh` can be used as the `.f` argument in `render_piece()` (#255)

* The following enhancements to the configurations returned by `game_systems()`:

  + New "alquerque" configuration that produces board/pieces for Alquerque.
  + Domino configurations now support up to double-18 sets
   (previously supported only up to double-12).
  + New "morris" configuration that can produce
    Three/Six/Seven/Nine/Twelve Men's Morris boards in a variety of colors (#250).

piecepackr 1.8.1
================

New features
------------

* New function `render_piece()` renders an image of game pieces to a file or graphics device.
  It is a wrapper around `pmap_piece()` that can auto-size files and graphic devices,
  apply axes offsets, annotate coordinates, and set up `rayrender` scenes (#245).
* New function `geom_piece()` provides `{ggplot2}` bindings (#209).

  + Helper function `aes_piece()` helps create an appropriate matching `ggplot2::aes()` "mapping".
  + `geom_piece()` requires a fixed scale coordinate system with an aspect ratio of 1
    as provided by `ggplot2::coord_fixed()`.
  + `geom_piece()` also requires that `cfg` is a character vector (and not a `pp_cfg()` object).
    In particular if using `op_transform()` one should set its argument `cfg_class = "character"`
    if intending for use with `geom_piece()`.
  + Requires Suggested package `{ggplot2}`.

* New function `aabb_piece()` which calculates axis-aligned bounding box (AABB) for set of game pieces
  with and without an "oblique projection".
* New function `has_font()` which returns `TRUE` or `FALSE` if it detects a given font is
  provided by the OS.  Needs either 1) Suggested package `{systemfonts}` or
  2) R compiled with "cairo" support plus system tool `pdffonts` installed.
* The R6 object returned by `pp_shape()` now has a new `pattern()` method that fills
  the shape with a specified pattern.  Requires the suggested package `{gridpattern}` (#228).
* `pieceGrob()` and `grid.piece()` now support a new `type` argument
  which modifies which type of grid grob is used.
  Can be either `"normal"` (default), `"picture"`, or `"raster"`.
  `"picture"` exports to (temporary) svg and re-imports as a `grImport2::pictureGrob`.
  `"raster"` exports to (temporary) png and re-imports as a `grid::rasterGrob`.
  The latter two can be useful if drawing pieces really big or small and don't want
  to mess with re-configuring fontsizes and linewidths.
* Now defaults for the following `piecepackr` function arguments may now be set globally via `base::options()`:

  + `piecepackr.cfg` Sets a new default for the `cfg` argument.
  + `piecepackr.default.units` Sets a new default for the `default.units` argument.
  + `piecepackr.envir` Sets a new default for the `envir` argument.
  + `piecepackr.op_angle` Sets a new default for the `op_angle` argument.
  + `piecepackr.op_scale` Sets a new default for the `op_scale` argument.
  + `piecepackr.trans` Sets a new default for the `trans` argument.

Bug fixes and minor improvements
--------------------------------

* If the suggested package `{readobj}` is installed then the default
  ``piece3d()`` method will import Wavefront OBJ files with ``readobj::read.obj()``
  instead of ``rgl::readOBJ()``.  In particular ``readobj::read.obj()`` can
  successfully triangulate the "meeple" shape and the "roundrect" shape (#220).
* Fixes bug when setting `alpha` transparency parameter in `grid.piece()` / `pieceGrob()`.
* `game_systems()` now provides a `message()` if `style = "dejavu"` and
  `has_font("Dejavu Sans")` is `FALSE`.
* Fixes bug in `picturePieceGrobFn()` when its `filename_fn` argument does not support
  a `cfg` argument.

Deprecated features
-------------------

The ``use_pictureGrob`` argument of ``pieceGrob()`` and ``grid.piece()`` is now deprecated
in favor of the new ``type`` argument.  Use ``type = "picture"`` instead of ``use_pictureGrob = TRUE``.

piecepackr 1.7.2
================

* No user-facing changes (we bumped the ``rgl`` package requirement to ``0.106.8``
  in order to avoid a bug introduced by ``0.106.6``).

piecepackr 1.7.1
================

Breaking changes
----------------

The following changes in ``pp_cfg()`` configuration lists seem unlikely to affect any users (but theoretically *could* do so):

* By default `"card_back"` is now assumed to lack a "rank".
  To re-allow the appearance of `"card_back"` pieces to differ by rank set the
  new ``lacks_rank`` style of ``pp_cfg()`` to a character vector that does not include `"card_back"`.
* A `"piece_side"` that is assumed to lack rank now
  is imputed a rank equal to ``n_ranks + 1L`` when drawn (instead of ``0``)
  and for a `"piece_side"` that is assumed to have a rank we impose an assumption
  that the rank will not exceed ``n_ranks + 2L`` (replacing with ``n_ranks + 2L`` if more than that).
  This now mirrors the pre-existing behaviour with suits.
  For example if ``n_ranks`` is set to 6 then the style ``background_color.r7 = "blue"``
  will set the background color for pieces that *lack* a rank to "blue".

New features
------------

* Function ``game_systems()`` ``pawn`` argument now supports a "joystick" value (#183).
  This allows one to draw a "joystick" style ``"pawn_top"`` in `{grid}`, `{rgl}`, and `{rayrender}`
  with the "piecepack" game system configurations.

  + Currently Wavefront OBJ export is not supported for "joystick" pawns.
  + Other pawn "sides" (e.g. ``"pawn_face"``) are not supported in ``grid.piece()`` / ``pieceGrob()``
    but are supported in ``piece3d()`` (rgl) and ``piece()`` (rayrender).
  + You may find it useful to set ``as_top = "pawn_face"`` in ``op_transform()`` (when using ``pmap_piece()``).

* The configuration lists supported by ``pp_cfg()`` now support two new "styles" (#237):

  + ``lacks_rank``: a character vector of `"piece_side"`'s that we should assume
    do not vary by rank.  By default these are the piecepack components that do
    not vary by rank plus `"card_back"`.
  + ``lacks_suit``: a character vector of `"piece_side"`'s that we should assume
    do not vary by suit. By default these are the piecepack components that do
    not vary by suit plus `"card_back"`.

* ``pp_cfg()`` objects now make their cache available as a public field named ``cache`` (#242):

  + The cache can be replaced with a different cache that obeys the ``cachem`` package cache API.
    In particular one could choose replace it with ``cachem::cache_mem()`` to cap
    the amount of memory that can used by the cache.
    Note our cache uses keys that include hyphens and underscores which older versions
    of the ``cachem`` package did not support.
  + Each ``pp_cfg()`` has a random prefix so (with a high probability) multiple
    objects could theoretically share the same cache.
  + The default cache is a memory cache that does not prune.
    It has a ``reset()`` method which clears it.

* The function that can be passed into the `filename_fn` argument of `picturePieceGrobFn()`
  can now accept an optional fifth argument named ``cfg`` that will (if present)
  be passed a ``pp_cfg()`` object.

Bug fixes and minor improvements
--------------------------------

* Can now draw "top", "left", "right", and "base" "edge" sides of two-sided tokens in grid
  (``grid.piece()`` / ``pieceGrob()``) if not using an 3D oblique projection (``op_scale`` over 0) (#135).

  + These "edge" sides are not properly supported in ``grid.piece()`` / ``pieceGrob()`` (grid)
    when using a 3D oblique projection (``op_scale`` over 0) but are supported in
    ``piece3d()`` (rgl) and ``piece()`` (rayrender).

* The "peg-doll" pawn (available via ``game_systems()`` ``pawn`` argument) now has basic
  support for a piecepack ``"pawn_top"`` in grid (#184).
  Previously only had support in rgl and rayrender.
* Piecepack pyramid dimensions have been fixed to better reflect their actual physical size (#241).
  Their layout in the print-and-play layouts produced by ``save_print_and_play()`` has also been updated.
* The appearance of piecepack pyramids returned by ``game_systems()``
  now more closely resemble the original piecepack pyramids.
* ``AA_to_R()`` is now more robust to minor numerical errors in ``axis_x`` and ``axis_y`` values.
* Fixed bug in ``op_transform()`` when calculating "z"-coordinate when a piece
  overlaps with multiple pieces and the "latest" one isn't actually the "highest" one.
* ``pp_cfg()`` objects now store more information in their internal lists
  (which can be exported via ``as.list()``) including any inferred `n_ranks` and `n_suits`.
* ``pp_cfg()``'s `print()` method now sorts fields by name and collapses vectors into a single string.
* Several of ``pp_cfg()``'s R6 class's fields are now active bindings.
* Fixed bug when setting custom ``pp_cfg()`` configuration list `"rgl_fn"` values.

Deprecated features
-------------------

The following ``pp_cfg()`` R6 class active bindings are now deprecated:

* ``cache_shadow``.  Use the new ``cache_op_fn`` instead.
* ``i_unsuit``.  Add one to the ``n_suits`` field instead.

piecepackr 1.6.5
================

No user-facing changes (we tweak our examples/tests so R CMD check passes without issue on CRAN's new 'M1mac' build).

piecepackr 1.6.3
================

New features
------------

* Function ``game_systems()`` now returns the following new configurations:

  + "go" to make "board" and "stone" "bit" pieces in six colors (#169, #176).
  + "chess1", "chess2" to make "board", "die", and "bit" chess pieces in six colors (#167).
    "chess1"'s "board" has 1" cells while "chess2"'s "board" has 2" cells.
    Currently the chess piece's are print-and-play style discs instead of 3D Staunton style pieces.

* New alternative Wavefront OBJ file generating functions intended to be used
  with the ``obj_fn`` attribute in ``pp_cfg()`` "configuration lists":

  + ``save_ellipsoid_obj()`` creates an ellipsoid piece colored with that piece's ``background_color``.
    Currently used for the go "stones" in ``game_systems()`` "go" configuration.
    Could also be used for colored "marble / ball" pieces (#176).
  + ``save_peg_doll_obj()`` creates a "peg doll" piece colored with that piece's ``edge_color``
    and with a "pawn belt" around its "waist" controlled by that suit and rank's ``belt_face`` component (#184).

* Function ``game_systems()`` now accepts new argument ``round``.
  If ``TRUE`` then the (retangular) tiles and cards will have a "roundrect" shape
  instead of a "rect" shape (the default) (#226).
  NB. ``piece3d`` (rgl bindings) currently cannot import the "roundrect" shape.

* Function ``game_systems()`` now accepts new argument ``pawn``.
  If ``"token"`` (the default) piecepack pawns are a two-sided token with a "halma" shape.
  If ``"peg-doll"`` then piecepack pawns will be a "peg doll" with a "pawn belt" (#184).
  NB. ``grid.piece`` (grid bindings) currently cannot draw the "peg-doll" style pawn.

Bug fixes and minor improvements
--------------------------------

* The center of the "face" and "joker" cards in the
  "playing_cards", "playing_cards_colored", and "playing_cards_tarot" configurations
  returned by ``game_systems()`` now have simple graphics
  using this packages built-in meeple shape (#193).
* The "playing_cards_tarot" configuration returned by ``game_systems()``
  now supports a 15th "Joker" "rank" for the first four "suits".
  This means all the [Playing cards in Unicode](https://en.wikipedia.org/wiki/Playing_cards_in_Unicode)
  now has a corresponding card in the "playing_cards_tarot" configuration
  (suggested Joker mapping is "White" = suit 1, "Black" = suit 2, and "Red" = suit 4)
* Fixes a bug in the oblique projection of piece "backs" of thick convex-curved 2-sided tokens.
* ``pmap_piece()`` now simply returns ``list()`` if its output has zero observations.
* ``file2grob()`` fixes a bug when importing images using the ``magick`` package
  (currently any image that is not PNG, JPEG, or SVG).
* Fixes bug in ``save_piece_obj()``: if multiple graphics devices are open
  it should no longer accidentally switch the "active" device (#239).
* The piecepack "matchsticks" page generated by ``save_print_and_play()``
  now does six copies of the "matchstick_face" component for six ranks and four suits.
  It used to do four copies of the "matchstick_face" and "matchstick_back"
  component for six ranks and five suits but a "standard" set of matchsticks is six copies.
  If there is sufficient space it will also add some additional (piecepack) dice (#194).

piecepackr 1.5.1
================

Breaking changes
----------------

There are no user-facing breaking changes in `piecepackr`'s API
but the internal `grid` structure of the grobs drawn/returned by `grid.piece()`, `pieceGrob()`, and `pmap_piece()`
have been changed to make it easier to query or edit the grid graphics post initial drawing
with functions like `grid::grid.get()`, `grid::grid.edit()`, `grid::grid.reorder()` etc. (#205).

* The "grob" returned by `grid.piece()` and `pieceGrob()` now has a "piece" class and "named slots" which match the arguments of `grid.piece()`.
* The new "piece" class grob only generates its content to draw at drawing time, one must use `grid::grid.force()` to view/edit its internal grob "children".
* The internal grob layout for included grob functions are better named and structured to more easily generate a matching ``grid::gPath``.
* `grobPoints()` methods have been written for the "grob" returned by `grid.piece() / pieceGrob()` and the "grob" returned by `pmap_piece()`.
* If no unique `name` in `.l` then `pmap_piece()` will now generate/replace a unique `name`
  by concatenating `piece.` with a unique `id` if it exists or if it does not then by row number
  in order to better label the "children" of the returned `gTree` grob object (#223).

Deprecated functions
--------------------

The following utility functions have been deprecated in favor of methods provided by the new `pp_shape()` object:

* `checkersGrob(c, s, t, n)`, use `pp_shape(s, t)$checkers(n, gp=gpar(fill=c))` instead.
* `concaveGrobFn(n, t, r)`, use `pp_shape(paste0("concave", n), t, r)$shape` instead.
* `convexGrobFn(n, t)`, use `pp_shape(paste0("convex", n), t)$shape` instead.
* `get_shape_grob_fn(s, t, r, b)`, use `pp_shape(s, t, r, b)$shape` instead.
* `gridlinesGrob(c, s, t, l, n)`, use `pp_shape(s, t)$gridlines(n, gp=gpar(col=c, lex=l))` instead.
* `halmaGrob(...)`, use `pp_shape("halma")$shape(...)` instead.
* `hexlinesGrob(c, s, n)`, use `pp_shape(s)$hexlines(n, gp=gpar(col=c))` instead.
* `kiteGrob(...)`, use `pp_shape("kite")$shape(...)` instead.
* `matGrob(c, s, t, mw, n)`, use `pp_shape(s, t)$mat(mw, n, gp=gpar(fill=c))` instead.
* `pyramidGrob(...)`, use `pp_shape("pyramid")$shape(...)` instead.

New features
------------

* New function `pp_shape()` returns an R6 object that provides the following methods to create
  various grobs for all the "shape"'s supported by `pp_cfg()`:

  + `shape()` returns a grob of the shape.
  + `mat()` returns a grob of a matting "mat" within the shape.
     Its argument `mat_width` controls how "wide" the matting should be.
  + `gridlines()` returns a grob of "gridlines" within the shape.
  + `checkers()` returns a grob of "checkers" within the shape.
  + `hexlines()` returns a grob of "hexlines" within the shape.
  + `polyclip()` returns a grob that is an "intersection", "minus", "union", or "xor" of another grob.
    Note unlike `gridGeometry::polyclipGrob` it can directly work with a `pieceGrob` "clip grob" argument.

* Default functions used by `save_piece_obj()`, `piece3d()`, `piece()` now support a
  "top", "base", "left", and "right" side for the two-sided token components:
  "bit", "board", "card", "matchstick", "pawn", "saucer", and "tile" (#135).
* Function ``game_systems()`` now returns a "meeple" configuration with "standard" meeple "bit" pieces (#232).

Bug fixes and minor improvements
--------------------------------

* `pp_cfg()` now supports a "roundrect" (rounded rectangle) `shape` (#214, #229).
  Curvature of the corners are controlled by the `shape_r` style.
* `pp_cfg()` now supports a "meeple" shape (#104).
  Meeple coordinates were extracted from [Meeple icon](https://game-icons.net/1x1/delapouite/meeple.html)
  by [Delapouite](https://delapouite.com/) / [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/).
* `pp_cfg()` now supports setting multiple styles by using a greater-than-length-one vectors
  (as well as continuing to support previously-supported comma-separated strings).
* The default OBJ generating function (used by the default rgl and rayrender functions)
  now produces six-faced cubes for the dice component (#186).
* `R_to_AA()` now always returns an axis-angle parameterization with positive `axis_z` value (#219).
* Fixes bug in `R_to_AA()` for some 3D rotations with an "exactly" 180 degree angle (in the axis-angle representation).
* `AA_to_R()` now accepts an optional `axis_z` argument and automatically normalizes the axis vector.
  In particular `R_to_AA(AA_to_R(...))` can now be used to normalize alternative axis-angle parameterizations
  with non-unit axis vectors and/or negative `axis_z` values.
* Improved drawing of oblique projection edges for the "oval" shape (#212) and the "halma" shape (#213).
* In oblique projection obscured edges are now drawn in case of transparent backgrounds (#221).
* `is_color_invisible()` will now correctly classify as "invisible" colors with an alpha channel value set to 0.
* The named list returned by `pp_cfg()$get_piece_opt()` now also contains a `back` value indicating whether this is a back of a piece
  (and hence for some shapes may need to be flipped across a vertical line) and a `bleed_color` value indicating a good
  color for a "bleed" effect (e.g. for print-and-play layouts) (#211).
* Fixes bug in "circle" shape "mat" when drawn in a non-square grid viewport.
* Fixes bug in `op_transform()` for pieces whose "shape" is a non-symmetric (across vertical axis) polygon.
* `grid.piece()` and `pmap_piece()` now returns the grob invisibly if `draw=TRUE` as documented
  (while continuing to draw the grob to the active graphics device).
* By default `piece3d()` will now try to infer a reasonable `textype` value by manually checking the png texture's alpha channel.

piecepackr 1.4.1
================

New features
------------

* ``save_piece_obj()``, ``piece3d()`` and ``piece()`` now have support for 3D rotations of pieces (#188)
  using an axis-angle representation parameterized by arguments ``angle``, ``axis_x``, and ``axis_y``.
* New functions ``AA_to_R()`` and ``R_to_AA()`` convert back and forth between the axis-angle representation
  used by ``piecepackr`` and 3D rotation matrices (post-multiplied).
* New functions ``R_x()``, ``R_y()``, and ``R_z()`` create simple 3D rotation matrices (post-multiplied) for
  rotations around the x, y, and z axes.
* Newly exported geometry helper functions ``to_degrees()`` and ``to_radians()`` convert back and forth between
  degrees and radians.

Bug fixes and minor improvements
--------------------------------

* ``pp_cfg()`` now supports new ``obj_fn``, ``rayrender_fn``, and ``rgl_fn`` styles
  allowing further piece customization within the 3D graphics functions
  ``save_piece_obj()``, ``piece()``, and ``piece3d()`` respectively (#200).
* ``piece3d()`` now supports user manually setting the piece's ``rgl`` "material" ``textype`` argument.
  When textures with alpha transparency are not needed then setting ``textype``
  to ``"rgb"`` avoids a rglWebGL rendering bug (#187).
* Utility function ``get_shape_grob_fn()`` now has a ``back`` argument to indicate
  we are drawing the back of the shape and should reflect it across a vertical axis (#218).
* Some bug fixes and enhancements in the Wavefront OBJ export for certain shapes (#207, #208, #215).

piecepackr 1.3.1
================

New features
------------

* Graphic functions now support more game pieces:
  - ``board_face``, ``board_back`` for representing boards (#153).
  - ``card_face``, ``card_back`` for representing cards (#124).
  - ``bit_face``, ``bit_back`` for representing miscellaneous game pieces (#155).
* Function ``game_systems()`` now returns more game systems:
  - ``dice``: Normal six-sided pipped dice in six color schemes (#166).
  - ``checkers1``, ``checkers2``: Checkered boards and checkers.  ``checkers1`` has 1" cells and ``checkers2`` has 2" cells (#168).
  - ``dominoes``, ``dominoes_black``, ``dominoes_blue``, ``dominoes_green``, ``dominoes_red``, ``dominoes_white``, ``dominoes_yellow``: Traditional Double-12 pipped dominoes in various color schemes (#159).
  - ``playing_cards``, ``playing_cards_colored``, ``playing_cards_tarot`` (#170):
    + ``playing cards`` is a traditional (French-suited) deck of (Poker) playing cards.
    + ``playing_cards_colored`` has five suits: red hearts, black spades, green clubs, blue diamonds, and yellow stars.
    + ``playing_cards_tarot`` is a (French Bourgeois) tarot deck with four suits (hearts, spades, clubs, diamonds) with 14 ranks (including knights) plus 22 trump cards (1-21 plus an "excuse").
* New function ``save_piece_obj()`` for writing Waveform OBJ files (and associated MTL files and png textures) (#189, #190).
  Note support for dice and pawns are currently rather crude.
* New function ``piece3d()`` for making graphics with the ``rgl`` package (#76).
* New function ``piece()`` for making ``rayrender`` objects.

Bug fixes and minor improvements
--------------------------------

* Configuration lists now support an "oval" shape (#182).
* Improved drawing of oblique projection edges for "concave" polygons (stars)
  and non-regular "convex" polygons (#174).
* Improved drawing of pyramids in the 3D oblique projection (#172).
* Fixes bug in ``op_transform()`` when sorting tile faces rotated in various directions (#163).
* ``op_transform()`` now accepts new argument ``pt_thickness``for improved handling of stacked pyramid (tops).
* The ``edge_color`` for dice, pawns, and pyramids now defaults to the background color of ``piece_side``.
* The ``edge_color`` for matchsticks now defaults to the background color of the ``matchstick_back``.
* Font sizes and locations of piecepack matchsticks in ``game_systems()`` made more "standard".
* ``pmap_piece()`` now supports an ``.f`` function to specify the function to apply to the rows of the data frame.
  By default it uses the (backwards-compatible) ``pieceGrob``.
* ``pp_cfg()$get_depth()`` now has better depth calculation for pyramid faces (representing laid down pyramids).
* ``pp_cfg()`` now supports a new ``op_grob_fn`` style indicating which function to draw pieces with
  when drawing with a 3D oblique projection in ``grid``.  The older ``shadow_fn`` style alternative is now deprecated.
* ``grid.piece()`` (and ``pieceGrob()``) now support a ``scale`` and ``alpha`` argument (#201).

Deprecated features
-------------------

The following ``pp_cfg()`` R6 class public method is now deprecated:

* ``get_pictureGrob()``.  Use ``get_grob(piece_type, suit, rank, type="picture")`` instead.

The following ``pp_cfg`` "style" is now deprecated:

* ``shadow_fn``.  Use the new ``op_grob_fn`` instead.

Breaking changes
----------------

* The function ``game_systems()`` no longer returns an ``icehouse_pieces`` configuration.
  Configurations for Looney Pyramids can be found at https://github.com/piecepackr/piecenikr

piecepackr 1.2.1
================

New features
------------

* New function ``picturePieceGrobFn()`` which returns a "grob" function that imports graphics
  from files found in its ``directory`` argument (#152).
* New function ``game_systems()`` which returns a list of configuration objects
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

* Helper functions ``to_subpack()`` and ``to_hexpack()`` which given a piecepack configuration
  attempts to generate an appropriate matching (piecepack stackpack) subpack and hexpack (#161).
* New function ``file2grob()`` that imports a given image file as a grob.

Bug fixes and minor improvements
--------------------------------

* Guesses made by ``op_transform()`` for whether two pieces
  overlap are now more accurate (#150).
* Fixes bug in ``op_transform()`` when computing bounding box for shapes
  when width and height are not equal.
* Fixes bug in ``op_transform()`` for inferring width/height of matchsticks
  and shape/depth of pyramids.
* Fixes bug in printing ``pp_cfg`` objects when a custom grob function had been set.
* Fixes bug in setting individual suit colors with configurations list styles like ``suit_color.s2="white"``
* ``pp_cfg()`` objects' ``get_suit_color()`` function is now vectorized
  (and by default now returns the suit colors for each suit).

piecepackr 1.1.1
================

* New helper function ``op_transform()`` and new ``pmap_piece()`` argument ``trans``
  to facilitate making graphics with an oblique projection (#138).
* Fixes bug when drawing non-regular-dimensioned "convex" shapes in an
  oblique 3D projection (#149)

piecepackr 1.0.2
================

* Initial "stable" release.

New features
------------

* ``save_print_and_play()`` function now supports adding piecepack pyramids (#37), matchsticks (#69), and subpacks (aka "travel" piecepacks) (#129) to the print-and-play layout.
* Now supports drawing most pieces with a simple 3D oblique projection (#76).
* New ``pp_cfg()`` function (and R6 class) which on-the-fly builds a cache to speed up component drawing (#112, #122).
  Can also signal to rule diagram functions whether the piecepack lacks certain components (#130).
* Now exports several additional utility functions for those who would like to use their own custom draw function (#116).  See ``help("pp_utils")``, ``help("grob_fn_helpers")``, and ``help("grid.piece")`` for more info.

Breaking changes
----------------

* ``component_opt`` color names are now ``_color`` instead of ``_col`` to better match "configuration lists".
* ``draw_component`` has been renamed ``grid.piece()`` (and many of its arguments vectorized) and
  ``draw_components`` has been renamed ``pmap_piece()``(#123).
* ``make_images`` has been renamed ``save_piece_images()`` and ``make_pnp`` has been renamed ``save_print_and_play()``.
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

* Bug in ``get_embedded_font()`` function fixed.
* ``save_print_and_play()`` function now supports the A5 page size and is more A4 page size friendly (#54).
* Can now specify ``fontface`` (#121) as well as ``width``, ``height``, and ``depth`` (#106) in configuration lists.
* Removed some package dependencies.
* ``pmap_piece()`` now supports ``angle=NA`` and ``grid.piece()`` no longer draws different output
  for components like ``tile_back`` if you specify a suit or rank (#120).
* "configuration lists" now support ``credit``, ``copyright``, and ``description`` fields
  which add extra info to the new "credit", "copyright", and "description" sections of the print-and-play layout.
* "configuration lists" now support ``coin_arrangement`` (#136).
*  Also if ``ghostscript`` installed will automatically embed metadata in the print-and-play pdf (#93).
* "configuration lists" now support ``border_lex`` and ``gridline_lex`` to allow customizing the width of the border and grid lines.
* ``save_piece_images()``'s ``format`` argument is now vectorized.
* ``save_print_and_play()`` can now save a print-and-play "svg" and "ps" file (besides "pdf").
* ``grid.piece()`` no longer throws a warning when ``use_pictureGrob==TRUE`` and ``angle!=0`` (#148).

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
* New ``draw_components()`` function allows one to draw several piecepack components specified in a data frame (#96).
* New ``load_configurations()`` function allows one to load in several JSON configuration files and/or internal piecepackr configurations in a layered (cascading) manner (#81).
* Made several non-reverse-compatible API changes to configuration and piecepack image making functions and scripts.
* Increased the width of a pawn belt from 1.5" to 2" (so can fit around a 5/8" diameter pawn).

* Changed the "light"-french-suited symbols in the "dual" demo from "black" symbols to "white" symbols.  Thanks Ron-Hale Evans for the suggestion.
* Changed default color scheme

piecepackr 0.9.0
================

* Added ``get_embedded_font()`` function and executable (#80).
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
