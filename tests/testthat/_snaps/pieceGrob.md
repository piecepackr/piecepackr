# `save_print_and_play()` errors for unsupported `bleed = "grouped"` combinations

    Code
      save_print_and_play(cfg_default, f, size = "A5", bleed = "grouped", quietly = TRUE)
    Condition
      Error in `save_print_and_play()`:
      ! `size = "A5"` not supported for `bleed = "grouped"`

---

    Code
      save_print_and_play(cfg_default, f, arrangement = "double-sided", bleed = "grouped",
        quietly = TRUE)
    Condition
      Error in `save_print_and_play()`:
      ! `arrangement = "double-sided"` not supported for `bleed = "grouped"`
      i The dice, belt, and pawns are printed once across the full width, so duplex printing would land them back to back.
      i To mount the two halves on opposite sides of the target material, cut the sheet apart along the "gutter" line instead.

---

    Code
      save_print_and_play(cfg_default, f, size = "4x6", bleed = "grouped", quietly = TRUE)
    Condition
      Warning:
      `size = "4x6"` is deprecated.
      Error in `save_print_and_play()`:
      ! `size = "4x6"` not supported for `bleed = "grouped"`

# `save_print_and_play()` doesn't leave its device open on error

    Code
      save_print_and_play(cfg_default, f, size = "A5", bleed = "grouped")
    Condition
      Error in `save_print_and_play()`:
      ! `size = "A5"` not supported for `bleed = "grouped"`

---

    Code
      save_print_and_play(cfg_default, f, quietly = TRUE)
    Condition
      Error in `print_and_play_paper()`:
      ! mid-draw

# `save_print_and_play()` errors if dice are wider than 3/4" with bleed

    Code
      save_print_and_play(cfg, f, bleed = "grouped", quietly = TRUE)
    Condition
      Error in `save_print_and_play()`:
      ! `cfg$get_width("die_face")` must be at most 3/4" for `bleed = "grouped"`, not 0.8"

---

    Code
      save_print_and_play(cfg, f, bleed = TRUE, quietly = TRUE)
    Condition
      Error in `save_print_and_play()`:
      ! `cfg$get_width("die_face")` must be at most 3/4" for `bleed = "individual"`, not 0.8"

# `save_print_and_play()` omits credits and instructions without {marquee}

    Code
      save_print_and_play(cfg, f, bleed = "grouped")
    Message
      x Omitting credits since {marquee} is not installed
      i These messages can be disabled via `options(piecepackr.marquee.inform = FALSE)`.
      x Omitting instructions since {marquee} is not installed
      i These messages can be disabled via `options(piecepackr.marquee.inform = FALSE)`.

# `save_print_and_play()` omits credits and instructions without glyph support

    Code
      save_print_and_play(cfg, f, bleed = TRUE)
    Message
      x Omitting credits since the graphics device doesn't support {marquee} glyph rendering
      i These messages can be disabled via `options(piecepackr.marquee.inform = FALSE)`.
      x Omitting instructions since the graphics device doesn't support {marquee} glyph rendering
      i These messages can be disabled via `options(piecepackr.marquee.inform = FALSE)`.

# `save_print_and_play()` omits credits and instructions on R < 4.3

    Code
      save_print_and_play(cfg, f, bleed = TRUE)
    Message
      x Omitting credits since R >= 4.3 is required for {marquee} glyph rendering
      i These messages can be disabled via `options(piecepackr.marquee.inform = FALSE)`.
      x Omitting instructions since R >= 4.3 is required for {marquee} glyph rendering
      i These messages can be disabled via `options(piecepackr.marquee.inform = FALSE)`.

# `save_print_and_play(size = "4x6")` is deprecated

    Code
      save_print_and_play(pp_cfg(), f, size = "4x6", pieces = "piecepack", quietly = TRUE)
    Condition
      Warning:
      `size = "4x6"` is deprecated.

# `save_piece_images()` works as expected

    Code
      save_piece_images(cfg_default, directory)
    Condition
      Error in `save_piece_images()`:
      ! `directory` must be an existing directory

# deprecated 'preview_layout' component warning

    Code
      invisible(pieceGrob("preview_layout", cfg = cfg_default, default.units = "npc"))
    Condition
      Warning:
      The "preview_layout" component is deprecated. Use `ppdf::piecepack_preview() |> pmap_piece(cfg = cfg, default.units = "in")` instead.

