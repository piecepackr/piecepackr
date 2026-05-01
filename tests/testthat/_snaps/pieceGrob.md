# `save_print_and_play()` errors for unsupported `bleed = "grouped"` combinations

    Code
      save_print_and_play(cfg_default, f, size = "A5", bleed = "grouped", quietly = TRUE)
    Condition
      Error in `print_and_play_paper_grouped()`:
      ! `size = "A5"` not supported for `bleed = "grouped"`

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

