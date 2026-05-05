# `save_print_and_play(size = "4x6")` is deprecated

    Code
      save_print_and_play(pp_cfg(), f, size = "4x6", pieces = "piecepack", quietly = TRUE)
    Condition
      Warning:
      `size = "4x6"` is deprecated.

# deprecated 'preview_layout' component warning

    Code
      invisible(pieceGrob("preview_layout", cfg = cfg_default, default.units = "npc"))
    Condition
      Warning:
      The "preview_layout" component is deprecated. Use `ppdf::piecepack_preview() |> pmap_piece(cfg = cfg, default.units = "in")` instead.

