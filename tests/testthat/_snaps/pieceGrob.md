# deprecated 'preview_layout' component warning

    Code
      invisible(pieceGrob("preview_layout", cfg = cfg_default, default.units = "npc"))
    Condition
      Warning:
      The "preview_layout" component is deprecated. Use `ppdf::piecepack_preview() |> pmap_piece(cfg = cfg, default.units = "in")` instead.

