test_that("crosshairs work as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    cfg <- pp_cfg(list(border_color = NA))
    df <- data.frame(piece_side = "tile_face", suit = 2, rank = 2,
                     x = 2, y = 2, angle = 0,
                     stringsAsFactors = FALSE)

    expect_doppelganger("crosshairs_squares", {
      pmap_piece(df, grid.piece, cfg = cfg, default.units = "in")
      pmap_piece(df, grid.crosshair, cfg = cfg, default.units = "in")
    })

    expect_doppelganger("crosshairs_segments", {
      pmap_piece(df, grid.piece, cfg = cfg, default.units = "in")
      pmap_piece(df, grid.crosshair, cfg = cfg, default.units = "in",
                 ch_grob = segmentsCrosshairGrob())
    })
})
