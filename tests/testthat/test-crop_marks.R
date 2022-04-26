test_that("crop_marks work as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    cfg <- pp_cfg(list(mat_color = "pink", mat_width=0.05, border_color=NA))
    df <- data.frame(piece_side = "tile_face", suit = 2, rank = 2,
                     x = 2, y = 2, angle = 0,
                     stringsAsFactors = FALSE)
    expect_doppelganger("crop_mark_outside", {
        pmap_piece(df, grid.cropmark, cfg = cfg, default.units = "in")
        pmap_piece(df, grid.piece, cfg = cfg, default.units = "in", bleed=TRUE)
    })

    df <- data.frame(piece_side = "coin_back", suit = 2, rank = 2,
                     x = 2, y = 2, angle = 0,
                     stringsAsFactors = FALSE)
    expect_doppelganger("crop_mark_outside_x2", {
        pmap_piece(df, grid.cropmark, cfg = cfg, default.units = "in",
                   bleed=TRUE, scale=2, cm_select="1357")
        pmap_piece(df, grid.piece, cfg = cfg, default.units = "in",
                   bleed=TRUE, scale=2)
    })
})
