context("no regressions in figures")
test_that("no regressions in figures", {
    # skip_on_cran()
    dc <- function(...) { draw_component(...) }
    vdiffr::expect_doppelganger("tile_back", function() dc("tile_back"))
    vdiffr::expect_doppelganger("tile_back-svg", function() dc("tile_back", svg=TRUE))
    vdiffr::expect_doppelganger("tile_back-hex", 
                                function() dc("tile_back", cfg=c2o("--shape.tile_back=6")))
    # expect_error(dc("tile_back", cfg=c2o("--shape.tile_back=halma")), "Don't know how to add")
    vdiffr::expect_doppelganger("tile_face.s1.r1", function() dc("tile_face", i_s=1, i_r=1))
    vdiffr::expect_doppelganger("tile_face.s3.r2", function() dc("tile_face", i_s=3, i_r=2))
    vdiffr::expect_doppelganger("tile_face.s3.r2-suit_as_ace", 
                                function() dc("tile_face", cfg=c2o("--use_suit_as_ace"), i_s=3, i_r=2))
    vdiffr::expect_doppelganger("tile_face.s2.r3", function() dc("tile_face", i_s=2, i_r=3))
    vdiffr::expect_doppelganger("coin_back.s4", function() dc("coin_back", i_s=4))
    vdiffr::expect_doppelganger("coin_back.s4-inverted",
                                function() dc("coin_back", cfg=c2o("--invert_colors"), i_s=4))
    vdiffr::expect_doppelganger("coin_face.r1", function() dc("coin_face", i_r=1))
    vdiffr::expect_doppelganger("coin_face.r2", function() dc("coin_face", i_r=2))
    vdiffr::expect_doppelganger("coin_face.r4", function() dc("coin_face", i_r=4))
    vdiffr::expect_doppelganger("ppdie_face.s4.r1", function() dc("ppdie_face", i_s=4, i_r=1))
    vdiffr::expect_doppelganger("ppdie_face.s3.r2", function() dc("ppdie_face", i_s=3, i_r=2))
    vdiffr::expect_doppelganger("ppdie_face.s2.r5", function() dc("ppdie_face", i_s=2, i_r=5))
    vdiffr::expect_doppelganger("suitdie_face.s1", function() dc("suitdie_face", i_s=1))
    vdiffr::expect_doppelganger("suitdie_face.s5", function() dc("suitdie_face", i_s=5))
    vdiffr::expect_doppelganger("suitdie_face.s6", function() dc("suitdie_face", i_s=6))
    vdiffr::expect_doppelganger("pawn_face.s3", function() dc("pawn_face", i_s=3))
    vdiffr::expect_doppelganger("pawn_back.s2", function() dc("pawn_back", i_s=2))
    vdiffr::expect_doppelganger("belt_face.s1", function() dc("belt_face", i_s=1))
    vdiffr::expect_doppelganger("saucer_face.s4", function() dc("saucer_face", i_s=4))
    vdiffr::expect_doppelganger("saucer_back", function() dc("saucer_back"))
    vdiffr::expect_doppelganger("chip_face.s1.r1", function() dc("chip_face", i_s=1, i_r=1))
    vdiffr::expect_doppelganger("chip_face.s2.r2", function() dc("chip_face", i_s=2, i_r=2))
    vdiffr::expect_doppelganger("chip_face.s3.r3", function() dc("chip_face", i_s=3, i_r=3))
    vdiffr::expect_doppelganger("chip_back.s4", function() dc("chip_back", i_s=4))
    vdiffr::expect_doppelganger("chip_back.s3-hex", 
                                function() dc("chip_back", cfg=c2o("--shape.chip=6"), i_s=3))
    vdiffr::expect_doppelganger("chip_back.s2-kite", 
                                function() dc("chip_back", cfg=c2o("--shape.chip=kite"), i_s=2))
    vdiffr::expect_doppelganger("chip_back.s1-star", 
                                function() dc("chip_back", cfg=c2o("--shape.chip=star"), i_s=1))

    vdiffr::expect_doppelganger("preview", draw_preview)
    vdiffr::expect_doppelganger("draw_suit_die-5suits", function() draw_suit_die(cfg=c2o(c("--suit_symbols=A,B,C,D,E,F", "--suit_colors=red,black,green,blue,orange,grey"))))
    vdiffr::expect_doppelganger("draw_suit_die-6suits", function() draw_suit_die(cfg=c2o(c("--suit_symbols=A,B,C,D,E,F,G", "--suit_colors=red,black,green,blue,orange,purple,grey"))))
    expect_error(draw_suit_die(cfg=c2o(c("--suit_symbols=A,B,C,D", "--suit_colors=red,black,green,blue"))), 
                         "Don't know how to draw suit die for 3 suits")
    vdiffr::expect_doppelganger("draw_suitrank_die-5suits", function() draw_suitrank_die(cfg=c2o(c("--suit_symbols=A,B,C,D,E,F", "--suit_colors=red,black,green,blue,orange,grey"))))
    vdiffr::expect_doppelganger("draw_suitrank_die-6suits", function() draw_suitrank_die(cfg=c2o(c("--suit_symbols=A,B,C,D,E,F,G", "--suit_colors=red,black,green,blue,orange,purple,grey"))))
    expect_error(draw_suitrank_die(cfg=c2o(c("--suit_symbols=A,B,C,D", "--suit_colors=red,black,green,blue"))), 
                         "Don't know how to draw suit/rank die for 3 suits")
})
