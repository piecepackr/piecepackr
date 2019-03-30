cfg_default <- pp_cfg()

context("make_pnp works as expected")
test_that("make_pnp works as expected", {

    pdf_deck_dir = tempfile()
    on.exit(unlink(pdf_deck_dir, recursive=TRUE))
    pdf_deck_filename <- file.path(pdf_deck_dir, "piecepack_deck.pdf")
    on.exit(unlink(pdf_deck_filename))

    make_pnp(cfg_default, pdf_deck_filename, "letter")

    expect_true(file.exists(pdf_deck_filename))
    expect_equal(get_n_pages(pdf_deck_filename), 6)
    expect_equal(get_n_pages_gs(pdf_deck_filename), 6)
    if (Sys.which("pdfinfo") != "")
        expect_equal(get_n_pages_pdfinfo(pdf_deck_filename), 6)

})

context("make_images works as expected")
test_that("make_images works as expected", {
    directory <- tempfile()
    on.exit(unlink(directory))
    dir.create(directory)
    make_images(cfg_default, directory)
    expect_equal(length(list.files(directory)), 108)
})

context("no regressions in figures")
test_that("no regressions in figures", {
    dc <- function(..., cfg=cfg_default) {
        draw_component(..., cfg=cfg) 
    }
    dce <- function(...) {
        tmpfile <- tempfile(fileext=".svg")
        on.exit(unlink(tmpfile))
        svg(tmpfile)
        on.exit(dev.off())
        draw_component(...)
    }
    # tile back
    vdiffr::expect_doppelganger("tile_back", function() dc("tile_back"))
    vdiffr::expect_doppelganger("tile_back-svg", function() dc("tile_back", svg=TRUE))
    vdiffr::expect_doppelganger("tile_back-hex", 
                                function() dc("tile_back", cfg=list(shape.tile_back="convex6")))
    vdiffr::expect_doppelganger("tile_back.checkers", function() dc("tile_back", cfg=list(checker_colors.tile_back = "blue")))
    vdiffr::expect_doppelganger("tile_back.checkers.hex", function() dc("tile_back", cfg=list(shape.tile_back = "convex6", checker_colors.tile_back = "blue")))
    vdiffr::expect_doppelganger("tile_back.hexlines", function() dc("tile_back", cfg=list(hexline_colors.tile_back = "green")))
    # tile face
    vdiffr::expect_doppelganger("tile_face.s1.r1", function() dc("tile_face", i_s=1, i_r=1))
    vdiffr::expect_doppelganger("tile_face.s3.r2", function() dc("tile_face", i_s=3, i_r=2))
    vdiffr::expect_doppelganger("tile_face.s3.r2-suit_as_ace", 
                                function() dc("tile_face", cfg=list(use_suit_as_ace=TRUE), i_s=3, i_r=2))
    vdiffr::expect_doppelganger("tile_face.s2.r3", function() dc("tile_face", i_s=2, i_r=3))
    # coin back
    vdiffr::expect_doppelganger("coin_back.s4", function() dc("coin_back", i_s=4))
    vdiffr::expect_doppelganger("coin_back.s4-inverted",
                                function() dc("coin_back", cfg=list(invert_colors=TRUE), i_s=4))
    # coin face
    vdiffr::expect_doppelganger("coin_face.r1", function() dc("coin_face", i_r=1))
    vdiffr::expect_doppelganger("coin_face.r2", function() dc("coin_face", i_r=2))
    vdiffr::expect_doppelganger("coin_face.r4", function() dc("coin_face", i_r=4))
    vdiffr::expect_doppelganger("coin_face.r4pyramid", 
                                function() dc("coin_face", i_r=4, cfg=list(shape.coin="pyramid")))
    vdiffr::expect_doppelganger("coin_face.r4convex8", 
                                function() dc("coin_face", i_r=4, cfg=list(shape.coin="convex8")))
    vdiffr::expect_doppelganger("coin_face.r4concave8", 
                                function() dc("coin_face", i_r=4, cfg=list(shape.coin="concave8", shape_r.coin=0.4)))
    # die faces
    vdiffr::expect_doppelganger("die_face.s4.r1", function() dc("die_face", i_s=4, i_r=1))
    vdiffr::expect_doppelganger("die_face.s3.r2", function() dc("die_face", i_s=3, i_r=2))
    vdiffr::expect_doppelganger("die_face.s2.r5", function() dc("die_face", i_s=2, i_r=5))
    vdiffr::expect_doppelganger("suitdie_face.s1", function() dc("suitdie_face", i_s=1))
    vdiffr::expect_doppelganger("suitdie_face.s5", function() dc("suitdie_face", i_s=5))
    vdiffr::expect_doppelganger("suitdie_face.s6", function() dc("suitdie_face", i_s=6))
    # pawns
    vdiffr::expect_doppelganger("pawn_face.s3", function() dc("pawn_face", i_s=3))
    vdiffr::expect_doppelganger("pawn_back.s2", function() dc("pawn_back", i_s=2))
    vdiffr::expect_doppelganger("belt_face.s1", function() dc("belt_face", i_s=1))
    vdiffr::expect_doppelganger("saucer_face.s4", function() dc("saucer_face", i_s=4))
    vdiffr::expect_doppelganger("saucer_back", function() dc("saucer_back"))
    # chips
    vdiffr::expect_doppelganger("chip_face.s1.r1", function() dc("chip_face", i_s=1, i_r=1))
    vdiffr::expect_doppelganger("chip_face.s2.r2", function() dc("chip_face", i_s=2, i_r=2))
    vdiffr::expect_doppelganger("chip_face.s3.r3", function() dc("chip_face", i_s=3, i_r=3))
    vdiffr::expect_doppelganger("chip_back.s4", function() dc("chip_back", i_s=4))
    vdiffr::expect_doppelganger("chip_back.s3-hex", 
                                function() dc("chip_back", cfg=list(shape.chip="convex6"), i_s=3))
    vdiffr::expect_doppelganger("chip_back.s2-kite", 
                                function() dc("chip_back", cfg=list(shape.chip="kite"), i_s=2))
    vdiffr::expect_doppelganger("chip_back.s1-star", 
                                function() dc("chip_back", cfg=list(shape.chip="concave5"), i_s=1))
    # pyramids
    vdiffr::expect_doppelganger("pyramid_face.s1.r6", function() dc("pyramid_face", i_s=1, i_r=6))
    vdiffr::expect_doppelganger("pyramid_left.s2.r5", function() dc("pyramid_left", i_s=2, i_r=5))
    vdiffr::expect_doppelganger("pyramid_right.s3.r2", function() dc("pyramid_right", i_s=3, i_r=2))
    vdiffr::expect_doppelganger("pyramid_back.s4.r1", function() dc("pyramid_back", i_s=4, i_r=1))
    vdiffr::expect_doppelganger("pyramid_face.s2.r1", function() dc("pyramid_face", i_s=2, i_r=1))
    vdiffr::expect_doppelganger("pyramid_top.s4.r3", function() dc("pyramid_top", i_s=4, i_r=3))
    vdiffr::expect_doppelganger("pyramid_layout.s3.r4", function() dc("pyramid_layout", i_s=3, i_r=4))

    # matchsticks
    vdiffr::expect_doppelganger("matchstick_face.s1.r1", function() dc("matchstick_face", i_s=1, i_r=1))
    vdiffr::expect_doppelganger("matchstick_face.s2.r2", function() dc("matchstick_face", i_s=2, i_r=2))
    vdiffr::expect_doppelganger("matchstick_face.s3.r3", function() dc("matchstick_face", i_s=3, i_r=3))
    vdiffr::expect_doppelganger("matchstick_face.s4.r4", function() dc("matchstick_face", i_s=4, i_r=4))
    vdiffr::expect_doppelganger("matchstick_face.s1.r5", function() dc("matchstick_face", i_s=1, i_r=5))
    vdiffr::expect_doppelganger("matchstick_face.s2.r6", function() dc("matchstick_face", i_s=2, i_r=6))
    vdiffr::expect_doppelganger("matchstick_back.s3.r6", function() dc("matchstick_back", cfg=list(invert_colors=TRUE), i_s=3, i_r=6))

    #### preview appears wrong #99
    # vdiffr::expect_doppelganger("preview", draw_preview)
    # dice
    cfg <- list(suit_symbols="-,0,+,")
    vdiffr::expect_doppelganger("suitdie_layoutLF", function() dc("suitdie_layoutLF", cfg=cfg))
    vdiffr::expect_doppelganger("suitdie_layoutRF", function() dc("suitdie_layoutRF"))
    vdiffr::expect_doppelganger("suitrankdie_layoutLF", function() dc("suitrankdie_layoutLF"))

    cfg <- list(pp_die_arrangement="opposites_sum_to_5")
    vdiffr::expect_doppelganger("die_layoutRF-opposites_sum_to_5", function() dc("die_layoutRF", i_s=3, cfg=cfg))

    cfg <- list(suit_symbols="A,B,C,D,E,F", suit_colors="red,black,green,blue,orange,grey")
    vdiffr::expect_doppelganger("suitdie_layoutRF-5suits", function() dc("suitdie_layoutRF", cfg=cfg))
    vdiffr::expect_doppelganger("suitrankdie_layoutRF-5suits", function() dc("suitrankdie_layoutRF", cfg=cfg))
    cfg <- list(suit_symbols="A,B,C,D,E,F,G", suit_colors="red,black,green,blue,orange,purple,grey")
    vdiffr::expect_doppelganger("suitdie_layoutRF-6suits", function() dc("suitdie_layoutRF", cfg=cfg))
    vdiffr::expect_doppelganger("suitrankdie_layoutRF-6suits", function() dc("suitrankdie_layoutRF", cfg=cfg))

    # draw_components
    df <- tibble::tribble(~component_side, ~x, ~y, ~i_s, ~i_r,
                          "tile_face", 0.25, 0.25, 1, 1,
                          "tile_face", 0.75, 0.25, 2, 2,
                          "tile_face", 0.25, 0.75, 3, 5,
                          "tile_face", 0.75, 0.75, 4, 6)
    vdiffr::expect_doppelganger("draw_components", function() {
                                    pushViewport(viewport(width=inch(4), height=inch(4)))
                                    draw_components(df)
                          })

    # errors
    expect_error(dce("coin_face", list(gridline_colors = "grey"), i_r = 3), "Don't know how to add grid lines to shape circle")
    expect_error(dce("coin_face", list(checker_colors = "grey"), i_r = 3), "Don't know how to add checkers to shape circle")
    expect_error(dce("coin_face", list(hexline_colors = "grey"), i_r = 3), "Don't know how to add hexlines to shape circle")
    expect_error(dce("coin_face", list(ribbon_colors = "grey"), i_r = 3), "Don't know how to add ribbons to shape circle")


})
