library("vdiffr")
cfg_default <- pp_cfg(list(title="default cfg"))
context("test pp_cfg")
test_that("pp_cfg works as expected", {
    expect_true("pp_cfg" %in% class(cfg_default))
    expect_equal(class(as.list(cfg_default)), "list")
    expect_output(print(cfg_default), "default cfg")
})

context("make_pnp works as expected")
test_that("make_pnp works as expected", {

    pdf_deck_dir = tempfile()
    on.exit(unlink(pdf_deck_dir, recursive=TRUE))
    pdf_deck_filename <- file.path(pdf_deck_dir, "piecepack_deck.pdf")
    on.exit(unlink(pdf_deck_filename))
    pdf_deck_filename_a4 <- file.path(pdf_deck_dir, "piecepack_deck_a4.pdf")
    on.exit(unlink(pdf_deck_filename_a4))
    pdf_deck_filename_a5 <- file.path(pdf_deck_dir, "piecepack_deck_a5.pdf")
    on.exit(unlink(pdf_deck_filename_a5))

    make_pnp(cfg_default, pdf_deck_filename, "letter")
    make_pnp(cfg_default, pdf_deck_filename_a4, "A4")
    make_pnp(cfg_default, pdf_deck_filename_a5, "A5")
    expect_error(make_pnp(cfg_default, tempfile(), "A6"), "Don't know how to handle paper A6")

    expect_true(file.exists(pdf_deck_filename))
    expect_equal(get_n_pages(pdf_deck_filename), 7)
    expect_equal(get_n_pages_gs(pdf_deck_filename), 7)
    if (Sys.which("pdfinfo") != "")
        expect_equal(get_n_pages_pdfinfo(pdf_deck_filename), 7)
    expect_equal(get_n_pages(pdf_deck_filename_a4), 7)
    expect_equal(get_n_pages(pdf_deck_filename_a5), 14)

    pdf_deck_filename_5s <- file.path(pdf_deck_dir, "piecepack_deck_5s.pdf")
    on.exit(unlink(pdf_deck_filename_5s))
    cfg_5s <- list(suit_text="♥,★,♣,♦,♠,꩜", suit_color="darkred,gold,darkgreen,darkblue,black,grey")
    make_pnp(cfg_5s, pdf_deck_filename_5s, "A5")

})

context("make_images works as expected")
test_that("make_images works as expected", {
    directory <- tempfile()
    on.exit(unlink(directory))
    dir.create(directory)
    make_images(cfg_default, directory, angles=90)
    expect_equal(length(list.files(directory)), 248)
})

context("draw_fn_helpers works as expected")
test_that("draw_fn_helpers works as expected", {
    expect_doppelganger("add_checkers", function() {
                    add_checkers("purple", "rect")
                    add_hexlines("yellow", "rect") 
    })
    expect_doppelganger("add_checkers.transparent", function() {
                    add_checkers("transparent", "rect")
                    add_hexlines("transparent", "rect") 
    })
    expect_doppelganger("add_checkers.convex8", function() {
                    add_checkers("purple", "convex8")
    })
    expect_error(add_checkers("blue", "circle"))
    expect_error(add_hexlines("blue", "circle"))
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
    expect_doppelganger("tile_back", function() dc("tile_back"))
    expect_doppelganger("tile_back-svg", function() dc("tile_back", svg=TRUE))
    expect_doppelganger("tile_back-hex", 
                                function() dc("tile_back", cfg=list(shape.tile_back="convex6")))
    # tile face
    expect_doppelganger("tile_face.s1.r1", function() dc("tile_face", i_s=1, i_r=1))
    expect_doppelganger("tile_face.s3.r2", function() dc("tile_face", i_s=3, i_r=2))
    expect_doppelganger("tile_face.s3.r2-suit_as_ace", 
                                function() dc("tile_face", cfg=list(use_suit_as_ace=TRUE), i_s=3, i_r=2))
    expect_doppelganger("tile_face.s2.r3", function() dc("tile_face", i_s=2, i_r=3))
    df_tile <- tibble::tribble( ~x, ~y, ~i_s, ~i_r,
                               1, 1, NA, NA,
                               3, 1, 3, NA,
                               1, 3, NA, 3,
                               3, 3, 3, 3)
    df_tile$component_side <- "tile_back"
    df_tile$angle <- NA
    expect_doppelganger("tiles_faces", function() draw_components(df_tile, 
                                                                  width=2, height=2,
                                                                  units="inches", cfg_name="cfg_default"))
    # 106
    expect_doppelganger("different_sizes", function() {
                            pushViewport(viewport(width=inch(6), height=inch(6)))
                            dc("tile_face", i_s=2, i_r=3, x=inch(1), y=inch(5))
                            dc("tile_face", i_s=2, i_r=3, x=inch(4), y=inch(4.5), cfg=list(width.tile=3))
                            dc("tile_face", i_s=2, i_r=3, x=inch(4), y=inch(1.5), cfg=list(width.tile=4, height.tile=3))
                            dc("coin_back", i_s=3, x=inch(1), y=inch(1), cfg=list(mat_width.coin=0.1))
                            dc("coin_back", i_s=3, x=inch(2), y=inch(1), cfg=list(width.coin=1, mat_width.coin=0.1))})

    #### coins appear wrong #99
    # coin back
    expect_doppelganger("coin_back.s4", function() dc("coin_back", i_s=4))
    expect_doppelganger("coin_back.s4-inverted",
                                function() dc("coin_back", cfg=list(invert_colors=TRUE), i_s=4))
    # coin face
    expect_doppelganger("coin_face.r1", function() dc("coin_face", i_r=1))
    expect_doppelganger("coin_face.r2", function() dc("coin_face", i_r=2))
    expect_doppelganger("coin_face.r4", function() dc("coin_face", i_r=4))
    expect_doppelganger("coin_face.r4italic", function() dc("coin_face", cfg=list(ps_fontface.coin_face="italic"), i_r=4))
    expect_doppelganger("coin_face.r4pyramid", 
                                function() dc("coin_face", i_r=4, cfg=list(shape.coin="pyramid")))
    expect_doppelganger("coin_face.r4convex8", 
                                function() dc("coin_face", i_r=4, cfg=list(shape.coin="convex8")))
    expect_doppelganger("coin_face.r4concave8", 
                                function() dc("coin_face", i_r=4, cfg=list(shape.coin="concave8", shape_r.coin=0.4)))
    # die faces
    expect_doppelganger("die_face.s4.r1", function() dc("die_face", i_s=4, i_r=1))
    expect_doppelganger("die_face.s3.r2", function() dc("die_face", i_s=3, i_r=2))
    expect_doppelganger("die_face.s2.r5", function() dc("die_face", i_s=2, i_r=5))
    expect_doppelganger("die_face.s2.r5.kite", 
            function() dc("die_face", i_s=2, i_r=5, cfg=list(shape.die_face="kite", dm_t=90, suit_scale="1,1,1,1")))
    expect_doppelganger("die_face.s2.r5.convex5mat", 
            function() dc("die_face", i_s=2, i_r=5, cfg=list(shape.die_face="convex5", mat_width.die_face=0.1, mat_color="pink")))
    expect_doppelganger("suitdie_face.s1", function() dc("suitdie_face", i_s=1))
    expect_doppelganger("suitdie_face.s5", function() dc("suitdie_face", i_s=5))
    expect_doppelganger("suitdie_face.s6", function() dc("suitdie_face", i_s=6))
    # pawns
    expect_doppelganger("pawn_face.s3", function() dc("pawn_face", i_s=3))
    expect_doppelganger("pawn_back.s2", function() dc("pawn_back", i_s=2))
    expect_doppelganger("belt_face.s1", function() dc("belt_face", i_s=1))
    expect_doppelganger("saucer_face.s4", function() dc("saucer_face", i_s=4))
    expect_doppelganger("saucer_back", function() dc("saucer_back"))
    # pyramids
    expect_doppelganger("pyramid_face.s1.r6", function() dc("pyramid_face", i_s=1, i_r=6))
    expect_doppelganger("pyramid_left.s2.r5", function() dc("pyramid_left", i_s=2, i_r=5))
    expect_doppelganger("pyramid_right.s3.r2", function() dc("pyramid_right", i_s=3, i_r=2))
    expect_doppelganger("pyramid_back.s4.r1", function() dc("pyramid_back", i_s=4, i_r=1))
    expect_doppelganger("pyramid_face.s2.r1", function() dc("pyramid_face", i_s=2, i_r=1))
    # expect_doppelganger("pyramid_top.s4.r3", function() dc("pyramid_top", i_s=4, i_r=3)) # svglite bug?
    expect_doppelganger("pyramid_layout.s3.r4", function() dc("pyramid_layout", i_s=3, i_r=4))

    # matchsticks
    expect_doppelganger("matchstick_face.s1.r1", function() dc("matchstick_face", i_s=1, i_r=1))
    expect_doppelganger("matchstick_face.s2.r2", function() dc("matchstick_face", i_s=2, i_r=2))
    expect_doppelganger("matchstick_face.s3.r3", function() dc("matchstick_face", i_s=3, i_r=3))
    expect_doppelganger("matchstick_face.s4.r4", function() dc("matchstick_face", i_s=4, i_r=4))
    expect_doppelganger("matchstick_face.s1.r5", function() dc("matchstick_face", i_s=1, i_r=5))
    expect_doppelganger("matchstick_face.s2.r6", function() dc("matchstick_face", i_s=2, i_r=6))
    cfg <- list(invert_colors=TRUE, draw_fn="basic_draw_fn")
    expect_doppelganger("matchstick_back.s3.r6", function() dc("matchstick_back", cfg=cfg, i_s=3, i_r=6))

    #### preview appears wrong #99
    expect_doppelganger("preview", function() draw_preview(cfg_default))
    expect_doppelganger("preview.5s", function() draw_preview(list(suit_text="A,B,C,D,E,", die_arrangement="counter_up")))
    expect_doppelganger("preview.6s", function() draw_preview(list(suit_text="I,II,III,IV,V,VI,")))

    # dice
    cfg <- list(suit_text="-,0,+,")
    expect_doppelganger("suitdie_layoutLF", function() dc("suitdie_layoutLF", cfg=cfg))
    expect_doppelganger("suitdie_layoutRF", function() dc("suitdie_layoutRF"))
    expect_doppelganger("suitrankdie_layoutLF", function() dc("suitrankdie_layoutLF"))

    cfg <- list(die_arrangement="opposites_sum_to_5")
    expect_doppelganger("die_layoutRF-opposites_sum_to_5", function() dc("die_layoutRF", i_s=3, cfg=cfg))

    cfg <- list(suit_text="A,B,C,D,E,F", suit_color="red,black,green,blue,orange,grey")
    expect_doppelganger("suitdie_layoutRF-5suits", function() dc("suitdie_layoutRF", cfg=cfg))
    expect_doppelganger("suitrankdie_layoutRF-5suits", function() dc("suitrankdie_layoutRF", cfg=cfg))
    cfg <- list(suit_text="A,B,C,D,E,F,G", suit_color="red,black,green,blue,orange,purple,grey")
    expect_doppelganger("suitdie_layoutRF-6suits", function() dc("suitdie_layoutRF", cfg=cfg))
    expect_doppelganger("suitrankdie_layoutRF-6suits", function() dc("suitrankdie_layoutRF", cfg=cfg))

    # draw_components
    df <- tibble::tribble(~component_side, ~x, ~y, ~i_s, ~i_r,
                          "tile_face", 0.25, 0.25, 1, 1,
                          "tile_face", 0.75, 0.25, 2, 2,
                          "tile_face", 0.25, 0.75, 3, 5,
                          "tile_face", 0.75, 0.75, 4, 6)
    expect_doppelganger("draw_components", function() {
                                    pushViewport(viewport(width=inch(4), height=inch(4)))
                                    draw_components(df, cfg_name="default", cfg_list=list(default=cfg_default))
                          })
    expect_doppelganger("draw_components.default", function() {
                                    pushViewport(viewport(width=inch(4), height=inch(4)))
                                    draw_components(df)
                          })

    # errors
    expect_error(dce("coin_face", list(gridline_color = "grey"), i_r = 3), "Don't know how to add grid lines to shape circle")
    expect_error(dce("coin_face", list(shape = "meeple"), i_r = 3), "Don't know how to draw shape meeple")
    expect_error(dce("coin_face", list(mat_width=0.2, mat_color="green", shape="kite"), i_r = 3), "Don't know how to add mat to shape kite")
    expect_error(cfg_default$get_pp_width("boo_back"), "Don't know width of component boo")
})


