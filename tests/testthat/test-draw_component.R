cfg_default <- pp_cfg(list(title="default cfg"))
cfg_3d <- game_systems("sans3d")$piecepack
test_that("pp_cfg works as expected", {
    expect_true(inherits(cfg_default, "pp_cfg"))
    expect_equal(class(as.list(cfg_default)), "list")
    expect_output(print(cfg_default), "default cfg")
})

test_that("update_names works as expected", {
    df <- tibble(x = 1:4, name = 1:4)
    expect_equal(update_name(df)$name, as.character(1:4))
    df <- tibble(x = 1:4, id = 1:4)
    expect_equal(update_name(df)$name, paste0("piece.", 1:4))
    df <- tibble(x = 1:4)
    expect_equal(update_name(df)$name, paste0("piece.", 1:4))
    df <- tibble(x = 1:4, name = 1)
    expect_warning(update_name(df)$name, "the name column in .l is not unique, generating new name column")
    df <- tibble(x = 1:4, id = 1)
    expect_warning(update_name(df)$name, "the id column in .l is not unique, generating new name column")
})

test_that("save_print_and_play works as expected", {
    skip_on_cran()
    pdf_deck_dir <- tempfile()
    dir.create(pdf_deck_dir)
    on.exit(unlink(pdf_deck_dir, recursive=TRUE))
    pdf_deck_filename <- file.path(pdf_deck_dir, "piecepack_deck.pdf")
    on.exit(unlink(pdf_deck_filename), add = TRUE)
    pdf_deck_filename_a4 <- file.path(pdf_deck_dir, "piecepack_deck_a4.pdf")
    on.exit(unlink(pdf_deck_filename_a4), add = TRUE)
    pdf_deck_filename_a5 <- file.path(pdf_deck_dir, "piecepack_deck_a5.pdf")
    on.exit(unlink(pdf_deck_filename_a5), add = TRUE)
    pdf_deck_filename_5s <- file.path(pdf_deck_dir, "piecepack_deck_5s.pdf")
    on.exit(unlink(pdf_deck_filename_5s), add = TRUE)
    pdf_deck_filename_4x6 <- file.path(pdf_deck_dir, "piecepack_deck_4x6.pdf")
    on.exit(unlink(pdf_deck_filename_4x6), add = TRUE)
    pdf_deck_filename_bleed <- file.path(pdf_deck_dir, "piecepack_deck_bleed.pdf")
    cfg_5s <- list(suit_text="♥,★,♣,♦,♠,꩜", suit_color="darkred,gold,darkgreen,darkblue,black,grey")

    skip_if_not(capabilities("cairo"))
    save_print_and_play(cfg_5s, pdf_deck_filename_5s, "A5", "all", "double-sided", quietly = TRUE)

    save_print_and_play(cfg_default, pdf_deck_filename, "letter", quietly = TRUE)

    cfg_large_dice <- pp_cfg(list(title="default cfg", width.die = 0.7))
    save_print_and_play(cfg_large_dice, pdf_deck_filename_a4, "A4", arrangement = "double-sided", quietly = TRUE)
    save_print_and_play(cfg_default, pdf_deck_filename_a5, "A5", quietly = TRUE)

    save_print_and_play(cfg_default, pdf_deck_filename_4x6, "4x6", pieces = "piecepack", quietly = TRUE)

    save_print_and_play(cfg_default, pdf_deck_filename_bleed, bleed=TRUE, quietly = TRUE)

    expect_true(file.exists(pdf_deck_filename))

    skip_if_not_installed("qpdf") # avoid potential "n_pages_gs()" bug on Windows
    skip_if_not_installed("xmpdf")
    expect_equal(xmpdf::n_pages(pdf_deck_filename), 7, ignore_attr = "names")
    expect_equal(xmpdf::n_pages(pdf_deck_filename_a4), 10, ignore_attr = "names")
    expect_equal(xmpdf::n_pages(pdf_deck_filename_a5), 14, ignore_attr = "names")
    expect_equal(xmpdf::n_pages(pdf_deck_filename_4x6), 13, ignore_attr = "names")
    expect_equal(xmpdf::n_pages(pdf_deck_filename_bleed), 7, ignore_attr = "names")
})

test_that("save_piece_images works as expected", {
    skip_if_not(capabilities("cairo"))
    directory <- tempfile()
    on.exit(unlink(directory))
    cfg <- pp_cfg(list(grob_fn=picturePieceGrobFn(directory)))
    g.p <- function(...) {
        grid.piece(..., op_scale=0.5, default.units="in")
    }

    current_dev <- grDevices::dev.cur()
    expect_error(save_piece_images(cfg_default, directory),
                 "dir.exists\\(directory\\) is not TRUE")
    expect_error(grid.piece("tile_back", cfg=cfg), "Couldn't find suitable")
    grDevices::dev.off()
    if (current_dev > 1)  {
        grDevices::dev.set(current_dev)
    }

    dir.create(directory)
    save_piece_images(cfg_default, directory, format="svgz", angle=c(0,90))
    expect_equal(length(list.files(directory)), 496)

    announce_snapshot_file(name = "diagram-op-ppgf.svg")
    skip_if_not(Sys.info()[["nodename"]] == "pesd-tld-laptop")
    library("vdiffr")
    suppressMessages({
      expect_doppelganger("diagram_op_ppgf", function() {
          g.p("tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1), cfg=cfg)
          g.p("tile_back", x=0.5+3, y=0.5+1, z=1/4+1/8, cfg=cfg)
          g.p("tile_back", x=0.5+3, y=0.5+1, z=2/4+1/8, cfg=cfg)
          g.p("die_face", x=1, y=1, z=1/4+1/4, cfg=cfg)
          g.p("pawn_face", x=1, y=4, z=1/4+1/8, angle=90, cfg=cfg)
          g.p("coin_face", x=3, y=4, z=1/4+1/16, angle=180, cfg=cfg)
          g.p("coin_back", x=3, y=4, z=1/4+1/8+1/16, angle=180, cfg=cfg)
          g.p("coin_back", x=3, y=1, z=3/4+1/16, angle=90, cfg=cfg)
      })
    }, classes="piecepackr_affine_transformation")
})

test_that("no regressions in figures", {
    skip_on_ci()
    skip_if_not(capabilities("cairo"))
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    dc <- function(..., cfg=cfg_default) {
        grid.piece(..., cfg=cfg, default.units="npc")
    }
    dce <- function(...) {
        tmpfile <- tempfile(fileext=".svg")
        on.exit(unlink(tmpfile))
        svg(tmpfile)
        on.exit(dev.off(), add = TRUE)
        grid.piece(..., default.units="npc")
    }
    # tile back
    expect_doppelganger("tile_back", function() dc("tile_back", bleed=TRUE))
    expect_doppelganger("tile_back_thickgrid", function() dc("tile_back", cfg=list(gridline_lex.tile_back=5)))
    expect_doppelganger("tile_back_thickborder", function() dc("tile_back", cfg=list(border_lex.tile_back=5)))
    expect_doppelganger("tile_back-svg", function() dc("tile_back", type="picture"))
    expect_doppelganger("tile_back-hex",
                                function() dc("tile_back", cfg=list(shape.tile_back="convex6")))
    # tile face
    expect_doppelganger("tile_face.s1.r1", function() dc("tile_face", suit=1, rank=1))
    expect_doppelganger("tile_face.s3.r2", function() dc("tile_face", suit=3, rank=2))
    expect_doppelganger("tile_face.s3.r2-suit_as_ace",
                                function() dc("tile_face", cfg=list(use_suit_as_ace=TRUE), suit=3, rank=2))
    expect_doppelganger("tile_face.s2.r3", function() dc("tile_face", suit=2, rank=3))
    df_tile <- tibble::tribble(~x, ~y, ~suit, ~rank,
                               1, 1, NA, NA,
                               3, 1, 3, NA,
                               1, 3, NA, 3,
                               3, 3, 3, 3)
    df_tile$piece_side <- "tile_back"
    df_tile$please_ignore <- "what"
    df_tile$id <- 1:4
    expect_doppelganger("tiles_faces", function()
        pmap_piece(df_tile, width=2, height=2, default.units="inches", cfg="cfg_default")
    )
    # 106
    expect_doppelganger("different_sizes", function() {
        pushViewport(viewport(width=inch(6), height=inch(6)))
        dc("tile_face", suit=2, rank=3, x=inch(1), y=inch(5))
        dc("tile_face", suit=2, rank=3, x=inch(4), y=inch(4.5), cfg=list(width.tile=3))
        dc("tile_face", suit=2, rank=3, x=inch(4), y=inch(1.5), cfg=list(width.tile=4, height.tile=3))
        dc("coin_back", suit=3, x=inch(1), y=inch(1), cfg=list(mat_width.coin=0.1))
        dc("coin_back", suit=3, x=inch(2), y=inch(1), cfg=list(width.coin=1, mat_width.coin=0.1))
    })

    #### coins appear wrong #99
    # coin back
    expect_doppelganger("coin_back.s4", function() dc("coin_back", suit=4))
    expect_doppelganger("coin_back.s4-inverted",
                                function() dc("coin_back", cfg=list(invert_colors=TRUE), suit=4))
    # coin face
    expect_doppelganger("coin_face.r1", function() dc("coin_face", rank=1))
    expect_doppelganger("coin_face.r2", function() dc("coin_face", rank=2))
    expect_doppelganger("coin_face.r4", function() dc("coin_face", rank=4))
    expect_doppelganger("coin_face.r4italic", function() {
        dc("coin_face", cfg=list(ps_fontface.coin_face="italic"), rank=4)
    })
    expect_doppelganger("coin_face.r4pyramid",
                                function() dc("coin_face", rank=4, cfg=list(shape.coin="pyramid")))
    expect_doppelganger("coin_face.r4convex8",
                                function() dc("coin_face", rank=4, cfg=list(shape.coin="convex8")))
    expect_doppelganger("coin_face.r4concave8",
                                function() dc("coin_face", rank=4, cfg=list(shape.coin="concave8", shape_r.coin=0.4)))
    expect_doppelganger("coin_face.r4oval",
                                function() dc("coin_face", rank=4, op_scale = 0.5,
                                              cfg=list(shape.coin="oval", height.coin=1.3)))
    # die faces
    expect_doppelganger("die_face.s4.r1", function() dc("die_face", suit=4, rank=1))
    expect_doppelganger("die_face.s3.r2", function() dc("die_face", suit=3, rank=2))
    expect_doppelganger("die_face.s2.r5", function() dc("die_face", suit=2, rank=5))
    expect_doppelganger("die_face.s2.r5.kite",
            function() dc("die_face", suit=2, rank=5, cfg=list(shape.die_face="kite", dm_t=90, suit_cex="1,1,1,1")))
    expect_doppelganger("die_face.s2.r5.convex5mat", function() {
        dc("die_face", suit=2, rank=5, cfg=list(shape.die_face="convex5", mat_width.die_face=0.1, mat_color="pink"))
    })
    expect_doppelganger("suitdie_face.s1", function() dc("suitdie_face", suit=1))
    expect_doppelganger("suitdie_face.s5", function() dc("suitdie_face", suit=5))
    expect_doppelganger("suitdie_face.s6", function() dc("suitdie_face", suit=6))
    # pawns
    expect_doppelganger("pawn_face.s3", function() dc("pawn_face", suit=3))
    expect_doppelganger("pawn_back.s2", function() dc("pawn_back", suit=2))
    expect_doppelganger("belt_face.s1", function() dc("belt_face", suit=1))
    expect_doppelganger("saucer_face.s4", function() dc("saucer_face", suit=4))
    expect_doppelganger("saucer_back", function() dc("saucer_back"))
    cfg <- list(shape.pawn="convex6", height.pawn=1, width.pawn=0.5)
    expect_doppelganger("pawn_face.irregular_convex",
                        function() dc("pawn_face", cfg=cfg, op_scale=0.5))

    # matchsticks
    expect_doppelganger("matchstick_face.s1.r1", function() dc("matchstick_face", suit=1, rank=1))
    expect_doppelganger("matchstick_face.s2.r2", function() dc("matchstick_face", suit=2, rank=2))
    expect_doppelganger("matchstick_face.s3.r3", function() dc("matchstick_face", suit=3, rank=3))
    expect_doppelganger("matchstick_face.s4.r4", function() dc("matchstick_face", suit=4, rank=4))
    expect_doppelganger("matchstick_face.s1.r5", function() dc("matchstick_face", suit=1, rank=5))
    expect_doppelganger("matchstick_face.s2.r6", function() dc("matchstick_face", suit=2, rank=6))
    cfg <- list(invert_colors.suited=TRUE, grob_fn="basicPieceGrob")
    expect_doppelganger("matchstick_back.s3.r6", function() dc("matchstick_back", cfg=cfg, suit=3, rank=6))

    expect_doppelganger("preview", function() dc("preview_layout"))
    expect_doppelganger("preview.5s", function() {
        dc("preview_layout", cfg=list(suit_text="A,B,C,D,E,", die_arrangement="counter_up"))
    })
    expect_doppelganger("preview.6s", function() dc("preview_layout", cfg=list(suit_text="I,II,III,IV,V,VI,")))

    # dice
    cfg <- list(suit_text="-,0,+,")
    expect_doppelganger("suitdie_layoutLF", function() dc("suitdie_layoutLF", cfg=cfg))
    expect_doppelganger("suitdie_layoutRF", function() dc("suitdie_layoutRF"))
    expect_doppelganger("suitrankdie_layoutLF", function() dc("suitrankdie_layoutLF"))

    cfg <- list(die_arrangement="opposites_sum_to_5")
    expect_doppelganger("die_layoutRF-opposites_sum_to_5", function() dc("die_layoutRF", suit=3, cfg=cfg))

    # 102
    expect_doppelganger("rankdie_layoutRF_suitasace", function() {
        dc("die_layoutRF", suit=6, cfg=list(use_suit_as_ace=TRUE))
    })

    # edges
    df <- tibble(piece_side = c("pawn_top", "coin_left", "tile_base"),
                 x = 1:3, y = 1:3, suit = 1:3, rank = 1:3)
    cfg <- list(border_color = "black", edge_color.pawn = "red",
                edge_color.coin = "grey", edge_color.tile = "grey")
    expect_doppelganger("piece_edges", function() {
                                    pushViewport(viewport(width=inch(4), height=inch(4)))
                                    pmap_piece(df, cfg=cfg, default.units="in")
                          })

    cfg <- list(suit_text="A,B,C,D,E,F", suit_color="red,black,green,blue,orange,grey")
    expect_doppelganger("suitdie_layoutRF-5suits", function() dc("suitdie_layoutRF", cfg=cfg))
    expect_doppelganger("suitrankdie_layoutRF-5suits", function() dc("suitrankdie_layoutRF", cfg=cfg))
    cfg <- list(suit_text="A,B,C,D,E,F,G", suit_color="red,black,green,blue,orange,purple,grey")
    expect_doppelganger("suitdie_layoutRF-6suits", function() dc("suitdie_layoutRF", cfg=cfg))
    expect_doppelganger("suitrankdie_layoutRF-6suits", function() dc("suitrankdie_layoutRF", cfg=cfg))

    # draw_pieces
    df <- tibble::tribble(~piece_side, ~x, ~y, ~suit, ~rank,
                          "tile_face", 0.25, 0.25, 1, 1,
                          "tile_face", 0.75, 0.25, 2, 2,
                          "tile_face", 0.25, 0.75, 3, 5,
                          "tile_face", 0.75, 0.75, 4, 6)
    expect_doppelganger("draw_components", function() {
                                    pushViewport(viewport(width=inch(4), height=inch(4)))
                                    pmap_piece(df, cfg="default", envir=list(default=cfg_default))
                          })
    expect_doppelganger("draw_components.default", function() {
                                    pushViewport(viewport(width=inch(4), height=inch(4)))
                                    pmap_piece(df, envir=list())
                          })

    # errors
    expect_error(dce("coin_face", rank = 3, cfg=list(gridline_color = "grey")),
                 "Don't know how to add grid lines to shape circle")
    expect_error(dce("coin_face", rank = 3, cfg=list(shape = "megahex")),
                 "Don't recognize shape label megahex")
    expect_error(dce("coin_face", rank = 3, cfg=list(mat_width=0.2, mat_color="green", shape="kite")),
                 "Don't know how to add mat to shape kite")
    expect_error(cfg_default$get_width("boo_back"), "Don't know width of piece boo")
    expect_error(game_systems("boobear"),
                 "Don't have a customized configuration for style boobear")

    # piecepack pyramids
    expect_doppelganger("pyramid_face.s1.r6", function() dc("pyramid_face", suit=1, rank=6))
    expect_doppelganger("pyramid_left.s2.r5", function() dc("pyramid_left", suit=2, rank=5))
    expect_doppelganger("pyramid_right.s3.r2", function() dc("pyramid_right", suit=3, rank=2))
    expect_doppelganger("pyramid_back.s4.r1", function() dc("pyramid_back", suit=4, rank=1))
    expect_doppelganger("pyramid_face.s2.r1", function() dc("pyramid_face", suit=2, rank=1))
    cfg <- list(invert_colors.suited=TRUE, grob_fn="basicPieceGrob")
    expect_doppelganger("pyramid_layout.s3.r4", function() dc("pyramid_layout", cfg=cfg, suit=3, rank=4))

    announce_snapshot_file(name = "pyramid-face-op.svg")
    announce_snapshot_file(name = "pyramid-top-op.svg")
    announce_snapshot_file(name = "pyramid-top-s4-r3.svg")
    skip_if_not(Sys.info()[["nodename"]] == "pesd-tld-laptop")
    suppressMessages({
      expect_doppelganger("pyramid_top.s4.r3", function()
          dc("pyramid_top", suit=4, rank=3))
      expect_doppelganger("pyramid_top_op", function()
          dc("pyramid_top", rank = 6, op_angle = 90, op_scale = 0.5, cfg = list(invert_colors = TRUE)))
      expect_doppelganger("pyramid_face_op", function()
          dc("pyramid_face", rank = 6, op_angle = 90, op_scale = 0.5, cfg = list(invert_colors = TRUE)))
    }, classes="piecepackr_affine_transformation")
})

test_that("oblique projection works", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not(capabilities("cairo"))
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    dc <- function(..., cfg=cfg_default) {
        grid.piece(..., cfg=cfg, op_scale=0.5, default.units="npc")
    }
    expect_doppelganger("tile_face_op", function() dc("tile_face"))
    expect_doppelganger("tile_face_op_roundrect", function() dc("tile_face", cfg=list(shape.tile="roundrect")))
    expect_doppelganger("coin_face_op", function() dc("coin_face"))
    expect_doppelganger("pawn_face_op", function() dc("pawn_face", cfg=cfg_3d))
    expect_doppelganger("matchstick_face_op", function() dc("matchstick_face"))
    suppressMessages({
      expect_doppelganger("die_face_op", function() dc("die_face"))
      g.p <- function(...) {
          grid.piece(..., op_scale=0.5, default.units="in")
      }
      cfg <- pp_cfg(list(depth.pawn=1, width.pawn=0.75, height.pawn=0.75,
                         dm_text.pawn="", shape.pawn="convex6", invert_colors.pawn=TRUE))
      expect_doppelganger("diagram_op", function() {
          g.p("tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1), cfg=cfg)
          g.p("tile_back", x=0.5+3, y=0.5+1, z=1/4+1/8, cfg=cfg)
          g.p("tile_back", x=0.5+3, y=0.5+1, z=2/4+1/8, cfg=cfg)
          g.p("die_face", x=1, y=1, z=1/4+1/4, cfg=cfg)
          g.p("pawn_face", x=1, y=4, z=1/4+1/2, angle=90, cfg=cfg)
          g.p("coin_back", x=3, y=4, z=1/4+1/16, angle=180, cfg=cfg)
          g.p("coin_back", x=3, y=4, z=1/4+1/8+1/16, angle=180, cfg=cfg)
          g.p("coin_back", x=3, y=1, z=3/4+1/16, angle=90, cfg=cfg)
      })
    }, classes="piecepackr_affine_transformation")

    cfg <- pp_cfg(list(depth.pawn = 10/25.4, width.pawn=16/25.4, height.pawn=16/25.4,
                       dm_cex.pawn=0.5, shape.pawn="meeple", invert_colors.pawn=TRUE))
    expect_doppelganger("meeple", function() {
        g.p("pawn_face", x=2, y=3, cfg=cfg, op_angle=90)
        g.p("pawn_face", x=3, y=3, cfg=cfg, op_angle=45)
        g.p("pawn_face", x=3, y=2, cfg=cfg, op_angle=0)
        g.p("pawn_face", x=3, y=1, cfg=cfg, op_angle=-45)
        g.p("pawn_face", x=2, y=1, cfg=cfg, op_angle=-90)
        g.p("pawn_face", x=1, y=1, cfg=cfg, op_angle=-135)
        g.p("pawn_face", x=1, y=2, cfg=cfg, op_angle=180)
        g.p("pawn_face", x=1, y=3, cfg=cfg, op_angle=135)
        g.p("coin_face", x=2, y=2, cfg=cfg, op_angle=0, angle=190)
    })
})

test_that("alpha and scale works", {
    skip_on_ci()
    skip_if_not(capabilities("cairo"))
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    expect_doppelganger("alpha", function() {
        cfg <- pp_cfg(list(shape.coin="convex6"))
        df <- tibble(piece_side="coin_back",
                     x=1:6, y=1, alpha=seq(0, 1, length.out=6))
        pmap_piece(df, default.units="in", cfg=cfg)
    })
    expect_doppelganger("scale", function() {
        cfg <- pp_cfg(list(shape.coin="convex6"))
        df <- tibble(piece_side="coin_back",
                     x=1:6, y=1,
                     scale=seq(0, 1, length.out=6))
        pmap_piece(df, default.units="in", cfg=cfg)
    })
    announce_snapshot_file(name = "alpha-and-scale-op.svg")
    skip_if_not(Sys.info()[["nodename"]] == "pesd-tld-laptop")
    expect_doppelganger("alpha_and_scale_op", function() {
        cfg <- pp_cfg(list(shape.coin="convex6"))
        df <- tibble(piece_side="coin_back",
                     x=1:6, y=1, alpha=seq(0, 1, length.out=6),
                     scale=seq(0, 1, length.out=6))
        g <- pmap_piece(df, default.units="in", cfg=cfg, op_scale=0.5)
        grid.draw(pp_shape()$polyclip(g, "minus", gp=gpar(fill="yellow")))
    })
})
