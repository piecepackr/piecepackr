library("tibble")
test_that("3d helper functions work", {

    dft <- tibble(piece_side="tile_back", x=1.5, y=1.5, rank=NA, width=NA)
    dfc <- tibble(piece_side="coin_face", x=c(1,2,2,1,1.5), y=c(2,2,1,1,1.5), rank=1:5, width=0.6)
    df <- rbind(dft, dfc)
    ans <- c(0.25, rep(0.375, 5))
    # close circles
    expect_equal(op_transform(df)$zt, ans)
    # close regular convex polygons
    expect_equal(op_transform(df, cfg=list(shape.coin="convex6"))$zt, ans)
    # close concave polygons (stars)
    expect_equal(op_transform(df, cfg=list(shape.coin="concave5"))$zt, ans)
    # close "kites"
    expect_equal(op_transform(df, cfg=list(shape.coin="kite"))$zt, ans)
    # close "pyramids"
    expect_equal(op_transform(df, cfg=list(shape.coin="pyramid"))$zt, ans)
    expect_warning(pp_cfg(list(shadow_fn = basicTokenEdge)))

    skip_on_ci()
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    opf <- function(op_angle=45, ...) {
        function() {
            pmap_piece(df, op_angle=op_angle, trans=op_transform,
                       op_scale=0.5, default.units="in", ...)
        }
    }

    # orthogonal tiles
    df <- tibble(piece_side="tile_back",
                         x=c(2,2,2,4,6,6,4,2,5),
                         y=c(4,4,4,4,4,2,2,2,3))
    expect_doppelganger("op_transform_045", opf(045))
    expect_doppelganger("op_transform_135", opf(135))
    expect_doppelganger("op_transform_225", opf(225))
    expect_doppelganger("op_transform_315", opf(315))

    # angled rectangles
    df1 <- tibble(piece_side="matchstick_face",
                         x=2+to_x(45, 0.19*1:4),
                         y=2+to_y(45, 0.19*1:4),
                         angle=45, rank=4, suit=1)
    df2 <- tibble(piece_side="matchstick_face",
                         x=2+to_x(-45, 0.19*1:4),
                         y=2+to_y(-45, 0.19*1:4),
                         angle=-45, rank=4, suit=2)
    df <- rbind(df1, df2, df1, df2, df1, df2)
    expect_doppelganger("matchsticks_045", opf(045))

    # rounding error #163
    df <- tibble(piece_side = "tile_face", x=rep(seq(1,7,2), 4), y=rep(seq(1,7,2), each=4),
                 angle = rep(90*0:3, 4), suit = rep(1:4, each=4), rank=rep_len(1:6, 16))
    expect_doppelganger("rotated_tile_faces", opf(045))

    # stacked pyramids
    suppressMessages({
    df <- tibble(piece_side = "pyramid_top", x = 2, y = 2, rank = 1:6,
                 suit = c(1:4, 1:2))
    expect_doppelganger("pyramid_tops_larger_on_top", opf(045))
    df <- tibble(piece_side = "pyramid_top", x = 2, y = 2, rank = 6:1,
                 suit = c(1:4, 1:2))
    expect_doppelganger("pyramid_tops_smaller_on_top", opf(045))

    cfg <- list(background_color.pyramid_face = "blue", background_color.pyramid_back = "red",
               background_color.pyramid_left = "green", background_color.pyramid_right = "grey")
    dft <- tibble(piece_side = "tile_back", x=rep(c(1, 3, 5), 3), y=rep(c(1, 3, 5), each=3),
                  rank = NA, suit = NA, angle = 0)
    dfp1 <- tibble(piece_side = "pyramid_top", x=3, y=3, rank = 3, suit = 2, angle = 0)
    dfp2 <- tibble(piece_side = rep(c("pyramid_face", "pyramid_back", "pyramid_left", "pyramid_right"), length.out=9),
                   x = rep(c(1, 3, 5), 3), y = rep(c(5, 3, 1), each=3),
                   angle = c(45, 0, -45, 90, 0, -90, 135, 180, -135),
                   rank = rep(1:6, length.out=9), suit = 1)[-5, ]
    df <- rbind(dft, dfp1, dfp2)
    expect_doppelganger("oblique_pyramids", opf(045, cfg = cfg))
    }, classes="piecepackr_affine_transformation")
})

test_that("SAT functions work", {
    r1 <- ConvexPolygon$new(x=c(0,0,1,1), y=c(0,1,1,0))
    r2 <- ConvexPolygon$new(x=0.5+c(0,0,1,1), y=0.5+c(0,1,1,0))
    r3 <- ConvexPolygon$new(x=1+c(0,0,1,1), y=1+c(0,1,1,0))
    r4 <- ConvexPolygon$new(x=c(0.25,0.25,0.75,0.75), y=c(0.25,0.75,0.75,0.25))

    c1 <- Circle$new(x=0.5, y=0.5, r=0.5)
    c2 <- Circle$new(x=1.0, y=1.0, r=0.5)
    c3 <- Circle$new(as_coord2d(x=2.0, y=2.0), r=0.5)

    expect_true(do_shapes_overlap("boo", "bar"))
    expect_true(do_shapes_overlap(r1, r2))
    expect_true(do_shapes_overlap(r2, r3))
    expect_false(do_shapes_overlap(r1, r3))
    expect_true(do_shapes_overlap(r1, r4))

    expect_true(do_shapes_overlap(c1, c2))
    expect_false(do_shapes_overlap(c2, c3))
    expect_false(do_shapes_overlap(c1, c3))

    expect_true(do_shapes_overlap(r1, c1))
    expect_false(do_shapes_overlap(r1, c3))
    expect_true(do_shapes_overlap(c1, r1))
    expect_false(do_shapes_overlap(c3, r1))
})

test_that("3D rotation functions work", {
    expect_equal(AA_to_R(angle=60), R_z(angle=60))
    R1 <- R_z(60) %*% R_x(50)
    R2 <- do.call(AA_to_R, R_to_AA(R1))
    expect_equal(R1, R2)
    expect_equal(AA_to_R(angle=60), R_z(angle=60))
    expect_equal(AA_to_R(angle=60), R_z(angle=60))
    expect_equal(AA_to_R(angle=45, axis_x=1), R_x(angle=45))
    expect_equal(AA_to_R(angle=30, axis_y=1), R_y(angle=30))
    expect_equal(R_to_AA(R_x(90)), list(angle = 90, axis_x = 1, axis_y = 0, axis_z = 0))
    expect_equal(R_to_AA(R_y(90)), list(angle = 90, axis_x = 0, axis_y = 1, axis_z = 0))
    expect_equal(R_to_AA(AA_to_R(180, 0.5, 0.5)),
                 list(angle = 180, axis_x = 0.5, axis_y = 0.5, axis_z = 0.7071068),
                 tolerance = 1e-6)
    expect_equal(R_to_AA(AA_to_R(180, -0.6, 0.6, 0.8)),
                 list(angle = 180, axis_x = -0.5144958, axis_y = 0.5144958, axis_z = 0.6859943),
                 tolerance = 1e-6)
    expect_equal(R_to_AA(AA_to_R(180,  0.6, -0.6)),
                 list(angle = 180, axis_x = 0.6, axis_y = -0.6, axis_z = 0.5291503),
                 tolerance = 1e-6)
    expect_equal(R_to_AA(AA_to_R(180, -0.6, -0.6)),
                 list(angle = 180, axis_x = -0.6, axis_y = -0.6, axis_z = 0.5291503),
                 tolerance = 1e-6)
    expect_equal(R_to_AA(AA_to_R(0, -0.6, 0.6)),
                 list(angle = 0, axis_x = 0, axis_y = 0, axis_z = 1),
                 tolerance = 1e-6)
})
