library("tibble")
library("vdiffr")
context("3d helper function")
test_that("3d helper functions work", {
    opf <- function(op_angle=45) {
        function() {
            pmap_piece(df, op_angle=op_angle, trans=op_transform,
                       op_scale=0.5, default.units="in")
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
})

test_that("SAT functions work", {
    r1 <- ConvexPolygon$new(x=c(0,0,1,1), y=c(0,1,1,0))
    r2 <- ConvexPolygon$new(x=0.5+c(0,0,1,1), y=0.5+c(0,1,1,0))
    r3 <- ConvexPolygon$new(x=1+c(0,0,1,1), y=1+c(0,1,1,0))
    r4 <- ConvexPolygon$new(x=c(0.25,0.25,0.75,0.75), y=c(0.25,0.75,0.75,0.25))

    c1 <- Circle$new(x=0.5, y=0.5, r=0.5)
    c2 <- Circle$new(x=1.0, y=1.0, r=0.5)
    c3 <- Circle$new(x=2.0, y=2.0, r=0.5)

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
