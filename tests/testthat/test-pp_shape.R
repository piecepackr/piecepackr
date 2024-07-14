library("grid", quietly = TRUE)
test_that("pp_shape() works as expected", {
    circle <- pp_shape("circle")
    current_dev <- grDevices::dev.cur()
    pdf_file <- tempfile(fileext = ".pdf")
    expect_error({
        pdf(pdf_file)
        grid.draw(circle$hexlines(gp=gpar(col="yellow")))
    })
    unlink(pdf_file)
    dev.off()
    if (current_dev > 1) grDevices::dev.set(current_dev)
    skip_on_ci()
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    expect_doppelganger("add_checkers", function() {
        rect <- pp_shape("rect")
        pushViewport(viewport(width=0.9, height=0.9))
        grid.draw(rect$shape(gp=gpar(fill="grey", col="NA")))
        grid.draw(rect$checkers(gp=gpar(fill="purple")))
        grid.draw(rect$gridlines(gp=gpar(col="black")))
        grid.draw(rect$mat(mat_width = 0.05, gp=gpar(fill="green")))
        grid.draw(rect$hexlines(gp=gpar(col="yellow")))
        grid.draw(rect$shape(gp=gpar(col="black", fill="NA", lex=4)))
    })
    expect_doppelganger("add_checkers.transparent", function() {
        rect <- pp_shape("rect")
        grid.draw(rect$checkers(gp=gpar(fill="transparent")))
        grid.draw(rect$hexlines(gp=gpar(col="transparent")))
    })
    expect_doppelganger("add_checkers.convex8", function() {
        convex8 <- pp_shape("convex8", back = TRUE)
        pushViewport(viewport(width=0.9, height=0.9))
        grid.draw(convex8$shape(gp=gpar(fill="grey", col="NA")))
        grid.draw(convex8$checkers(gp=gpar(fill="purple")))
        grid.draw(convex8$gridlines(gp=gpar(col="black")))
        grid.draw(convex8$mat(mat_width = 0.05, gp=gpar(fill="green")))
        grid.draw(convex8$shape(gp=gpar(col="black", fill="NA", lex=4)))
    })
    expect_doppelganger("roundrect", function() {
        roundrect <- pp_shape("roundrect")
        pushViewport(viewport(width=0.9, height=0.9))
        grid.draw(roundrect$shape(gp=gpar(fill="grey", col="NA")))
        grid.draw(roundrect$checkers(gp=gpar(fill="purple")))
        grid.draw(roundrect$gridlines(gp=gpar(col="black")))
        grid.draw(roundrect$mat(mat_width = 0.05, gp=gpar(fill="green")))
        grid.draw(roundrect$hexlines(gp=gpar(col="yellow")))
        grid.draw(roundrect$shape(gp=gpar(col="black", fill="NA", lex=4)))
    })
    expect_doppelganger("circle", function() {
        circle <- pp_shape("circle")
        pushViewport(viewport(width=0.9, height=0.9))
        grid.draw(circle$shape(gp=gpar(fill="grey", col="NA")))
        grid.draw(circle$checkers(gp=gpar(fill="purple")))
        grid.draw(circle$mat(mat_width = 0.05, gp=gpar(fill="green")))
        # grid.draw(circle$hexlines(gp=gpar(col="yellow"))) # nolint
        # grid.draw(circle$gridlines(gp=gpar(col="black"))) # nolint
        grid.draw(circle$shape(gp=gpar(col="black", fill="NA", lex=4)))
    })
    expect_doppelganger("polyclip.circle", function() {
        circle <- pp_shape("circle")
        g <- pieceGrob("coin_face", op_scale=1, default.units="npc")
        grid.draw(circle$polyclip(g, "minus", gp=gpar(fill="blue")))
    })
    skip_if_not_installed("gridpattern")
    expect_doppelganger("hex_pattern", function() {
        hex <- pp_shape("convex6")
        gp <- gpar(fill = c("blue", "yellow", "red"), col = "black")
        grid.draw(hex$pattern("polygon_tiling", gp = gp, spacing = 0.1,
                              type = "truncated_trihexagonal"))
        gp <- gpar(fill = "black", col = NA)
        grid.draw(hex$mat(mat_width = 0.025, gp = gp))
    })
})

test_that("npc_coords() works", {
    expect_equal(pp_shape("kite")$npc_coords, kite_xy(r = 0.2))
    expect_equal(pp_shape("kite", radius = 0.25)$npc_coords, kite_xy(r = 0.25))
    expect_equal(pp_shape("halma")$npc_coords, halma_xy())
    expect_equal(pp_shape("pyramid")$npc_coords, pyramid_xy)
    expect_equal(pp_shape("concave4")$npc_coords, concave_xy(4, 90, 0.2))
    expect_error(pp_shape("boobah")$npc_coords, "Don't recognize shape label boobah")
})

test_that("partition_edges works", {
    s <- pp_shape("kite")
    expect_equal(partition_edges(s)$type, rep("flat", 4))
    expect_equal(partition_edges(s)$indices, list(1:2, 2:3, 3:4, c(4, 1)))

    s <- pp_shape("oval")
    expect_equal(partition_edges(s)$type, "ring")
    expect_equal(partition_edges(s)$indices, list(1:72))

    s <- pp_shape("halma")
    expect_equal(partition_edges(s)$type, c("flat", "flat", "flat", "curved", "flat", "flat"))
    expect_equal(partition_edges(s)$indices, list(c(33, 1), 1:2, 2:3, 3:31, 31:32, 32:33))
})

test_that("Token2S works as expected", {
    shape <- pp_shape("kite")
    token <- Token2S$new(shape)
    expect_length(token$edges, 4)
})
