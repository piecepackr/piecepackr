test_that("Defunct functions throw errors", {
    skip_on_ci()

    expect_error(checkersGrob(shape = "convex8", col = "purple"),
                 class = "defunctError")
    expect_error(concaveGrobFn(3, 90),
                 class = "defunctError")
    expect_error(convexGrobFn(3, 90),
                 class = "defunctError")
    expect_error(get_shape_grob_fn("rect"),
                 class = "defunctError")
    expect_error(gridlinesGrob(col = "black", shape = "rect"),
                 class = "defunctError")
    expect_error(halmaGrob(),
                 class = "defunctError")
    expect_error(hexlinesGrob(shape = "rect", col = "transparent"),
                 class = "defunctError")
    expect_error(kiteGrob(),
                 class = "defunctError")
    expect_error(matGrob(col = "black", shape = "convex3"),
                 class = "defunctError")
    expect_error(pyramidGrob(),
                 class = "defunctError")
})
