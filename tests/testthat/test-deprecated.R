context("grob_fn_helpers works as expected")
test_that("grob_fn_helpers works as expected", {
    expect_error(checkersGrob(shape = "circle", gp=gpar(fill="purple")))
    expect_error(hexlinesGrob(shape = "circle", gp=gpar(col="yellow")))

    skip_on_ci()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_warning(checkersGrob(shape = "convex8", col = "purple"),
                   class = "deprecatedWarning")
    expect_warning(concaveGrobFn(3, 90), class = "deprecatedWarning")
    expect_warning(convexGrobFn(3, 90), class = "deprecatedWarning")
    expect_warning(get_shape_grob_fn("rect"), class = "deprecatedWarning")
    expect_warning(gridlinesGrob(col = "black", shape = "rect"),
                   class = "deprecatedWarning")
    expect_warning(halmaGrob(), class = "deprecatedWarning")
    expect_warning(hexlinesGrob(shape = "rect", col = "transparent"),
                   class = "deprecatedWarning")
    expect_warning(kiteGrob(), class = "deprecatedWarning")
    expect_warning(matGrob(col = "black", shape = "convex3"),
                   class = "deprecatedWarning")
    expect_warning(pyramidGrob(), class = "deprecatedWarning")
})
