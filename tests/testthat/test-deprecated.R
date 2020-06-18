library("vdiffr")
context("grob_fn_helpers works as expected")
test_that("grob_fn_helpers works as expected", {
    expect_error(checkersGrob(shape = "circle", gp=gpar(fill="purple")))
    expect_error(hexlinesGrob(shape = "circle", gp=gpar(col="yellow")))
    skip_on_ci()
    expect_doppelganger("add_checkers", function() {
        suppressWarnings({
            grid.draw(checkersGrob(shape = "rect", col="purple"))
            grid.draw(hexlinesGrob(shape = "rect", col="yellow"))
        }, classes = "deprecatedWarning")
    })
    expect_doppelganger("add_checkers.transparent", function() {
        suppressWarnings({
            grid.draw(checkersGrob(shape = "rect", col="transparent"))
            grid.draw(hexlinesGrob(shape = "rect", col="transparent"))
        }, classes = "deprecatedWarning")
    })
    expect_doppelganger("add_checkers.convex8", function() {
        suppressWarnings({
            grid.draw(checkersGrob(shape = "convex8", col="purple"))
        }, classes = "deprecatedWarning")
    })
})
