library("vdiffr")
context("piecepackr-deprecated works as expected")
test_that("piecepackr-deprecated works as expected", {
    expect_error(checkersGrob(shape = "circle", gp=gpar(fill="purple")))
    expect_error(hexlinesGrob(shape = "circle", gp=gpar(col="yellow")))
    skip_on_ci()
    expect_doppelganger("add_checkers.deprecated", function() {
                    grid.draw(checkersGrob(shape = "rect", col="purple"))
                    grid.draw(hexlinesGrob(shape = "rect", col="yellow"))
    })
    expect_doppelganger("add_checkers.transparent.deprecated", function() {
                    grid.draw(checkersGrob(shape = "rect", col="transparent"))
                    grid.draw(hexlinesGrob(shape = "rect", col="transparent"))
    })
    expect_doppelganger("add_checkers.convex8.deprecated", function() {
                    grid.draw(checkersGrob(shape = "convex8", col="purple"))
    })
})
