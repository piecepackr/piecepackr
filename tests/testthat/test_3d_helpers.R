library("vdiffr")
context("3d helper function")
test_that("3d helper functions work", {
    df <- tibble::tibble(piece_side="tile_back", 
                         x=c(2,2,2,4,6,6,4,2,5),
                         y=c(4,4,4,4,4,2,2,2,3))
    opf <- function(op_angle=45) { 
        function() {
            pmap_piece(df, op_angle=op_angle, trans=op_transform, 
                       op_scale=0.5, default.units="in")
        }
    }
    expect_doppelganger("op_transform_045", opf( 45))
    expect_doppelganger("op_transform_135", opf(135))
    expect_doppelganger("op_transform_225", opf(225))
    expect_doppelganger("op_transform_315", opf(315))
})
