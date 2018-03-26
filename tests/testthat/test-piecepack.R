# expect_that(, throws_error())

f2o <- function(args=NULL) {
    process_configuration(configuration_options(args))
}

context("Test options are working")
test_that("options work as expected", {
    extra_flags <- c("--invert_colors.suited", "--directional_marker=matching",
                     "--background_color=white", "--suit_colors=darkred,black,darkgreen,darkblue,grey")
    opts <- f2o(extra_flags)
    expect_equal(get_suit_color("coin_back", 1, opts), "white")
    expect_equal(get_suit_color("coin_face", 5, opts), "grey")

    expect_equal(should_invert("coin_back", f2o()), FALSE)
    expect_equal(should_invert("coin_back", f2o("--invert_colors")), TRUE)
    expect_equal(should_invert("coin_back", f2o("--invert_colors.suited")), TRUE)
    expect_equal(should_invert("coin_back", f2o("--invert_colors.unsuited")), FALSE)
    expect_equal(should_invert("coin_back", f2o("--invert_colors.coin_back")), TRUE)
    expect_equal(should_invert("coin_back", f2o("--invert_colors.coin_face")), FALSE)
    expect_equal(should_invert("coin_back", f2o("--uninvert_colors")), FALSE)
    expect_equal(should_invert("coin_face", f2o()), FALSE)
    expect_equal(should_invert("coin_face", f2o("--invert_colors")), TRUE)
    expect_equal(should_invert("coin_face", f2o("--invert_colors.suited")), FALSE)
    expect_equal(should_invert("coin_face", f2o("--invert_colors.unsuited")), TRUE)
    expect_equal(should_invert("coin_face", f2o("--invert_colors.coin_back")), FALSE)
    expect_equal(should_invert("coin_face", f2o("--invert_colors.coin_face")), TRUE)
    expect_equal(should_invert("coin_face", f2o("--uninvert_colors")), FALSE)

    expect_equal(get_rank_symbol("chip_face", 1, 2, f2o(c("--suit_symbols=1,2,3,4,5", "--rank_symbols=A,B,C,D,E,F",
                                                       "--use_suit_as_ace"))), "1")
    args <- c("--suit_symbols=1,2,3,4,5", "--rank_symbols=A,B,C,D,E,F", 
              "--use_suit_as_ace", "--use_ace_as_ace.chip_face")
    expect_equal(f2o(args)$use_suit_as_ace.chip_face, FALSE)
    expect_equal(get_rank_symbol("chip_face", 1, 2, f2o(args)), "B")

    expect_equal(get_background_color("tile_face", 1, f2o("--background_color.unsuited=orange")), "white")
    expect_equal(get_background_color("tile_back", 1, f2o("--background_color.unsuited=orange")), "orange")

})
