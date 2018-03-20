# expect_that(, throws_error())

context("Test directional marker colors")
test_that("directional marker colors are as expected", {
    extra_flags <- c("--invert_colors.suited", "--directional_marker=matching",
                     "--background_color=white", "--suit_colors=darkred,black,darkgreen,darkblue,grey")
    opts <- process_configuration(configuration_options(extra_flags))
    expect_equal(get_suit_color("coin_back", 1, opts), "white")
    expect_equal(get_suit_color("coin_face", 5, opts), "grey")
})

