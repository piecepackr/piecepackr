context("make_images works as expected")
test_that("make_images works as expected", {
    directory <- tempfile()
    on.exit(unlink(directory))
    dir.create(directory)
    make_images(list(), directory)
    expect_equal(length(list.files(directory)), 108)
})

