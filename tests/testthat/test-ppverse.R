test_that("pkgs_ppverse()", {
    expect_true(length(pkgs_ppverse(free_libre_only = FALSE)) >
                length(pkgs_ppverse(free_libre_only = TRUE)))
})
