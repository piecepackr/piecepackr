context("make_pnp_piecepack works as expected")
test_that("make_pnp_piecepack works as expected", {

    cfg <- c2o(paste0("--pdf_deck_dir=", tempdir()))
    json_filename <- tempfile(fileext=".json")
    on.exit(unlink(json_filename))
    .to_json(cfg, json_filename)
    expect_true(file.exists(json_filename))

    pdf_filename <- file.path(tempdir(), "piecepack_deck.pdf")
    on.exit(unlink(pdf_filename))
    opts <- read_configuration(paste0("--file=", json_filename))
    make_pnp_piecepack(opts)
    expect_true(file.exists(pdf_filename))
    expect_equal(n_pages(pdf_filename), 6)
    expect_equal(n_pages_gs(pdf_filename), 6)
    if (Sys.which("pdfinfo") != "")
        expect_equal(n_pages_pdfinfo(pdf_filename), 6)
})
