context("make_pnp_piecepack works as expected")
test_that("make_pnp_piecepack works as expected", {

    json_filename <- tempfile(fileext=".json")
    on.exit(unlink(json_filename))
    pdf_deck_dir = tempfile()
    on.exit(unlink(pdf_deck_dir, recursive=TRUE))
    svg_preview_dir = tempfile()
    on.exit(unlink(svg_preview_dir, recursive=TRUE))
    pdf_collection_dir = tempfile()
    on.exit(unlink(pdf_collection_dir, recursive=TRUE))
    pdf_preview_dir = tempfile()
    on.exit(unlink(pdf_preview_dir, recursive=TRUE))
    pdf_filename <- file.path(pdf_deck_dir, "piecepack_deck.pdf")
    on.exit(unlink(pdf_filename))
    svg_component_dir = tempfile()
    on.exit(unlink(svg_component_dir, recursive=TRUE))

    configure_piecepack(c(paste0("--pdf_deck_dir=", pdf_deck_dir), paste0("--svg_preview_dir=", svg_preview_dir),
               paste0("--svg_component_dir=", svg_component_dir), "--component_formats=svg", "--component_thetas=0",
               paste0("--file=", json_filename)))
    expect_true(file.exists(json_filename))

    opts <- read_configuration(paste0("--file=", json_filename))
    make_pnp_piecepack(opts)
    make_piecepack_preview(opts)
    make_piecepack_images(opts)

    expect_true(file.exists(pdf_filename))
    expect_equal(get_n_pages(pdf_filename), 6)
    expect_equal(get_n_pages_gs(pdf_filename), 6)
    if (Sys.which("pdfinfo") != "")
        expect_equal(get_n_pages_pdfinfo(pdf_filename), 6)

    opts <- get_arrangement_opts(c(paste0("--pdf_deck_dir=", pdf_deck_dir), paste0("--pdf_collection_dir=", pdf_collection_dir),
               paste0("--pdf_preview_dir=", pdf_preview_dir), paste0("--svg_preview_dir=", svg_preview_dir)))
    make_collection_preview(opts)
    make_collection(opts)

})
