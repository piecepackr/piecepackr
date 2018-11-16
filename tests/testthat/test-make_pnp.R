context("make_pnp works as expected")
test_that("make_pnp works as expected", {

    pdf_deck_dir = tempfile()
    on.exit(unlink(pdf_deck_dir, recursive=TRUE))
    pdf_deck_filename <- file.path(pdf_deck_dir, "piecepack_deck.pdf")
    on.exit(unlink(pdf_deck_filename))

    svg_preview_dir = tempfile()
    on.exit(unlink(svg_preview_dir, recursive=TRUE))
    svg_filename <- file.path(svg_preview_dir, "piecepack_deck.svg")
    on.exit(unlink(svg_filename))

    pdf_preview_dir = tempfile()
    on.exit(unlink(pdf_preview_dir, recursive=TRUE))
    pdf_preview_filename <- file.path(pdf_preview_dir, "piecepack_preview.pdf")
    on.exit(unlink(pdf_preview_filename))

    pdf_collection_dir = tempfile()
    on.exit(unlink(pdf_collection_dir, recursive=TRUE))
    pdf_collection_filename <- file.path(pdf_collection_dir, "piecepack_collection.pdf")
    on.exit(unlink(pdf_collection_filename))

    make_pnp(list(), pdf_deck_filename, "letter")
    make_preview(list(), svg_filename)

    expect_true(file.exists(pdf_deck_filename))
    expect_equal(get_n_pages(pdf_deck_filename), 6)
    expect_equal(get_n_pages_gs(pdf_deck_filename), 6)
    if (Sys.which("pdfinfo") != "")
        expect_equal(get_n_pages_pdfinfo(pdf_deck_filename), 6)

    make_collection_preview(pdf_preview_filename, svg_filename, "letter")
    expect_equal(get_n_pages(pdf_preview_filename), 2)
    make_collection(pdf_collection_filename, c(pdf_preview_filename, pdf_deck_filename), "letter", list())
    expect_equal(get_n_pages(pdf_collection_filename), 8)

})
