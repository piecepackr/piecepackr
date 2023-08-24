LETTER_WIDTH <- 8.5
LETTER_HEIGHT <- 11
A4_WIDTH <- 8.27
A4_HEIGHT <- 11.69

#' Save piecepack print-and-play (PnP) file
#'
#' Save piecepack print-and-play (PnP) file
#'
#' @param cfg Piecepack configuration list or `pp_cfg` object
#' @param output_filename Filename for print-and-play file
#' @param size PnP output size (currently supports either "letter", "A4", "A5", or "4x6").
#'             This is the targeted \dQuote{trim} size of the print-and-play file
#'             (`size_bleed` can be used to make the print-and-play file larger than this).
#'             Size "4x6" currently only supports `pieces = "piecepack"`
#'             and doesn't support `bleed = TRUE`.
#'             "A5" is in \dQuote{portrait} mode whereas the other sizes are in \dQuote{landscape} mode.
#' @param pieces Character vector of desired PnP pieces.
#'        Supports "piecepack", "matchsticks", "pyramids", "subpack", or "all".
#'        If `NULL` and combination of `size` / `bleed` values supports "matchsticks" and "pyramids"
#'        then defaults to `c("piecepack", "pyramids", "matchsticks")` else just "piecepack".
#' @param arrangement Either "single-sided" or "double-sided".
#'                    Ignored if `size = "4x6"`.
#' @param quietly Whether to hide messages about missing metadata
#'                in the provided configuration.
#' @param ... Currently ignored.
#' @param bleed If `TRUE` produce a variant print-and-play file with "bleed" zones
#'              and "crop marks" around game pieces.
#'              Currently only supports `pieces = "piecepack"` and doesn't
#'              support `size = "4x6"`.
#' @param size_bleed A list with names "top", "right", "bottom", "left"
#'                   containing numeric values indicating the inches "bleed" to add to
#'                   the `size` of the print-and-play layout.
#'                   The default `NULL` means no such bleed added to "letter", "A4", "A5"
#'                   layouts and a small bleed added to "4x6" layouts
#'                   (1/16" to top/bottom and 3/32" to left/right).
#'                   NB. multiply millimeters by `0.0393700787` to convert to inches.
#'                   We currently don't support an asymmetric left/right bleed combined with
#'                   `arrangement = "double-sided"`.
#' @inheritParams render_piece
#' @examples
#' \donttest{# May take more than 5 seconds on CRAN servers
#' if (capabilities("cairo")) {
#'   cfg <- pp_cfg(list(invert_colors.suited=TRUE))
#'   cfg$description <- 'Piecepack with an "inverted" color scheme.'
#'   cfg$title <- '"Inverted" piecepack'
#'   cfg$copyright <- "\u00a9 2022 Trevor L Davis.  Some Right Reserved."
#'   cfg$spdx_id <- "CC-BY-4.0"
#'   cfg$credit <- ""
#'
#'   file <- tempfile("my_pnp_file", fileext = ".pdf")
#'   file_ds <- tempfile("my_pnp_file_ds", fileext = ".pdf")
#'   file_a4 <- tempfile("my_pnp_file_a4", fileext = ".pdf")
#'   file_a5 <- tempfile("my_pnp_file_a5", fileext = ".pdf")
#'
#'   save_print_and_play(cfg, file)
#'   save_print_and_play(cfg, file_ds, arrangement="double-sided")
#'   save_print_and_play(cfg, file_a4, size="A4", pieces="all")
#'   save_print_and_play(cfg, file_a5, size="A5")
#' }
#' }
#' @export
save_print_and_play <- function(cfg = getOption("piecepackr.cfg", pp_cfg()),
                                output_filename = "piecepack.pdf",
                                size = c("letter", "A4", "A5", "4x6"),
                                pieces = NULL,
                                arrangement=c("single-sided", "double-sided"),
                                dev = NULL,
                                dev.args = list(family = cfg$fontfamily,
                                                onefile = TRUE,
                                                units = "in",
                                                bg = "white",
                                                res = 300),
                                quietly = FALSE, ...,
                                bleed = FALSE, size_bleed = NULL) {

    opt <- options(piecepackr.op_scale = 0)
    on.exit(options(opt))

    stopifnot(is.null(dev) || is.function(dev))
    size <- match.arg(size)
    arrangement <- match.arg(arrangement)
    if (is.null(pieces)) {
        if (size == "4x6" || bleed)
            pieces <- "piecepack"
        else
            pieces <- c("piecepack", "pyramids", "matchsticks")
    }
    if ("all" %in% pieces)
        pieces <- c("piecepack", "pyramids", "matchsticks", "subpack")
    if (is.null(size_bleed)) {
        if (size == "4x6")
            size_bleed <- list(top = 1/16, right = 3/32, bottom = 1/16, left = 3/32)
        else
            size_bleed <- list(top = 0, right = 0, bottom = 0, left = 0)
    }
    stopifnot(all(c("top", "right", "bottom", "left") %in% names(size_bleed)),
              all(sapply(size_bleed, is.numeric)),
              arrangement == "single-sided" || size_bleed$left == size_bleed$right )

    cfg <- as_pp_cfg(cfg)
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    height <- switch(size,
                    letter = LETTER_WIDTH,
                    A4 = A4_WIDTH,
                    A5 = A4_WIDTH,
                    `4x6` = 4) + size_bleed$top + size_bleed$bottom
    width <- switch(size,
                    letter = LETTER_HEIGHT,
                    A4 = A4_HEIGHT,
                    A5 = A4_HEIGHT / 2,
                    `4x6` = 6) + size_bleed$right + size_bleed$left

    if (is.null(dev))
        dev <- pp_device_fn(output_filename)

    args <- list(filename = output_filename, width = width, height = height)
    args <- c(args, dev.args)
    args <- args[names(args) %in% names(formals(dev))]
    onefile <- has_onefile(dev, args)
    if (!onefile && !has_c_integer_format(output_filename))
        abort(paste("`save_print_and_play()` generates multiple pages.",
                    'Either `dev` needs to support `onefile` and `isTRUE(dev.args[["onefile"]])`',
                    "or else `output_filename` needs a C integer format in the string."))
    do.call(dev, args)

    pl <- switch(size,
                 `4x6` = print_and_play_4x6(cfg, pieces, quietly, bleed, size_bleed),
                 print_and_play_paper(cfg, size, pieces, arrangement, quietly, bleed, size_bleed))

    invisible(grDevices::dev.off())

    if (onefile && tools::file_ext(output_filename) == "pdf") {
        if(requireNamespace("xmpdf", quietly = TRUE)) {
            add_pdf_metadata(output_filename, cfg, pl)
        } else if (!isFALSE(getOption("piecepackr.metadata.inform"))) {
            msg <- c(x = "Need the {xmpdf} package to embed pdf metadata",
                     i = '`install.packages("xmpdf")`',
                     i = "These messages can be disabled via `options(piecepackr.metadata.inform = FALSE)`.")
            inform(msg, class = "piecepackr_embed_metadata")

        }
    }
    invisible(NULL)
}

has_onefile <- function(dev, args) {
    hasName(formals(dev), "onefile") && isTRUE(args[["onefile"]])
}
has_c_integer_format <- function(filename) {
    grepl("%[[:digit:]]+d", filename)
}

add_pdf_metadata <- function(output_filename, cfg=pp_cfg(), pl=list()) {
    assert_suggested("xmpdf")

    if (xmpdf::supports_set_docinfo()) {
        docinfo <- xmpdf::docinfo(title = cfg$title,
                                  creator = paste0("piecepackr v", packageDescription("piecepackr")$Version),
                                  subject = paste(cfg$description, collapse = "\n"),
                                  keywords = "piecepack")
        xmpdf::set_docinfo(docinfo, output_filename)
    } else if (!isFALSE(getOption("piecepackr.metadata.inform"))) {
        msg <- c(x = "Unable to embed pdf documentation info metadata",
                 xmpdf::enable_feature_message("set_docinfo"),
                 i = "These messages can be disabled via `options(piecepackr.metadata.inform = FALSE)`.")
        inform(msg, class = "piecepackr_embed_metadata")
    }

    if (xmpdf::supports_set_bookmarks()) {
        starting_pages <- head(c(1, 1+cumsum(sapply(pl, identity))), -1)
        bookmarks <- data.frame(title = names(pl), page = starting_pages,
                                stringsAsFactors = FALSE)
        xmpdf::set_bookmarks(bookmarks, output_filename)
    } else if (!isFALSE(getOption("piecepackr.metadata.inform"))) {
        msg <- c(x = "Unable to embed pdf bookmarks metadata",
                 xmpdf::enable_feature_message("set_bookmarks"),
                 i = "These messages can be disabled via `options(piecepackr.metadata.inform = FALSE)`.")
        inform(msg, class = "piecepackr_embed_metadata")
    }

    if (xmpdf::supports_set_xmp()) {
        x <- xmpdf::xmp(title = cfg$title,
                        creator_tool = paste0("piecepackr v", packageDescription("piecepackr")$Version),
                        description = paste(cfg$description, collapse = "\n"),
                        keywords = "piecepack", subject = "piecepack",
                        rights = cfg$copyright,
                        spdx_id = cfg$spdx_id
        )
        xmpdf::set_xmp(x, output_filename)
    } else if (!isFALSE(getOption("piecepackr.metadata.inform"))) {
        msg <- c(x = "Unable to embed pdf XMP metadata",
                 xmpdf::enable_feature_message("set_xmp"),
                 i = "These messages can be disabled via `options(piecepackr.metadata.inform = FALSE)`.")
        inform(msg, class = "piecepackr_embed_metadata")
    }
}

print_and_play_paper <- function(cfg, size, pieces, arrangement, quietly, bleed, size_bleed) {
    if (bleed)
        print_and_play_paper_bleed(cfg, size, pieces, arrangement, quietly, size_bleed)
    else
        print_and_play_paper_compact(cfg, size, pieces, arrangement, quietly, size_bleed)
}
