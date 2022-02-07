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
#' @param size PnP output size (currently either "letter", "A4", "A5", or "4x6")
#' @param pieces Character vector of desired PnP pieces.
#'        Supports "piecepack", "matchsticks", "pyramids", "subpack", or "all".
#' @param arrangement Either "single-sided" or "double-sided".
#'                    Ignored if `size = "4x6"`.
#' @param quietly Whether to hide messages about missing metadata
#'                in the provided configuration.
#' @inheritParams render_piece
#' @examples
#'   \donttest{
#'     is_mac <- tolower(Sys.info()[["sysname"]]) == "darwin"
#'     if (capabilities("cairo") && !is_mac) {
#'         cfg <- pp_cfg(list(invert_colors.suited=TRUE))
#'         cfg$description <- 'Piecepack with an "inverted" color scheme.'
#'         cfg$title <- '"Inverted" piecepack'
#'         cfg$copyright <- "\u00a9 2022 Trevor L Davis.  Some Right Reserved."
#'         cfg$spdx_id <- "CC-BY-4.0"
#'         cfg$credit <- ""
#'         save_print_and_play(cfg, "my_pnp_file.pdf")
#'         save_print_and_play(cfg, "my_pnp_file_ds.pdf", arrangement="double-sided")
#'         save_print_and_play(cfg, "my_pnp_file_A4.pdf", size="A4", pieces="all")
#'         save_print_and_play(cfg, "my_pnp_file_A5.pdf", size="A5")
#'         unlink("my_pnp_file.pdf")
#'         unlink("my_pnp_file_ds.pdf")
#'         unlink("my_pnp_file_A4.pdf")
#'         unlink("my_pnp_file_A5.pdf")
#'     }
#'   }
#' @export
save_print_and_play <- function(cfg = getOption("piecepackr.cfg", pp_cfg()),
                                output_filename="piecepack.pdf",
                                size=c("letter", "A4", "A5", "4x6"),
                                pieces=c("piecepack", "matchsticks", "pyramids"),
                                arrangement=c("single-sided", "double-sided"),
                                dev = NULL,
                                dev.args = list(family = cfg$fontfamily,
                                                onefile = TRUE,
                                                units = "in",
                                                bg = "white",
                                                res = 72),
                                quietly = FALSE) {

    stopifnot(is.null(dev) || is.function(dev))
    size <- match.arg(size)
    arrangement <- match.arg(arrangement)
    if ("all" %in% pieces)
        pieces <- c("piecepack", "pyramids", "matchsticks", "subpack")

    cfg <- as_pp_cfg(cfg)
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    height <- switch(size,
                    letter = LETTER_WIDTH,
                    A4 = A4_WIDTH,
                    A5 = A4_WIDTH,
                    `4x6` = 4)
    width <- switch(size,
                    letter = LETTER_HEIGHT,
                    A4 = A4_HEIGHT,
                    A5 = A4_HEIGHT / 2,
                    `4x6` = 6)

    if (is.null(dev))
        dev <- pp_device_fn(output_filename)

    args <- list(filename = output_filename, width = width, height = height)
    args <- c(args, dev.args)
    args <- args[names(args) %in% names(formals(dev))]
    onefile <- has_onefile(dev, dev.args)
    if (!onefile && !has_c_integer_format(output_filename))
        abort(paste("`save_print_and_play()` generates multiple pages.",
                    'Either `dev` needs to support `onefile` and `isTRUE(dev.args[["onefile"]])`',
                    "or else `output_filename` needs a C integer format in the string."))
    do.call(dev, args)

    pl <- switch(size,
                 `4x6` = print_and_play_4x6(cfg, pieces, quietly),
                 print_and_play_paper(cfg, size, pieces, arrangement, quietly))

    invisible(grDevices::dev.off())

    if (onefile && tools::file_ext(output_filename) == "pdf" && has_gs()) {
        add_pdf_metadata(output_filename, cfg, pl)
    }
    invisible(NULL)
}

has_onefile <- function(dev, dev.args) {
    hasName(formals(dev), "onefile") && isTRUE(dev.args[["onefile"]])
}
has_c_integer_format <- function(filename) {
    grepl("%[[:digit:]]+d", filename)
}

add_pdf_metadata <- function(output_filename, cfg=pp_cfg(), pl=list()) {
    temp_pdf <- tempfile(fileext=".pdf")
    on.exit(unlink(temp_pdf))
    temp_txt <- tempfile(fileext=".txt")
    on.exit(unlink(temp_txt))
    file.copy(output_filename, temp_pdf)

    starting_pages <- c(1, 1+cumsum(sapply(pl, identity)))
    ns <- names(pl)
    txt <- character(0)
    for (ii in seq(pl)) {
        line <- sprintf("[/Page %s /View [/XYZ null null null] /Title (%s) /OUT pdfmark",
                   starting_pages[ii], ns[ii])
        txt <- append(txt, line)
    }
    title <- sprintf(" /Title (%s)\n", cfg$title)
    creator <- sprintf(" /Creator (piecepackr v%s)\n", packageDescription("piecepackr")$Version)
    subject <- sprintf(" /Subject (%s)\n", cfg$description)
    keywords <- " /Keywords (piecepack)\n"
    line <- sprintf("[%s%s%s%s /DOCINFO pdfmark",
                    ifelse(length(title), title, ""), creator,
                    ifelse(length(subject), subject, ""), keywords)
    txt <- append(txt, line)
    writeLines(txt, temp_txt)

    args <- c("-q", "-o", shQuote(output_filename), "-sDEVICE=pdfwrite", shQuote(temp_txt), shQuote(temp_pdf))
    system2(gs(), args)
}
