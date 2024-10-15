#' Font utility functions
#'
#' `get_embedded_font()` returns which font is actually embedded
#' by `cairo_pdf()` for a given character.
#' `has_font()` tries to determine if a given font is available on the OS.
#' @name font_utils
#' @rdname font_utils
#' @param font A character vector of font(s).
#' @param char A character vector of character(s) to be embedded by `grid::grid.text()`
#' @return `get_embedded_font()` returns character vector of fonts that were actually embedded by `cairo_pdf()`.
#'         \code{NA}'s means no embedded font detected: this either means that no font
#'          was found or that a color emoji font was found and instead of a font an image was embedded.
#' @details `get_embedded_font()` depends on the suggested `pdftools` package being installed
#'          and R being compiled with Cairo support.
#'          `has_font()` depends on either the suggested `systemfonts` (preferred) or `pdftools`
#'          packages being installed.
#' @examples
#'  if (requireNamespace("pdftools", quietly = TRUE) &&
#'      capabilities("cairo") &&
#'      !piecepackr:::is_cairo_maybe_buggy()) {
#'    chars <- c("a", "\u2666")
#'    fonts <- c("sans", "Sans Noto", "Noto Sans", "Noto Sans Symbols2")
#'    try(get_embedded_font(fonts, chars))
#'  }
#'
#'  if (requireNamespace("systemfonts", quietly = TRUE) ||
#'      (requireNamespace("pdftools", quietly = TRUE) &&
#'       capabilities("cairo")) && !piecepackr:::is_cairo_maybe_buggy()) {
#'    try(has_font("Dejavu Sans"))
#'  }
#' @export
get_embedded_font <- function(font, char) {
    if (!isTRUE(capabilities("cairo"))) {
        abort("`get_embedded_font()` requires that R has been compiled with `cairo` support.")
    }
    if (!requireNamespace("pdftools", quietly = TRUE)) {
       assert_suggested("pdftools")
    }
    if (!isFALSE(getOption("piecepackr.check.cairo")) && is_cairo_maybe_buggy()) {
        warn(c(sprintf("Your cairographics (version %s) may embed malformed font names.",
                       grDevices::grSoftVersion()[["cairo"]]),
               i = "cairographics 1.17.8 is known to embed malformed font names.",
               i = "cairographics's issue tracker suggests 1.17.4 and 1.17.6 may also do this.",
               i = "See https://github.com/piecepackr/piecepackr/issues/334 for more info.",
               i = "These warnings can be disabled via `options(piecepackr.check.cairo = FALSE)`."),
             class = "piecepackr_buggy_cairo")
    }
    df <- expand.grid(char, font, stringsAsFactors=FALSE)
    names(df) <- c("char", "requested_font")
    df$embedded_font <- NA
    for (ii in seq_len(nrow(df))) {
        df[ii, 3] <- get_embedded_font_helper(df[ii, 2], df[ii, 1])
    }
    df
}

get_embedded_font_helper <- function(font, char) {
    stopifnot(requireNamespace("pdftools", quietly = TRUE))

    file <- tempfile(fileext=".pdf")
    on.exit(unlink(file))
    grDevices::cairo_pdf(file)
    grid::grid.text(char, gp=grid::gpar(fontsize=72, fontfamily=font))
    invisible(grDevices::dev.off())

    df <- pdftools::pdf_fonts(file)
    if (nrow(df) == 0L)
        embedded_font <- NA # probably some color emoji font used
    else
        embedded_font <- gsub("^[^+]*\\+(.*)", "\\1", df$name)
    embedded_font
}

#' @rdname font_utils
#' @export
has_font <- function(font) {
    stopifnot(length(font) == 1)
    if (requireNamespace("systemfonts", quietly = TRUE)) {
        if (packageVersion("systemfonts") >= '1.1.0')
            font_file <- basename(systemfonts::match_fonts(family = font)$path)
        else
            font_file <- basename(systemfonts::match_font(family = font)$path)
        grepl(simplify_font(font), simplify_font(font_file))
    } else if (requireNamespace("pdftools", quietly = TRUE) &&
               isTRUE(capabilities("cairo"))) {
        embedded_font <- get_embedded_font(font, "A")$embedded_font
        grepl(simplify_font(font), simplify_font(embedded_font))
    } else {
        warn(paste("`has_font()` needs either the suggested 'systemfonts' package installed",
                   "or R compiled with 'cairo' support plus the suggested 'pdftools' package installed.",
                   "Conservatively returning `FALSE`."))
        FALSE
    }
}

simplify_font <- function(font) {
    tolower(gsub(" ", "", font))
}


# `cairo_pdf()` embedded fontnames with are definitely buggy in cairo 1.17.8
# they may also be buggy with cairo 1.17.4 and 1.17.6:
# https://gitlab.freedesktop.org/cairo/cairo/-/issues/449
is_cairo_maybe_buggy <- function() {
    cairo_version <- grDevices::grSoftVersion()[["cairo"]]
    stopifnot("cairographics is not available" = cairo_version != "")
    cairo_version <- numeric_version(cairo_version)
    if (cairo_version <= numeric_version("1.17.8") &&
        cairo_version >= numeric_version("1.17.4")) {
        TRUE
    } else {
        FALSE
    }
}
