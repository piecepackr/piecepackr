get_embedded_font_helper <- function(font, char) {
    file <- tempfile(fileext=".pdf")
    on.exit(unlink(file))
    grDevices::cairo_pdf(file)
    grid::grid.text(char, gp=grid::gpar(fontsize=72, fontfamily=font))
    invisible(grDevices::dev.off())

    pf_output <- system2("pdffonts", file, stdout=TRUE)
    if (length(pf_output) == 2)
        embedded_font <- NA # probably some color emoji font used
    else
        embedded_font <- gsub(".*\\+(.*)", "\\1", strsplit(pf_output[3], " +")[[1]][1])
    embedded_font
}

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
#' @details `get_embedded_font()` depends on `pdffonts` being on the system path
#'          (on many OSes found in a `poppler-utils` package).
#' @examples
#'  if ((Sys.which("pdffonts") != "") && capabilities("cairo")) {
#'      chars <- c("a", "\u2666")
#'      fonts <- c("sans", "Sans Noto", "Noto Sans", "Noto Sans Symbols2")
#'      get_embedded_font(fonts, chars)
#'
#'      has_font("Dejavu Sans")
#'  }
#' @export
get_embedded_font <- function(font, char) {
    if (Sys.which("pdffonts") == "") {
        abort("'get_embedded_font()' depends on 'pdffonts' being on the system path. ",
              " On many OSes it is found in a 'poppler-utils' package.")
    }
    if (!capabilities("cairo")) {
        abort("'get_embedded_font()' requires that R has been compiled with 'cairo' support. ")
    }
    df <- expand.grid(char, font, stringsAsFactors=FALSE)
    names(df) <- c("char", "requested_font")
    df$embedded_font <- NA
    for (ii in seq(nrow(df))) {
        df[ii, 3] <- get_embedded_font_helper(df[ii,2], df[ii,1])
    }
    df
}

#' @rdname font_utils
#' @export
has_font <- function(font) {
    stopifnot(length(font) == 1)
    if (requireNamespace("systemfonts", quietly = TRUE)) {
        font_file <- basename(systemfonts::match_font(family = font)$path)
        grepl(simplify_font(font), simplify_font(font_file))
    } else if (Sys.which("pdffonts") != "" && capabilities("cairo")) {
        embedded_font <- get_embedded_font(font, "A")$embedded_font
        grepl(simplify_font(font), simplify_font(embedded_font))
    } else {
        warn(paste("has_font() needs either the suggested 'systemfonts' package installed",
                   "or R compiled with 'cairo' support plus the system tool 'pdffonts' installed.",
                   "Conservatively returning `FALSE`."))
        FALSE
    }
}

simplify_font <- function(font) {
    tolower(gsub(" ", "", font))
}
