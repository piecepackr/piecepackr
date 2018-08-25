get_embedded_font_helper <- function(font, char) {
    file <- tempfile(fileext=".pdf")
    grDevices::cairo_pdf(file)
    grid::grid.text(char, gp=grid::gpar(fontsize=72, fontfamily=font))
    invisible(dev.off())

    pf_output <- system2("pdffonts", file, stdout=TRUE)
    if (length(pf_output) == 2)
        embedded_font <- NA
    else
        embedded_font <- gsub(".*\\+(.*)", "\\1", strsplit(pf_output[3], " +")[[1]][1])
    embedded_font
}

#' Get embedded font utility function
#'
#' This (vectorized) function returns which font is actually embedded by \code{cairo_pdf}.
#' 
#' @param font A character vector of font(s) passed to the \code{fontfamily} argument of \code{grid::gpar}.
#' @param char A character vector of character(s) to be embedded by \code{grid::grid.text}
#' @return A character vector of fonts that were actually embedded by \code{cairo_pdf}.  \code{NA}'s means no embedded font detected. 
#'        This either means that no font was found or that a color emoji font was found and instead of a font an image was embedded.
#' @details This functions depends on \code{pdffonts} being on the system path.
#' @export
get_embedded_font <- Vectorize(get_embedded_font_helper)
