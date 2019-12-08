get_embedded_font_helper <- function(font, char) {
    file <- tempfile(fileext=".pdf")
    on.exit(unlink(file))
    grDevices::cairo_pdf(file)
    grid::grid.text(char, gp=grid::gpar(fontsize=72, fontfamily=font))
    invisible(grDevices::dev.off())

    pf_output <- system2("pdffonts", file, stdout=TRUE)
    if (length(pf_output) == 2)
        embedded_font <- NA
    else
        embedded_font <- gsub(".*\\+(.*)", "\\1", strsplit(pf_output[3], " +")[[1]][1])
    embedded_font
}

#' \code{piecepackr} utility functions
#'
#' \code{get_embedded_font} returns which font is actually embedded by \code{cairo_pdf}.
#' \code{cleave} converts a delimiter separated string into a vector.
#' \code{inch(x)} is equivalent to \code{unit(x, "in")}.
#' \code{to_x}, \code{to_y}, \code{to_r}, \code{to_t} convert between polar coordinates (in degrees)
#' and Cartesian coordinates.
#'
#' @examples
#'  to_x(90, 1)
#'  to_y(180, 0.5)
#'  to_t(0, -1)
#'  to_r(0.5, 0)
#'
#'  cleave("0.5,0.2,0.4,0.5", float=TRUE)
#'  cleave("black,darkred,#050EAA,,", color=TRUE)
#'
#'  if (require("grid")) {
#'      grid.rect(width=inch(1), height=inch(3), gp=gpar(fill="blue"))
#'  }
#'  if ((Sys.which("pdffonts") != "") && capabilities("cairo")) {
#'      chars <- c("a", "\u2666")
#'      fonts <- c("sans", "Sans Noto", "Noto Sans", "Noto Sans Symbols2")
#'      get_embedded_font(fonts, chars)
#'  }
#'
#' @name pp_utils
NULL

#' @rdname pp_utils
#' @param font A character vector of font(s) passed to the \code{fontfamily} argument of \code{grid::gpar}.
#' @param char A character vector of character(s) to be embedded by \code{grid::grid.text}
#' @return \code{get_embedded_font} returns character vector of fonts that were actually embedded by \code{cairo_pdf}.
#'         \code{NA}'s means no embedded font detected: this either means that no font
#'          was found or that a color emoji font was found and instead of a font an image was embedded.
#' @details \code{get_embedded_font} depends on \code{pdffonts} being on the system path
#'          (on many OSes found in a \code{poppler-utils} package).
#' @export
get_embedded_font <- function(font, char) {
    df <- expand.grid(char, font, stringsAsFactors=FALSE)
    names(df) <- c("char", "requested_font")
    df$embedded_font <- NA
    for (ii in seq(nrow(df))) {
        df[ii, 3] <- get_embedded_font_helper(df[ii,2], df[ii,1])
    }
    df
}

#' @rdname pp_utils
#' @param inches Number representing number of inches
#' @export
inch <- function(inches) unit(inches, "in")

get_n_pages_pdfinfo <- function(pdf_filename) {
    pdf_filename <- shQuote(normalizePath(pdf_filename))
    pdfinfo <- system2("pdfinfo", pdf_filename, stdout=TRUE)
    pdfinfo <- grep("^Pages:", pdfinfo, value=TRUE)
    as.numeric(strsplit(pdfinfo, " +")[[1]][2])
}
get_n_pages_gs <- function(pdf_filename) {
    pdf_filename <- normalizePath(pdf_filename, winslash="/")
    cmd <- gs()
    args <- c("-q", "-dNODISPLAY", "-c", paste(paste0('"(', pdf_filename, ")"),
              "(r)", "file", "runpdfbegin", "pdfpagecount", "=", 'quit"'))
    as.numeric(system2(cmd, args, stdout=TRUE))
}
get_n_pages <- function(pdf_filename) {
    if (Sys.which("pdfinfo") != "") {
        np <- get_n_pages_pdfinfo(pdf_filename)
    } else {
        np <- get_n_pages_gs(pdf_filename)
    }
    np
}

has_gs <- function() {
    tools::find_gs_cmd() != ""
}

gs <- function() {
    cmd <- tools::find_gs_cmd()
    if (cmd == "")
        stop("Can't find system dependency ghostscript on PATH")
    cmd
}

to_radians <- function(t) pi * t / 180
to_degrees <- function(t) 180 * t / pi

#' @rdname pp_utils
#' @param t Polar angle in degrees
#' @param r Radial distance
#' @export
to_x <- function(t, r) {
    r * cos(to_radians(t))
}

#' @rdname pp_utils
#' @export
to_y <- function(t, r) {
    r * sin(to_radians(t))
}

#' @rdname pp_utils
#' @param x Cartesian x coordinate
#' @param y Cartesian y coordinate
#' @export
to_r <- function(x, y) {
    sqrt(x^2 + y^2)
}

#' @rdname pp_utils
#' @export
to_t <- function(x, y) {
    to_degrees(atan2(y, x))
}

#' @rdname pp_utils
#' @param s String to convert
#' @param sep Delimiter (defaults to ",")
#' @param float If `TRUE` cast to numeric
#' @param color if `TRUE` convert empty strings to `"transparent"`
#' @export
cleave <- function(s, sep=",", float=FALSE, color=FALSE) {
    vec <- stringr::str_split(s, sep)
    if (length(vec))
        vec <- vec[[1]]
    if (float) {
        as.numeric(vec)
    } else if (color) {
        gsub("^$", "transparent", vec)
    } else {
        vec
    }
}
col_cleave <- function(s, sep=",") cleave(s, sep, color=TRUE)
numeric_cleave <- function(s, sep=",") cleave(s, sep, float=TRUE)

as_picture <- function(grob, width, height) {
    svg_file <- tempfile(fileext=".svg")
    on.exit(unlink(svg_file))

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    grDevices::svg(svg_file, width=width, height=height, bg="transparent")
    grid.draw(grob)
    invisible(grDevices::dev.off())
    file2grob(svg_file)
}

#' @rdname pp_utils
#' @param file Filename of image
#' @param distort Logical value of whether one should preserve the aspect ratio
#'                or distort to fit the area it is drawn in
#' @export
file2grob <- function(file, distort=TRUE) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    format <- tools::file_ext(file)
    if (format %in% c("svgz", "svg")) {
        return(to_pictureGrob(grImport2::readPicture(file, warn=FALSE), distort))
    } else if (format == "png") {
        return(to_rasterGrob(grDevices::as.raster(png::readPNG(file)), distort))
    } else if (format %in% c("jpg", "jpeg")) {
        return(to_rasterGrob(grDevices::as.raster(jpeg::readJPEG(file)), distort))
    } else {
        return(to_rasterGrob(magick::image_read(file), distort))
    }
}

to_rasterGrob <- function(obj, distort=TRUE) {
    if (distort) {
        rasterGrob(grDevices::as.raster(obj), height=unit(1, "npc"), width=unit(1, "npc"))
    } else {
        rasterGrob(grDevices::as.raster(obj))
    }
}
to_pictureGrob <- function(obj, distort=TRUE) grImport2::pictureGrob(obj, expansion=0, clip="off", distort=distort)
