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

#' \code{piecepackr} utility functions
#'
#' \code{get_embedded_font} returns which font is actually embedded by \code{cairo_pdf}.
#' \code{cleave} converts a delimiter separated string into a vector.
#' \code{inch(x)} is equivalent to \code{unit(x, "in")}.
#' \code{is_color_invisible} tells whether the color is transparent (and hence need not be drawn).
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
#'  is_color_invisible("transparent")
#'  is_color_invisible(NA)
#'  is_color_invisible("blue")
#'  is_color_invisible("#05AE9C")
#'
#' @name pp_utils
NULL

#' @param col Color
#' @rdname pp_utils
#' @export
is_color_invisible <- function(col) {
    if (is.na(col))
        return(TRUE)
    if (grDevices::col2rgb(col, alpha=TRUE)[4] == 0)
        return(TRUE)
    return(FALSE)
}

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
    if (Sys.which("pdffonts") == "") {
        stop("'get_embedded_font' depends on 'pdffonts' being on the system path. ",
             " On many OSes it is found in a 'poppler-utils' package.")
    }
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

cleave2 <- function(s, sep=",", ...) {
    if (length(s) > 1)
        s <- paste(s, collapse=sep)
    cleave(s, sep, ...)
}
col_cleave <- function(s, sep=",") cleave2(s, sep, color=TRUE)
numeric_cleave <- function(s, sep=",") cleave2(s, sep, float=TRUE)

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
        picture <- grImport2::readPicture(file, warn=FALSE)
    } else if (format == "png") {
        picture <- grDevices::as.raster(png::readPNG(file))
    } else if (format %in% c("jpg", "jpeg")) {
        picture <- grDevices::as.raster(jpeg::readJPEG(file))
    } else {
        assert_suggested("magick")
        picture <- magick::image_read(file)
    }
    if (grDevices::is.raster(picture)) {
        to_rasterGrob(picture, distort)
    } else {
        ppPictureGrob(picture, distort)
    }
}

to_rasterGrob <- function(obj, distort=TRUE) {
    if (distort) {
        rasterGrob(grDevices::as.raster(obj), height=unit(1, "npc"), width=unit(1, "npc"), name = "raster")
    } else {
        rasterGrob(grDevices::as.raster(obj), name = "raster")
    }
}

# adds support for a 'vp' viewport plus hiding grob details in `grid.ls()` (i.e. ``grid.revert()`` if "forced")
ppPictureGrob <- function(picture, distort = TRUE, ..., name = NULL, gp = gpar(), vp = NULL) {
    gTree(picture = picture, distort = distort, name = name, gp = gp, vp = vp, cl = "pp_picture")
}

#' @export
makeContent.pp_picture <- function(x) {
    grob <- grImport2::pictureGrob(x$picture, expansion=0, clip="off", distort=x$distort)
    setChildren(x, gList(grob))
}

assert_suggested <- function(package) {
    calling_fn <- deparse(sys.calls()[[sys.nframe()-1]])
    if (!requireNamespace(package, quietly = TRUE)) {
        stop(paste("You need to install the suggested package", sQuote(package),
                   sprintf("to use %s.", sQuote(calling_fn)),
                   sprintf("Use %s.", sQuote(sprintf('install.packages("%s")', package)))))
    }
}
