#' Miscellaneous \code{piecepackr} utility functions
#'
#' \code{cleave} converts a delimiter separated string into a vector.
#' \code{inch(x)} is equivalent to \code{unit(x, "in")}.
#' \code{is_color_invisible} tells whether the color is transparent (and hence need not be drawn).
#' @examples
#'  cleave("0.5,0.2,0.4,0.5", float=TRUE)
#'  cleave("black,darkred,#050EAA,,", color=TRUE)
#'
#'  is_color_invisible("transparent")
#'  is_color_invisible(NA)
#'  is_color_invisible("blue")
#'  is_color_invisible("#05AE9C")
#'
#'  if (requireNamespace("grid", quietly = TRUE)) {
#'      identical(inch(1), grid::unit(1, "inch"))
#'  }
#'
#' @name pp_utils
NULL

#' @param col Color
#' @rdname pp_utils
#' @export
is_color_invisible <- function(col) {
    as.logical(grDevices::col2rgb(col, alpha=TRUE)[4, ] == 0)
}

#' @rdname pp_utils
#' @param inches Number representing number of inches
#' @export
inch <- function(inches) unit(inches, "in")

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
    if (inherits(picture, "Picture")) {
        ppPictureGrob(picture, distort)
    } else { # grDevices::is.raster(picture) # nolint
        to_rasterGrob(picture, distort)
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
        msg <- c(sprintf("You need to install the suggested package %s to use %s.",
                         sQuote(package), sQuote(calling_fn)),
                 i = sprintf("Use %s.", sQuote(sprintf('install.packages("%s")', package))))
        abort(msg, class = "piecepackr_suggested_package")
    }
}

# base R's Cairo/Quartz devices as well as {ragg} / {svglite} / {vdiffr} devices
# should support Unicode without complaint
# Notably `pdf()` is a device that does not...
# Any other devices to add?
device_supports_unicode <- function() {
    device <- names(grDevices::dev.cur())
    if (device %in% c("agg_jpeg", "agg_ppm", "agg_png", "agg_tiff", # {ragg}
                      "devSVG", # {svglite} / {vdiffr}
                      "quartz", "quartz_off_screen", # Quartz
                      "cairo_pdf", "cairo_ps", "svg", "X11cairo") # Cairo
    ) {
        TRUE
    } else if (device %in% c("bmp", "jpeg", "png", "tiff")) {
        # on unix non-"cairo" type have different device names from "cairo" type
        # but on Windows can't distinguish between `type = "windows"` or `type = "cairo"`
        # Windows device doesn't support new patterns feature
        if (getRversion() >= "4.2.0") {
            "LinearGradient" %in% grDevices::dev.capabilities()$patterns
        } else {
            .Platform$OS.type == "unix"
        }
    } else {
        FALSE
    }
}
