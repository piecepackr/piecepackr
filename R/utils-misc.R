#' Miscellaneous `piecepackr` utility functions
#'
#' `cleave()` converts a delimiter separated string into a vector.
#' `inch(x)` is equivalent to `grid::unit(x, "in")`.
#' `is_color_invisible()` tells whether the color is transparent (and hence need not be drawn).
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
    vec <- str_split(s, sep)
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
    on.exit(unlink(svg_file), add = TRUE)

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
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
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
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
    unicode_devices <- c("agg_capture", "agg_jpeg", "agg_ppm", "agg_png", "agg_record", "agg_tiff", # {ragg}
                         "devSVG", "devSVG_vdiffr", # {svglite} / {vdiffr}
                         "quartz", "quartz_off_screen", # Quartz
                         "cairo_pdf", "cairo_ps", "svg", "X11cairo") # Cairo
    if (any(vapply(unicode_devices, function(x) grepl(paste0("^", x), device),
                   FUN.VALUE = logical(1L)))) {
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


# From `{gridpattern}`
update_alpha_col <- function (color, alpha = NA_real_) {
    n <- max(lengths(list(color, alpha)))
    color <- rep_len(color, n)
    alpha <- rep_len(alpha, n)
    m <- grDevices::col2rgb(color, alpha = TRUE)/255
    m[4, ] <- ifelse(is.na(alpha), m[4, ], alpha)
    apply(m, 2, function(x) grDevices::rgb(x[1], x[2], x[3], x[4]))
}

# From `{oblicubes}`
cheap_darken <- function(color, amount) {
  mat <- col2rgb(color, alpha = TRUE)
  mat[1:3, ] <- mat[1:3, ] * (1 - amount)
  rgb(mat[1, ], mat[2, ], mat[3, ], mat[4, ], maxColorValue = 255)
}

as_fill_stroke_grob <- function(grob, fill, col = "black", lwd = 1.5) {
    if (has_fill_strokes()) {
        gp <- gpar(fill = fill, col = col, lwd = lwd)
        fillStrokeGrob(grob, gp = gp)
    } else {
        fs_inform()
        grob
    }
}

has_transformations <- function() {
    getRversion() >= '4.2.0' && isTRUE(dev.capabilities()$transformations)
}

has_fill_strokes <- function() {
    getRversion() >= '4.2.0' && isTRUE(dev.capabilities()$paths)
}

has_radial_gradients <- function() {
    getRversion() >= '4.1.0' &&
        isTRUE("RadialGradient" %in% dev.capabilities("patterns")$patterns)
}

at_inform <- function(fallback = "picture") {
    if(isFALSE(getOption("piecepackr.at.inform")))
        return(invisible(NULL))

    msg <- "Affine transformation support not detected in the active graphics device."
    if (fallback == "picture")
        msg <- paste(msg, "Falling back to rendering piece side with `grImport2::pictureGrob(..., distort=TRUE)`.")
    else
        msg <- paste(msg, "Falling back to rendering piece side with a `grid::polygonGrob()`.")
    if (getRversion() < '4.2.0') {
        msg <- c(msg,
                 i = paste("Current R is version `%s`", getRversion()),
                 i = "Affine transformation support requires R version 4.2 or greater.")
    } else {
        msg <- c(msg,
                 i = "`dev.capabilities()$transformations` is not `TRUE`.",
                 i = "Perhaps try one of the cairo devices like `png(..., type='cairo')` or `cairo_pdf()`.")
    }
    msg <- c(msg,
             i = "These messages can be disabled via `options(piecepackr.at.inform = FALSE)`.",
             i = 'These messages can be suppressed via `suppressMessages(expr, classes = "piecepackr_affine_transformation")`.')
    inform(msg, class = "piecepackr_affine_transformation")
}

fs_inform <- function() {
    if(isFALSE(getOption("piecepackr.fs.inform")))
        return(invisible(NULL))

    msg <- paste("Stroking and filling path support not detected in the active graphics device.",
                 "Falling back to rendering glyph with a `grid::textGrob()`.")
    if (getRversion() < '4.2.0') {
        msg <- c(msg,
                 i = paste("Current R is version `%s`", getRversion()),
                 i = "Stroking and filling path support requires R version 4.2 or greater.")
    } else {
        msg <- c(msg,
                 i = "`dev.capabilities()$paths` is not `TRUE`.",
                 i = "Perhaps try one of the cairo devices like `png(..., type='cairo')` or `cairo_pdf()`.")
    }
    msg <- c(msg,
             i = "These messages can be disabled via `options(piecepackr.fs.inform = FALSE)`.",
             i = 'These messages can be suppressed via `suppressMessages(expr, classes = "piecepackr_fill_stroke")`.')
    inform(msg, class = "piecepackr_fill_stroke")
}

rgr_inform <- function() {
    if(isFALSE(getOption("piecepackr.rgr.inform")))
        return(invisible(NULL))

    msg <- paste("Radial gradient support not detected in the active graphics device.",
                 "Falling back to rendering without a gradient.")
    if (getRversion() < '4.1.0') {
        msg <- c(msg,
                 i = paste("Current R is version `%s`", getRversion()),
                 i = "Radial gradient support requires R version 4.1 or greater.")
    } else {
        msg <- c(msg,
                 i = '`"RadialGradient" not in `dev.capabilities()$patterns`.',
                 i = "Perhaps try one of the cairo devices like `png(..., type='cairo')` or `cairo_pdf()`.")
    }
    msg <- c(msg,
             i = "These messages can be disabled via `options(piecepackr.rgr.inform = FALSE)`.",
             i = 'These messages can be suppressed via `suppressMessages(expr, classes = "piecepackr_radial_gradient")`.')
    inform(msg, class = "piecepackr_radial_gradient")
}
