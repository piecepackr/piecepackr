# Utilities to help use R 4.2's group affine transformation feature
# https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html

#' Affine transformation grob
#'
#' `transformationGrob()` is a grid grob function to facilitate
#' using the group affine transformation features introduced in R 4.2.
#' @param grob A grid grob to perform affine transformations on.
#' @param vp.define Viewport to define grid group in.
#' @param transform An affine transformation function.
#' @inheritParams pieceGrob
#' @examples
#' if (getRversion() >= '4.2.0' && require("grid")) {
#'   grob <- grobTree(circleGrob(gp=gpar(fill="yellow", col="blue")),
#'                    textGrob("RSTATS", gp=gpar(fontsize=32)))
#'
#'   vp.define <- viewport(width=unit(2, "in"), height=unit(2, "in"))
#'   transformation <- transformationGrob(grob, vp.define=vp.define)
#'
#'   # Only works if active graphics device supports affine transformations
#'   # such as `X11(type="cairo")` on R 4.2+
#'   \dontrun{
#'   # we currently don't export this function nor generate an Rd file
#'   grid.newpage()
#'   pushViewport(viewport(width=unit(3, "in"), height=unit(2, "in")))
#'   grid.draw(grob)
#'   popViewport()
#'
#'   grid.newpage()
#'   pushViewport(viewport(width=unit(3, "in"), height=unit(2, "in")))
#'   grid.draw(transformation)
#'   popViewport()
#'   }
#' }
#'
#' @noRd
transformationGrob <- function(grob,
                               vp.define = NULL,
                               transform = NULL,
                               name = NULL, gp = gpar(), vp = NULL) {
    stopifnot(getRversion() >= '4.2.0')
    if (is.null(transform))
        transform <- viewportTransform
    gTree(grob=grob, vp.define=vp.define, transform=transform,
          scale = 1,
          name = name, gp = gp, vp = vp, cl = "pp_transformation")
}

#' @export
makeContent.pp_transformation <- function(x) {
    stopifnot(isTRUE(dev.capabilities()$transformations))
    define <- defineGrob(x$grob, vp=x$vp.define)
    use <- useGrob(define$name, transform=x$transform)
    gl <- gList(define, use)
    setChildren(x, gl)
}

has_transformations <- function() {
    getRversion() >= '4.2.0' && isTRUE(dev.capabilities()$transformations)
}

# given (x,y) coordinates of four points in (affine transformed) rectangle
# get if flipped horizontally
# get viewport angle, width, height
# get x-axis shear
#
# original (before transformation) (x,y) coordinates were in
# "upper_left", "lower_left", "lower_right", "upper_right" order
at_vp_info <- function(df = data.frame(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1))) {
    df <- at_label_face_coords(df)
    x <- at_x(df)
    y <- at_y(df)
    flipped <- at_is_flipped(df)
    origin <- at_origin(df, flipped)
    df <- at_translate_to_origin(df, origin)
    angle <- at_get_angle(df, flipped)
    df <- at_rotate(df, angle)
    width <- at_width(df)
    height <- at_height(df)
    shear <- at_shear_sx(df, flipped, height, width)

    list(x = x, y = y, width = width, height = height, angle = angle,
         flipped = flipped, shear = shear)
}

# given (x,y) coordinates of four points in (affine transformed) rectangle
# label points before and after transformation by position in rectangle
at_label_face_coords <- function(df ) {
    df$before <- c("upper_left", "lower_left", "lower_right", "upper_right")
    df$after <- ""
    i_left <- order(df$x, -df$y) # leftmost points with tie-brakes by y

    # "lower-left" will be the lowest of the two left-most vertices
    if (df[i_left[1], "y"] < df[i_left[2], "y"]) {
        i_ll <- i_left[1]
    } else {
        i_ll <- i_left[2]
    }
    df[i_ll, "after"] <- "lower_left"
    df[shift_idx(i_ll, 2), "after"] <- "upper_right"
    # translate lower-left to origin and rotate so lower-left and upper-right on x-axis
    # then upper-left will be above x-axis and lower-right will be below x-axis
    df0 <- at_translate_to_origin(df, df[i_ll,])
    df0 <- at_rotate(df0, df0[shift_idx(i_ll, 2), "theta"])
    if (df0[shift_idx(i_ll, 1), "y"] > 0) {
        df[shift_idx(i_ll, 1), "after"] <- "upper_left"
        df[shift_idx(i_ll, 3), "after"] <- "lower_right"
    } else {
        df[shift_idx(i_ll, 1), "after"] <- "lower_right"
        df[shift_idx(i_ll, 3), "after"] <- "upper_left"
    }
    df
}

shift_idx <- function(x, i) {
    x2 <- (x + i) %% 4
    if (x2 == 0)
        4
    else
        x2
}

at_is_flipped <- function(df) {
    i_ul <- which(df$after == "upper_left")
    i_next <- ifelse(i_ul < 4L, i_ul + 1L, 1L)
    df[i_next, "after"] != "lower_left"
}

# translate "lower left" (after flipping but before rotation) to (0,0)
# add polar r and theta coordinates
at_translate_to_origin <- function(df, origin) {
    df$x <- df$x - origin$x
    df$y <- df$y - origin$y
    df$theta <- to_t(df$x, df$y)
    df$r <- to_r(df$x, df$y)
    df
}

at_shear_sx <- function(df, flipped, height, width) {
    if (flipped) {
        i_ll <- which(df$before == "lower_right")
        i_ul <- which(df$before == "upper_right")
    } else {
        i_ll <- which(df$before == "lower_left")
        i_ul <- which(df$before == "upper_left")
    }
    (df[i_ul, "x"] - df[i_ll, "x"]) / height
}

at_origin <- function(df, flipped = FALSE) {
    if (flipped)
        i_ll <- which(df$before == "lower_right")
    else
        i_ll <- which(df$before == "lower_left")

    df[i_ll, ]
}

at_x <- function(df) {
    mean(df$x)
}

at_y <- function(df) {
    mean(df$y)
}

at_get_angle <- function(df, flipped = FALSE) {
    if (flipped)
        i_lr <- which(df$before == "lower_left")
    else
        i_lr <- which(df$before == "lower_right")
    df[i_lr, "theta"]
}

at_rotate <- function(df, angle) {
    df$theta <- df$theta - angle
    df$x <- to_x(df$theta, df$r)
    df$y <- to_y(df$theta, df$r)
    df
}

at_width <- function(df) {
    x_ll <- df[which(df$before == "lower_left"), "x"]
    x_lr <- df[which(df$before == "lower_right"), "x"]
    abs(x_lr - x_ll)
}

at_height <- function(df) {
    y_ll <- df[which(df$before == "lower_left"), "y"]
    y_ul <- df[which(df$before == "upper_left"), "y"]
    abs(y_ul - y_ll)
}

at_viewport <- function(vp_info) {
    viewport(x = inch(vp_info$x), y = inch(vp_info$y),
             width = inch(vp_info$width), height = inch(vp_info$height),
             angle = vp_info$angle)
}

at_trans <- function(vp_info) {
    function(group, ...) viewportTransform(group, ...,
                                  shear = groupShear(sx = vp_info$shear),
                                  flip = groupFlip(flipX = vp_info$flipped))
}

at_vp_define <- function(piece_side, suit, rank, cfg) {
    viewport(width = inch(cfg$get_width(piece_side, suit, rank)),
             height = inch(cfg$get_height(piece_side, suit, rank)))
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
             i = "These messages can be disabled via `options(piecepackr.at.inform = FALSE)`.")
    inform(msg, class = "piecepackr_affine_transformation")
}

at_ps_grob <- function(piece_side, suit, rank, cfg, xy_vp, xy_polygon,
                       name="piece_side") {
    vp_info <- at_vp_info(as.data.frame(xy_vp))
    vp.define = at_vp_define(piece_side, suit, rank, cfg)

    if (nigh(vp_info$width, 0) || nigh(vp_info$height, 0)) {
        ps_grob <- nullGrob()
    } else if (nigh(vp_info$width, vp.define$width) &&
               nigh(vp_info$height, vp.define$height) &&
               nigh(vp_info$shear, 0) &&
               !vp_info$flipped) {
        ps_grob <- cfg$get_grob(piece_side, suit, rank)
        ps_grob$vp <- at_viewport(vp_info)
    } else if (has_transformations()) { #### && !vp_info$flipped ?
        grob <- cfg$get_grob(piece_side, suit, rank)
        has_border <- hasName(grob, "border")
        if (has_border)
            grob$border <- FALSE
        if (vp_info$flipped && hasName(grob, "flip"))
            grob$flip <- TRUE
        ps_grob <- transformationGrob(
                          grob,
                          vp.define = vp.define,
                          vp = at_viewport(vp_info),
                          transform = at_trans(vp_info)
                       )
        if (has_border) {
            opt <- cfg$get_piece_opt(piece_side, suit, rank)
            gp <- gpar(col=opt$border_color, fill="transparent", lex=opt$border_lex)
            border_grob <- polygonGrob(x=xy_polygon$x, y=xy_polygon$y,
                                       default.units="in", gp=gp)
            ps_grob <- gList(ps_grob, border_grob)
        }
    } else if (nigh(vp_info$shear, 0) && !vp_info$flipped) {
        at_inform(fallback = "picture")
        ps_grob <- cfg$get_grob(piece_side, suit, rank, "picture")
        ps_grob$vp <- at_viewport(vp_info)
    } else {
        at_inform(fallback = "polygon")
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        gp <- gpar(col=opt$border_color, fill=opt$background_color, lex=opt$border_lex)
        ps_grob <- polygonGrob(x=xy_polygon$x, y=xy_polygon$y,
                               default.units="in", gp=gp)
    }
    gTree(scale = 1,
          vp_info = vp_info,
          xy_polygon = xy_polygon,
          name = name,
          children = gList(ps_grob),
          cl = "pp_ps_transformation")
}

#' @export
grobCoords.pp_ps_transformation <- function(x, closed, ...) {
    if (getRversion() >= '4.2.0' && !closed)
        return(emptyGrobCoords(x$name))

    if (getRversion() >= '4.2.0' &&
        (nigh(x$vp_info$width, 0) || nigh(x$vp_info$height, 0)))
        return(emptyGrobCoords(x$name))

    grobCoords(polygonGrob(x = x$xy_polygon$x,
                           y = x$xy_polygon$y,
                           default.units = "in",
                           vp = x$vp, name=x$name),
               closed = closed, ...)
}

#' @export
makeContent.pp_ps_transformation <- function(x) {
    if (length(x$children) == 1) {
        grob <- x$children[[1]]
        if (inherits(grob, c("polygon", "grob"))) {
            grob <- update_gp(grob, gp = gpar(cex = x$scale, lex = x$scale))
        } else if(hasName(grob, "scale")) {
            grob$scale <- x$scale
        }
        x$children[[1]] <- grob
    } else { # transformation grob plus manual border
        x$children[[2]] <- update_gp(x$children[[2]],
                                     gp = gpar(cex = x$scale, lex = x$scale))
    }
    x
}
