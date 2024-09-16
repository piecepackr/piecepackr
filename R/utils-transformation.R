has_transformations <- function() {
    getRversion() >= '4.2.0' && isTRUE(dev.capabilities()$transformations)
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
    at_settings <- affiner::affine_settings(as.data.frame(xy_vp))
    vp_define <- at_vp_define(piece_side, suit, rank, cfg)

    if (nigh(at_settings$width, 0) || nigh(at_settings$height, 0)) {
        ps_grob <- nullGrob()
    } else if (nigh(at_settings$width, vp_define$width) &&
               nigh(at_settings$height, vp_define$height) &&
               nigh(at_settings$sx, 0) &&
               !at_settings$flipX) {
        ps_grob <- cfg$get_grob(piece_side, suit, rank)
        ps_grob$vp <- at_settings$vp
    } else if (has_transformations()) { #### && !at_settings$flipX ?
        grob <- cfg$get_grob(piece_side, suit, rank)
        has_border <- hasName(grob, "border")
        if (has_border)
            grob$border <- FALSE
        if (at_settings$flipX && hasName(grob, "flip"))
            grob$flip <- TRUE
        ps_grob <- affiner::affineGrob(grob,
                                       vp_define = vp_define,
                                       vp_use = at_settings$vp,
                                       transform = at_settings$transform)
        if (has_border) {
            opt <- cfg$get_piece_opt(piece_side, suit, rank)
            gp <- gpar(col=opt$border_color, fill="transparent", lex=opt$border_lex)
            border_grob <- polygonGrob(x=xy_polygon$x, y=xy_polygon$y,
                                       default.units="in", gp=gp)
            ps_grob <- gList(ps_grob, border_grob)
        }
    } else if (nigh(at_settings$sx, 0) && !at_settings$flipX) {
        at_inform(fallback = "picture")
        ps_grob <- cfg$get_grob(piece_side, suit, rank, "picture")
        ps_grob$vp <- at_settings$vp
    } else {
        at_inform(fallback = "polygon")
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        gp <- gpar(col=opt$border_color, fill=opt$background_color, lex=opt$border_lex)
        ps_grob <- polygonGrob(x=xy_polygon$x, y=xy_polygon$y,
                               default.units="in", gp=gp)
    }
    gTree(scale = 1,
          xy_polygon = xy_polygon,
          name = name,
          children = gList(ps_grob),
          cl = "pp_ps_transformation")
}

#' @export
grobCoords.pp_ps_transformation <- function(x, closed, ...) {
    if (getRversion() >= '4.2.0' &&
        (!closed || inherits(x$children[[1]], c("null", "grob"))))
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
