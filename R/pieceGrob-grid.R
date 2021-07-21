#' Draw board game pieces with grid
#'
#' \code{grid.piece} draws board game pieces onto the graphics device.
#' \code{pieceGrob} is its \code{grid} \code{grob} counterpart.
#'
#' @param piece_side A string with piece and side separated by a underscore e.g. "coin_face"
#' @param cfg Piecepack configuration list or `pp_cfg` object,
#'        a list of `pp_cfg` objects,
#'        or a character vector referring to names in `envir`
#'        or a character vector referring to object names that
#'        can be retrieved by `base::dynGet()`.
#' @param suit Number of suit (starting from 1).
#' @param rank Number of rank (starting from 1)
#' @param x Where to place piece on x axis of viewport
#' @param y Where to place piece on y axis of viewport
#' @param z z-coordinate of the piece.  Has no effect if \code{op_scale} is \code{0}.
#' @param angle Angle (on xy plane) to draw piece at
#' @param use_pictureGrob Deprecated argument.  If `TRUE` sets `type` argument to `"picture"`.
#' @param width Width of piece
#' @param height Height of piece
#' @param depth Depth (thickness) of piece.  Has no effect if \code{op_scale} is \code{0}.
#' @param op_scale How much to scale the depth of the piece in the oblique projection
#'              (viewed from the top of the board).
#'              \code{0} (the default) leads to an \dQuote{orthographic} projection,
#'              \code{0.5} is the most common scale used in the \dQuote{cabinet} projection,
#'               and \code{1.0} is the scale used in the \dQuote{cavalier} projection.
#' @param op_angle What is the angle of the oblique projection?  Has no effect if \code{op_scale} is \code{0}.
#' @param default.units  A string indicating the default units to use if
#'   'x', 'y', 'width', and/or 'height' are only given as numeric vectors.
#' @param envir Environment (or named list) containing configuration list(s).
#' @param name A character identifier (for grid)
#' @param gp An object of class \code{'gpar'}.
#' @param draw A logical value indicating whether graphics output should be produced.
#' @param vp A \code{grid} viewport object (or \code{NULL}).
#' @param ... Ignored.
#' @param scale Multiplicative scaling factor to apply to width, height, and depth.
#' @param alpha Alpha channel for transparency.
#' @param type Type of grid grob to use.  Either `"normal"` (default), `"picture"`, or `"raster"`.
#'             `"picture"` exports to (temporary) svg and re-imports as a \code{grImport2::pictureGrob}.
#'             `"raster"` exports to (temporary) png and re-imports as a \code{grid::rasterGrob}.
#'             The latter two can be useful if drawing pieces really big or small and don't want
#'             to mess with re-configuring fontsizes and linewidths.
#' @return A \code{grob} object.  If \code{draw} is \code{TRUE} then as a side effect
#'         will also draw it to the graphics device.
#' @examples
#'    if (require("grid")) {
#'        draw_pp_diagram <- function(cfg=pp_cfg(), op_scale=0) {
#'            g.p <- function(...) {
#'                grid.piece(..., op_scale=op_scale, cfg=cfg, default.units="in")
#'            }
#'            g.p("tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1))
#'            g.p("tile_back", x=0.5+3, y=0.5+1, z=1/4+1/8)
#'            g.p("tile_back", x=0.5+3, y=0.5+1, z=2/4+1/8)
#'            g.p("die_face", suit=3, rank=5, x=1, y=1, z=1/4+1/4)
#'            g.p("pawn_face", x=1, y=4, z=1/4+1/2, angle=90)
#'            g.p("coin_back", x=3, y=4, z=1/4+1/16, angle=180)
#'            g.p("coin_back", suit=4, x=3, y=4, z=1/4+1/8+1/16, angle=180)
#'            g.p("coin_back", suit=2, x=3, y=1, z=3/4+1/8, angle=90)
#'        }
#'
#'        # default piecepack, orthogonal projection
#'        draw_pp_diagram(cfg=pp_cfg())
#'
#'        # custom configuration, orthogonal projection
#'        grid.newpage()
#'        dark_colorscheme <- list(suit_color="darkred,black,darkgreen,darkblue,black",
#'                             invert_colors.suited=TRUE, border_color="black", border_lex=2)
#'        traditional_ranks <- list(use_suit_as_ace=TRUE, rank_text=",a,2,3,4,5")
#'        cfg <- c(dark_colorscheme, traditional_ranks)
#'        draw_pp_diagram(cfg=pp_cfg(cfg))
#'
#'        # custom configuration, oblique projection
#'        grid.newpage()
#'        cfg3d <- list(width.pawn=0.75, height.pawn=0.75, depth.pawn=1,
#'                           dm_text.pawn="", shape.pawn="convex6", invert_colors.pawn=TRUE,
#'                           edge_color.coin="tan", edge_color.tile="tan")
#'        cfg <- pp_cfg(c(cfg, cfg3d))
#'        draw_pp_diagram(cfg=pp_cfg(cfg), op_scale=0.5)
#'    }
#'
#' @seealso \code{\link{pmap_piece}} which applies \code{pieceGrob}
#'      over rows of a data frame.
#' @name grid.piece
NULL

pieceGrobHelper <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(),
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                           angle=0, width=NA, height=NA, depth=NA,
                           op_scale=0, op_angle=45,
                           default.units = "npc",
                           scale=1, alpha=1, type=type, name="") {
    if (scale == 0 || alpha == 0) return(nullGrob())
    cfg <- as_pp_cfg(cfg)
    rank <- impute_rank(piece_side, rank, cfg)
    suit <- impute_suit(piece_side, suit, cfg)
    if (is.na(angle)) angle <- 0
    if (is.na(width)) width <- inch(cfg$get_width(piece_side, suit, rank))
    if (is.na(height)) height <- inch(cfg$get_height(piece_side, suit, rank))
    if (is.na(depth)) depth <- inch(cfg$get_depth(piece_side, suit, rank))
    if (is.na(z)) z <- 0.5 * depth
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    if (!is.unit(z)) z <- unit(z, default.units)
    if (!is.unit(width)) width <- unit(width, default.units)
    if (!is.unit(height)) height <- unit(height, default.units)
    if (!is.unit(depth)) depth <- unit(depth, default.units)
    width <- scale * width
    height <- scale * height
    depth <- scale * depth
    angle <- angle %% 360
    op_angle <- op_angle %% 360
    if (op_scale < 0.01) {
        grob <- cfg$get_grob(piece_side, suit, rank, type)
        cvp <- viewport(x, y, width, height, angle=angle)
        name <- paste0("piece_side", name)
        grid::editGrob(grob, vp=cvp, name=name)
    } else {
        grob <- cfg$get_op_grob(piece_side, suit, rank,
                            x, y, z, angle, type,
                            width, height, depth,
                            op_scale, op_angle)
        name <- paste0("projected_piece", name)
        grid::editGrob(grob, name=name)
    }
}

#' @rdname grid.piece
#' @export
pieceGrob <- function(piece_side="tile_back", suit=NA, rank=NA,
                         cfg=getOption("piecepackr.cfg", pp_cfg()),
                         x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                         angle=0, use_pictureGrob=FALSE,
                         width=NA, height=NA, depth=NA,
                         op_scale = getOption("piecepackr.op_scale", 0),
                         op_angle = getOption("piecepackr.op_angle", 45),
                         default.units = getOption("piecepackr.default.units", "npc"),
                         envir = getOption("piecepackr.envir"),
                         name=NULL, gp=NULL, vp=NULL, ...,
                         scale=1, alpha=1, type="normal") {
    if (!missing(use_pictureGrob)) {
        .Deprecated(msg = paste("`use_pictureGrob = TRUE` is deprecated.",
                                'Use `type = "picture"` instead.'))
        type <- ifelse(use_pictureGrob, "picture", "normal")
    }
    gTree(piece_side=piece_side, suit=suit, rank=rank, cfg=cfg,
          x=x, y=y, z=z, angle=angle,
          width=width, height=height, depth=depth,
          op_scale=op_scale, op_angle=op_angle,
          default.units=default.units, envir=envir,
          scale=scale, alpha=alpha, type=type,
          name=name, gp=gp, vp=vp,
          cl="piece")
}

#' @export
makeContext.piece <- function(x) {
    scale <- x$scale
    alpha <- x$alpha
    gp <- x$gp %||% gpar()
    if (scale != 1) {
        gp$cex <- scale * (gp$cex %||% 1)
        gp$lex <- scale * (gp$lex %||% 1)
    }
    if (alpha != 1) {
        gp$alpha <- alpha * (gp$alpha %||% 1)
    }
    x$gp <- gp
    x
}

#' @export
makeContent.piece <- function(x) {
    nn <- max(lengths(list(x$piece_side, x$suit, x$rank, x$x, x$y, x$z, x$angle,
                           x$type, x$width, x$height, x$depth)))
    piece_side <- rep(x$piece_side, length.out=nn)
    suit <- rep(x$suit, length.out=nn)
    rank <- rep(x$rank, length.out=nn)
    xc <- rep(x$x, length.out=nn)
    yc <- rep(x$y, length.out=nn)
    zc <- rep(x$z, length.out=nn)
    angle <- rep(x$angle, length.out=nn)
    type <- rep(x$type, length.out=nn)
    width <- rep(x$width, length.out=nn)
    height <- rep(x$height, length.out=nn)
    depth <- rep(x$depth, length.out=nn)

    cfg <- get_cfg(x$cfg, x$envir)
    cfg <- rep(c(cfg), length.out=nn)

    gl <- gList()
    for (i in seq(nn)) {
        name <- paste0(".", i)
        gl[[i]] <- pieceGrobHelper(piece_side[i], suit[i], rank[i], cfg[[i]],
                                        xc[i], yc[i], zc[i], angle[i],
                                        width[i], height[i], depth[i],
                                        x$op_scale, x$op_angle, x$default.units,
                                        x$scale, x$alpha, type[i], name)
    }
    setChildren(x, gl)
}

#' @export
grobPoints.piece <- function(x, closed = TRUE) {
    gc <- grobCoords(x, closed = closed)
    f <- function(x, y) gridGeometry::polyclip(x, y, "union")
    Reduce(f, gc)
}

get_cfg <- function(cfg=pp_cfg(), envir=NULL) {
    if (is_pp_cfg(cfg)) {
        cfg <- cfg
    } else if (is.character(cfg)) {
        if (!is.null(envir)) {
            envir <- as.environment(envir)
            cfg <- lapply(cfg, function(cc) as_pp_cfg(envir[[cc]]))
        } else {
            cfg <- lapply(cfg, function(cc) as_pp_cfg(dynGet(cc)))
        }
        if (length(cfg) == 1) {
            cfg <- cfg[[1]]
        }
    } else {
        if (is.list(cfg)) {
            if (!all(sapply(cfg, inherits, "pp_cfg")))
                cfg <- pp_cfg(cfg)
        } else {
            stop("Don't know how to parse cfg argument") # nocov
        }
    }
    cfg
}

#' @rdname grid.piece
#' @export
grid.piece <- function(piece_side="tile_back", suit=NA, rank=NA,
                       cfg=getOption("piecepackr.cfg", pp_cfg()),
                       x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                       angle=0, use_pictureGrob=FALSE,
                       width=NA, height=NA, depth=NA,
                       op_scale = getOption("piecepackr.op_scale", 0),
                       op_angle = getOption("piecepackr.op_angle", 45),
                       default.units = getOption("piecepackr.default.units", "npc"),
                       envir = getOption("piecepackr.envir"),
                       name=NULL, gp=NULL, draw=TRUE, vp=NULL, ...,
                       scale=1, alpha=1, type="normal") {
    if (!missing(use_pictureGrob)) {
        .Deprecated(msg = paste("`use_pictureGrob = TRUE` is deprecated.",
                                'Use `type = "picture"` instead.'))
        type <- ifelse(use_pictureGrob, "picture", "normal")
    }
    grob <- pieceGrob(piece_side, suit, rank, cfg,
                          x, y, z, angle,
                          width = width, height = height, depth = depth,
                          op_scale = op_scale, op_angle = op_angle,
                          default.units = default.units,
                          envir = envir, name = name, gp = gp, vp =vp,
                          scale = scale, alpha = alpha, type = type)
    if (draw) {
        grid.draw(grob)
        invisible(grob)
    } else {
        grob
    }
}
