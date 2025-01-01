#' Draw board game pieces with grid
#'
#' `grid.piece()` draws board game pieces onto the graphics device.
#' `pieceGrob()` is its `grid` \dQuote{grob} counterpart.
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
#' @param gp An object of class \dQuote{gpar}.
#' @param draw A logical value indicating whether graphics output should be produced.
#' @param vp A `grid` viewport object (or `NULL`).
#' @param ... Ignored.
#' @param scale Multiplicative scaling factor to apply to width, height, and depth.
#' @param alpha Alpha channel for transparency.
#' @param type Type of grid grob to use.  Either `"normal"` (default), `"picture"`, `"raster"`, or `"transformation"`.
#'             `"picture"` exports to (temporary) svg and re-imports as a \code{grImport2::pictureGrob}.
#'             `"raster"` exports to (temporary) png and re-imports as a \code{grid::rasterGrob}.
#'             `"transformation"` uses the affine transformation feature only supported in
#'             R 4.2+ within select graphic devices.
#'             The latter three can be useful if drawing pieces really big or small and don't want
#'             to mess with re-configuring fontsizes and linewidths.
#' @param bleed If `FALSE` do not add a \dQuote{bleed} zone around the piece,
#'              otherwise add a \dQuote{bleed} zone around the piece:\itemize{
#'                 \item{If `bleed` is `TRUE` we will add 1/8 inch bleeds}
#'                 \item{If `bleed` is a [grid::unit()] we will use it as bleed size}
#'                 \item{If `bleed` is numeric we will convert to [grid::unit()] via `grid::unit(bleed, default.units)`}
#'              }
#'              A non-`FALSE` `bleed` is incompatible with `op_scale > 0` (drawing in an \dQuote{oblique projection}).
#' @return A `grid` grob object.  If `draw` is `TRUE` then as a side effect
#'         `grid.piece()` will also draw it to the graphics device.
#' @examples
#'   if (requireNamespace("grid", quietly = TRUE) && piecepackr:::device_supports_unicode()) {
#'     opt <- options(piecepackr.at.inform = FALSE)
#'     on.exit(options(opt))
#'
#'     draw_pp_diagram <- function(cfg=pp_cfg(), op_scale=0) {
#'         g.p <- function(...) {
#'             grid.piece(..., op_scale=op_scale, cfg=cfg, default.units="in")
#'         }
#'         g.p("tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1))
#'         g.p("tile_back", x=0.5+3, y=0.5+1, z=1/4+1/8)
#'         g.p("tile_back", x=0.5+3, y=0.5+1, z=2/4+1/8)
#'         g.p("die_face", suit=3, rank=5, x=1, y=1, z=1/4+1/4)
#'         g.p("pawn_face", x=1, y=4, z=1/4+1/2, angle=90)
#'         g.p("coin_back", x=3, y=4, z=1/4+1/16, angle=180)
#'         g.p("coin_back", suit=4, x=3, y=4, z=1/4+1/8+1/16, angle=180)
#'         g.p("coin_back", suit=2, x=3, y=1, z=3/4+1/8, angle=90)
#'     }
#'
#'     # default piecepack, orthogonal projection
#'     draw_pp_diagram(cfg=pp_cfg())
#'   }
#'   if (requireNamespace("grid", quietly = TRUE) && piecepackr:::device_supports_unicode()) {
#'     # custom configuration, orthogonal projection
#'     grid::grid.newpage()
#'     dark_colorscheme <- list(suit_color="darkred,black,darkgreen,darkblue,black",
#'                          invert_colors.suited=TRUE, border_color="black", border_lex=2)
#'     traditional_ranks <- list(use_suit_as_ace=TRUE, rank_text=",a,2,3,4,5")
#'     cfg <- c(dark_colorscheme, traditional_ranks)
#'     draw_pp_diagram(cfg=pp_cfg(cfg))
#'   }
#'   if (requireNamespace("grid", quietly = TRUE) && piecepackr:::device_supports_unicode()) {
#'     # custom configuration, oblique projection
#'     grid::grid.newpage()
#'     cfg3d <- list(width.pawn=0.75, height.pawn=0.75, depth.pawn=1,
#'                        dm_text.pawn="", shape.pawn="convex6", invert_colors.pawn=TRUE,
#'                        edge_color.coin="tan", edge_color.tile="tan")
#'     cfg <- pp_cfg(c(cfg, cfg3d))
#'     draw_pp_diagram(cfg=pp_cfg(cfg), op_scale=0.5)
#'   }
#' @seealso [pmap_piece()] which applies `pieceGrob()` over rows of a data frame.
#' @name grid.piece
NULL

pieceGrobHelper <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(),
                            x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                            angle=0, width=NA, height=NA, depth=NA,
                            op_scale=0, op_angle=45,
                            default.units = "npc",
                            scale=1, alpha=1, type="normal", name="",
                            bleed=FALSE) {
    stopifnot(isFALSE(bleed) || op_scale < 0.0001)
    if (scale == 0 || alpha == 0) return(nullGrob())
    cfg <- as_pp_cfg(cfg)
    rank <- impute_rank(piece_side, rank, cfg)
    suit <- impute_suit(piece_side, suit, cfg)
    if (is.na(angle)) angle <- 0

    has_bleed <- !isFALSE(bleed)

    if (isTRUE(bleed)) bleed <- unit(0.125, "in")
    if (isFALSE(bleed)) bleed <- unit(0, "in")
    if (!is.unit(bleed)) bleed <- unit(bleed, default.units)

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
    width <- scale * width + 2 * bleed
    height <- scale * height + 2 * bleed
    depth <- scale * depth
    angle <- angle %% 360
    op_angle <- op_angle %% 360

    if (op_scale < 0.0001) {
        if (has_bleed) {
            grob <- cfg$get_grob_with_bleed(piece_side, suit, rank)
            if (hasName(grob, "bleed"))
                grob$bleed <- bleed
        } else {
            grob <- cfg$get_grob(piece_side, suit, rank, type)
        }
        cvp <- viewport(x, y, width, height, angle=angle)
        name <- paste0("piece_side", name)
        grob <- grid::editGrob(grob, vp=cvp)
    } else {
        grob <- cfg$get_op_grob(piece_side, suit, rank,
                            x, y, z, angle, type,
                            width, height, depth,
                            op_scale, op_angle)
        name <- paste0("projected_piece", name)
    }

    # update name, cex, lex, alpha
    grob <- grid::editGrob(grob, name=name)
    if (hasName(grob, "scale"))
        grob$scale <- scale
    else
        grob <- update_gp(grob, gp = gpar(cex = scale, lex = scale))
    if (!nigh(alpha, 1))
        grob <- update_gp(grob, gp = gpar(alpha = alpha))
    grob
}

#' @rdname grid.piece
#' @export
pieceGrob <- function(piece_side="tile_back", suit=NA, rank=NA,
                      cfg=getOption("piecepackr.cfg", pp_cfg()),
                      x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                      angle=0, ...,
                      width=NA, height=NA, depth=NA,
                      op_scale = getOption("piecepackr.op_scale", 0),
                      op_angle = getOption("piecepackr.op_angle", 45),
                      default.units = getOption("piecepackr.default.units", "npc"),
                      envir = getOption("piecepackr.envir"),
                      name=NULL, gp=NULL, vp=NULL,
                      scale=1, alpha=1, type="normal",
                      bleed=FALSE) {

    stopifnot(!(bleed && op_scale > 0))
    if (is_angle(angle))
        angle <- as.double(angle, "degrees")
    if (is_angle(op_angle))
        op_angle <- as.double(op_angle, "degrees")

    gTree(piece_side=piece_side, suit=suit, rank=rank, cfg=cfg,
          x=x, y=y, z=z, angle=angle,
          width=width, height=height, depth=depth,
          op_scale=op_scale, op_angle=op_angle,
          default.units=default.units, envir=envir,
          scale=scale, alpha=alpha, type=type,
          bleed=bleed,
          name=name, gp=gp, vp=vp,
          cl=c("piece", "pp_grobCoords"))
}

#' @export
makeContent.piece <- function(x) {
    nn <- max(lengths(list(x$piece_side, x$suit, x$rank, x$x, x$y, x$z, x$angle,
                           x$width, x$height, x$depth,
                           x$op_scale, x$op_angle,
                           x$scale, x$alpha, x$type,
                           x$bleed)))
    piece_side <- rep(x$piece_side, length.out=nn)
    suit <- rep(x$suit, length.out=nn)
    rank <- rep(x$rank, length.out=nn)
    xc <- rep(x$x, length.out=nn)
    yc <- rep(x$y, length.out=nn)
    zc <- rep(x$z, length.out=nn)
    angle <- rep(x$angle, length.out=nn)

    width <- rep(x$width, length.out=nn)
    height <- rep(x$height, length.out=nn)
    depth <- rep(x$depth, length.out=nn)

    op_scale <- rep(x$op_scale, length.out=nn)
    op_angle <- rep(x$op_angle, length.out=nn)

    scale <- rep(x$scale, length.out=nn)
    alpha <- rep(x$alpha, length.out=nn)
    type <- rep(x$type, length.out=nn)

    bleed <- rep(x$bleed, length.out=nn)

    cfg <- get_cfg(x$cfg, x$envir)
    cfg <- rep(c(cfg), length.out=nn)

    gl <- gList()
    for (i in seq(nn)) {
        name <- paste0(".", i)
        gl[[i]] <- pieceGrobHelper(piece_side[i], suit[i], rank[i], cfg[[i]],
                                   xc[i], yc[i], zc[i], angle[i],
                                   width[i], height[i], depth[i],
                                   op_scale[i], op_angle[i], x$default.units,
                                   scale[i], alpha[i], type[i],
                                   name,
                                   bleed[i])
    }
    setChildren(x, gl)
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
            abort("Don't know how to parse cfg argument") # nocov
        }
    }
    cfg
}

#' @rdname grid.piece
#' @export
grid.piece <- function(piece_side="tile_back", suit=NA, rank=NA,
                       cfg=getOption("piecepackr.cfg", pp_cfg()),
                       x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                       angle=0, ...,
                       width=NA, height=NA, depth=NA,
                       op_scale = getOption("piecepackr.op_scale", 0),
                       op_angle = getOption("piecepackr.op_angle", 45),
                       default.units = getOption("piecepackr.default.units", "npc"),
                       envir = getOption("piecepackr.envir"),
                       name=NULL, gp=NULL, draw=TRUE, vp=NULL,
                       scale=1, alpha=1, type="normal",
                       bleed=FALSE) {
    grob <- pieceGrob(piece_side, suit, rank, cfg,
                      x, y, z, angle,
                      width = width, height = height, depth = depth,
                      op_scale = op_scale, op_angle = op_angle,
                      default.units = default.units,
                      envir = envir, name = name, gp = gp, vp =vp,
                      scale = scale, alpha = alpha, type = type,
                      bleed = bleed)
    if (draw) {
        grid.draw(grob)
        invisible(grob)
    } else {
        grob
    }
}
