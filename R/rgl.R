plot_raster_face <- function(filename, x = 0, y = 0, z = 0,
                             angle = 0, axis_x = 0, axis_y = 0,
                             width = 1, height = 1, flip = FALSE) {
    if (flip) {
        p <- Point$new(c(1, 0, 0, 1), c(0, 0, 1, 1))
    } else {
        p <- Point$new(c(0, 1, 1, 0), c(0, 0, 1, 1))
    }
    p <- p$npc_to_in(x, y, width, height, angle)
    texcoords <- matrix(c(0,1,1,0,0,0,1,1), ncol=2)
    rgl::rgl.quads(p$x, y=p$y,z=z, texcoords=texcoords, texture=filename, textype="rgba",
                   back = "culled",
                   lit = FALSE,
                   # fog = FALSE,
                   # texmipmap = TRUE,
                   )
}

to_alpha <- function(color) grDevices::col2rgb(color, alpha=TRUE)[4, ] / 255

rgl.two_sided_token <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(),
                                x=0, y=0, z=0, angle=0, axis_x=0, axis_y=0,
                                width=NA, height=NA, depth=NA, res=72) {
    if (is.na(width)) width <- cfg$get_width(piece_side, suit, rank)
    if (is.na(height)) height <- cfg$get_height(piece_side, suit, rank)
    if (is.na(depth)) depth <- cfg$get_depth(piece_side, suit, rank)

    pc <- Point3D$new(x, y, z)

    # top
    pu <- pc$translate(z = depth/2)
    raster_top <- cfg$get_raster(piece_side, suit, rank, value = "filename", res=res)
    plot_raster_face(raster_top, pu$x, pu$y, pu$z, angle, axis_x, axis_y, width, height)

    # bottom
    pl <- pc$translate(z = -depth/2)
    bottom <- if (grepl("face", piece_side)) {
        gsub("face", "back", piece_side)
    } else {
        gsub("back", "face", piece_side)
    }
    raster_bot <- cfg$get_raster(bottom, suit, rank, value = "filename", res=res)
    plot_raster_face(raster_bot, pl$x, pl$y, pl$z, angle, axis_x, axis_y, width, height, flip=TRUE)

    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    xy_npc <- Point$new(get_shape_xy(opt$shape, opt$shape_t, opt$shape_r))
    xy <- xy_npc$npc_to_in(x, y, width, height, angle)
    cp <- Polygon$new(xy)
    vertices <- cp$edges$face_matrix(z = z, depth = depth)
    rgl::rgl.quads(vertices, color = opt$edge_color, alpha = get_alpha(opt$edge_color))
}

get_alpha <- function(color) grDevices::col2rgb(color, alpha = TRUE)[4, 1] / 255

#### res as an argument?
#### return list of quad numbers so easier to delete?

#' Draw board game pieces using rgl
#'
#' \code{rgl.piece} draws board games pieces using the rgl package.
#' @inheritParams grid.piece
#' @param axis_x Ignored for now.
#' @param axis_y Ignored for now.
#' @param res Resolution of the faces.
#' @examples
#'   \donttest{
#'     if (require("rgl")) {
#'         rgl.open()
#'         cfg <- pp_cfg()
#'         rgl.piece("tile_face", suit=3, rank=3, cfg=cfg, x=0, y=0, z=0)
#'         rgl.piece("coin_back", suit=4, rank=2, cfg=cfg, x=2, y=0, z=0)
#'         rgl.piece("saucer_back", suit=1, cfg=cfg, x=2, y=2, z=-2)
#'         rgl.piece("pawn_face", suit=2, cfg=cfg, x=1, y=1, z=2)
#'     }
#'   }
#' @export
rgl.piece <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(),
                           x = 0, y = 0, z = NA,
                           angle = 0, axis_x = 0, axis_y = 0,
                           width = NA, height = NA, depth = NA,
                           envir = NULL, ..., res = 72) {
    if (!requireNamespace("rgl")) {
        stop("You must install the suggested package rgl to use 'rgl.piece'.  Try ",
             'install.packages("rgl")')
    }
    nn <- max(lengths(list(piece_side, suit, rank, x, y, z, angle, axis_x, axis_y, width, height, depth)))
    piece_side <- rep(piece_side, length.out=nn)
    suit <- rep(suit, length.out=nn)
    rank <- rep(rank, length.out=nn)
    x <- rep(x, length.out=nn)
    y <- rep(y, length.out=nn)
    z <- rep(z, length.out=nn)
    angle <- rep(angle, length.out=nn)
    axis_x <- rep(axis_x, length.out=nn)
    axis_y <- rep(axis_y, length.out=nn)
    width <- rep(width, length.out=nn)
    height <- rep(height, length.out=nn)
    depth <- rep(depth, length.out=nn)

    cfg <- get_cfg(cfg, envir)
    cfg <- rep(c(cfg), length.out=nn)
    for (i in seq(nn)) {
        rgl.piece_helper(piece_side[i], suit[i], rank[i], cfg[[i]],
                         x[i], y[i], z[i],
                         angle[i], axis_x[i], axis_y[i],
                         width[i], height[i], depth[i], res)
    }
    invisible(NULL)
}

rgl.piece_helper <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(),
                           x = 0, y = 0, z = NA,
                           angle = 0, axis_x = 0, axis_y = 0,
                           width = NA, height = NA, depth = NA, res = 72) {
    cfg <- as_pp_cfg(cfg)
    # nolint start
    # suit <- ifelse(has_suit(piece_side), ifelse(is.na(suit), 1, suit), cfg$i_unsuit)
    # suit <- ifelse(suit > cfg$i_unsuit+1, cfg$i_unsuit+1, suit)
    # rank <- ifelse(has_rank(piece_side), ifelse(is.na(rank), 1, rank), 0)
    # nolint end
    if (is.na(angle)) angle <- 0
    if (is.na(axis_x)) axis_x <- 0
    if (is.na(axis_y)) axis_y <- 0
    if (is.na(width)) width <- cfg$get_width(piece_side, suit, rank)
    if (is.na(height)) height <- cfg$get_height(piece_side, suit, rank)
    if (is.na(depth)) depth <- cfg$get_depth(piece_side, suit, rank)
    if (is.na(z)) z <- 0.5 * depth
    if (grepl("tile|coin|saucer|pawn|matchstick", piece_side)) {
        rgl.two_sided_token(piece_side, suit, rank, cfg,
                            x, y, z, angle, axis_x, axis_y,
                            width, height, depth, res)
    } else {
        stop("Don't know how to draw ", piece_side, " yet with rgl.")
    }
    invisible(NULL)
}
