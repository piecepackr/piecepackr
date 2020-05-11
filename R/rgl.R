#' Draw board game pieces using rgl
#'
#' \code{piece3d} draws board games pieces using the rgl package.
#' @inheritParams grid.piece
#' @inheritParams save_piece_obj
#' @param lit logical, specifying if rgl lighting calculation should take place
#' @param shininess Properties for rgl lighting calculation
#' @return A numeric vector of rgl object IDs.
#' @examples
#' if ((Sys.getenv("TRAVIS") == "") && require("rgl")) {
#'     open3d()
#'     cfg <- pp_cfg()
#'     piece3d("tile_face", suit = 3, rank = 3, cfg = cfg, x = 0, y = 0, z = 0)
#'     piece3d("coin_back", suit = 4, rank = 2, cfg = cfg, x = 2, y = 0, z = 0)
#'     piece3d("saucer_back", suit = 1, cfg = cfg, x = 2, y = 2, z=-2)
#'     piece3d("pawn_face", suit = 2, cfg = cfg, x = 1, y = 1, z = 2)
#' }
#' @export
piece3d <- function(piece_side = "tile_back", suit = NA, rank = NA, cfg = pp_cfg(), # nolint
                           x = 0, y = 0, z = NA,
                           angle = 0, axis_x = 0, axis_y = 0,
                           width = NA, height = NA, depth = NA,
                           envir = NULL, ..., scale = 1, res = 72,
                           alpha = 1.0, lit = FALSE, shininess = 50.0) {
    if (!requireNamespace("rgl", quietly = TRUE)) {
        stop("You need to install the suggested package rgl to use 'piece3d'.",
             "Use 'install.packages(\"rgl\")'")
    }
    nn <- max(lengths(list(piece_side, suit, rank, x, y, z, angle, axis_x, axis_y, width, height, depth)))
    piece_side <- rep(piece_side, length.out = nn)
    suit <- rep(suit, length.out = nn)
    rank <- rep(rank, length.out = nn)
    x <- rep(x, length.out = nn)
    y <- rep(y, length.out = nn)
    z <- rep(z, length.out = nn)
    angle <- rep(angle, length.out = nn)
    axis_x <- rep(axis_x, length.out = nn)
    axis_y <- rep(axis_y, length.out = nn)
    width <- rep(width, length.out = nn)
    height <- rep(height, length.out = nn)
    depth <- rep(depth, length.out = nn)

    scale <- rep(scale, length.out = nn)
    alpha <- rep(alpha, length.out = nn)
    lit <- rep(lit, length.out = nn)
    shininess <- rep(shininess, length.out = nn)

    cfg <- get_cfg(cfg, envir)
    cfg <- rep(c(cfg), length.out = nn)
    l <- lapply(seq(nn), function(i) {
        rgl_piece_helper(piece_side[i], suit[i], rank[i], cfg[[i]],
                         x[i], y[i], z[i],
                         angle[i], axis_x[i], axis_y[i],
                         width[i], height[i], depth[i],
                         scale = scale[i], res = res,
                         alpha = alpha[i], lit = lit[i], shininess = shininess[i])
    })
    do.call(c, l)
}

rgl_piece_helper <- function(piece_side = "tile_back", suit = NA, rank = NA, cfg = pp_cfg(), # nolint
                           x = 0, y = 0, z = NA,
                           angle = 0, axis_x = 0, axis_y = 0,
                           width = NA, height = NA, depth = NA,
                           scale = 1, res = 72,
                           alpha = 1, lit = FALSE, shininess = 50.0) {
    if (scale == 0 || alpha == 0) return(numeric(0))
    obj <- save_piece_obj(piece_side, suit, rank, cfg,
                        x = x, y = y, z = z,
                        angle = angle, axis_x = axis_x, axis_y = axis_y,
                        width = width, height = height, depth = depth,
                        scale = scale, res = res)
    material <- list(color = "white", alpha = alpha, lit = lit, shininess = shininess,
                     texture = obj$png, textype = "rgba")
    mesh <- suppressWarnings(rgl::readOBJ(obj$obj, material = material))
    invisible(as.numeric(rgl::shade3d(mesh)))
}
