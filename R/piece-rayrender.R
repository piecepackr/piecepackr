#' Create rayrender board game piece objects
#'
#' \code{piece} creates 3d board game piece objects for use with the rayrender package.
#' @inheritParams piece3d
#' @return A rayrender object.
#' @examples
#'   \donttest{# May take more than 5 seconds on CRAN servers
#'   opt <- options(cores = getOption("Ncpus"))
#'   cfg <- game_systems("sans3d")$piecepack
#'   if (requireNamespace("rayrender", quietly = TRUE) && all(capabilities(c("cairo", "png")))) {
#'       rayrender::render_scene(piece("tile_face", suit = 3, rank = 3, cfg = cfg))
#'   }
#'   if (requireNamespace("rayrender", quietly = TRUE) && all(capabilities(c("cairo", "png")))) {
#'       rayrender::render_scene(piece("coin_back", suit = 4, rank = 2, cfg = cfg))
#'   }
#'   if (requireNamespace("rayrender", quietly = TRUE) && all(capabilities(c("cairo", "png")))) {
#'       rayrender::render_scene(piece("pawn_face", suit = 2, cfg = cfg))
#'   }
#'   options(opt)
#'   }
#' @export
#' @seealso See \url{https://www.rayrender.net} for more information about the \code{rayrender} package.
#'          See \code{\link{geometry_utils}} for a discussion of the 3D rotation parameterization.
piece <- function(piece_side = "tile_back", suit = NA, rank = NA, # nolint
                  cfg = getOption("piecepackr.cfg", pp_cfg()),
                  x = 0, y = 0, z = NA,
                  angle = 0, axis_x = 0, axis_y = 0,
                  width = NA, height = NA, depth = NA,
                  envir = getOption("piecepackr.envir"),
                  ...,
                  scale = 1, res = 72) {
    assert_suggested("rayrender")
    if (is_angle(angle))
        angle <- as.double(angle, "degrees")

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

    cfg <- get_cfg(cfg, envir)
    cfg <- rep(c(cfg), length.out = nn)
    l <- lapply(seq(nn), function(i) {
        cfg[[i]]$rayrender(piece_side[i], suit[i], rank[i],
                     x[i], y[i], z[i],
                     angle[i], axis_x[i], axis_y[i],
                     width[i], height[i], depth[i],
                     scale = scale[i], res = res)
    })
    Reduce(rayrender::add_object, l)
}

rr_piece_helper <- function(piece_side = "tile_back", suit = NA, rank = NA, cfg = pp_cfg(), # nolint
                           x = 0, y = 0, z = NA,
                           angle = 0, axis_x = 0, axis_y = 0,
                           width = NA, height = NA, depth = NA, scale = 1, res = 72) {
    if (scale == 0) return(NULL)
    l_obj <- save_piece_obj(piece_side, suit, rank, cfg,
                            x = x, y = y, z = z,
                            angle = angle, axis_x = axis_x, axis_y = axis_y,
                            width = width, height = height, depth = depth,
                            scale = scale, res = res)
    if (packageVersion("rayrender") < "0.28.0")
        l <- lapply(l_obj$obj, rayrender::obj_model, texture = TRUE)
    else
        l <- lapply(l_obj$obj, rayrender::obj_model, load_material = TRUE, load_textures = TRUE)
    Reduce(rayrender::add_object, l)
}
