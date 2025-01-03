#' Render board game pieces with rgl
#'
#' \code{piece3d} draws board games pieces using the rgl package.
#' @inheritParams grid.piece
#' @inheritParams save_piece_obj
#' @param lit logical, specifying if rgl lighting calculation should take place.
#' @param shininess Properties for rgl lighting calculation.
#' @param textype Use \code{"rgba"} when sure texture will have alpha transparency.
#'                Use \code{"rgb"} when sure texture will not have alpha transparency
#'                (in particular \code{rgl}'s WebGL export will likely work better).
#'                If \code{NA} we will read the texture and figure out a reasonable value.
#' @return A numeric vector of rgl object IDs.
#' @examples
#' if (requireNamespace("rgl", quietly = TRUE) && all(capabilities(c("cairo", "png")))) {
#'   rgl::open3d()
#'   cfg <- game_systems("sans3d")$piecepack
#'   piece3d("tile_back", suit = 3, rank = 3, cfg = cfg, x = 0, y = 0, z = 0)
#'   piece3d("coin_back", suit = 4, rank = 2, cfg = cfg, x = 0.5, y = 0.5, z = 0.25)
#'   piece3d("pawn_top", suit = 1, cfg = cfg, x = -0.5, y = 0.5, z = 0.6)
#'   piece3d("die_face", suit = 3, cfg = cfg, x = -0.5, y = -0.5, z = 0.375)
#'   piece3d("pyramid_top", suit = 2, rank = 3, cfg = cfg, x = 1.5, y = 0.0, z = 0.31875)
#'   invisible(NULL)
#' }
#' @export
#' @seealso See \code{\link[rgl]{rgl-package}} for more information about the \code{rgl} package.
#'          See [rgl::material3d()] for more info about setting \code{rgl} material properties.
#'          See \code{\link{geometry_utils}} for a discussion of the 3D rotation parameterization.
piece3d <- function(piece_side = "tile_back", suit = NA, rank = NA, # nolint
                    cfg = getOption("piecepackr.cfg", pp_cfg()),
                    x = 0, y = 0, z = NA,
                    angle = 0, axis_x = 0, axis_y = 0,
                    width = NA, height = NA, depth = NA,
                    envir = getOption("piecepackr.envir"),
                    ...,
                    scale = 1, res = 72,
                    alpha = 1.0, lit = FALSE,
                    shininess = 50.0, textype = NA) {
    assert_suggested("rgl")
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
    alpha <- rep(alpha, length.out = nn)
    lit <- rep(lit, length.out = nn)
    shininess <- rep(shininess, length.out = nn)
    textype <- rep(textype, length.out = nn)

    cfg <- get_cfg(cfg, envir)
    cfg <- rep(c(cfg), length.out = nn)
    l <- lapply(seq(nn), function(i) {
        cfg[[i]]$rgl(piece_side[i], suit[i], rank[i],
                     x[i], y[i], z[i],
                     angle[i], axis_x[i], axis_y[i],
                     width[i], height[i], depth[i],
                     scale = scale[i], res = res,
                     alpha = alpha[i], lit = lit[i],
                     shininess = shininess[i], textype = textype[i])
    })
    unlist(l)
}

rgl_piece_helper <- function(piece_side = "tile_back", suit = NA, rank = NA, cfg = pp_cfg(), # nolint
                             x = 0, y = 0, z = NA,
                             angle = 0, axis_x = 0, axis_y = 0,
                             width = NA, height = NA, depth = NA,
                             scale = 1, res = 72,
                             alpha = 1, lit = FALSE,
                             shininess = 50.0, textype = NA) {
    if (scale == 0 || alpha == 0) return(invisible(numeric(0)))
    l_obj <- save_piece_obj(piece_side, suit, rank, cfg,
                            x = x, y = y, z = z,
                            angle = angle, axis_x = axis_x, axis_y = axis_y,
                            width = width, height = height, depth = depth,
                            scale = scale, res = res)
    l <- purrr::pmap(l_obj, rgl_piece_helper_obj, alpha = alpha, lit = lit, shininess = shininess, textype = textype)
    unlist(l)
}

rgl_piece_helper_obj <- function(obj, mtl, png, alpha = 1, lit = FALSE, shininess = 50.0, textype = NA) {
    if (is.na(textype)) {
        r <- png::readPNG(png)
        textype <- ifelse(dim(r)[3] > 3 && mean(r[,,4]) < 0.999, "rgba", "rgb")
    }
    material <- list(color = "white", alpha = alpha,
                     lit = lit, shininess = shininess,
                     front = "filled", back = "filled",
                     texture = png, textype = textype)
    if (requireNamespace("readobj", quietly = TRUE))
        mesh <- readobj::read.obj(obj, convert.rgl = TRUE)
    else
        mesh <- suppressWarnings(rgl::readOBJ(obj))
    id <- rgl::shade3d(mesh, material = material)
    invisible(as.numeric(id))
}
