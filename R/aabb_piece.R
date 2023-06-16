#' Calculate axis-aligned bounding box for set of game pieces
#'
#' Calculate axis-aligned bounding box (AABB) for set of game pieces
#' with and without an \dQuote{oblique projection}.
#'
#' The \dQuote{oblique projection} of a set of \eqn{(x,y,z)} points onto the xy-plane
#' is \eqn{(x + \lambda * z * cos(\alpha), y + \lambda * z * sin(\alpha))}
#' where \eqn{\lambda} is the scale factor and \eqn{\alpha} is the angle.
#'
#' @param df A data frame of game piece information with (at least) the
#'        named columns \dQuote{piece_side}, \dQuote{x}, and \dQuote{y}.
#' @inheritParams grid.piece
#' @param ... Ignored
#' @return A named list of ranges with five named elements `x`, `y`, and `z` for
#'         the axis-aligned bounding cube
#'         in xyz-space plus `x_op` and `y_op` for the axis-aligned bounding box
#'         of the \dQuote{oblique projection} onto the xy plane.
#' @examples
#'  df_tiles <- data.frame(piece_side="tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1),
#'                         suit=NA, angle=NA, z=NA, stringsAsFactors=FALSE)
#'  df_coins <- data.frame(piece_side="coin_back", x=rep(4:1, 4), y=rep(4:1, each=4),
#'                         suit=1:16%%2+rep(c(1,3), each=8),
#'                         angle=rep(c(180,0), each=8), z=1/4+1/16, stringsAsFactors=FALSE)
#'  df <- rbind(df_tiles, df_coins)
#'
#'  aabb_piece(df, op_scale = 0)
#'  aabb_piece(df, op_scale = 1, op_angle = 45)
#'  aabb_piece(df, op_scale = 1, op_angle = -90)
#' @export
aabb_piece <- function(df,
                       cfg = getOption("piecepackr.cfg", pp_cfg()),
                       envir = getOption("piecepackr.envir"),
                       op_scale = getOption("piecepackr.op_scale", 0),
                       op_angle = getOption("piecepackr.op_angle", 45),
                       ...) {
    rlang::local_options(affiner_angular_unit = "degrees")
    if (nrow(df) == 0) {
        return(list(x = c(NA_real_, NA_real_),
                    y = c(NA_real_, NA_real_),
                    z = c(NA_real_, NA_real_),
                    x_op = c(NA_real_, NA_real_),
                    y_op = c(NA_real_, NA_real_)))
    }

    df <- add_3d_info(df, cfg = cfg, envir = envir)
    x <- c(df$xl, df$xr)
    y <- c(df$yb, df$yt)
    z <- c(df$zb, df$zt)

    llb <- as_coord2d(as_coord3d(df$xll, df$yll, df$zb), alpha = op_angle, scale = op_scale)
    llt <- as_coord2d(as_coord3d(df$xll, df$yll, df$zt), alpha = op_angle, scale = op_scale)
    ulb <- as_coord2d(as_coord3d(df$xul, df$yul, df$zb), alpha = op_angle, scale = op_scale)
    ult <- as_coord2d(as_coord3d(df$xul, df$yul, df$zt), alpha = op_angle, scale = op_scale)
    lrb <- as_coord2d(as_coord3d(df$xlr, df$ylr, df$zb), alpha = op_angle, scale = op_scale)
    lrt <- as_coord2d(as_coord3d(df$xlr, df$ylr, df$zt), alpha = op_angle, scale = op_scale)
    urb <- as_coord2d(as_coord3d(df$xur, df$yur, df$zb), alpha = op_angle, scale = op_scale)
    urt <- as_coord2d(as_coord3d(df$xur, df$yur, df$zt), alpha = op_angle, scale = op_scale)
    x_op <- c(llb$x, llt$x, ulb$x, ult$x, lrb$x, lrt$x, urb$x, urt$x)
    y_op <- c(llb$y, llt$y, ulb$y, ult$y, lrb$y, lrt$y, urb$y, urt$y)

    list(x = range(x), y = range(y), z = range(z),
         x_op = range(x_op), y_op = range(y_op))
}
