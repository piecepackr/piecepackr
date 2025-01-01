add_cfg <- function(df, cfg=pp_cfg(), envir=NULL) {
    df <- tibble::as_tibble(df)
    if (hasName(df, "cfg")) {
        df$cfg <- lapply(df$cfg, get_cfg, envir)
    } else {
        df$cfg <- rep(c(get_cfg(cfg, envir)), length.out=nrow(df))
    }
    df
}

add_field <- function(df, key, value) {
    if (hasName(df, key)) {
        df[[key]] <- ifelse(is.na(df[[key]]), value, df[[key]])
    } else {
        df[[key]] <- value
    }
    df
}

add_measurements <- function(df) {
    df <- add_field(df, "width", an_pmap(df, gwh))
    df <- add_field(df, "height", an_pmap(df, ghh))
    df <- add_field(df, "depth", an_pmap(df, gdh))
    df <- add_field(df, "angle", 0)
    df
}

an_pmap <- function(...) as.numeric(purrr::pmap(...))
gwh <- function(piece_side, cfg, ..., suit=1, rank=1) cfg$get_width(piece_side, suit, rank)
ghh <- function(piece_side, cfg, ..., suit=1, rank=1) cfg$get_height(piece_side, suit, rank)
gdh <- function(piece_side, cfg, ..., suit=1, rank=1) cfg$get_depth(piece_side, suit, rank)

#' Oblique projection helper function
#'
#' Guesses \code{z} coordinates and
#' sorting order to more easily
#' make 3D graphics with \code{pmap_piece}.
#'
#' The heuristics used to generate guesses
#' for \code{z} coordinates and sorting order
#' aren't guaranteed to work in every case.
#' In some cases you may get better sorting results
#' by changing the \code{op_angle} or the dimensions of pieces.
#' @seealso \url{https://trevorldavis.com/piecepackr/3d-projections.html} for more details
#'   and examples of oblique projections in \code{piecepackr}.
#' @param df A data frame with coordinates and dimensions in inches
#' @param ... Ignored
#' @param cfg Piecepack configuration list or \code{pp_cfg} object,
#'        a list of \code{pp_cfg} objects,
#'        or a character vector of \code{pp_cfg} objects
#' @param envir Environment (or named list) containing configuration list(s).
#' @param op_angle Intended oblique projection angle (used for re-sorting)
#' @param pt_thickness Thickness of pyramid tip i.e. value to add to the z-value of a pyramid top
#'                  if it is a (weakly) smaller ranked pyramid (top)
#'                  placed on top of a larger ranked pyramid (top).
#' @param as_top Character vector of components whose \dQuote{side}
#'               should be converted to \dQuote{top} e.g. \code{c("pawn_face")}.
#' @param cfg_class Either `"list"` (default) or `"character"`.
#'                  Desired class of the `cfg` column in the returned tibble.
#'                  `"list"` is more efficient for use with `pmap_piece()` but
#'                  `geom_piece()` needs `"character"`.
#' @return A tibble with extra columns added
#'         and re-sorted rows
#' @examples
#' df <- tibble::tibble(piece_side="tile_back",
#'                      x=c(2,2,2,4,6,6,4,2,5),
#'                      y=c(4,4,4,4,4,2,2,2,3))
#' cfg <- game_systems()$piecepack
#' pmap_piece(df, op_angle=135, trans=op_transform,
#'            op_scale=0.5, default.units="in", cfg=cfg)
#' @export
op_transform <- function(df, ...,
                         cfg=getOption("piecepackr.cfg", pp_cfg()),
                         envir=getOption("piecepackr.envir"),
                         op_angle = getOption("piecepackr.op_angle", 45),
                         pt_thickness = 0.01,
                         as_top = character(0),
                         cfg_class = "list") {
    if (is_angle(op_angle))
        op_angle <- as.double(op_angle, "degrees")
    for (ps in as_top) {
        indices <- which(df$piece_side == ps)
        piece <- get_piece(ps)
        df$piece_side[indices] <- gsub(ps, paste0(piece, "_top"), df$piece_side[indices])
    }
    df <- add_3d_info(df, cfg = cfg, envir = envir, pt_thickness = pt_thickness, cfg_class = cfg_class)
    df <- op_sort(df, op_angle = op_angle)
    df
}

add_3d_info <- function(df, ..., cfg=pp_cfg(), envir=NULL, pt_thickness = 0.01, cfg_class = "list") {
    # Do more stuff if units aren't in inches?
    if (cfg_class == "character")
        cfg_character <- get_cfg_character(df, cfg)
    df <- add_cfg(df, cfg, envir)
    df <- add_measurements(df)
    df <- add_bounding_box(df)
    df <- add_z(df, pt_thickness = pt_thickness)
    df <- add_field(df, "zt", df$z + 0.5*df$depth)
    df <- add_field(df, "zb", df$z - 0.5*df$depth)
    if (cfg_class == "character")
        df$cfg <- cfg_character
    df
}

get_cfg_character <- function(df, cfg) {
    if (hasName(df, "cfg")) {
        df$cfg
    } else if (is.null(cfg)) {
        rep_len("piecepack", nrow(df))
    } else if (is.character(cfg)) {
        rep_len(cfg, nrow(df))
    } else {
        abort("Can't compute \"character\" `cfg_class`")
    }
}

op_sort <- function(df, ..., op_angle=45) {
    op_angle <- op_angle %% 360
    if ((0 <= op_angle) && (op_angle < 90)) {
        df <- df[order(df$zt, -df$yb, -df$xl), ]
    } else if ((90 <= op_angle) && (op_angle < 180)) {
        df <- df[order(df$zt, -df$yb, df$xr), ]
    } else if ((180 <= op_angle) && (op_angle < 270)) {
        df <- df[order(df$zt, df$yt, df$xr), ]
    } else {
        df <- df[order(df$zt, df$yt, -df$xl), ]
    }
    df
}

# Axis-Aligned Bounding Box (AABB)
add_bounding_box <- function(df) {
    zeros <- rep_len(0, nrow(df))
    ones <- rep_len(1, nrow(df))
    ll <- npc_to_in(as_coord2d(zeros, zeros), df$x, df$y, df$width, df$height, df$angle)
    ul <- npc_to_in(as_coord2d(zeros, ones), df$x, df$y, df$width, df$height, df$angle)
    ur <- npc_to_in(as_coord2d(ones, ones), df$x, df$y, df$width, df$height, df$angle)
    lr <- npc_to_in(as_coord2d(ones, zeros), df$x, df$y, df$width, df$height, df$angle)
    df$xll <- ll$x
    df$yll <- ll$y
    df$xul <- ul$x
    df$yul <- ul$y
    df$xlr <- lr$x
    df$ylr <- lr$y
    df$xur <- ur$x
    df$yur <- ur$y
    df$xl <- op_round(pmin(df$xll, df$xul, df$xur, df$xlr))
    df$xr <- op_round(pmax(df$xll, df$xul, df$xur, df$xlr))
    df$yb <- op_round(pmin(df$yll, df$yul, df$yur, df$ylr))
    df$yt <- op_round(pmax(df$yll, df$yul, df$yur, df$ylr))
    df
}

op_round <- function(x) round(x, 9)

do_ranges_overlap <- function(l1, r1, l2, r2) {
    (less_than_equal(l1, l2) & less_than(l2, r1)) |
    (less_than_equal(l2, l1) & less_than(l1, r2))
}

which_AABB_overlap <- function(dfi, dfs) {
    rev(which(do_ranges_overlap(dfi$xl, dfi$xr, dfs$xl, dfs$xr) &
              do_ranges_overlap(dfi$yb, dfi$yt, dfs$yb, dfs$yt)))
}

less_than <- function(x, y) 1e-6 < y - x # in case of trigonometric precision issues
less_than_equal <- function(x, y) 0 < y - x + 1e-6 # in case of trigonometric precision issues

add_z <- function(df, pt_thickness = 0.01) {
    shapes <- get_shapes(df)
    zp <- 0.5*df$depth
    for (i in seq(length.out=nrow(df))) {
        dfi <- df[i, ]
        dfs <- df[seq_len(i-1L), ]
        zc <- zp[i]
        for (j in which_AABB_overlap(dfi, dfs)) {
            if (do_shapes_overlap(shapes[[i]], shapes[[j]])) {
                zc0 <- compute_z(df, zp, i, j, pt_thickness)
                if (zc0 > zc) zc <- zc0
            }
        }
        zp[i] <- zc
    }
    df <- add_field(df, "z", zp)
    df
}
# @param df Data frame
# @param zp Depths
# @param i Index of top piece
# @param j Index of top overlapping piece underneath
# @param pt_thickness Thickness of tip of pyramid
compute_z <- function(df, zp, i, j, pt_thickness = 0.01) {
    if (all(df$piece_side[c(i,j)] == "pyramid_top")) {
        if (df$rank[i] <= df$rank[j]) { # top piece is (weakly) smaller
            zp[j] + 0.5 * df$depth[j] + pt_thickness - 0.5 * df$depth[i]
        } else { # top piece is (strictly) bigger
            zp[j] - 0.5 * df$depth[j] + 0.5 * df$depth[i]
        }
    } else {
        zp[j] + 0.5 * df$depth[j] + 0.5 * df$depth[i]
    }
}

get_shapes <- function(df) {
    shapes <- vector("list", nrow(df))
    for (ii in seq(length.out=nrow(df))) {
        dfi <- df[ii, ]
        cfg <- df$cfg[[ii]]
        piece_side <- df$piece_side[ii]
        suit <- ifelse(hasName(df, "suit"), df$suit[ii], NA)
        rank <- ifelse(hasName(df, "rank"), df$rank[ii], NA)
        opt <- try(cfg$get_piece_opt(piece_side, suit, rank), silent = TRUE)
        if (inherits(opt, "try-error") || opt$shape %in% c("rect", "halma", "meeple", "roundrect")) {
            shapes[[ii]] <- ConvexPolygon$new(x=c(dfi$xll, dfi$xul, dfi$xur, dfi$xlr),
                                              y=c(dfi$yll, dfi$yul, dfi$yur, dfi$ylr))
        } else if (opt$shape == "circle") {
            shapes[[ii]] <- Circle$new(x=dfi$x, y=dfi$y, r=min(dfi$width/2, dfi$height/2))
        } else {
            label <- opt$shape
            if (grepl("^concave", label)) label <- gsub("concave", "convex", label)
            shape <- pp_shape(label, opt$shape_t, opt$shape_r, opt$back)
            xy_c <- npc_to_in(as_coord2d(shape$npc_coords),
                              dfi$x, dfi$y, dfi$width, dfi$height, dfi$angle)
            shapes[[ii]] <- ConvexPolygon$new(xy_c$x, xy_c$y)
        }
    }
    shapes
}
