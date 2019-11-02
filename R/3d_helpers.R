has_name <- function(df, name) { name %in% names(df) }

add_cfg <- function(df, cfg=pp_cfg(), envir=NULL) {
    df <- tibble::as_tibble(df)
    if (has_name(df, "cfg")) {
        df$cfg <- lapply(df$cfg, get_cfg, envir)
    } else {
        df$cfg <- rep(c(get_cfg(cfg, envir)), length.out=nrow(df))
    }
    df
}

gwh <- function(piece_side, cfg, ..., suit=1, rank=1) { cfg$get_width(piece_side, suit, rank) }
ghh <- function(piece_side, cfg, ..., suit=1, rank=1) { cfg$get_height(piece_side, suit, rank) }
gdh <- function(piece_side, cfg, ..., suit=1, rank=1) { cfg$get_depth(piece_side, suit, rank) }

add_field <- function(df, key, value) {
    if (has_name(df, key)) {
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

an_pmap <- function(...) { as.numeric(purrr::pmap(...)) }

get_r <- function(width, height) { sqrt((width/2)^2+(height/2)^2) }
get_a <- function(width, height) { 90 - 360 * acos((height/2)/get_r(width, height)) / (2*pi) }

get_xll <- function(x, angle, width, height, ...) { x + to_x((angle+180+get_a(width, height))%%360, get_r(width, height)) }
get_xul <- function(x, angle, width, height, ...) { x + to_x((angle+180-get_a(width, height))%%360, get_r(width, height)) }
get_xur <- function(x, angle, width, height, ...) { x + to_x((angle+get_a(width, height))%%360, get_r(width, height)) }
get_xlr <- function(x, angle, width, height, ...) { x + to_x((angle-get_a(width, height))%%360, get_r(width, height)) }
get_yll <- function(y, angle, width, height, ...) { y + to_y((angle+180+get_a(width, height))%%360, get_r(width, height)) }
get_yul <- function(y, angle, width, height, ...) { y + to_y((angle+180-get_a(width, height))%%360, get_r(width, height)) }
get_yur <- function(y, angle, width, height, ...) { y + to_y((angle+get_a(width, height))%%360, get_r(width, height)) }
get_ylr <- function(y, angle, width, height, ...) { y + to_y((angle-get_a(width, height))%%360, get_r(width, height)) }


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
#' @seealso <https://trevorldavis.com/piecepackr/3d-projections.html> for more details
#'   and examples of oblique projections in \code{piecepackr}.
#' @param df A data frame with coordinates and dimensions in inches
#' @param ... Ignored
#' @param cfg Piecepack configuration list or \code{pp_cfg} object, 
#'        a list of \code{pp_cfg} objects,
#'        or a character vector of \code{pp_cfg} objects
#' @param envir Environment (or named list) containing configuration list(s).
#' @param op_angle Intended oblique projection angle (used for re-sorting)
#' @return A tibble with extra columns added 
#'         and re-sorted rows
#' @examples
#' df <- tibble::tibble(piece_side="tile_back",  
#'                      x=c(2,2,2,4,6,6,4,2,5),  
#'                      y=c(4,4,4,4,4,2,2,2,3))  
#' pmap_piece(df, op_angle=135, trans=op_transform, 
#'            op_scale=0.5, default.units="in")
#'  
#' @export
op_transform <- function(df, ..., cfg=pp_cfg(), envir=NULL, op_angle=45) {
    df <- add_3d_info(df, cfg, envir)
    df <- op_sort(df, op_angle)
    df
}

add_3d_info <- function(df, cfg=pp_cfg(), envir=NULL) {
    # Do more stuff if units aren't in inches?
    df <- add_cfg(df, cfg, envir)
    df <- add_measurements(df)
    df <- add_bounding_box(df)
    df <- add_z(df)
    df <- add_field(df, "zt", df$z + 0.5*df$depth)
    df <- add_field(df, "zb", df$z - 0.5*df$depth)
    df
}

op_sort <- function(df, op_angle=45) {
    op_angle <- op_angle %% 360
    if ((0 <= op_angle) && (op_angle < 90)) {
        df <- df[order(df$zt, -df$yb, -df$xl),]
    } else if ((90 <= op_angle) && (op_angle < 180)) {
        df <- df[order(df$zt, -df$yb, df$xr),]
    } else if ((180 <= op_angle) && (op_angle < 270)) {
        df <- df[order(df$zt, df$yt, df$xr),]
    } else {
        df <- df[order(df$zt, df$yt, -df$xl),]
    }
    df
}

# Axis-Aligned Bounding Box (AABB)
add_bounding_box <- function(df) { 
    df$xll <- an_pmap(df, get_xll)   
    df$xul <- an_pmap(df, get_xul)   
    df$xur <- an_pmap(df, get_xur)   
    df$xlr <- an_pmap(df, get_xlr)   
    df$yll <- an_pmap(df, get_yll)   
    df$yul <- an_pmap(df, get_yul)   
    df$yur <- an_pmap(df, get_yur)   
    df$ylr <- an_pmap(df, get_ylr)   
    df$xl <- pmin(df$xll, df$xul, df$xur, df$xlr)
    df$xr <- pmax(df$xll, df$xul, df$xur, df$xlr)
    df$yb <- pmin(df$yll, df$yul, df$yur, df$ylr)
    df$yt <- pmax(df$yll, df$yul, df$yur, df$ylr)
    df
}

do_ranges_overlap <- function(l1, r1, l2, r2) {
    (less_than_equal(l1, l2) & less_than(l2, r1)) |
    (less_than_equal(l2, l1) & less_than(l1, r2))
}

which_AABB_overlap <- function(dfi, dfs) {
    rev(which(do_ranges_overlap(dfi$xl, dfi$xr, dfs$xl, dfs$xr) & 
              do_ranges_overlap(dfi$yb, dfi$yt, dfs$yb, dfs$yt)))
}

less_than <- function(x, y) { 1e-6 < y - x }  # in case of trigonometric precision issues
less_than_equal <- function(x, y) { 0 < y - x + 1e-6 }  # in case of trigonometric precision issues

add_z <- function(df) {
    shapes <- get_shapes(df)
    zp <- 0.5*df$depth
    for (ii in seq(length.out=nrow(df))) {
        dfi <- df[ii,]
        dfs <- df[0:(ii-1),]
        for (jj in which_AABB_overlap(dfi, dfs)) { 
            if (do_shapes_overlap(shapes[[ii]], shapes[[jj]])) {
                zp[ii] <- as.numeric(zp[jj] + 0.5*df[jj,"depth"] + 0.5*df[ii,"depth"])
                break
            }
        } 
    }
    df <- add_field(df, "z", zp)
    df
}

get_shapes <- function(df) {
    shapes <- vector("list", nrow(df))
    for (ii in seq(length.out=nrow(df))) {
        dfi <- df[ii,]
        cfg <- df$cfg[[ii]]
        piece_side <- df$piece_side[ii]
        suit <- ifelse(has_name(df, "suit"), df$suit[ii], NA)
        rank <- ifelse(has_name(df, "rank"), df$rank[ii], NA)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        if (opt$shape == "circle") {
            shapes[[ii]] <- Circle$new(x=dfi$x, y=dfi$y, r=min(dfi$width/2, dfi$height/2))
        } else { # rect, others
            shapes[[ii]] <- ConvexPolygon$new(x=c(dfi$xll, dfi$xul, dfi$xur, dfi$xlr),
                                              y=c(dfi$yll, dfi$yul, dfi$yur, dfi$ylr))
        }
    }
    shapes
}
