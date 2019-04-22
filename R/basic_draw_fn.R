#' \code{basic_draw_fn} helper functions
#' 
#' \code{add_gridlines} adds gridlines.
#' \code{add_mat} adds a mat.
#' \code{add_checkers} adds checkers.
#' \code{add_hexlines} adds hexlines.
#' \code{get_grid_shape} returns a function that draws the component shape.
#' \code{is_color_invisible} tells whether the color is transparent (and hence need not be drawn).
#' @param col Color
#' @param shape String of the shape
#' @param shape_t Angle (in degrees) of first vertex of shape (ignored by many shapes).
#' @param shape_r Radial distance (from 0 to 0.5) (ignored by most shapes)
#' 
#' @name draw_fn_helpers
NULL

#' @rdname draw_fn_helpers
#' @export
add_gridlines <- function(col, shape = "rect", shape_t = 90) {
    if (is_color_invisible(col)) return (invisible(NULL))
    o <- 0.02
    if (shape == "rect") {
        lwd <- 8
        gp_gl <- gpar(col=col, lwd=lwd, lineend="butt")
        grid.lines(x=0.5, gp=gp_gl)
        grid.lines(y=0.5, gp=gp_gl)
        # seg(0.5, 0+o, 0.5, 1-o, col, lwd=lwd, lineend="square")
        # seg(0+o, 0.5, 1-o, 0.5, col, lwd=lwd, lineend="square")
    } else if (shape %in% c("circle", "kite", "halma")) {
        stop(paste("Don't know how to add grid lines to shape", shape))
    } else {
        o <- 0.01
        lwd <- 4
        n_vertices <- get_n_vertices(shape)
        t <- seq(0, 360, length.out=n_vertices+1) + shape_t
        t <- t[1:(length(t)-1)]
        nt <- length(t)
        n <- floor(nt / 2)
        r <- 0.5 - o
        x <- 0.5 + to_x(t, r)
        y <- 0.5 + to_y(t, r)
        for (ii in 1:nt) {
            i_next <- ii+n
            if (i_next > nt)
                i_next <- i_next %% nt
            seg(x[ii], y[ii], x[i_next], y[i_next] , col, lwd=lwd)
        }
    }
}

#' @rdname draw_fn_helpers
#' @param mat_width Numeric vector of mat widths
#' @export
add_mat <- function(col, shape = "rect", shape_t = 90, mat_width = 0) {
    if (is_color_invisible(col) || all(mat_width==0))
        return (invisible(NULL))
    gp_mat <- gpar(col=NA, fill=col)
    if (shape == "rect") {
        grid.rect_mat_fn(mat_width)(gp=gp_mat)
    } else if (shape == "circle") {
        grid.convex_mat_fn(60, 0, mat_width[1])(gp=gp_mat)
    } else if (grepl("^convex", shape)) {
        grid.convex_mat_fn(get_n_vertices(shape), shape_t, mat_width[1])(gp=gp_mat)
    } else {
        stop(paste("Don't know how to add mat to shape", shape))
    }
}

#' @rdname draw_fn_helpers
#' @export
add_checkers <- function(col, shape = "rect", shape_t=90) {
    if (is_color_invisible(col)) return (invisible(NULL))
    if (shape == "rect") {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(col=NA, fill=col))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(col=NA, fill=col))
    } else if (grepl("^concave", shape) || grepl("^convex", shape)) {
        n_vertices <- get_n_vertices(shape)
        t <- seq(0, 360, length.out=n_vertices+1) + shape_t
        nt <- length(t) - 1
        r <- 0.5
        x <- 0.5 + to_x(t, r)
        y <- 0.5 + to_y(t, r)
        for (ii in 1:nt) {
            if( ii %% 2) {
                xs <- c(0.5, x[ii], x[ii+1])
                ys <- c(0.5, y[ii], y[ii+1])
                grid.polygon(xs, ys, gp=gpar(col=NA, fill=col))
            }
        }
    } else {
        stop(paste("Don't know how to add checkers to shape", shape))
    }
}

# add_hexlines <- function(col, shape, omit_direction=FALSE) {
#     if(is_color_invisible(col)) return (invisible(NULL))
#     if (shape != "rect") {
#         stop(paste("Don't know how to add hexlines to shape", shape))
#     }
#     ho <- 0.25
#     hl_size <- 4
#     if (omit_direction %in% 1:2)  # upper left
#         NULL
#     else
#         seg(0, 1 - ho, ho, 1, col, lwd=hl_size) 
#     if (omit_direction %in% 3:4)  # lower left
#         NULL
#     else
#         seg(0, ho, ho, 0, col, lwd=hl_size) 
#     if (omit_direction %in% 5:6)  # lower right
#         NULL
#     else
#         seg(1, ho, 1 - ho, 0, col, lwd=hl_size) 
#     if (omit_direction %in% 7:8)  # upper right
#         NULL
#     else
#         seg(1, 1 - ho, 1 - ho, 1, col, lwd=hl_size) 
# }

#' @rdname draw_fn_helpers
#' @export
add_hexlines <- function(col, shape = "rect") {
    if(is_color_invisible(col)) return (invisible(NULL))
    if (shape == "rect") {
        ho <- 0.25
        hl_size <- 4
        seg(0, 1 - ho, ho, 1, col, lwd=hl_size) 
        seg(0, ho, ho, 0, col, lwd=hl_size) 
        seg(1, ho, 1 - ho, 0, col, lwd=hl_size) 
        seg(1, 1 - ho, 1 - ho, 1, col, lwd=hl_size) 
    } else {
        stop(paste("Don't know how to add hexlines to shape", shape))
    }
}

#' @rdname draw_fn_helpers
#' @export
get_grid_shape <- function(shape, shape_t=90, shape_r=0.2) {
    if (shape == "circle") {
        grid.circle
    } else if (shape == "rect") {
        grid.rect
    } else if (shape == "kite") {
        grid.kite
    } else if (shape == "halma") {
        grid.halma
    } else if (shape == "pyramid") {
        grid.pyramid
    } else if (grepl("^concave", shape)) {
        grid.concave_fn(get_n_vertices(shape), shape_t, shape_r) 
    } else if (grepl("^convex", shape)) {
        grid.pp.convex_fn(get_n_vertices(shape), shape_t)
    } else {
        stop(paste("Don't know how to draw shape", shape)) 
    }
}

#' @rdname draw_fn_helpers
#' @export
is_color_invisible <- function(col) {
    if (is.na(col))
        return (TRUE)
    if (col == "transparent")
        return (TRUE)
    return (FALSE)
}

