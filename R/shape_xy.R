get_shape_xy <- function(shape, shape_t=90, shape_r=0.2) {
    if (shape == "rect") {
        rect_xy
    } else if (grepl(shape, "circle|oval")) {
        oval_xy()
    } else if (shape == "kite") {
        kite_xy
    } else if (shape == "halma") {
        halma_xy()
    } else if (shape == "pyramid") {
        pyramid_xy
    } else if (grepl("^concave", shape)) {
        concave_xy(get_n_vertices(shape), shape_t, shape_r)
    } else if (grepl("^convex", shape)) {
        convex_xy(get_n_vertices(shape), shape_t)
    } else {
        stop(paste("Don't know how to get xy coordinates for", shape))
    }
}

pyramid_xy <- list(x = c(0.5, 0, 1), y = c(1, 0, 0))
kite_xy <- list(x = c(0.5, 0, 0.5, 1), y = c(1, 0.25, 0, 0.25))
rect_xy <- list(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1))
oval_xy <- function() convex_xy(36, 90)

convex_xy <- function(n_vertices, t) {
    t <- seq(0, 360, length.out = n_vertices + 1) + t
    r <- 0.5
    x <- to_x(t, r) + 0.5
    y <- to_y(t, r) + 0.5
    list(x = utils::head(x, -1), y = utils::head(y, -1))
}

halma_xy <- function() {
    y_cutoff <- 0.55
    y_frac <- 0.5
    t <- seq(0, 360, length.out = 36 + 1) - 90
    r <- 0.5
    x <- 0.5 + to_x(t, r)
    y <- 1 + y_frac * (to_y(t, r) - r)
    indices <- which(y >= y_cutoff)
    list(x = c(1, 1, x[indices], 0, 0),
         y = c(0, 0.3, y[indices], 0.3, 0))
}

concave_xy <- function(n_vertices, t, r = 0.2) {
    t_outer <- seq(0, 360, length.out = n_vertices+1) + t
    n_degrees <- 360 / n_vertices / 2
    t_inner <- seq(n_degrees, 360 - n_degrees, length.out = n_vertices) + t
    x_outer <- to_x(t_outer, 0.5) + 0.5
    x_inner <- to_x(t_inner,   r) + 0.5
    y_outer <- to_y(t_outer, 0.5) + 0.5
    y_inner <- to_y(t_inner,   r) + 0.5
    x <- splice(x_outer, x_inner)
    y <- splice(y_outer, y_inner)
    list(x = x, y = y)
}

splice <- function(x0, x1) {
    vec <- as.numeric()
    for (ii in seq_along(x1)) {
        vec <- append(vec, x0[ii])
        vec <- append(vec, x1[ii])
    }
    append(vec, x0[ii+1])
}
