#' Grid shape grob utility functions
#' 
#' Utility functions that produce grobs of various shapes 
#' or function that returns a function that produces a grob.
#' These are usually wrappers of \code{polygonGrob} or \code{pathGrob}.
#' @param gp An object of class ‘gpar’, typically the output from a call
#'        to the function ‘gpar’.  This is basically a list of
#'        graphical parameter settings.
#' @param name A character identifier (for grid)
#' @param n_vertices Number of vertices
#' @param t Angle (in degrees) of first vertex of shape 
#' @param r Radial distance (from 0 to 0.5) 
#' 
#' @name grid_shape_grobs
NULL

#' @rdname grid_shape_grobs
#' @export
halmaGrob <- function(gp=gpar(), name = NULL) {
    y_cutoff <- 0.55
    y_frac <- 0.5
    t <- rev(seq(0, 360, length.out=100) - 90)
    r <- 0.5
    x <- 0.5 + to_x(t, r)
    y <- 1 + y_frac * (to_y(t, r) - r)
    indices <- which(y >= y_cutoff)
    polygonGrob(x = c(0,0, x[indices],1,1), y=c(0,0.3,y[indices],0.3,0), 
                gp=gp, name=name)
}

#' @rdname grid_shape_grobs
#' @export
pyramidGrob <- function(gp=gpar(), name = NULL) {
    polygonGrob(x = c(0, 0.5, 1), y = c(0, 1, 0), gp=gp, name=name)
}

#' @rdname grid_shape_grobs
#' @export
kiteGrob <- function(gp=gpar(), name = NULL) {
    x <- c(0.5, 0, 0.5, 1, 0.5)
    y <- c(0, 0.25, 1, 0.25, 0)
    polygonGrob(x, y, gp=gp, name=name)
}

#' @rdname grid_shape_grobs
#' @export
convexGrobFn <- function(n_vertices, t) {
    t <- seq(0, 360, length.out=n_vertices+1) + t
    r <- 0.5
    x <- to_x(t, r) + 0.5
    y <- to_y(t, r) + 0.5
    function(gp=gpar(), name=NULL) { polygonGrob(x, y, gp=gp, name=name) }
}

#' @rdname grid_shape_grobs
#' @export
concaveGrobFn <- function(n_vertices, t, r=0.2) {
    t_outer <- seq(0, 360, length.out=n_vertices+1) + t
    n_degrees <- 360 / n_vertices / 2
    t_inner <- seq(n_degrees, 360-n_degrees, length.out=n_vertices) + t
    x_outer <- to_x(t_outer, 0.5) + 0.5
    x_inner <- to_x(t_inner,   r) + 0.5
    y_outer <- to_y(t_outer, 0.5) + 0.5
    y_inner <- to_y(t_inner,   r) + 0.5
    x <- splice(x_outer, x_inner)
    y <- splice(y_outer, y_inner)
    function(gp=gpar(), name=NULL) { polygonGrob(x, y, gp=gp, name=name) }
}

splice <- function(x0, x1) {
    vec <- as.numeric()
    for (ii in 1:length(x1)) {
        vec <- append(vec, x0[ii])
        vec <- append(vec, x1[ii])
    }
    append(vec, x0[ii+1])
}

convexMatGrobFn <-  function(n_vertices, t=90, width=0.2) {
    t <- seq(0, 360, length.out=n_vertices+1) + t
    r <- 0.5-width[1]
    x_in <- 0.5 + to_x(t, r)
    y_in <- 0.5 + to_y(t, r)
    x_out <- 0.5 + to_x(t, 0.5)
    y_out <- 0.5 + to_y(t, 0.5)
    x <- c(x_in, x_out)
    y <- c(y_in, y_out)
    id <- rep(1:2, each=n_vertices+1)
    function(gp=gpar(), name=NULL) { pathGrob(x, y, id=id, rule="evenodd", gp=gp, name=name) }
}

rectMatGrobFn <- function(width=0.2) {
    width = rep(width, length.out=4)
    x_out <- c(0, 1, 1, 0)
    y_out <- c(1, 1, 0, 0)
    x_in <- c(width[4], 1-width[2], 1-width[2], width[4])
    y_in <- c(1-width[1], 1-width[1], width[3], width[3])
    x <- c(x_in, x_out)
    y <- c(y_in, y_out)
    id <- rep(1:2, each=4)
    function(gp=gpar(), name=NULL) { pathGrob(x, y, id=id, rule="evenodd", gp=gp, name=name) }
}
