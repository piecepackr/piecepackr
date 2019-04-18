seg <- function(x, y, xend, yend, color="black", ...) {
    grid.segments(x0=x, y0=y, x1=xend, y1=yend, gp=gpar(col=color, ...))
}

grid.halma <- function(gp=gpar()) {
    y_cutoff <- 0.55
    y_frac <- 0.5
    t <- rev(seq(0, 360, length.out=100) - 90)
    r <- 0.5
    x <- 0.5 + to_x(t, r)
    y <- 1 + y_frac * (to_y(t, r) - r)
    indices <- which(y >= y_cutoff)
    grid.polygon(x = c(0,0, x[indices],1,1), y=c(0,0.3,y[indices],0.3,0), gp=gp)
}

grid.pyramid <- function(gp=gpar()) {
    grid.polygon(x = c(0, 0.5, 1), y = c(0, 1, 0), gp=gp)
}

grid.kite <- function(gp=gpar()) {
    x <- c(0.5, 0, 0.5, 1, 0.5)
    y <- c(0, 0.25, 1, 0.25, 0)
    grid.polygon(x, y, gp=gp)
}

grid.pp.convex_fn <- function(n_vertices, t) { 
    t <- seq(0, 360, length.out=n_vertices+1) + t
    r <- 0.5
    x <- to_x(t, r) + 0.5
    y <- to_y(t, r) + 0.5
    function(gp=gpar()) { grid.polygon(x, y, gp=gp) } 
}

splice <- function(x0, x1) {
    vec <- as.numeric()
    for (ii in 1:length(x1)) {
        vec <- append(vec, x0[ii])
        vec <- append(vec, x1[ii])
    }
    append(vec, x0[ii+1])
}

grid.concave_fn <- function(n_vertices, t, r=0.2) {
    t_outer <- seq(0, 360, length.out=n_vertices+1) + t
    n_degrees <- 360 / n_vertices / 2
    t_inner <- seq(n_degrees, 360-n_degrees, length.out=n_vertices) + t
    x_outer <- to_x(t_outer, 0.5) + 0.5
    x_inner <- to_x(t_inner,   r) + 0.5
    y_outer <- to_y(t_outer, 0.5) + 0.5
    y_inner <- to_y(t_inner,   r) + 0.5
    x <- splice(x_outer, x_inner)
    y <- splice(y_outer, y_inner)
    function(gp=gpar()) { grid.polygon(x, y, gp=gp) }
}

grid.convex_mat_fn <- function(n_vertices, t=90, width=0.2) {
    t <- seq(0, 360, length.out=n_vertices+1) + t
    r <- 0.5-width[1]
    x_in <- 0.5 + to_x(t, r)
    y_in <- 0.5 + to_y(t, r)
    x_out <- 0.5 + to_x(t, 0.5)
    y_out <- 0.5 + to_y(t, 0.5)
    x <- c(x_in, x_out)
    y <- c(y_in, y_out)
    id <- rep(1:2, each=n_vertices+1)
    function(gp=gpar()) { grid.path(x, y, id=id, rule="evenodd", gp=gp) }
}

grid.rect_mat_fn <- function(width=0.2) {
        width = rep(width, length.out=4)
        x_out <- c(0, 1, 1, 0)
        y_out <- c(1, 1, 0, 0)
        x_in <- c(width[4], 1-width[2], 1-width[2], width[4])
        y_in <- c(1-width[1], 1-width[1], width[3], width[3])
        x <- c(x_in, x_out)
        y <- c(y_in, y_out)
        id <- rep(1:2, each=4)
        function(gp=gpar()) { grid.path(x, y, id=id, rule="evenodd", gp=gp) }
}
