# "mat" grob functions
halmaMatGrobFn <- function(width=0.2) {
    width <- rep(width, length.out=2)
    xy_out <- halma_xy()
    xy_in <- Point2D$new(xy_out)$npc_to_in(h=1-width[1], w=1-2.5*width[2])
    x <- c(xy_in$x, xy_out$x)
    y <- c(xy_in$y, xy_out$y)
    id <- rep(1:2, each=length(xy_out$x))
    function(name=NULL, gp=gpar(), vp=NULL) pathGrob(x, y, id=id, rule="evenodd", name=name, gp=gp, vp=vp)
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
    function(name=NULL, gp=gpar(), vp=NULL) pathGrob(x, y, id=id, rule="evenodd", name=name, gp=gp, vp=vp)
}

diffMatGrobFn <- function(width=0.2, shape = pp_shape()) {
    width <- rep(width, length.out=4)
    x_in <- c(width[4], 1-width[2], 1-width[2], width[4])
    y_in <- c(1-width[1], 1-width[1], width[3], width[3])
    vpi_x <- mean(x_in[1:2])
    vpi_y <- mean(y_in[2:3])
    vpi_w <- x_in[2] - x_in[1]
    vpi_h <- y_in[2] - y_in[3]
    function(name=NULL, gp=gpar(), vp=NULL) {
        vpi <- viewport(x=vpi_x, y=vpi_y, width=vpi_w, height=vpi_h)
        inner <- shape$shape(vp=vpi)
        shape$polyclip(inner, op="minus", name=name, gp=gp, vp=vp)
    }
}

rectMatGrobFn <- function(width=0.2) {
    width <- rep(width, length.out=4)
    x_out <- c(0, 1, 1, 0)
    y_out <- c(1, 1, 0, 0)
    x_in <- c(width[4], 1-width[2], 1-width[2], width[4])
    y_in <- c(1-width[1], 1-width[1], width[3], width[3])
    x <- c(x_in, x_out)
    y <- c(y_in, y_out)
    id <- rep(1:2, each=4)
    function(name=NULL, gp=gpar(), vp=NULL) pathGrob(x, y, id=id, rule="evenodd", name=name, gp=gp, vp=vp)
}
