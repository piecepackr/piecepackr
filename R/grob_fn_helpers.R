#' \code{pieceGrob} helper functions
#'
#' \code{gridlinesGrob} returns a grob that produces gridlines.
#' \code{matGrob} returns a grob that produces a mat.
#' \code{checkersGrob} returns a grob that adds checkers.
#' \code{hexlinesGrob} returns a grob that adds hexlines.
#' \code{get_shape_grob_fn} returns a function that returns a grob of the piece shape.
#' \code{is_color_invisible} tells whether the color is transparent (and hence need not be drawn).
#' @param col Color
#' @param shape String of the shape
#' @param shape_t Angle (in degrees) of first vertex of shape (ignored by many shapes).
#' @param shape_r Radial distance (from 0 to 0.5) (ignored by most shapes)
#' @param name A character identifier (for grid)
#' @param lex Multiplier to apply to the line width
#' @examples
#'  is_color_invisible("transparent")
#'  is_color_invisible(NA)
#'  is_color_invisible("blue")
#'  is_color_invisible("#05AE9C")
#'
#'  if (require("grid")) {
#'      gp <- gpar(col="black", fill="yellow")
#'      pushViewport(viewport(x=0.25, y=0.75, width=1/2, height=1/2))
#'      grid.draw(get_shape_grob_fn("rect")(gp=gp))
#'      grid.draw(gridlinesGrob("blue", lex=4))
#'      grid.draw(hexlinesGrob("green"))
#'      popViewport()
#'
#'      pushViewport(viewport(x=0.75, y=0.75, width=1/2, height=1/2))
#'      grid.draw(get_shape_grob_fn("convex6")(gp=gp))
#'      grid.draw(checkersGrob("blue", shape="convex6"))
#'      popViewport()
#'
#'      pushViewport(viewport(x=0.25, y=0.25, width=1/2, height=1/2))
#'      grid.draw(get_shape_grob_fn("circle")(gp=gp))
#'      grid.draw(matGrob("blue", shape="circle", mat_width=0.2))
#'      popViewport()
#'
#'      pushViewport(viewport(x=0.75, y=0.25, width=1/2, height=1/2))
#'      grid.draw(get_shape_grob_fn("rect")(gp=gp))
#'      grid.draw(matGrob("blue", shape="rect", mat_width=c(0.2, 0.1, 0.3, 0.4)))
#'      popViewport()
#'  }
#'
#' @name grob_fn_helpers
NULL

#' @rdname grob_fn_helpers
#' @export
gridlinesGrob <- function(col, shape = "rect", shape_t = 90, lex = 1, name = NULL) {
    if (is_color_invisible(col)) return(nullGrob())
    gl <- gList()
    o <- 0.02
    if (shape == "rect") {
        lwd <- 8
        gp_gl <- gpar(col=col, lwd=lwd, lineend="butt", lex=lex)
        gl[[1]] <- linesGrob(x=0.5, gp=gp_gl)
        gl[[2]] <- linesGrob(y=0.5, gp=gp_gl)
    } else if (shape %in% c("circle", "kite", "halma", "oval")) {
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
            gp <- gpar(col=col, lwd=lwd, lex=lex)
            gl[[ii]] <- segmentsGrob(x[ii], y[ii], x[i_next], y[i_next], gp=gp)
        }
    }
    gl
}

#' @rdname grob_fn_helpers
#' @param mat_width Numeric vector of mat widths
#' @export
matGrob <- function(col, shape = "rect", shape_t = 90, mat_width = 0, name = NULL) {
    if (is_color_invisible(col) || all(mat_width==0))
        return(nullGrob())
    gp_mat <- gpar(col=NA, fill=col)
    if (shape == "rect") {
        rectMatGrobFn(mat_width)(gp=gp_mat)
    } else if (shape == "circle" || shape == "oval") {
        convexMatGrobFn(60, 0, mat_width[1])(gp=gp_mat)
    } else if (grepl("^convex", shape)) {
        convexMatGrobFn(get_n_vertices(shape), shape_t, mat_width[1])(gp=gp_mat)
    } else if (shape == "halma") {
        halmaMatGrobFn(mat_width)(gp=gp_mat)
    } else {
        stop(paste("Don't know how to add mat to shape", shape))
    }
}

#' @rdname grob_fn_helpers
#' @export
checkersGrob <- function(col, shape = "rect", shape_t=90, name = NULL) {
    if (is_color_invisible(col)) return(nullGrob(name=name))
    if (shape == "rect") {
        gl <- gList(
            rectGrob(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(col=NA, fill=col)),
            rectGrob(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(col=NA, fill=col))
        )
        gTree(children=gl, name=name)
    } else if (grepl("^concave", shape) || grepl("^convex", shape)) {
        n_vertices <- get_n_vertices(shape)
        t <- seq(0, 360, length.out=n_vertices+1) + shape_t
        nt <- length(t) - 1
        r <- 0.5
        x <- 0.5 + to_x(t, r)
        y <- 0.5 + to_y(t, r)
        gl <- gList()
        for (ii in 1:nt) {
            if (ii %% 2) {
                xs <- c(0.5, x[ii], x[ii+1])
                ys <- c(0.5, y[ii], y[ii+1])
                gl[[ii]] <- polygonGrob(xs, ys, gp=gpar(col=NA, fill=col))
            }
        }
        gTree(children=gl, name=name)
    } else {
        stop(paste("Don't know how to add checkers to shape", shape))
    }
}

#' @rdname grob_fn_helpers
#' @export
hexlinesGrob <- function(col, shape = "rect", name = NULL) {
    if (is_color_invisible(col)) return(nullGrob(name=name))
    if (shape == "rect") {
        ho <- 0.25
        hl_size <- 4
        gp <- gpar(col=col, lwd=hl_size)
        gl <- gList(
            segmentsGrob(0, 1 - ho, ho, 1, gp=gp),
            segmentsGrob(0, ho, ho, 0, gp=gp),
            segmentsGrob(1, ho, 1 - ho, 0, gp=gp),
            segmentsGrob(1, 1 - ho, 1 - ho, 1, gp=gp)
        )
        gTree(children=gl, name=name)
    } else {
        stop(paste("Don't know how to add hexlines to shape", shape))
    }
}

#' @rdname grob_fn_helpers
#' @param back Logical of whether back of the piece, in which case will reflect shape along vertical axis.
#' @export
get_shape_grob_fn <- function(shape, shape_t=90, shape_r=0.2, back=FALSE) {
    if (back) shape_t <- 180 - shape_t
    if (shape == "circle") {
        circleGrob
    } else if (shape == "rect") {
        rectGrob
    } else if (shape == "halma") {
        halmaGrob
    } else if (shape == "pyramid") {
        pyramidGrob
    } else if (grepl("^convex", shape)) {
        convexGrobFn(get_n_vertices(shape), shape_t)
    } else if (grepl("^concave", shape)) {
        concaveGrobFn(get_n_vertices(shape), shape_t, shape_r)
    } else if (shape == "kite") {
        kiteGrob
    } else if (shape == "oval") {
        ovalGrob
    } else {
        stop(paste("Don't know how to draw shape", shape))
    }
}

#' @rdname grob_fn_helpers
#' @export
is_color_invisible <- function(col) {
    if (is.na(col))
        return(TRUE)
    if (col == "transparent")
        return(TRUE)
    return(FALSE)
}
