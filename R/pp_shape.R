#' shape object
#'
#' \code{pp_shape()} creates an R6 object with methods for creating various shape based grobs.
#'
#' \code{pp_shape} objects serve the following purposes:\enumerate{
#'  \item{Make it easier for developers to customize game piece appearances
#'        either through a "grob_fn" or "op_grob_fn" styles in \code{pp_cfg()}
#'        or manipulate a piece post drawing via functions like \code{grid::grid.edit()}.}
#'  \item{Used internally to generate \code{piecepackr}'s built-in game piece grobs.}
#'   }
#'
#' @section \code{pp_shape} R6 Class Method Arguments:\describe{
#' \item{\code{mat_width}}{Numeric vector of mat widths}
#' \item{\code{name}}{Grid grob \code{name} value.}
#' \item{\code{gp}}{Grid \code{gpar} list.}
#' \item{\code{vp}}{Grid viewport or \code{NULL}.}
#' }
#' @section \code{pp_shape} R6 Class Methods:\describe{
#' \item{\code{shape}}{Returns a grob of the shape.}
#' \item{\code{mat}}{Returns a grob for a matting \dQuote{mat} for that shape.}
#' \item{\code{checkers}}{Returns a grob of checkers for that shape.}
#' \item{\code{gridlines}}{Returns a grob of gridlines for that shape.}
#' \item{\code{hexlines}}{Returns a grob of hexlines for that shape.}
#' }
#' @param label Label of the shape.
#' @param theta \code{convex} and \code{concave} polygon shapes
#'                    use this to determine where the first point is drawn.
#' @param radius \code{concave} polygon and \code{roundrect} use this
#'                     to control appearance of the shape.
#' @param back Whether the shape should be reflected across a vertical line.
#' @examples
#'  if (require("grid")) {
#'      gp <- gpar(col="black", fill="yellow")
#'      rect <- pp_shape(label="rect")
#'      convex6 <- pp_shape(label="convex6")
#'      circle <- pp_shape(label="circle")
#'
#'      pushViewport(viewport(x=0.25, y=0.75, width=1/2, height=1/2))
#'      grid.draw(rect$shape(gp=gp))
#'      grid.draw(rect$gridlines(gp=gpar(col="blue", lex=4)))
#'      grid.draw(rect$hexlines(gp=gpar(col="green")))
#'      popViewport()
#'
#'      pushViewport(viewport(x=0.75, y=0.75, width=1/2, height=1/2))
#'      grid.draw(convex6$shape(gp=gp))
#'      grid.draw(convex6$checkers(gp=gpar(fill="blue")))
#'      popViewport()
#'
#'      pushViewport(viewport(x=0.25, y=0.25, width=1/2, height=1/2))
#'      grid.draw(circle$shape(gp=gp))
#'      grid.draw(circle$mat(mat_width=0.2, gp=gpar(fill="blue")))
#'      popViewport()
#'
#'      pushViewport(viewport(x=0.75, y=0.25, width=1/2, height=1/2))
#'      grid.draw(rect$shape(gp=gp))
#'      grid.draw(rect$mat(mat_width=c(0.2, 0.1, 0.3, 0.4), gp=gpar(fill="blue")))
#'      popViewport()
#'  }
#' @export
pp_shape <- function(label = "rect", theta = 90, radius = 0.2, back = FALSE) {
    Shape$new(label, theta, radius, back)
}

Shape <- R6Class("pp_shape",
    public = list(label = NULL,
                  theta = NULL,
                  radius = NULL,
                  back = NULL,
                  initialize = function(label = "rect", theta = 90,
                                        radius = 0.2, back = FALSE) {
                      self$label <- label
                      self$theta <- theta
                      self$radius <- radius
                      self$back <- back
                  },
                  shape = function(name = NULL, gp = gpar(), vp = NULL) {
                      fn <- shape_grob_fn(self$label, self$theta,
                                          self$radius, self$back)
                      fn(name = name, gp = gp, vp = vp)
                  },
                  gridlines = function(name = NULL, gp = gpar(), vp = NULL) {
                      gTree(shape = self, name = name, gp = gp, vp = vp, cl = "gridlines")
                  },
                  mat = function(mat_width = 0, name = NULL, gp = gpar(), vp = NULL) {
                      gTree(mat_width = mat_width, shape = self,
                            name = name, gp = gp, vp = vp, cl = "mat")
                  },
                  checkers = function(name = NULL, gp = gpar(), vp = NULL) {
                      gTree(shape = self, name = name, gp = gp, vp = vp, cl = "checkers")
                  },
                  hexlines = function(name = NULL, gp = gpar(), vp = NULL) {
                      gTree(shape = self, name = name, gp = gp, vp = vp, cl = "hexlines")
                  }))

#' @export
makeContext.gridlines <- function(x) {
    if (is.null(x$gp$col)) x$gp$col <- "black"
    if (x$shape$label %in% c("rect", "roundrect")) {
        x$gp$lwd <- 8
        x$gp$lineend <- "butt"
    } else {
        x$gp$lwd <- 4
    }
    x
}

#' @export
makeContent.gridlines <- function(x) {
    label <- x$shape$label
    theta <- x$shape$theta
    radius <- x$shape$radius
    back <- x$shape$back
    if (is_color_invisible(x$gp$col)) {
        return(setChildren(x, gList(nullGrob(name = "invisible"))))
    }
    if (label %in% c("circle", "kite", "halma", "oval")) {
        stop(paste("Don't know how to add grid lines to shape", label))
    }
    gl <- gList()
    if (label %in% c("rect", "roundrect")) {
        gl[[1]] <- linesGrob(x=0.5, name="line1")
        gl[[2]] <- linesGrob(y=0.5, name="line2")
    } else {
        theta <- if (back) 180 - theta else theta
        n_vertices <- get_n_vertices(label)
        t <- seq(0, 360, length.out=n_vertices+1) + theta
        t <- t[1:(length(t)-1)]
        nt <- length(t)
        n <- floor(nt / 2)
        r <- 0.5 - 0.01
        xc <- 0.5 + to_x(t, r)
        yc <- 0.5 + to_y(t, r)
        for (ii in 1:nt) {
            i_next <- ii+n
            if (i_next > nt)
                i_next <- i_next %% nt
            gl[[ii]] <- segmentsGrob(xc[ii], yc[ii], xc[i_next], yc[i_next], name=paste0("line", ii))
        }
    }
    setChildren(x, gl)
}

#' @export
makeContext.mat <- function(x) {
    if (is.null(x$gp$col)) x$gp$col <- NA
    if (is.null(x$gp$fill)) x$gp$fill <- NA
    x
}

#' @export
makeContent.mat <- function(x) {
    mat_width <- x$mat_width
    label <- x$shape$label
    theta <- x$shape$theta
    radius <- x$shape$radius
    back <- x$shape$back

    is_mat_width_zero <- near(mat_width, rep(0, length(mat_width)))
    if (is_color_invisible(x$gp$fill) || is_mat_width_zero) {
        gl <- gList(nullGrob())
    } else if (label == "rect") {
        gl <- gList(rectMatGrobFn(mat_width)())
    } else if (label == "oval") {
        gl <- gList(convexMatGrobFn(60, 0, mat_width[1])())
    } else if (grepl("^convex", label)) {
        if (back) theta <- 180 - theta
        gl <- gList(convexMatGrobFn(get_n_vertices(label), theta, mat_width[1])())
    } else if (label == "halma") {
        gl <- gList(halmaMatGrobFn(mat_width)())
    } else if (label %in% c("circle", "roundrect")) {
        gl <- gList(diffMatGrobFn(mat_width, x$shape)())
    } else {
        stop(paste("Don't know how to add mat to shape", label))
    }
    setChildren(x, gl)
}

#' @export
makeContext.checkers <- function(x) {
    if (is.null(x$gp$col)) x$gp$col <- NA
    x
}

#' @export
makeContent.checkers <- function(x) {
    label <- x$shape$label
    theta <- x$shape$theta
    back <- x$shape$back
    if (is_color_invisible(x$gp$fill)) {
        gl <- gList(nullGrob())
    } else if (label == "rect") {
        gl <- gList(
            rectGrob(x=0.25, y=0.25, width=0.5, height=0.5),
            rectGrob(x=0.75, y=0.75, width=0.5, height=0.5)
        )
    } else if (grepl("^concave", label) || grepl("^convex", label)) {
        if (back) theta <- 180 - theta
        n_vertices <- get_n_vertices(label)
        t <- seq(0, 360, length.out=n_vertices+1) + theta
        nt <- length(t) - 1
        r <- 0.5
        xc <- 0.5 + to_x(t, r)
        yc <- 0.5 + to_y(t, r)
        gl <- gList()
        for (ii in 1:nt) {
            if (ii %% 2) {
                xs <- c(0.5, xc[ii], xc[ii+1])
                ys <- c(0.5, yc[ii], yc[ii+1])
                gl[[ii]] <- polygonGrob(xs, ys)
            }
        }
    } else {
        squares <- gList(
            rectGrob(x=0.25, y=0.25, width=0.5, height=0.5),
            rectGrob(x=0.75, y=0.75, width=0.5, height=0.5)
        )
        shape <- x$shape$shape()
        checkers <- gridGeometry::polyclipGrob(squares, shape, op="intersection")
        gl <- gList(checkers)
    }
    setChildren(x, gl)
}

#' @export
makeContext.hexlines <- function(x) {
    if (is.null(x$gp$lwd)) x$gp$lwd <- 4
    x
}

#' @export
makeContent.hexlines <- function(x) {
    label <- x$shape$label
    if (is_color_invisible(x$gp$col)) {
        gl <- gList(nullGrob())
    } else if (label %in% c("rect", "roundrect")) {
        ho <- 0.25
        gl <- gList(
            segmentsGrob(0, 1 - ho, ho, 1),
            segmentsGrob(0, ho, ho, 0),
            segmentsGrob(1, ho, 1 - ho, 0),
            segmentsGrob(1, 1 - ho, 1 - ho, 1)
        )
    } else {
        stop(paste("Don't know how to add hexlines to shape", label))
    }
    setChildren(x, gl)
}

shape_grob_fn <- function(label, theta = 90, radius = 0.2, back = FALSE) {
    if (back) theta <- 180 - theta
    if (label == "circle") {
        circleGrob
    } else if (label == "rect") {
        rectGrob
    } else if (label == "halma") {
        halmaGrob
    } else if (label == "pyramid") {
        pyramidGrob
    } else if (grepl("^convex", label)) {
        convexGrobFn(get_n_vertices(label), theta)
    } else if (grepl("^concave", label)) {
        concaveGrobFn(get_n_vertices(label), theta, radius)
    } else if (label == "kite") {
        kiteGrob
    } else if (label == "roundrect") {
        roundrectGrobFn(radius)
    } else if (label == "oval") {
        ovalGrob
    } else {
        stop(paste("Don't know how to draw shape", label))
    }
}
