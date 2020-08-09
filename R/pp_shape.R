#' Shape object for generating various grobs
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
#' \item{\code{mat_width}}{Numeric vector of mat widths.}
#' \item{\code{clip}}{\dQuote{clip grob} to perform polyclip operation with.
#'                    See \code{\link[gridGeometry]{grid.polyclip}} for more info.}
#' \item{\code{op}}{Polyclip operation to perform.
#'                  See \code{\link[gridGeometry]{grid.polyclip}} for more info.}
#' \item{\code{name}}{Grid grob \code{name} value.}
#' \item{\code{gp}}{Grid \code{gpar} list.  See \code{\link[grid]{gpar}} for more info.}
#' \item{\code{vp}}{Grid viewport or \code{NULL}.}
#' }
#' @section \code{pp_shape} R6 Class Methods:\describe{
#' \item{\code{checkers(name = NULL, gp = gpar(), vp = NULL)}}{Returns a grob of checkers for that shape.}
#' \item{\code{gridlines(name = NULL, gp = gpar(), vp = NULL)}}{Returns a grob of gridlines for that shape.}
#' \item{\code{hexlines(name = NULL, gp = gpar(), vp = NULL)}}{Returns a grob of hexlines for that shape.}
#' \item{\code{mat(mat_width = 0, name = NULL, gp = gpar(), vp = NULL)}}{
#'    Returns a grob for a matting \dQuote{mat} for that shape.}
#' \item{\code{polyclip(clip, op = "intersection", name = NULL, gp = gpar(), vp = NULL)}}{
#'    Returns a grob that is an \dQuote{intersection}, \dQuote{minus}, \dQuote{union}, or \dQuote{xor} of another grob.
#'    Note unlike \code{gridGeometry::polyclipGrob} it can directly work with a \code{pieceGrob} "clip grob" argument.}
#' \item{\code{shape(name = NULL, gp = gpar(), vp = NULL)}}{Returns a grob of the shape.}
#' }
#' @section \code{pp_shape} R6 Class Active Bindings:\describe{
#' \item{\code{label}}{The shape's label.}
#' \item{\code{theta}}{The shape's theta.}
#' \item{\code{radius}}{The shape's radius.}
#' \item{\code{back}}{A boolean of whether this is the shape's \dQuote{back} side.}
#' \item{\code{npc_coords}}{A named list of \dQuote{npc} coordinates along the perimeter of the shape.}
#' }
#' @param label Label of the shape.  One of \describe{
#'      \item{\dQuote{circle}}{Circle.}
#'      \item{\dQuote{convexN}}{An \code{N}-sided convex polygon.
#'                              \code{theta} controls which direction the first vertex is drawn.}
#'      \item{\dQuote{concaveN}}{A \dQuote{star} (concave) polygon with \code{N} \dQuote{points}.
#'                              \code{theta} controls which direction the first point is drawn.
#'                              \code{radius} controls the distance of the \dQuote{inner} vertices from the center.}
#'      \item{\dQuote{halma}}{A 2D outline of a \dQuote{Halma pawn}.}
#'      \item{\dQuote{kite}}{\dQuote{Kite} quadrilateral shape.}
#'      \item{\dQuote{meeple}}{A 2D outline of a \dQuote{meeple}.}
#'      \item{\dQuote{oval}}{Oval.}
#'      \item{\dQuote{pyramid}}{An \dQuote{Isosceles} triangle whose base is the bottom of the viewport.
#'                              Typically used to help draw the face of the \dQuote{pyramid} piece.}
#'      \item{\dQuote{rect}}{Rectangle.}
#'      \item{\dQuote{roundrect}}{\dQuote{Rounded} rectangle.  \code{radius} controls curvature of corners.}
#' }
#' @param theta \code{convex} and \code{concave} polygon shapes
#'                    use this to determine where the first point is drawn.
#' @param radius \code{concave} polygon and \code{roundrect} use this
#'                     to control appearance of the shape.
#' @param back Whether the shape should be reflected across a vertical line in the middle of the viewport.
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
#'
#'      grid.newpage()
#'      gp <- gpar(col="black", fill="yellow")
#'
#'      vp <- viewport(x=1/4, y=1/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("halma")$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=3/4, y=1/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("pyramid")$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=3/4, y=3/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("kite")$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=1/4, y=3/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("meeple")$shape(gp=gp, vp=vp))
#'
#'      grid.newpage()
#'      vp <- viewport(x=1/4, y=1/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("convex3", 0)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=3/4, y=1/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("convex4", 90)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=3/4, y=3/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("convex5", 180)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=1/4, y=3/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("convex6", 270)$shape(gp=gp, vp=vp))
#'
#'      grid.newpage()
#'      vp <- viewport(x=1/4, y=1/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("concave3", 0, 0.1)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=3/4, y=1/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("concave4", 90, 0.2)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=3/4, y=3/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("concave5", 180, 0.3)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=1/4, y=3/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("concave6", 270)$shape(gp=gp, vp=vp))
#'  }
#' @export
pp_shape <- function(label = "rect", theta = 90, radius = 0.2, back = FALSE) {
    Shape$new(label, theta, radius, back)
}

Shape <- R6Class("pp_shape",
    public = list(
        initialize = function(label = "rect", theta = 90,
                              radius = 0.2, back = FALSE) {
            if (!is_known_shape_label(label))
                stop("Don't recognize shape label ", label)
            private$shape_label <- label
            private$shape_theta <- theta
            private$shape_radius <- radius
            private$shape_back <- back
        },
        shape = function(name = NULL, gp = gpar(), vp = NULL) {
            private$shape_grob_fn()(name = name, gp = gp, vp = vp)
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
        },
        polyclip = function(clip, op = "intersection",
                            name = NULL, gp = gpar(), vp = NULL) {
            gTree(shape = self, clip = clip, op = op,
                  name = name, gp = gp, vp = vp, cl = "pp_polyclip")
        }),
    active = list(
        label = function() private$shape_label,
        theta = function() private$shape_theta,
        radius = function() private$shape_radius,
        back = function() private$shape_back,
        npc_coords = function() {
            label <- self$label
            theta <- self$theta
            radius <- self$radius
            back <- self$back
            if (label == "rect") {
                rect_xy
            } else if (grepl(label, "circle|oval")) {
                xy <- convex_xy(36, 90) #### increase number of vertices?
                xy$c <- rep("C1", length(xy$x))
                xy
            } else if (label == "kite") {
                kite_xy
            } else if (label == "halma") {
                halma_xy()
            } else if (label == "meeple") {
                meeple_xy
            } else if (label == "pyramid") {
                pyramid_xy
            } else if (label == "roundrect") {
                roundrect_xy(radius)
            } else if (grepl("^concave", label)) {
                if (back) theta <- 180 - theta
                concave_xy(get_n_vertices(label), theta, radius)
            } else if (grepl("^convex", label)) {
                if (back) theta <- 180 - theta
                convex_xy(get_n_vertices(label), theta)
            }
        }),
    private = list(
        shape_label = NULL,
        shape_theta = NULL,
        shape_radius = NULL,
        shape_back = NULL,
        shape_grob_fn = function() {
            label <- self$label
            if (label == "circle") {
                circleGrob
            } else if (label == "rect") {
                rectGrob
            } else if (label == "roundrect") {
                roundrectGrobFn(self$radius)
            } else {
                coords <- self$npc_coords
                polygonGrobFn(coords$x, coords$y)
            }
        })
)

polygonGrobFn <- function(x, y) {
    function(name=NULL, gp=gpar(), vp=NULL) polygonGrob(x, y, name=name, gp=gp, vp=vp)
}

roundrectGrobFn <- function(r = 0.05) {
    function(name=NULL, gp=gpar(), vp=NULL) roundrectGrob(r = unit(r, "snpc"), name=name, gp=gp, vp=vp)
}

is_known_shape_label <- function(label) {
    if (label %in% c("circle", "halma", "kite", "meeple", "oval", "pyramid", "rect", "roundrect")) {
        return(TRUE)
    }
    if (grepl("^concave[0-9]+$", label)) return(TRUE)
    if (grepl("^convex[0-9]+$", label)) return(TRUE)
    FALSE
}

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
    if (label %in% c("circle", "kite", "halma", "oval", "meeple")) {
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

    if (is_color_invisible(x$gp$fill) || nigh(mat_width, 0)) {
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
        gl <- gList(x$shape$polyclip(squares, "intersection"))
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

#' @export
makeContent.pp_polyclip <- function(x) {
    subject <- x$shape$shape()
    clip <- x$clip
    if (inherits(clip, "piece") || inherits(clip, "pmap_piece")) {
        clip <- gridGeometry::xyListPath(grobPoints(clip))
    }
    gl <- gList(gridGeometry::polyclipGrob(subject, clip, x$op))
    setChildren(x, gl)
}

# xy functions and/or constants
roundrect_xy <- function(shape_r) {
    # avoid opening a blank graphic device window if no graphic devices open
    if (length(grDevices::dev.list()) == 0) {
        pdf_file <- tempfile(fileext=".pdf")
        unlink(pdf_file)
        grDevices::pdf(pdf_file)
        on.exit(grDevices::dev.off())
    }
    coords <- grid::grobCoords(grid::roundrectGrob(r=grid::unit(shape_r, "snpc")), closed=TRUE)[[1]]
    x <- as.numeric(grid::convertX(grid::unit(coords$x, "in"), "npc"))
    y <- as.numeric(grid::convertY(grid::unit(coords$y, "in"), "npc"))
    list(x = x, y = y, c = rep("C1", length(x)))
}

kite_xy <- list(x = c(0.5, 0, 0.5, 1), y = c(1, 0.25, 0, 0.25), c = rep("C0", 4))
pyramid_xy <- list(x = c(0.5, 0, 1), y = c(1, 0, 0), c = rep("C0", 3))
rect_xy <- list(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), c = rep("C0", 4))

convex_xy <- function(n_vertices, t) {
    t <- seq(0, 360, length.out = n_vertices + 1) + t
    r <- 0.5
    x <- to_x(t, r) + 0.5
    y <- to_y(t, r) + 0.5
    list(x = utils::head(x, -1), y = utils::head(y, -1), c = rep("C0", n_vertices))
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
         y = c(0, 0.3, y[indices], 0.3, 0),
         c = c("C0", "C0", "C0", rep("C1", length(indices)-2),  "C0", "C0", "C0"))
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
    list(x = x, y = y, c = rep("C0", length(x)))
}

splice <- function(x0, x1) {
    vec <- as.numeric()
    for (ii in seq_along(x1)) {
        vec <- append(vec, x0[ii])
        vec <- append(vec, x1[ii])
    }
    append(vec, x0[ii+1])
}
