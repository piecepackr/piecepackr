#' Shape object for generating various grobs
#'
#' `pp_shape()` creates an R6 object with methods for creating various shape based grobs.
#'
#' `pp_shape` objects serve the following purposes:\enumerate{
#'  \item{Make it easier for developers to customize game piece appearances
#'        either through a "grob_fn" or "op_grob_fn" styles in `pp_cfg()`
#'        or manipulate a piece post drawing via functions like `grid::grid.edit()`.}
#'  \item{Used internally to generate `piecepackr`'s built-in game piece grobs.}
#'   }
#'
#' @section `pp_shape` R6 Class Method Arguments:\describe{
#' \item{`mat_width`}{Numeric vector of mat widths.}
#' \item{`clip`}{\dQuote{clip grob} to perform polyclip operation with.
#'                    See [gridGeometry::grid.polyclip()] for more info.}
#' \item{`op`}{Polyclip operation to perform.
#'                  See [gridGeometry::grid.polyclip()] for more info.}
#' \item{`pattern`}{Pattern to fill in shape with.
#'                  See [gridpattern::patternGrob()] for more info.}
#' \item{`...`}{Passed to `gridpattern::patternGrob()`.}
#' \item{`name`}{Grid grob `name` value.}
#' \item{`gp`}{Grid `gpar` list.  See [grid::gpar()] for more info.}
#' \item{`vp`}{Grid viewport or `NULL`.}
#' }
#' @section `pp_shape` R6 Class Methods:\describe{
#' \item{`checkers(name = NULL, gp = gpar(), vp = NULL)`}{Returns a grob of checkers for that shape.}
#' \item{`gridlines(name = NULL, gp = gpar(), vp = NULL)`}{Returns a grob of gridlines for that shape.}
#' \item{`hexlines(name = NULL, gp = gpar(), vp = NULL)`}{Returns a grob of hexlines for that shape.}
#' \item{`mat(mat_width = 0, name = NULL, gp = gpar(), vp = NULL)`}{
#'    Returns a grob for a matting \dQuote{mat} for that shape.}
#' \item{`pattern(pattern = "stripe", ..., name = NULL, gp = gpar(), vp = NULL)`}{
#'       Fills in the shape's `npc_coords` with a pattern.
#'       See [gridpattern::patternGrob()] for more information.
#' }
#' \item{`polyclip(clip, op = "intersection", name = NULL, gp = gpar(), vp = NULL)`}{
#'    Returns a grob that is an \dQuote{intersection}, \dQuote{minus}, \dQuote{union}, or \dQuote{xor} of another grob.
#'    Note unlike `gridGeometry::polyclipGrob` it can directly work with a `pieceGrob` "clip grob" argument.}
#' \item{`shape(name = NULL, gp = gpar(), vp = NULL)`}{Returns a grob of the shape.}
#' }
#' @section `pp_shape` R6 Class Active Bindings:\describe{
#' \item{`label`}{The shape's label.}
#' \item{`theta`}{The shape's theta.}
#' \item{`radius`}{The shape's radius.}
#' \item{`back`}{A boolean of whether this is the shape's \dQuote{back} side.}
#' \item{`npc_coords`}{A named list of \dQuote{npc} coordinates along the perimeter of the shape.}
#' }
#' @param label Label of the shape.  One of \describe{
#'      \item{\dQuote{circle}}{Circle.}
#'      \item{\dQuote{convexN}}{An `N`-sided convex polygon.
#'                              `theta` controls which direction the first vertex is drawn.}
#'      \item{\dQuote{concaveN}}{A \dQuote{star} (concave) polygon with `N` \dQuote{points}.
#'                              `theta` controls which direction the first point is drawn.
#'                              `radius` controls the distance of the \dQuote{inner} vertices from the center.}
#'      \item{\dQuote{halma}}{A 2D outline of a \dQuote{Halma pawn}.}
#'      \item{\dQuote{kite}}{\dQuote{Kite} quadrilateral shape.}
#'      \item{\dQuote{meeple}}{A 2D outline of a \dQuote{meeple}.}
#'      \item{\dQuote{oval}}{Oval.}
#'      \item{\dQuote{pyramid}}{An \dQuote{Isosceles} triangle whose base is the bottom of the viewport.
#'                              Typically used to help draw the face of the \dQuote{pyramid} piece.}
#'      \item{\dQuote{rect}}{Rectangle.}
#'      \item{\dQuote{roundrect}}{\dQuote{Rounded} rectangle.  `radius` controls curvature of corners.}
#' }
#' @param theta `convex` and `concave` polygon shapes
#'                    use this to determine where the first point is drawn.
#' @param radius `concave` polygon and `roundrect` use this
#'                     to control appearance of the shape.
#' @param back Whether the shape should be reflected across a vertical line in the middle of the viewport.
#' @examples
#'  if (require("grid", quietly = TRUE)) {
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
#'  if (require("grid", quietly = TRUE)) {
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
#'  }
#'  if (require("grid", quietly = TRUE)) {
#'      grid.newpage()
#'      vp <- viewport(x=1/4, y=1/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("convex3", 0)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=3/4, y=1/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("convex4", 90)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=3/4, y=3/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("convex5", 180)$shape(gp=gp, vp=vp))
#'      vp <- viewport(x=1/4, y=3/4, width=1/2, height=1/2)
#'      grid.draw(pp_shape("convex6", 270)$shape(gp=gp, vp=vp))
#'  }
#'  if (require("grid", quietly = TRUE)) {
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
#'  if (require("grid", quietly = TRUE) &&
#'      requireNamespace("gridpattern", quietly = TRUE)) {
#'      grid.newpage()
#'      hex <- pp_shape("convex6")
#'      gp <- gpar(fill = c("blue", "yellow", "red"), col = "black")
#'      grid.draw(hex$pattern("polygon_tiling", gp = gp, spacing = 0.1,
#'                            type = "truncated_trihexagonal"))
#'      gp <- gpar(fill = "black", col = NA)
#'      grid.draw(hex$mat(mat_width = 0.025, gp = gp))
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
                abort(paste("Don't recognize shape label", label))
            if (is_angle(theta))
                theta <- as.double(theta, "degrees")
            private$shape_label <- label
            private$shape_theta <- theta
            private$shape_radius <- radius
            private$shape_back <- back
        },
        shape = function(name = NULL, gp = gpar(), vp = NULL, mat_width = 0) {
            private$shape_grob_fn(mat_width = mat_width)(name = name, gp = gp, vp = vp)
        },
        gridlines = function(name = NULL, gp = gpar(), vp = NULL, mat_width = 0) {
            gTree(shape = self, mat_width = mat_width,
                  name = name, gp = gp, vp = vp, cl = "gridlines")
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
        pattern = function(pattern = "stripe", ..., name = NULL, gp = gpar(), vp = NULL) {
            assert_suggested("gridpattern")
            xy <- self$npc_coords
            gridpattern::patternGrob(pattern, xy$x, xy$y, ...,
                                     name = name, gp = gp, vp = vp)
        },
        polyclip = function(clip, op = "intersection",
                            name = NULL, gp = gpar(), vp = NULL) {
            gTree(shape = self, clip = clip, op = op,
                  name = name, gp = gp, vp = vp, cl = "pp_polyclip")
        }),
    active = list(
        back = function() private$shape_back,
        convex = function() {
            if (grepl(self$label, "^concave") ||
                self$label %in% c("halma", "meeple"))
                FALSE
            else
                TRUE
        },
        label = function() private$shape_label,
        npc_coords = function() {
            label <- self$label
            theta <- self$theta
            radius <- self$radius
            back <- self$back
            if (label == "rect") {
                rect_xy
            } else if (grepl(label, "circle|oval")) {
                xy <- convex_xy(72, 90) #### increase number of vertices?
                xy$c <- rep("C1", length(xy$x))
                xy
            } else if (label == "kite") {
                if (back) theta <- 180 - theta
                kite_xy(theta, radius)
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
        },
        radius = function() private$shape_radius,
        theta = function() private$shape_theta
        ),
    private = list(
        shape_label = NULL,
        shape_theta = NULL,
        shape_radius = NULL,
        shape_back = NULL,
        shape_grob_fn = function(mat_width = 0) {
            label <- self$label
            theta <- self$theta
            back <- self$back
            if (label == "rect") {
                rectGrobFn(mat_width)
            } else if (label == "circle") {
                circleGrobFn(r = 0.5 - mat_width[1])
            } else if (label == "roundrect") {
                roundrectGrobFn(self$radius)
            } else if (grepl("^convex", label)) {
                if (back) theta <- 180 - theta
                coords <- convex_xy(get_n_vertices(label), theta, 0.5 - mat_width[1])
                polygonGrobFn(coords$x, coords$y)
            } else if (label == "oval") {
                coords <- convex_xy(72, 90, 0.5 - mat_width[1])
                polygonGrobFn(coords$x, coords$y)
            } else {
                #### for halma and roundrect no mat shrinkage (yet)
                #### For other shapes no mat support at all
                coords <- self$npc_coords
                polygonGrobFn(coords$x, coords$y)
            }
        })
)

rectGrobFn <- function(mat_width = 0) {
    mat_width <- rep(mat_width, length.out = 4L)
    t <- 1 - mat_width[1]
    r <- 1 - mat_width[2]
    b <- mat_width[3]
    l <- mat_width[4]
    function(name=NULL, gp=gpar(), vp=NULL) polygonGrob(x = c(l, l, r, r), y = c(t, b, b, t),
                                                       name=name, gp=gp, vp=vp)
}

circleGrobFn <- function(r = 0.5) {
    function(name=NULL, gp=gpar(), vp=NULL) circleGrob(r = unit(r, "snpc"), name=name, gp=gp, vp=vp)
}

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
    x$gp$col <- x$gp$col %||% "black"
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
        abort(paste("Don't know how to add grid lines to shape", label))
    }
    gl <- gList()
    if (label %in% c("rect", "roundrect")) {
        width <- rep(x$mat_width, 4)
        gl[[1]] <- segmentsGrob(0.5, width[3] , 0.5, 1 - width[1], name="line1")
        gl[[2]] <- segmentsGrob(width[4], 0.5, 1 - width[2] , 0.5, name="line2")
    } else {
        theta <- if (back) 180 - theta else theta
        n_vertices <- get_n_vertices(label)
        t <- seq(0, 360, length.out=n_vertices+1) + theta
        t <- t[1:(length(t)-1)]
        nt <- length(t)
        n <- floor(nt / 2)
        r <- (0.5 - x$mat_width[1]) - 0.01
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
    x$gp$col <- x$gp$col %||% NA_character_
    x$gp$fill <- x$gp$fill %||% NA_character_
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
        gl <- gList(convexMatGrobFn(72, 90, mat_width[1])())
    } else if (grepl("^convex", label)) {
        if (back) theta <- 180 - theta
        gl <- gList(convexMatGrobFn(get_n_vertices(label), theta, mat_width[1])())
    } else if (label == "halma") {
        gl <- gList(halmaMatGrobFn(mat_width)())
    } else if (label %in% c("circle", "roundrect")) {
        gl <- gList(diffMatGrobFn(mat_width, x$shape)())
    } else {
        abort(paste("Don't know how to add mat to shape", label))
    }
    setChildren(x, gl)
}

#' @export
makeContext.checkers <- function(x) {
    x$gp$col <- x$gp$col %||% NA_character_
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
    x$gp$lwd <- x$gp$lwd %||% 4
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
        abort(paste("Don't know how to add hexlines to shape", label))
    }
    setChildren(x, gl)
}

#' @export
makeContent.pp_polyclip <- function(x) {
    subject <- x$shape$shape()
    clip <- x$clip
    if (inherits(clip, "pp_grobCoords") || inherits(clip, "pmap_piece")) {
        xylist <- grid::grobCoords(clip, closed = TRUE)
        clip <- gridGeometry::xyListPath(xylist)
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
    coords <- grid::grobCoords(grid::roundrectGrob(r=grid::unit(shape_r, "snpc")),
                               closed=TRUE)[[1]]
    x <- convertX(grid::unit(coords$x, "in"), "npc", valueOnly = TRUE)
    y <- convertY(grid::unit(coords$y, "in"), "npc", valueOnly = TRUE)
    list(x = x, y = y, c = rep("C1", length(x)))
}

kite_xy <- function(t = 90, r = 0.25) {
    t <- t %% 360
    stopifnot(nigh(t, 0) || nigh(t, 90) || nigh(t, 180) || nigh(t, 270))
    if (nigh(t, 0)) {
        list(x = c(1, 0.5 - r, 0, 0.5 - r), y = c(0.5, 1, 0.5, 0), c = rep("C0", 4))
    } else if (nigh(t, 90)) {
        list(x = c(0.5, 0, 0.5, 1), y = c(1, 0.5 - r, 0, 0.5 - r), c = rep("C0", 4))
    } else if (nigh(t, 180)) {
        list(x = c(0, 0.5 + r, 1, 0.5 + r), y = c(0.5, 0, 0.5, 1), c = rep("C0", 4))
    } else {
        list(x = c(0.5, 1, 0.5, 0), y = c(0, 0.5 + r, 1, 0.5 + r), c = rep("C0", 4))
    }
}
pyramid_xy <- list(x = c(0.5, 0, 1), y = c(1, 0, 0), c = rep("C0", 3))
rect_xy <- list(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), c = rep("C0", 4))

convex_xy <- function(n_vertices, t, r = 0.5) {
    t <- seq(0, 360, length.out = n_vertices + 1) + t
    x <- to_x(t, r) + 0.5
    y <- to_y(t, r) + 0.5
    list(x = utils::head(x, -1),
         y = utils::head(y, -1),
         c = rep("C0", n_vertices))
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
