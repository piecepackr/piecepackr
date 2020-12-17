#' Deprecated functions
#'
#' These functions are Deprecated in this release of piecepackr,
#' they will be marked as Defunct and removed in a future version.
#'
#' \enumerate{
#' \item{For \code{get_shape_grob_fn} use \code{pp_shape()$shape} instead.}
#' \item{For \code{gridlinesGrob()} use \code{pp_shape()$gridlines()} instead.}
#' \item{For \code{matGrob()} use \code{pp_shape()$mat()} instead.}
#' \item{For \code{checkersGrob()}() use \code{pp_shape()$checkers()} instead.}
#' \item{For \code{hexlinesGrob()} use \code{pp_shape()$hexlines()} instead.}
#' \item{For \code{halmaGrob()} use \code{pp_shape("halma")$shape()} instead.}
#' \item{For \code{kiteGrob()} use \code{pp_shape("kite")$shape()} instead.}
#' \item{For \code{pyramidGrob()} use \code{pp_shape("pyramid")$shape()} instead.}
#' \item{For \code{convexGrobFn(n, t)} use \code{pp_shape(paste0("convex", n), t)$shape} instead.}
#' \item{For \code{concaveGrobFn(n, t, r)} use \code{pp_shape(paste0("concave", n), t, r)$shape} instead.}
#' }
#' @param col Color
#' @param lex Scales width of line.
#' @param mat_width Numeric vector of mat widths
#' @param shape Label of shape
#' @param shape_t Angle (in degrees) of first vertex of shape (ignored by many shapes).
#' @param shape_r Radial distance (from 0 to 0.5) (ignored by most shapes)
#' @param back Logical of whether back of the piece, in which case will reflect shape along vertical axis.
#' @param name A character identifier (for grid)
#' @param gp An object of class \sQuote{gpar}
#' @param vp A \code{grid} viewport object (or NULL).
#' @param n_vertices Number of vertices
#' @param t Angle (in degrees) of first vertex of shape
#' @param r Radial distance (from 0 to 0.5)
#' @examples
#'  if (require("grid")) {
#'      if (getRversion() < "4.0.0") suppressWarnings <- backports::suppressWarnings
#'      suppressWarnings({
#'          gp <- gpar(col="black", fill="yellow")
#'          pushViewport(viewport(x=0.25, y=0.75, width=1/2, height=1/2))
#'          grid.draw(get_shape_grob_fn("rect")(gp=gp))
#'          grid.draw(gridlinesGrob("blue", lex=4))
#'          grid.draw(hexlinesGrob("green"))
#'          popViewport()
#'
#'          pushViewport(viewport(x=0.75, y=0.75, width=1/2, height=1/2))
#'          grid.draw(get_shape_grob_fn("convex6")(gp=gp))
#'          grid.draw(checkersGrob("blue", shape="convex6"))
#'          popViewport()
#'
#'          pushViewport(viewport(x=0.25, y=0.25, width=1/2, height=1/2))
#'          grid.draw(get_shape_grob_fn("circle")(gp=gp))
#'          grid.draw(matGrob("blue", shape="circle", mat_width=0.2))
#'          popViewport()
#'
#'          pushViewport(viewport(x=0.75, y=0.25, width=1/2, height=1/2))
#'          grid.draw(get_shape_grob_fn("rect")(gp=gp))
#'          grid.draw(matGrob("blue", shape="rect", mat_width=c(0.2, 0.1, 0.3, 0.4)))
#'          popViewport()
#'
#'          grid.newpage()
#'          gp <- gpar(col="black", fill="yellow")
#'
#'          vp <- viewport(x=1/3-1/6, width=1/3)
#'          grid.draw(halmaGrob(gp=gp, vp=vp))
#'          vp <- viewport(x=2/3-1/6, width=1/3)
#'          grid.draw(pyramidGrob(gp=gp, vp=vp))
#'          vp <- viewport(x=3/3-1/6, width=1/3)
#'          grid.draw(kiteGrob(gp=gp, vp=vp))
#'
#'          grid.newpage()
#'          vp <- viewport(x=1/4, y=1/4, width=1/2, height=1/2)
#'          grid.draw(convexGrobFn(3, 0)(gp=gp, vp=vp))
#'          vp <- viewport(x=3/4, y=1/4, width=1/2, height=1/2)
#'          grid.draw(convexGrobFn(4, 90)(gp=gp, vp=vp))
#'          vp <- viewport(x=3/4, y=3/4, width=1/2, height=1/2)
#'          grid.draw(convexGrobFn(5, 180)(gp=gp, vp=vp))
#'          vp <- viewport(x=1/4, y=3/4, width=1/2, height=1/2)
#'          grid.draw(convexGrobFn(6, 270)(gp=gp, vp=vp))
#'
#'          grid.newpage()
#'          vp <- viewport(x=1/4, y=1/4, width=1/2, height=1/2)
#'          grid.draw(concaveGrobFn(3, 0, 0.1)(gp=gp, vp=vp))
#'          vp <- viewport(x=3/4, y=1/4, width=1/2, height=1/2)
#'          grid.draw(concaveGrobFn(4, 90, 0.2)(gp=gp, vp=vp))
#'          vp <- viewport(x=3/4, y=3/4, width=1/2, height=1/2)
#'          grid.draw(concaveGrobFn(5, 180, 0.3)(gp=gp, vp=vp))
#'          vp <- viewport(x=1/4, y=3/4, width=1/2, height=1/2)
#'          grid.draw(concaveGrobFn(6, 270)(gp=gp, vp=vp))
#'      }, classes = "deprecatedWarning")
#'  }
#' @name piecepackr-deprecated
NULL

#' @rdname piecepackr-deprecated
#' @export
halmaGrob <- function(name=NULL, gp=gpar(), vp=NULL) {
    .Deprecated('pp_shape("halma")$shape()')
    pp_shape("halma")$shape(name=name, gp=gp, vp=vp)
}

#' @rdname piecepackr-deprecated
#' @export
kiteGrob <- function(name=NULL, gp=gpar(), vp=NULL) {
    .Deprecated('pp_shape("kite")$shape()')
    pp_shape("kite")$shape(name=name, gp=gp, vp=vp)
}

#' @rdname piecepackr-deprecated
#' @export
pyramidGrob <- function(name=NULL, gp=gpar(), vp=NULL) {
    .Deprecated('pp_shape("pyramid")$shape()')
    pp_shape("pyramid")$shape(name=name, gp=gp, vp=vp)
}

#' @rdname piecepackr-deprecated
#' @export
convexGrobFn <- function(n_vertices, t) {
    .Deprecated('pp_shape("convexN", t)$shape')
    label <- paste0("convex", n_vertices)
    pp_shape(label, t)$shape
}

#' @rdname piecepackr-deprecated
#' @export
concaveGrobFn <- function(n_vertices, t, r=0.2) {
    .Deprecated('pp_shape("concaveN", t, r)$shape')
    label <- paste0("concave", n_vertices)
    pp_shape(label, t, r)$shape
}

#' @rdname piecepackr-deprecated
#' @export
gridlinesGrob <- function(col, shape = "rect", shape_t = 90, lex = 1, name = NULL) {
    .Deprecated("pp_shape()$gridlines()")
    shape <- pp_shape(shape, shape_t)
    shape$gridlines(name = name, gp = gpar(col = col, lex = lex))
}

#' @rdname piecepackr-deprecated
#' @param mat_width Numeric vector of mat widths
#' @export
matGrob <- function(col, shape = "rect", shape_t = 90, mat_width = 0, name = NULL) {
    .Deprecated("pp_shape()$mat()")
    shape <- pp_shape(shape, shape_t)
    shape$mat(mat_width = mat_width, name = name, gp = gpar(fill = col))
}

#' @rdname piecepackr-deprecated
#' @export
checkersGrob <- function(col, shape = "rect", shape_t=90, name = NULL) {
    .Deprecated("pp_shape()$checkers()")
    shape <- pp_shape(shape, shape_t)
    shape$checkers(name = name, gp = gpar(fill = col))
}

#' @rdname piecepackr-deprecated
#' @export
hexlinesGrob <- function(col, shape = "rect", name = NULL) {
    .Deprecated("pp_shape()$hexlines()")
    shape <- pp_shape(shape)
    shape$hexlines(name = name, gp = gpar(col = col))
}

#' @rdname piecepackr-deprecated
#' @export
get_shape_grob_fn <- function(shape, shape_t=90, shape_r=0.2, back=FALSE) {
    .Deprecated("pp_shape()$shape")
    shape <- pp_shape(shape, shape_t, shape_r, back)
    shape$shape
}
