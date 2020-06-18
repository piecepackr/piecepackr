#' Deprecated functions
#'
#' These functions are Deprecated in this release of piecepackr,
#' they will be marked as Defunct and removed in a future version.
#'
#' \enumerate{
#' \item{\code{get_shape_grob_fn} returns a function that returns a grob of the piece shape.
#'       Use \code{pp_shape()$shape} instead.}
#' \item{\code{gridlinesGrob()} returns a grob that produces gridlines.  Use \code{pp_shape()$gridlines()} instead.}
#' \item{\code{matGrob()} returns a grob that produces a mat.  Use \code{pp_shape()$mat()} instead.}
#' \item{\code{checkersGrob()}() returns a grob that adds checkers.  Use \code{pp_shape()$checkers()} instead.}
#' \item{\code{hexlinesGrob()} returns a grob that adds hexlines.  Use \code{pp_shape()$hexlines()} instead.}
#' }
#' @param col Color
#' @param lex Scales width of line.
#' @param mat_width Numeric vector of mat widths
#' @param name grob 'name' value
#' @param shape Label of shape
#' @param shape_t Angle (in degrees) of first vertex of shape (ignored by many shapes).
#' @param shape_r Radial distance (from 0 to 0.5) (ignored by most shapes)
#' @param back Logical of whether back of the piece, in which case will reflect shape along vertical axis.
#' @examples
#'  if (require("grid")) {
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
#'      }, classes = "deprecatedWarning")
#'  }
#' @name piecepackr-deprecated
NULL

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
