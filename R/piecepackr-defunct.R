#' Defunct functions
#'
#' These functions are Defunct and have been removed from piecepackr.
#'
#' \enumerate{
#' \item{For `get_shape_grob_fn` use `pp_shape()$shape` instead.}
#' \item{For `gridlinesGrob()` use `pp_shape()$gridlines()` instead.}
#' \item{For `matGrob()` use `pp_shape()$mat()` instead.}
#' \item{For `checkersGrob()`() use `pp_shape()$checkers()` instead.}
#' \item{For `hexlinesGrob()` use `pp_shape()$hexlines()` instead.}
#' \item{For `halmaGrob()` use `pp_shape("halma")$shape()` instead.}
#' \item{For `kiteGrob()` use `pp_shape("kite")$shape()` instead.}
#' \item{For `pyramidGrob()` use `pp_shape("pyramid")$shape()` instead.}
#' \item{For `convexGrobFn(n, t)` use `pp_shape(paste0("convex", n), t)$shape` instead.}
#' \item{For `concaveGrobFn(n, t, r)` use `pp_shape(paste0("concave", n), t, r)$shape` instead.}
#' }
#' @param ... Ignored
#' @name piecepackr-defunct
NULL

#' @rdname piecepackr-defunct
halmaGrob <- function(...) {
    .Defunct('pp_shape("halma")$shape()')
}

#' @rdname piecepackr-defunct
kiteGrob <- function(...) {
    .Defunct('pp_shape("kite")$shape()')
}

#' @rdname piecepackr-defunct
pyramidGrob <- function(...) {
    .Defunct('pp_shape("pyramid")$shape()')
}

#' @rdname piecepackr-defunct
convexGrobFn <- function(...) {
    .Defunct('pp_shape("convexN", t)$shape')
}

#' @rdname piecepackr-defunct
concaveGrobFn <- function(...) {
    .Defunct('pp_shape("concaveN", t, r)$shape')
}

#' @rdname piecepackr-defunct
gridlinesGrob <- function(...) {
    .Defunct("pp_shape()$gridlines()")
}

#' @rdname piecepackr-defunct
matGrob <- function(...) {
    .Defunct("pp_shape()$mat()")
}

#' @rdname piecepackr-defunct
checkersGrob <- function(...) {
    .Defunct("pp_shape()$checkers()")
}

#' @rdname piecepackr-defunct
hexlinesGrob <- function(...) {
    .Defunct("pp_shape()$hexlines()")
}

#' @rdname piecepackr-defunct
get_shape_grob_fn <- function(...) {
    .Defunct("pp_shape()$shape")
}
