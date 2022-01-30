#' Defunct functions
#'
#' These functions are Defunct and have been removed from piecepackr.
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
