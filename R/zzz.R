#' @import grid
#' @importFrom R6 R6Class
#' @importFrom grDevices dev.new devAskNewPage
#' @importFrom rlang .data abort inform warn %||%
#' @importFrom utils head packageDescription tail
NULL

hasName <- function(x, name) match(name, names(x), nomatch = 0L) > 0L
