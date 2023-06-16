#' @import grid

#' @importFrom affiner as_coord2d as_coord3d convex_hull degrees
#' @importFrom grDevices dev.capabilities dev.new devAskNewPage
#' @importFrom R6 R6Class
#' @importFrom rlang .data abort inform warn %||%
#' @importFrom tibble tibble
#' @importFrom utils head packageDescription packageVersion tail
NULL

hasName <- function(x, name) match(name, names(x), nomatch = 0L) > 0L
