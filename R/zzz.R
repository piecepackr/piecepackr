#' @import grid

#' @importFrom R6 R6Class

#' @importFrom grDevices dev.capabilities dev.new devAskNewPage
#' @importFrom rlang .data abort inform warn %||%
#' @importFrom tibble tibble
#' @importFrom utils head packageDescription packageVersion tail
NULL

hasName <- function(x, name) match(name, names(x), nomatch = 0L) > 0L
