#' @import grid
#' @importFrom affiner as_coord2d as_coord3d as_plane3d convex_hull2d distance2d distance3d degrees is_angle
#' @importFrom grDevices col2rgb dev.capabilities dev.list dev.new devAskNewPage rgb
#' @importFrom R6 R6Class
#' @importFrom rlang .data abort inform warn %||% check_dots_empty
#' @importFrom stringr str_count str_glue str_pad str_split
#' @importFrom tibble tibble
#' @importFrom utils head packageDescription packageVersion tail
NULL

hasName <- function(x, name) match(name, names(x), nomatch = 0L) > 0L

# https://en.wikipedia.org/wiki/Spherical_circle
# BC^2 = AB^2 + AC^2 <=> AB^2 = BC^2 - AC^2
# where BC = 1.0 diameter marble radius = 0.5 and AC = BC - holed board depth = 0.5 - 0.25
RADIUS_BOARD_HOLES <- sqrt(0.5^2 - 0.25^2)
