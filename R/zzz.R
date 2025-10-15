#' @import grid
#' @importFrom affiner as_coord2d as_coord3d as_plane3d convex_hull2d distance2d distance3d degrees is_angle
#' @importFrom grDevices col2rgb dev.capabilities dev.list dev.new devAskNewPage rgb
#' @importFrom R6 R6Class
#' @importFrom rlang .data abort inform warn %||% check_dots_empty
#' @importFrom stringr str_count str_glue str_pad str_split
#' @importFrom tibble tibble
#' @importFrom utils hasName head packageDescription packageVersion tail
NULL

# https://en.wikipedia.org/wiki/Spherical_circle
# BC^2 = AB^2 + AC^2 <=> AB^2 = BC^2 - AC^2
# where BC = 1.0 diameter marble radius = 0.5 and AC = BC - holed board depth = 0.5 - 0.25
RADIUS_BOARD_HOLES <- sqrt(0.5^2 - 0.25^2)

# Timeline of {piecepackr} archival:
# 2025-10-09 {pieceepackr} submitted (for non-reproducible "donttest" issue)
# 2025-10-09 automatic checks not passed due to "donttest" NOTE of
#   Examples with CPU times > 2.5 times elapsed time
# 2025-10-11 emailed CRAN that this rejection is a false positive
# 2025-10-15 {piecepackr} archived (without reply of my email by CRAN)

# Use for examples that
# may take more than 5 seconds
# or with CPU time > 2.5 times elapsed time on CRAN servers
# theoretically you "should" use \donttest{} but
# in practice this led to package archival without response by CRAN
donttest <- function() {
	interactive() ||
		identical(Sys.getenv("NOT_CRAN"), "true") ||
		identical(Sys.getenv("IN_PKGDOWN"), "true")
}
