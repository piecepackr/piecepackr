# Simplify 'grobCoords' methods

coords_to_xylist <- function(coords) {
	if (inherits(coords, "GridGTreeCoords")) {
		xyl <- lapply(coords, coords_to_xylist)
		f <- function(x, y) gridGeometry::polyclip(x, y, "union")
	} else {
		xyl <- lapply(coords, identity)
		# Use "xor" to preserve holes (e.g. marbles board) #368
		# For non-overlapping shapes xor == union
		f <- function(x, y) gridGeometry::polyclip(x, y, "xor")
	}
	xyl <- Filter(function(x) length(x) > 0, xyl)
	Reduce(f, xyl)
}

xylists_to_grobcoords <- function(xyl, name, closed = TRUE) {
	if (getRversion() >= '4.2.0' && !closed) {
		return(emptyGrobCoords(name))
	}

	if (getRversion() < '4.2.0') {
		if (!is.list(xyl[[1]])) {
			list(xyl)
		} else {
			xyl
		}
	} else {
		if (!is.list(xyl[[1]])) {
			xyl <- list(gridCoords(xyl$x, xyl$y))
		} else {
			xyl <- lapply(xyl, function(x) gridCoords(x$x, x$y))
		}
		gridGrobCoords(xyl, name)
	}
}

#' @export
grobCoords.pp_grobCoords <- function(x, closed, ...) {
	if (getRversion() >= '4.2.0' && !closed) {
		return(emptyGrobCoords(x$name))
	}

	coords <- NextMethod()
	xyl <- coords_to_xylist(coords)
	xylists_to_grobcoords(xyl, x$name)
}

#' @export
grobPoints.pp_grobCoords <- function(x, closed, ...) {
	grobCoords(x, closed, ...)
}
