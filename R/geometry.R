# "collision detection" via Separating Axis Theorem
# Arguments of point is vectorized
Point2D <- R6Class("point2d",
                 public = list(x=NULL, y=NULL,
                               initialize = function(x=0.5, y=0.5) {
                                   if (is.list(x) || inherits(x, "point2d")) {
                                       xt <- x$x
                                       y <- x$y
                                   } else {
                                       xt <- x
                                       y <- y
                                   }
                                   n <- max(lengths(list(xt, y)))
                                   self$x <- rep(xt, length.out = n)
                                   self$y <- rep(y, length.out = n)
                               },
                               diff = function(p) {
                                   Vector$new(p$x-self$x, p$y-self$y)
                               },
                               distance_to = function(p) {
                                   sqrt((p$x - self$x)^2 + (p$y - self$y)^2)
                               },
                               midpoint = function(p) {
                                   x <- mean(c(p$x, self$x))
                                   y <- mean(c(p$y, self$y))
                                   Point2D$new(x, y)
                               },
                               translate = function(x = 0, y = 0) {
                                   Point2D$new(self$x + x, self$y + y)
                               },
                               translate_polar = function(t = 0, r = 1) {
                                   x <- self$x + to_x(t, r)
                                   y <- self$y + to_y(t, r)
                                   Point2D$new(x, y)
                               },
                               rotate = function(t = 0) {
                                   x <- self$x * cos(to_radians(t)) - self$y * sin(to_radians(t))
                                   y <- self$x * sin(to_radians(t)) + self$y * cos(to_radians(t))
                                   Point2D$new(x, y)
                               },
                               dilate = function(width = 1, height = width) {
                                   x <- width * self$x
                                   y <- height * self$y
                                   Point2D$new(x, y)
                               },
                               npc_to_in = function(x=0.5, y=0.5, w=1, h=1, t=0) {
                                   self$translate(-0.5, -0.5)$dilate(w, h)$rotate(t)$translate(x, y)
                               },
                               plot = function(gp = gpar()) {
                                   grid.points(x=self$x, y=self$y, default.units="in", gp = gp)
                               }
                               ))
#' @export
`[.point2d` <- function(x, i) Point2D$new(x$x[i], x$y[i])
#' @export
length.point2d <- function(x) length(x$x)
#' @export
as.matrix.point2d <- function(x, ...) {
    m <- cbind(x$x, x$y)
    colnames(m) <- c("x", "y")
    m
}
#' @export
as.list.point2d <- function(x, ...) {
    n <- length(x)
    ll <- vector("list", n)
    for (i in seq_len(n)) {
        ll[[i]] <- x[i]
    }
    ll
}

Point3D <- R6Class("point3d",
                   public = list(x=NULL, y=NULL, z=NULL,
                                 initialize = function(x=0, y=0, z=0) {
                                     if (is.list(x) || inherits(x, "point2d") || inherits(x, "point3d")) {
                                         xt <- x$x
                                         y <- x$y
                                         z <- if (is.null(x$z)) z else x$z
                                     } else if (is.matrix(x)) {
                                         xt <- x[, 1]
                                         y <- x[, 2]
                                         z <- x[, 3]
                                     } else {
                                         xt <- x
                                         y <- y
                                         z <- z
                                     }
                                     n <- max(lengths(list(xt, y, z)))
                                     self$x <- rep(xt, length.out = n)
                                     self$y <- rep(y, length.out = n)
                                     self$z <- rep(z, length.out = n)
                                 },
                                 distance_to = function(p) {
                                     sqrt((p$x - self$x)^2 + (p$y - self$y)^2 + (p$z  - self$z)^2)
                                 },
                                 project_op = function(angle, scale) {
                                   x <- self$x + scale * self$z * cos(to_radians(angle))
                                   y <- self$y + scale * self$z * sin(to_radians(angle))
                                   Point2D$new(x, y)
                                 },
                                 dilate = function(width = 1, height = width, depth = width) {
                                     if (is.list(width)) {
                                         x <- width$width * self$x
                                         y <- width$height * self$y
                                         z <- width$depth * self$z
                                     } else {
                                         x <- width * self$x
                                         y <- height * self$y
                                         z <- depth * self$z
                                     }
                                     Point3D$new(x, y, z)
                                 },
                                 # rotation matrix 'R' is post-multiplied...
                                 rotate = function(t = 0, axis_x = 0, axis_y = 0) {
                                     m <- as.matrix(self)
                                     if (is.matrix(t)) {
                                         R <- t
                                     } else {
                                         R <- AA_to_R(t, axis_x, axis_y)
                                     }
                                     Point3D$new(m %*% R)
                                 },
                                 translate = function(x = 0, y = 0, z = 0) {
                                     if (is.list(x) || inherits(x, "point2d") || inherits(x, "point3d")) {
                                         xt <- x$x
                                         y <- x$y
                                         z <- if (is.null(x$z)) z else x$z
                                     } else {
                                         xt <- x
                                         y <- y
                                         z <- z
                                     }
                                    Point3D$new(self$x + xt, self$y + y, self$z + z)
                                 }),
                   active = list(c = function() {
                                     Point3D$new(mean(self$x), mean(self$y), mean(self$z))
                                 },
                                 width = function() {
                                     2 * max(self$distance_to(self$c))
                                 })
)
#' @export
`[.point3d` <- function(x, i) Point3D$new(x$x[i], x$y[i], x$z[i])
#' @export
length.point3d <- function(x) length(x$x)
#' @export
as.matrix.point3d <- function(x, ...) {
    m <- cbind(x$x, x$y, x$z)
    colnames(m) <- c("x", "y", "z")
    m
}

Vector <- R6Class("geometry_vector", # vector is R builtin class
                  inherit = Point2D,
                  public = list(dot = function(v) {
                                    self$x * v$x + self$y * v$y
                                }),
                  active = list(orthogonal = function() {
                                    Vector$new(self$y, -self$x)
                                }))
`[.geometry_vector` <- function(x, i) Vector$new(x$x[i], x$y[i])

Circle <- R6Class("circle",
    public = list(c=NULL, r=NULL,
                  initialize = function(x=0.5, y=0.5, r=0.5) {
                      if (is.list(x) || inherits(x, "point2d")) {
                          self$c <- Point2D$new(x)
                      } else {
                          self$c <- Point2D$new(x, y)
                      }
                      self$r <- r
                  },
                  project = function(v) {
                      center <- v$dot(self$c)
                      c(center - self$r, center + self$r)
                  }))

Polygon <- R6Class("polygon",
    public = list(vertices=NULL, edges=NULL, normals=NULL,
               initialize = function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0)) {
                   self$vertices <- Point2D$new(x, y)
                   xy <- cbind(self$x, self$y)
                   n <- length(self$vertices)
                   # edges
                   p <- self$vertices[c(seq(2, n), 1)]
                   self$edges <- LineSegment$new(self$vertices, p)
                   self$normals <- self$edges$orthogonal
               },
               plot = function(gp = gpar()) {
                   grid.polygon(x=self$x, y=self$y, default.units="in", gp = gp)
               },
               project = function(v) {
                   n <- length(self$x)
                   projections <- numeric(n)
                   for (ii in seq(n)) {
                       projections[ii] <- v$dot(self$vertices[ii])
                   }
                   range(projections)
               },
               op_edge_order = function(angle) {
                   op_ref <- self$c$translate_polar(angle + 180, 10 * self$width)
                   dists <- sapply(self$edges$mid_point, function(x) x$distance_to(op_ref))
                   order(dists, decreasing = TRUE)
               },
               op_edges = function(angle) {
                   self$edges[self$op_edge_order(angle)]
               }),
    private = list(center = NULL),
    active = list(x = function() self$vertices$x,
                  y = function() self$vertices$y,
                  c = function() {
                      if (is.null(private$center)) {
                          x <- mean(self$vertices$x)
                          y <- mean(self$vertices$y)
                          private$center <- Point2D$new(x, y)
                      }
                      private$center
                  },
                  width = function() {
                      dx <- diff(range(self$vertices$x))
                      dy <- diff(range(self$vertices$y))
                      max(dx, dy)
                  })
    )

LineSegment <- R6Class("line_segment",
    public = list(p1=NULL, p2=NULL,
                  initialize = function(p1, p2) {
                      self$p1 <- p1
                      self$p2 <- p2
                  },
                  face_matrix = function(z=0, depth=1) {
                      vs <- list()
                      vs[[1]] <- Point3D$new(self$p1, z = z - 0.5*depth)
                      vs[[2]] <- Point3D$new(self$p1, z = z + 0.5*depth)
                      vs[[3]] <- Point3D$new(self$p2, z = z + 0.5*depth)
                      vs[[4]] <- Point3D$new(self$p2, z = z - 0.5*depth)
                      n <- max(lengths(c(self$p1, self$p2)))
                      m <- matrix(0, nrow = 4 * n, ncol = 3)
                      for (i_v in seq_len(n)) {
                          i_m <- 4 * i_v - 4
                          for (i in 1:4) {
                              v <- vs[[i]][i_v]
                              m[i_m + i, ] <- c(v$x, v$y, v$z)
                          }
                      }
                      colnames(m) <- c("x", "y", "z")
                      m
                  }),
   active = list(mid_point = function() {
                     x <- (self$p1$x + self$p2$x) / 2
                     y <- (self$p1$y + self$p2$y) / 2
                     Point2D$new(x, y)
                  },
                  orthogonal = function() {
                      self$p1$diff(self$p2)$orthogonal
                  })
    )
#' @export
`[.line_segment` <- function(x, i) LineSegment$new(x$p1[i], x$p2[i])

ConvexPolygon <- R6Class("convex_polygon", inherit = Polygon)
#### ConcavePolygon, add a list of convex polygons that cover it to test SAT
#### Most value adding if adding something like megahexes but could be used for stars as well

do_projections_overlap <- function(r1, r2) {
    do_ranges_overlap(r1[1], r1[2], r2[1], r2[2])
}

do_convex_polygons_overlap <- function(s1, s2) {
    for (n in c(as.list(s1$normals), as.list(s2$normals))) {
        if (!do_projections_overlap(s1$project(n), s2$project(n))) {
            return(FALSE)
        }
    }
    TRUE
}

does_convex_polygon_overlap_circle <- function(p, c) { # nolint
    n_vertices <- length(p$vertices)
    c_normals <- p$vertices$diff(c$c)$orthogonal
    for (n in c(as.list(p$normals), as.list(c_normals))) {
        if (!do_projections_overlap(p$project(n), c$project(n))) {
            return(FALSE)
        }
    }
    TRUE
}

do_shapes_overlap <- function(s1, s2) {
    if (inherits(s1, "circle") && inherits(s2, "circle")) {
        less_than(s1$c$distance_to(s2$c), s1$r + s2$r)
    } else if (inherits(s1, "convex_polygon") && inherits(s2, "convex_polygon")) {
        do_convex_polygons_overlap(s1, s2)
    } else if (inherits(s1, "convex_polygon") && inherits(s2, "circle")) {
        does_convex_polygon_overlap_circle(s1, s2)
    } else if (inherits(s1, "circle") && inherits(s2, "convex_polygon")) {
        does_convex_polygon_overlap_circle(s2, s1)
    } else {
        TRUE
    }
}

# Axis-angle representation to rotation matrix
# https://en.wikipedia.org/wiki/Axis-angle_representation
# Because we do rotation matrix post-multiplication instead of pre-multiplication we usually need to multiply angles
# in following algorithms by -1

#' @rdname geometry_utils
#' @inheritParams save_piece_obj
#' @param axis_z Third coordinate of the axis unit vector (usually inferred).
#' @param angle Angle in degrees (counter-clockwise)
#' @param ... Ignored
#' @export
AA_to_R <- function(angle = 0, axis_x = 0, axis_y = 0, axis_z = NA, ...) {
    if (is.na(axis_z)) {
        axis_z <- sqrt(1 - axis_x^2 - axis_y^2)
    } else {
        norm <- sqrt(axis_x^2 + axis_y^2 + axis_z^2)
        if (!nigh(norm, 1)) {
            axis_x <- axis_x / norm
            axis_y <- axis_y / norm
            axis_z <- axis_z / norm
        }
    }
    e <- c(axis_x, axis_y, axis_z)
    I <- diag(3)
    K <- cross_matrix(e)
    c <- cos(to_radians(-angle))
    s <- sin(to_radians(-angle))
    R <- I + s * K + (1 - c) * K %*% K
    R
}

# "cross" product matrix
# https://en.wikipedia.org/wiki/Cross_product#Conversion_to_matrix_multiplication
cross_matrix <- function(v) {
    m <- matrix(0, nrow=3, ncol=3)
    m[1, 2] <- -v[3]
    m[1, 3] <- v[2]
    m[2, 1] <- v[3]
    m[2, 3] <- -v[1]
    m[3, 1] <- -v[2]
    m[3, 2] <- v[1]
    m
}

# trace of a (square) matrix
trace <- function(m) sum(diag(m))

# Name 'nigh' to avoid potential conflict with 'dplyr::near()'
nigh <- function(x, y, tolerance = 1e-6) {
    if (length(y) < length(x)) y <- rep(y, length.out=length(x))
    isTRUE(all.equal(x, y, tolerance = tolerance))
}

# more robust handling of arccosine input
arccos <- function(x) {
    if (nigh(x, 1) && x > 1) x <- 1
    if (nigh(x, -1) && x < -1) x <- -1
    acos(x)
}

# Rotation matrix to Axis-angle representation
# https://en.wikipedia.org/wiki/Axis-angle_representation
#' @param R 3D rotation matrix (post-multiplied)
#' @rdname geometry_utils
#' @export
R_to_AA <- function(R = diag(3)) {
    t <- arccos(0.5 * (trace(R) - 1))
    if (nigh(R, diag(3))) { # no rotation
        t <- 0
        e <- c(0, 0, 1)
    } else if (nigh(t, pi)) { # 180 degree rotation
        t <- pi
        B <- 0.5 * (R + diag(3))
        e <- sqrt(diag(B))
        sB <- sign(B)
        if (nigh(sB, ppn)) {
            e[3] <- -e[3]
            t <- -pi
        } else if (nigh(sB, pnp)) {
            e[2] <- -e[2]
        } else if (nigh(sB, npp)) {
            e[1] <- -e[1]
        }
    } else {
        e <- numeric(3)
        e[1] <- R[3,2] - R[2,3]
        e[2] <- R[1,3] - R[3,1]
        e[3] <- R[2,1] - R[1,2]
        e <- e / (2 * sin(-t))
    }
    if (e[3] < 0) { # Force z-axis element positive
        e <- -e
        t <- -t
    }
    list(angle = to_degrees(t), axis_x = e[1], axis_y = e[2], axis_z = e[3])
}

# Sign matrices for "B" matrix used to tell signs for axis unit vector (up to sign ambiguity) when angle = 180 degrees
# signs for ppp and nnn don't need to be changed
# ppn and npp
ppn <- matrix(c(1, 1, -1,
                1, 1, -1,
                -1, -1, 1), ncol = 3, byrow = TRUE)
# pnp and npn
pnp <- matrix(c(1, -1, 1,
                -1, 1, -1,
                1, -1, 1), ncol = 3, byrow = TRUE)
# pnn and npp
npp <- matrix(c(1, -1, -1,
                -1, 1, 1,
                -1, 1, 1), ncol = 3, byrow = TRUE)

# Basic 3D rotation matrices
# https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions

#' Helper functions for making geometric calculations.
#'
#' \code{to_x}, \code{to_y}, \code{to_r}, \code{to_t} convert
#' between polar coordinates (in degrees) and Cartesian coordinates.
#' \code{to_degrees} and \code{to_radians} converts between degrees and radians.
#' \code{AA_to_R} and \code{R_to_AA} convert back and forth between (post-multiplied) rotation matrix
#' and axis-angle representations of 3D rotations.
#' \code{R_x}, \code{R_y}, and \code{R_z} build (post-multiplied) rotation matrices for simple rotations around
#' the x, y, and z axes.
#'
#' \code{pp_cfg} uses polar coordinates to determine where the "primary" and "directional"
#'      symbols are located on a game piece.
#' They are also useful for drawing certain shapes and for making game diagrams on hex boards.
#'
#' \code{piecepackr} and \code{grid} functions use angles in degrees
#'  but the \code{base} trigonometry functions usually use radians.
#'
#' \code{piecepackr}'s 3D graphics functions \code{save_piece_obj}, \code{piece}, and \code{piece3d}
#' use the axis-angle representation for 3D rotations.
#' The axis-angle representation involves specifying a unit vector
#' indicating the direction of an axis of rotation and an angle describing the (counter-clockwise)
#' rotation around that axis.  Because it is a unit vector one only needs to specify the first two elements,
#' \code{axis_x} and \code{axis_y}, and we are able to infer the 3rd element \code{axis_z}.  The default of
#' \code{axis = 0}, \code{axis_y = 0}, and implied \code{axis_z = 1}
#' corresponds to a rotation around the z-axis which is reverse-compatible
#' with the originally 2D \code{angle} interpretation in \code{grid.piece}.  In order to figure out the appropriate
#' axis-angle representation parameters \code{R_to_AA}, \code{R_x}, \code{R_y}, and \code{R_z} allow one
#' to first come up with an appropriate (post-multiplied) 3D rotation matrix by chaining simple rotations
#' and then convert them to the corresponding axis-angle representation.
#' Pieces are rotated as if their center was at the origin.
#'
#' @examples
#'  to_x(90, 1)
#'  to_y(180, 0.5)
#'  to_t(0, -1)
#'  to_r(0.5, 0)
#'  all.equal(pi, to_radians(to_degrees(pi)))
#'  # default axis-angle axis is equivalent to a rotation about the z-axis
#'  all.equal(AA_to_R(angle=60), R_z(angle=60))
#'  # axis-angle representation of 90 rotation about the x-axis
#'  R_to_AA(R_x(90))
#'  # find Axis-Angle representation of first rotating about x-axis 180 degrees
#'  # and then rotating about z-axis 45 degrees
#'  R_to_AA(R_x(180) %*% R_z(45))
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Axis-angle_representation} for more details
#'   about the Axis-angle representation of 3D rotations.
#'   See \code{\link[base]{Trig}} for R's built-in trigonometric functions.
#' @name geometry_utils
NULL

#' @rdname geometry_utils
#' @export
R_x <- function(angle = 0) {
    c <- cos(to_radians(-angle))
    s <- sin(to_radians(-angle))
    matrix(c(1, 0, 0,
             0, c, -s,
             0, s, c),
           ncol = 3, byrow = TRUE)
}

#' @rdname geometry_utils
#' @export
R_y <- function(angle = 0) {
    c <- cos(to_radians(-angle))
    s <- sin(to_radians(-angle))
    matrix(c(c, 0, s,
             0, 1, 0,
             -s, 0, c),
           ncol = 3, byrow = TRUE)
}

#' @rdname geometry_utils
#' @export
R_z <- function(angle = 0) {
    c <- cos(to_radians(-angle))
    s <- sin(to_radians(-angle))
    matrix(c(c, -s, 0,
             s, c, 0,
             0, 0, 1),
           ncol = 3, byrow = TRUE)
}

#' @rdname geometry_utils
#' @export
to_radians <- function(t) pi * t / 180

#' @rdname geometry_utils
#' @export
to_degrees <- function(t) 180 * t / pi

#' @rdname geometry_utils
#' @param t Angle in degrees (counter-clockwise)
#' @param r Radial distance
#' @export
to_x <- function(t, r) {
    r * cos(to_radians(t))
}

#' @rdname geometry_utils
#' @export
to_y <- function(t, r) {
    r * sin(to_radians(t))
}

#' @rdname geometry_utils
#' @param x Cartesian x coordinate
#' @param y Cartesian y coordinate
#' @export
to_r <- function(x, y) {
    sqrt(x^2 + y^2)
}

#' @rdname geometry_utils
#' @export
to_t <- function(x, y) {
    to_degrees(atan2(y, x))
}
