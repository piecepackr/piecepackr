# "collision detection" via Separating Axis Theorem
# Arguments of point is vectorized
Point <- R6Class("point",
                 public = list(x=NULL, y=NULL,
                               initialize = function(x=0.5, y=0.5) {
                                   if (is.list(x) || inherits(x, "point")) {
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
                                   Point$new(x, y)
                               },
                               translate = function(x = 0, y = 0) {
                                   Point$new(self$x + x, self$y + y)
                               },
                               translate_polar = function(t = 0, r = 1) {
                                   x <- self$x + to_x(t, r)
                                   y <- self$y + to_y(t, r)
                                   Point$new(x, y)
                               },
                               rotate = function(t = 0) {
                                   x <- self$x * cos(to_radians(t)) - self$y * sin(to_radians(t))
                                   y <- self$x * sin(to_radians(t)) + self$y * cos(to_radians(t))
                                   Point$new(x, y)
                               },
                               dilate = function(width = 1, height = width) {
                                   x <- width * self$x
                                   y <- height * self$y
                                   Point$new(x, y)
                               },
                               npc_to_in = function(x=0.5, y=0.5, w=1, h=1, t=0) {
                                   self$translate(-0.5, -0.5)$dilate(w, h)$rotate(t)$translate(x, y)
                               },
                               plot = function(gp = gpar()) {
                                   grid.points(x=self$x, y=self$y, default.units="in", gp = gpar())
                               }
                               ))
#' @export
`[.point` <- function(x, i) Point$new(x$x[i], x$y[i])
#' @export
length.point <- function(x) length(x$x)
#' @export
as.list.point <- function(x, ...) {
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
                                     if (is.list(x) || inherits(x, "point") || inherits(x, "point3d")) {
                                         xt <- x$x
                                         y <- x$y
                                         z <- if (is.null(x$z)) z else x$z
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
                                 project_op = function(angle, scale) {
                                   x <- self$x + scale * self$z * cos(to_radians(angle))
                                   y <- self$y + scale * self$z * sin(to_radians(angle))
                                   Point$new(x, y)
                                 },
                                 dilate = function(width = 1, height = width, depth = width) {
                                     x <- width * self$x
                                     y <- height * self$y
                                     z <- depth * self$z
                                     Point3D$new(x, y, z)
                                 },
                                 rotate = function(t = 0, axis_x = 0, axis_y = 0) {
                                     if (axis_x != 0 || axis_y != 0) {
                                         stop("Don't know how to do this rotation yet")
                                     }
                                     x <- self$x * cos(to_radians(t)) - self$y * sin(to_radians(t))
                                     y <- self$x * sin(to_radians(t)) + self$y * cos(to_radians(t))
                                     Point3D$new(x, y, self$z)
                                 },
                                 translate = function(x = 0, y = 0, z = 0) {
                                     if (is.list(x) || inherits(x, "point") || inherits(x, "point3d")) {
                                         xt <- x$x
                                         y <- x$y
                                         z <- if (is.null(x$z)) z else x$z
                                     } else {
                                         xt <- x
                                         y <- y
                                         z <- z
                                     }
                                    Point3D$new(self$x + xt, self$y + y, self$z + z)
                                 }
                                 )
)
#' @export
`[.point3d` <- function(x, i) Point3D$new(x$x[i], x$y[i], x$z[i])
#' @export
length.point3d <- function(x) length(x$x)

Vector <- R6Class("geometry_vector", # vector is R builtin class
                  inherit = Point,
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
                      if (is.list(x) || inherits(x, "point")) {
                          self$c <- Point$new(x)
                      } else {
                          self$c <- Point$new(x, y)
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
                   self$vertices <- Point$new(x, y)
                   xy <- cbind(self$x, self$y)
                   n <- length(self$vertices)
                   # edges
                   p <- self$vertices[c(seq(2, n), 1)]
                   self$edges <- LineSegment$new(self$vertices, p)
                   self$normals <- self$edges$orthogonal
               },
               plot = function(gp = gpar()) {
                   grid.polygon(x=self$x, y=self$y, default.units="in", gp = gpar())
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
                          private$center <- Point$new(x, y)
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
                     Point$new(x, y)
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
