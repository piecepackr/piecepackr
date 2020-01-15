# "collision detection" via Separating Axis Theorem
# Arguments of point is vectorized
Point <- R6Class("point",
                 public = list(x=NULL, y=NULL,
                               initialize = function(x=0.5, y=0.5) {
                                   if (is.list(x) || inherits(x, "point")) {
                                       self$x <- x$x
                                       self$y <- x$y
                                   } else {
                                       self$x <- x
                                       self$y <- y
                                   }
                               },
                               diff = function(p) {
                                   Vector$new(p$x-self$x, p$y-self$y)
                               },
                               distance_to = function(p) {
                                   sqrt((p$x - self$x)^2 + (p$y - self$y)^2)
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
                               dilate = function(width = 1, height = 1) {
                                   x <- width * self$x
                                   y <- height * self$y
                                   Point$new(x, y)
                               },
                               project_op = function(z = 0, angle = 45, scale = 0) {
                                   x <- self$x + scale * z * cos(to_radians(angle))
                                   y <- self$y + scale * z * sin(to_radians(angle))
                                   Point$new(x, y)
                               },
                               npc_to_in = function(x=0.5, y=0.5, w=1, h=1, t=0) {
                                   self$translate(-0.5, -0.5)$dilate(w, h)$rotate(t)$translate(x, y)
                               }
                               ))
`[.point` <- function(x, i) Point$new(x$x[i], x$y[i])
length.point <- function(x) length(x$x)
as.list.point <- function(x, ...) {
    n <- length(x)
    ll <- vector("list", n)
    for (i in seq_len(n)) {
        ll[[i]] <- x[i]
    }
    ll
}

Vector <- R6Class("geometry_vector", # vector is R builtin class
                  inherit = Point,
                  public = list(dot = function(v) {
                                    self$x * v$x + self$y * v$y
                                },
                                orthogonal = function() {
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

ConvexPolygon <- R6Class("convex_polygon",
    public = list(vertices=NULL, edges=NULL, normals=NULL,
               initialize = function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0)) {
                   self$vertices <- Point$new(x, y)
                   xy <- cbind(self$x, self$y)
                   n <- length(self$vertices)
                   # edges
                   p <- self$vertices[c(seq(2,n), 1)]
                   self$edges <- self$vertices$diff(p)
                   self$normals <- self$edges$orthogonal()
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
               }),
    active = list(x = function() self$vertices$x,
                  y = function() self$vertices$y)
    )

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
    c_normals <- p$vertices$diff(c$c)$orthogonal()
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
