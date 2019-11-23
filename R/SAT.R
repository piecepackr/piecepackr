# "collision detection" via Separating Axis Theorem
Point <- R6Class("point",
                 public = list(x=NULL, y=NULL,
                               initialize = function(x=0.5, y=0.5) {
                                   self$x <- x
                                   self$y <- y
                               },
                               diff = function(p) {
                                   Vector$new(p$x-self$x, p$y-self$y)
                               },
                               distance_to = function(p) {
                                   sqrt((p$x - self$x)^2 + (p$y - self$y)^2)
                               }))

Vector <- R6Class("vector",
                  public = list(x=NULL, y=NULL,
                                initialize = function(x=1, y=1) {
                                    self$x <- x
                                    self$y <- y
                                },
                                dot = function(v) {
                                    self$x * v$x + self$y * v$y
                                },
                                orthogonal = function() {
                                    Vector$new(self$y, -self$x)
                                }))

Circle <- R6Class("circle",
    public = list(c=NULL, r=NULL,
                  initialize = function(x=0.5, y=0.5, r=0.5) {
                      self$c <- Point$new(x=x, y=y)
                      self$r <- r
                  },
                  project = function(v) {
                      center <- v$dot(self$c)
                      c(center - self$r, center + self$r)
                  }))

#### Get rid of duplicate normals? need to keep edges at all?
ConvexPolygon <- R6Class("convex_polygon",
    public = list(x=NULL, y=NULL, vertices=NULL, edges=NULL, normals=NULL,
               initialize = function(x=c(0, 0.5, 1, 0.5),
                                     y=c(0.5, 1, 0.5, 0)) {
                   xy <- cbind(x, y)
                   n <- nrow(xy)
                   # edges
                   edges <- vector("list", n)
                   for (ii in seq(n-1)) {
                       edges[[ii]] <- Vector$new(xy[ii+1,1]-xy[ii,1],
                                                 xy[ii+1,2]-xy[ii,2])
                   }
                   edges[[n]] <- Vector$new(xy[1,1]-xy[n,1],
                                            xy[1,2]-xy[n,2])
                   # normals
                   normals <- vector("list", n)
                   for (ii in seq(n)) {
                       normals[[ii]] <- edges[[ii]]$orthogonal()
                   }
                   # vertices
                   vertices <- vector("list", n)
                   for (ii in seq(n)) {
                       vertices[[ii]] <- Point$new(xy[ii,1], xy[ii,2])
                   }

                   self$x <- xy[,1]
                   self$y <- xy[,2]
                   self$edges <- edges
                   self$normals <- normals
                   self$vertices <- vertices
               },
               project = function(v) {
                   n <- length(self$x)
                   projections <- numeric(n)
                   for (ii in seq(n)) {
                       projections[ii] <- v$dot(self$vertices[[ii]])
                   }
                   range(projections)
               }))

do_projections_overlap <- function(r1, r2) {
    do_ranges_overlap(r1[1], r1[2], r2[1], r2[2])
}

do_convex_polygons_overlap <- function(s1, s2) {
    for (n in c(s1$normals, s2$normals)) {
        if (!do_projections_overlap(s1$project(n), s2$project(n))) {
            return(FALSE)
        }
    }
    TRUE
}

does_convex_polygon_overlap_circle <- function(p, c) { # nolint
    n_vertices <- length(p$vertices)
    c_normals <- vector("list", n_vertices)
    for (ii in seq(n_vertices)) {
        c_normals[[ii]] <- p$vertices[[ii]]$diff(c$c)$orthogonal()
    }
    for (n in c(p$normals, c_normals)) {
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
