Token2S <- R6Class("token2s",
   public = list(initialize = function(shape = pp_shape(),
                                       whd = list(width = 1, height = 1, depth = 1),
                                       center = Point3D$new(),
                                       R = diag(3)) {
                        # coordinates
                        xy_npc <- Point2D$new(shape$npc_coords)
                        xyz_scaled <- Point3D$new(xy_npc)$translate(-0.5, -0.5, 0.5)$dilate(whd)
                        xyz_f <- xyz_scaled$rotate(R)$translate(center)

                        xy_npc <- Point2D$new(shape$npc_coords)
                        xyz_scaled <- Point3D$new(xy_npc)$translate(-0.5, -0.5, -0.5)$dilate(whd)
                        xyz_b <- xyz_scaled$rotate(R)$translate(center)

                        self$xyz <- Point3D$new(x = c(xyz_f$x, xyz_b$x),
                                                y = c(xyz_f$y, xyz_b$y),
                                                z = c(xyz_f$z, xyz_b$z))

                        # edges
                        edge_partition <- partition_edges(shape)
                        n_points <- length(xyz_f)
                        n_edges <- length(edge_partition$type)
                        edges <- vector("list", n_edges)
                        for (i in seq(n_edges)) {
                            vertices <- self$xyz[edge_indices(edge_partition$indices[[i]], n_points)]
                            type <- edge_partition$type[i]
                            edges[[i]] <- edge_class(type, vertices)
                        }
                        self$edges <- edges
                    },
                    op_edge_order = function(angle) {
                        r <- 10 * self$xyz$width
                        op_diff <- Point2D$new(0, 0)$translate_polar(angle, r)
                        op_diff <- Point3D$new(op_diff, z = r / sqrt(2))
                        op_ref <- self$xyz$c$translate(op_diff)
                        dists <- sapply(self$edges, function(x) op_ref$distance_to(x$vertices$c))
                        order(dists)
                    },
                    op_edges = function(angle) {
                        self$edges[self$op_edge_order(angle)]
                    },
                    xyz = NULL, edges = NULL
   ),
   private = list(),
   active = list(xyz_face = function() {
                    n <- length(self$xyz$x) / 2
                    self$xyz[seq(n)]
                 }, xyz_back = function() {
                    n <- length(self$xyz$x) / 2
                    self$xyz[seq(n + 1, 2 * n)]
                 })
)

edge_indices <- function(indices, n_points) {
    c(indices, n_points + rev(indices))
}

edge_class <- function(type, vertices) {
    switch(type,
           curved = CurvedEdge$new(vertices),
           flat = FlatEdge$new(vertices),
           ring = RingEdge$new(vertices))
}

partition_edges <- function(shape) {
    classes <- shape$npc_coords$c
    n <- length(classes)
    if (all(classes == "C0")) {
        list(type = rep("flat", n),
             indices = lapply(seq_along(classes), function(x) c(x, x %% n + 1)))
    } else if (all(classes == "C1")) {
        list(type = "ring", indices = list(seq_along(classes)))
    } else {
        if (!(classes[n] == "C0")) abort("Can only handle case when last class is 'C0'")
        type <- vector("character")
        indices <- vector("list")
        index <- 1
        prev <- classes[n]
        curve_start <- NULL
        for (i in seq_along(classes)) {
            class <- classes[i]
            if (all(c(prev, class) == "C0")) {
                type[index] <- "flat"
                i_prev <- ifelse(i == 1, n, i - 1)
                indices[[index]] <- c(i_prev, i)
                index <- index + 1
            } else if (prev == "C1" && class == "C0") { # end of curved
                type[index] <- "curved"
                if (curve_start == 0) {
                    indices[[index]] <- c(n, seq(i))
                } else {
                    indices[[index]] <- seq(curve_start, i)
                }
                index <- index + 1
            } else if (prev == "C0" && class == "C1") { # start of curved
                curve_start <- i - 1
            }
            prev <- class
        }
        list(type = type, indices = indices)
    }
}

FlatEdge <- R6Class("edge_flat",
    public = list(vertices = NULL,
                  initialize = function(vertices = NULL) self$vertices <- vertices,
                  op_grob = function(angle, scale, ...) {
                      xy <- self$vertices$project_op(angle, scale)
                      polygonGrob(xy$x, xy$y, default.units = "in", ...)
                  }),
    private = list(),
    active = list()
)

#### Convex, closed, curved edge
#### algorithm won't necessarily work if disc stood on edge AND rotated orthogonally with regard towards op viewer
RingEdge <- R6Class("edge_ring",
    public = list(vertices = NULL,
                  initialize = function(vertices = NULL) self$vertices <- vertices,
                  op_grob = function(angle, scale, ...) {
                      n <- length(self$vertices) / 2
                      xy_f <- self$vertices[seq(n)]$project_op(angle, scale)
                      projections <- numeric(n)
                      proj_vec <- Vector$new(to_x(angle - 90, 1), to_y(angle - 90, 1))
                      for (ii in seq(n)) {
                          projections[ii] <- proj_vec$dot(xy_f[ii])
                      }
                      i_min <- which.min(projections)
                      i_max <- which.max(projections)
                      if (i_min < i_max) {
                          indices1 <- seq(i_min, i_max)
                          if (i_max < n) {
                              indices2 <- c(seq(i_max + 1, n), seq_len(i_min - 1))
                          } else {
                              indices2 <- seq(1, i_min - 1)
                          }
                      } else {
                          indices1 <- c(seq(i_min, n), seq(1, i_max))
                          indices2 <- seq(i_max + 1, i_min - 1)
                      }
                      # figure out which part farthest
                      r <- 10 * self$vertices$width
                      op_diff <- Point2D$new(0, 0)$translate_polar(angle, r)
                      op_diff <- Point3D$new(op_diff, z = r / sqrt(2))
                      op_ref <- self$vertices$c$translate(op_diff)
                      d1 <- op_ref$distance_to(self$vertices[indices1]$c)
                      d2 <- op_ref$distance_to(self$vertices[indices2]$c)
                      if (d1 > d2) {
                          indices_obscured <- indices2
                          indices_visible <- indices1
                      } else {
                          indices_obscured <- indices1
                          indices_visible <- indices2
                      }
                      xy <- self$vertices$project_op(angle, scale)
                      x_obscured <- xy$x[full_indices(indices_obscured, n)]
                      y_obscured <- xy$y[full_indices(indices_obscured, n)]
                      x_visible <- xy$x[full_indices(indices_visible, n)]
                      y_visible <- xy$y[full_indices(indices_visible, n)]
                      polygonGrob(x=c(x_obscured, x_visible),
                                  y=c(y_obscured, y_visible),
                                  id.lengths = c(length(x_obscured), length(x_visible)),
                                  default.units="in", ...)
                  }),
    private = list(),
    active = list()
)

full_indices <- function(indices, n) c(indices, rev(2 * n + 1 - indices))

## Convex, open curved edge
CurvedEdge <- R6Class("edge_curved",
    public = list(vertices = NULL,
                  initialize = function(vertices = NULL) self$vertices <- vertices,
                  op_grob = function(angle, scale, ...) {
                      n <- length(self$vertices) / 2
                      xy_f <- self$vertices[seq(n)]$project_op(angle, scale)
                      projections <- numeric(n)
                      proj_vec <- Vector$new(to_x(angle - 90, 1), to_y(angle - 90, 1))
                      for (ii in seq(n)) {
                          projections[ii] <- proj_vec$dot(xy_f[ii])
                      }
                      i_min <- which.min(projections)
                      i_max <- which.max(projections)
                      l_indices <- list()
                      i_l <- min(i_min, i_max)
                      i_u <- max(i_min, i_max)
                      if (i_l == 1 && i_u == n) {
                          l_indices[[1]] <- seq(n)
                      } else if (i_l == 1) {
                          l_indices[[1]] <- seq(i_u + 1, n)
                          l_indices[[2]] <- seq(1, i_u)
                      } else if (i_u == n) {
                          l_indices[[1]] <- seq(1, i_l - 1)
                          l_indices[[2]] <- seq(i_l, n)
                      } else {
                          l_indices[[1]] <- seq(1, i_l - 1)
                          l_indices[[2]] <- seq(i_u + 1, n)
                          l_indices[[3]] <- seq(i_l, i_u)
                      }
                      xy <- self$vertices$project_op(angle, scale)
                      x <- numeric(0)
                      y <- numeric(0)
                      id <- numeric(0)

                      # figure out which part farthest
                      r <- 10 * self$vertices$width
                      op_diff <- Point2D$new(0, 0)$translate_polar(angle, r)
                      op_diff <- Point3D$new(op_diff, z = r / sqrt(2))
                      op_ref <- self$vertices$c$translate(op_diff)
                      dists <- sapply(l_indices,
                                      function(x) {
                                          indices <- full_indices(x, n)
                                          op_ref$distance_to(self$vertices[indices]$c)
                                      })
                      l_indices <- l_indices[order(dists)]
                      for (i in seq_along(l_indices)) {
                          indices <- full_indices(l_indices[[i]], n)
                          x <- append(x, xy$x[indices])
                          y <- append(y, xy$y[indices])
                          id <- append(id, rep(i, length(indices)))
                      }
                      polygonGrob(x=x, y=y, id=id, default.units="in", ...)
                  }),
    private = list(),
    active = list()
)
