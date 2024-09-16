Token2S <- R6Class("token2s",
   public = list(initialize = function(shape = pp_shape(),
                                       whd = list(width = 1, height = 1, depth = 1),
                                       center = as_coord3d(0, 0, 0),
                                       R = diag(3)) {
                        # coordinates
                        xyz_scaled <- as_coord3d(shape$npc_coords)$
                            translate(as_coord3d(-0.5, -0.5, 0.5))$
                            scale(whd$width, whd$height, whd$depth)
                        xyz_f <- xyz_scaled$transform(R)$translate(center)

                        xyz_scaled <- as_coord3d(shape$npc_coords)$
                            translate(as_coord3d(-0.5, -0.5, -0.5))$
                            scale(whd$width, whd$height, whd$depth)
                        xyz_b <- xyz_scaled$transform(R)$translate(center)

                        self$xyz <- as_coord3d(x = c(xyz_f$x, xyz_b$x),
                                               y = c(xyz_f$y, xyz_b$y),
                                               z = c(xyz_f$z, xyz_b$z))

                        xy_vp <- as_coord2d(rect_xy)
                        xyz_scaled <- as_coord3d(xy_vp)$
                            translate(as_coord3d(-0.5, -0.5, 0.5))$
                            scale(whd$width, whd$height, whd$depth)
                        self$xyz_vp_face <- xyz_scaled$transform(R)$translate(center)

                        # Flip so back also in "upper_left", "lower_left", "lower_right", "upper_right" order
                        xy_vp <- as_coord2d(x = rect_xy$x[4:1], y = rect_xy$y[4:1])
                        xyz_scaled <- as_coord3d(xy_vp)$
                            translate(as_coord3d(-0.5, -0.5, -0.5))$
                            scale(whd$width, whd$height, whd$depth)
                        self$xyz_vp_back <- xyz_scaled$transform(R)$translate(center)

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
                        r <- 10 * radius(self$xyz)
                        op_ref <- as_coord3d(degrees(180 + angle), radius = r, z = 0)
                        op_plane <- as_plane3d(normal = op_ref, p1 = op_ref)
                        depths <- sapply(self$edges, function(x) mean(x$vertices)$z)
                        dists <- sapply(self$edges, function(x) distance3d(op_plane, mean(x$vertices)))
                        order(round(depths, 6), -dists) # `round()` avoids weird sorting errors
                    },
                    op_edges = function(angle) {
                        self$edges[self$op_edge_order(angle)]
                    },
                    op_xy_vp = function(angle, scale, side) {
                       switch(side,
                             face = as_coord2d(self$xyz_vp_face, alpha = degrees(angle), scale = scale),
                             back = as_coord2d(self$xyz_vp_back, alpha = degrees(angle), scale = scale),
                             abort(paste("Can't handle side", side)))
                    },
                    #### Handle edge case for token (almost) parallel to xy-axis
                    visible_side = function(angle) {
                        r <- 10 * radius(self$xyz)
                        op_ref <- as_coord3d(degrees(180 + angle), radius = r, z = 0)
                        op_plane <- as_plane3d(normal = op_ref, p1 = op_ref)
                        if (distance3d(op_plane, mean(self$xyz_face)) <
                            distance3d(op_plane, mean(self$xyz_back)))
                            "face"
                        else
                            "back"
                    },
                    xyz_side = function(side) {
                        switch(side,
                               face = self$xyz_face,
                               back = self$xyz_back)
                    },
                    xyz = NULL, edges = NULL,
                    xyz_vp_face = NULL, xyz_vp_back = NULL
   ),
   private = list(),
   active = list(xyz_face = function() {
                    n <- length(self$xyz) / 2
                    self$xyz[seq(n)]
                 },
                 xyz_back = function() {
                    n <- length(self$xyz) / 2
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

Edge <- R6Class("edge",
                public = list(vertices = NULL,
                          initialize = function(vertices = NULL) self$vertices <- vertices,
                          visible_side = function(angle) {
                              r <- 10 * radius(self$vertices)
                              op_ref <- as_coord3d(degrees(180 + angle), radius = r, z = 0)
                              op_plane <- as_plane3d(normal = op_ref, p1 = op_ref)
                              if (distance3d(op_plane, mean(self$vertices_face)) <
                                  distance3d(op_plane, mean(self$vertices_back)))
                                  "face"
                              else
                                  "back"
                          },
                          vertices_visible_side = function(angle) {
                              side <- self$visible_side(angle)
                              switch(side,
                                     face = self$vertices_face,
                                     back = self$vertices_back)
                          }),
                private = list(),
                active = list(
                          # won't work for "FlatEdge" but doesn't need to
                          is_perpendicular_xyplane = function() {
                              length(unique(round(self$vertices$z, 6))) == 2
                          },
                          vertices_face = function() {
                                  n <- length(self$vertices) / 2
                                  self$vertices[seq(n)]
                          },
                          vertices_back = function() {
                              n <- length(self$vertices) / 2
                              self$vertices[seq(n + 1, 2 * n)]
                          })
)

FlatEdge <- R6Class("edge_flat", inherit = Edge,
    public = list(op_grob = function(angle, scale, name = NULL) {
                      xy <- as_coord2d(self$vertices, alpha = degrees(angle), scale = scale)
                      polygonGrob(xy$x, xy$y, default.units = "in", name = name)
                  }),
    private = list(),
    active = list()
)

## Convex, closed curved edge
RingEdge <- R6Class("edge_ring", inherit = Edge,
    public = list(op_grob = function(angle, scale, name = NULL) {
                      if (self$is_perpendicular_xyplane) {
                          private$op_grob_perpendicular(angle, scale, name = name)
                      } else {
                          private$op_grob_nonperpendicular(angle, scale, name = name)
                      }
                  }),
    private = list(
                   op_grob_nonperpendicular = function(angle, scale, name = NULL) {
                       xyh <- convex_hull2d(as_coord2d(self$vertices, alpha = degrees(angle), scale = scale))

                       xy_visible <- as_coord2d(self$vertices_visible_side(angle), alpha = degrees(angle), scale = scale)
                       xyh_visible <- convex_hull2d(xy_visible)

                       A <- list(list(x = xyh$x, y = xyh$y))
                       B <- list(list(x = xyh_visible$x, y = xyh_visible$y))
                       xy_minus <- gridGeometry::polyclip(A, B, op = "minus")

                       #### also include obscured edge (i.e. A intersection B)?
                       gridGeometry::xyListPolygon(xy_minus, name = name)
                   },
                   op_grob_perpendicular = function(angle, scale, name = NULL) {
                      n <- length(self$vertices) / 2
                      xy_f <- as_coord2d(self$vertices[seq(n)], alpha = degrees(angle), scale = scale)
                      projections <- numeric(n)
                      proj_vec <- as_coord2d(degrees(angle - 90), radius = 1)
                      for (ii in seq(n)) {
                          projections[ii] <- proj_vec * xy_f[ii]
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
                      r <- 10 * radius(self$vertices)
                      op_diff <- as_coord2d(degrees(angle), r)
                      op_diff <- as_coord3d(op_diff$x, op_diff$y, z = r / sqrt(2))
                      op_ref <- mean(self$vertices)$translate(op_diff)
                      d1 <- abs(op_ref - mean(self$vertices[indices1]))
                      d2 <- abs(op_ref - mean(self$vertices[indices2]))
                      if (d1 > d2) {
                          indices_obscured <- indices2
                          indices_visible <- indices1
                      } else {
                          indices_obscured <- indices1
                          indices_visible <- indices2
                      }
                      xy <- as_coord2d(self$vertices, alpha = degrees(angle), scale = scale)
                      x_obscured <- xy$x[full_indices(indices_obscured, n)]
                      y_obscured <- xy$y[full_indices(indices_obscured, n)]
                      x_visible <- xy$x[full_indices(indices_visible, n)]
                      y_visible <- xy$y[full_indices(indices_visible, n)]
                      polygonGrob(x=c(x_obscured, x_visible),
                                  y=c(y_obscured, y_visible),
                                  id.lengths = c(length(x_obscured), length(x_visible)),
                                  default.units="in", name = name)
                   }),
    active = list()
)

full_indices <- function(indices, n) c(indices, rev(2 * n + 1 - indices))

## Convex, open curved edge
CurvedEdge <- R6Class("edge_curved", inherit = Edge,
    public = list(op_grob = function(angle, scale, name = NULL) {
                      if (self$is_perpendicular_xyplane) {
                          private$op_grob_perpendicular(angle, scale, name = name)
                      } else {
                          private$op_grob_nonperpendicular(angle, scale, name = name)
                      }
                  }),
    private = list(
                   op_grob_nonperpendicular = function(angle, scale, name = NULL) {
                       xy <- as_coord2d(self$vertices, alpha = degrees(angle), scale = scale)
                       xyh <- convex_hull2d(xy)

                       xy_visible <- as_coord2d(self$vertices_visible_side(angle), alpha = degrees(angle), scale = scale)
                       xyh_visible <- convex_hull2d(xy_visible)

                       A <- list(list(x = xyh$x, y = xyh$y))
                       B <- list(list(x = xyh_visible$x, y = xyh_visible$y))
                       xy_minus <- gridGeometry::polyclip(A, B, op = "minus")

                       #### also include obscured edge (i.e. A intersection B)?
                       gridGeometry::xyListPolygon(xy_minus, name = name)
                   },
                   op_grob_perpendicular = function(angle, scale, name = NULL) {
                      n <- length(self$vertices) / 2
                      xy_f <- as_coord2d(self$vertices[seq(n)], alpha = degrees(angle), scale = scale)
                      projections <- numeric(n)
                      proj_vec <- as_coord2d(degrees(angle - 90), radius = 1)
                      for (ii in seq(n)) {
                          projections[ii] <- proj_vec * xy_f[ii]
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
                      xy <- as_coord2d(self$vertices, alpha = degrees(angle), scale = scale)
                      x <- numeric(0)
                      y <- numeric(0)
                      id <- numeric(0)

                      # figure out which part farthest
                      r <- 10 * radius(self$vertices)
                      op_diff <- as_coord2d(degrees(angle), r)
                      op_diff <- as_coord3d(op_diff$x, op_diff$y, z = r / sqrt(2))
                      op_ref <- mean(self$vertices)$translate(op_diff)
                      dists <- sapply(l_indices,
                                      function(x) {
                                          indices <- full_indices(x, n)
                                          abs(op_ref - mean(self$vertices[indices]))
                                      })
                      l_indices <- l_indices[order(dists)]
                      for (i in seq_along(l_indices)) {
                          indices <- full_indices(l_indices[[i]], n)
                          x <- append(x, xy$x[indices])
                          y <- append(y, xy$y[indices])
                          id <- append(id, rep(i, length(indices)))
                      }
                      polygonGrob(x=x, y=y, id=id, default.units="in", name = name)
                  }),
    active = list()
)
