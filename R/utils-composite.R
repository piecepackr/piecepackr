# pieces that are composites of other pieces
# earlier pieces (in df) are "beneath" later pieces (view from the top)
CompositePiece <- R6Class("pp_composite",
    public = list(
        initialize = function(df = tibble(), envir = list()) {
            private$df <- df
            private$envir <- envir
        }),
    active = list(
        grob_fn = function() function(piece_side, suit, rank, cfg) {
            side <- get_side(piece_side)
            df <- switch(side, top = private$df, stop("don't know how to draw", piece_side, "yet"))
            #### set a different grob?
            pmap_piece(df, suit=suit, envir = private$envir, draw = FALSE)
        },
        op_grob_fn = function() function(piece_side, suit, rank, cfg,
                                         x, y, z, angle, type, width, height, depth,
                                         op_scale, op_angle) {
            x <- as.numeric(convertX(x, "in"))
            y <- as.numeric(convertY(y, "in"))
            z <- as.numeric(convertX(z, "in"))
            width <- as.numeric(convertX(width, "in"))
            height <- as.numeric(convertY(height, "in"))
            depth <- as.numeric(convertX(depth, "in"))
            side <- get_side(piece_side)
            df <- private$transform_df(piece_side, x, y, z, angle, width, height, depth)
            df <- switch(side, top = df, stop("don't know how to draw", piece_side, "yet"))
            #### need to re-order elements using `op_transform`?
            #### set a different grob?
            pmap_piece(df, suit = suit, envir = private$envir, draw = FALSE,
                       default.units = "in", op_scale = op_scale, op_angle = op_angle)
        },
        rayrender_fn = function() function(piece_side, suit, rank, cfg,
                                           x, y, z, angle, axis_x, axis_y,
                                           width, height, depth,
                                           scale = scale, res = res) {

            suit <- ifelse(is.na(suit), 1, suit)
            rank <- ifelse(is.na(rank), 1, rank)
            if (is.na(angle)) angle <- 0
            if (is.na(axis_x)) axis_x <- 0
            if (is.na(axis_y)) axis_y <- 0
            if (is.na(width)) width <- cfg$get_width(piece_side, suit, rank)
            if (is.na(height)) height <- cfg$get_height(piece_side, suit, rank)
            if (is.na(depth)) depth <- cfg$get_depth(piece_side, suit, rank)
            if (is.na(z)) z <- 0.5 * depth

            side <- get_side(piece_side)
            df <- private$transform_df(piece_side, x, y, z, angle, width, height, depth, axis_x, axis_y, scale)
            l <- pmap_piece(df, suit = suit, envir = private$envir, .f = piece, res = res)
            Reduce(rayrender::add_object, l)
        },
        rayvertex_fn = function() function(piece_side, suit, rank, cfg,
                                           x, y, z, angle, axis_x, axis_y,
                                           width, height, depth,
                                           scale = scale, res = res) {

            suit <- ifelse(is.na(suit), 1, suit)
            rank <- ifelse(is.na(rank), 1, rank)
            if (is.na(angle)) angle <- 0
            if (is.na(axis_x)) axis_x <- 0
            if (is.na(axis_y)) axis_y <- 0
            if (is.na(width)) width <- cfg$get_width(piece_side, suit, rank)
            if (is.na(height)) height <- cfg$get_height(piece_side, suit, rank)
            if (is.na(depth)) depth <- cfg$get_depth(piece_side, suit, rank)
            if (is.na(z)) z <- 0.5 * depth

            side <- get_side(piece_side)
            df <- private$transform_df(piece_side, x, y, z, angle, width, height, depth, axis_x, axis_y, scale)
            l <- pmap_piece(df, suit = suit, envir = private$envir, .f = piece_mesh, res = res)
            Reduce(rayvertex::add_shape, l)
        },
        rgl_fn = function() function(piece_side, suit, rank, cfg,
                                     x, y, z, angle, axis_x, axis_y,
                                     width, height, depth,
                                     scale = scale, res = res,
                                     alpha = alpha, lit = lit,
                                     shininess = shininess, textype = textype) {

            suit <- ifelse(is.na(suit), 1, suit)
            rank <- ifelse(is.na(rank), 1, rank)
            if (is.na(angle)) angle <- 0
            if (is.na(axis_x)) axis_x <- 0
            if (is.na(axis_y)) axis_y <- 0
            if (is.na(width)) width <- cfg$get_width(piece_side, suit, rank)
            if (is.na(height)) height <- cfg$get_height(piece_side, suit, rank)
            if (is.na(depth)) depth <- cfg$get_depth(piece_side, suit, rank)
            if (is.na(z)) z <- 0.5 * depth

            side <- get_side(piece_side)
            df <- private$transform_df(piece_side, x, y, z, angle, width, height, depth, axis_x, axis_y, scale)
            l <- pmap_piece(df, suit = suit, envir = private$envir, .f = piece3d,
                            res = res, alpha = alpha, lit = lit, shininess = shininess, textype = textype)
            as.integer(unlist(l))
        }),
    private = list(
        df = NULL, envir = NULL,
        transform_df = function(piece_side, x, y, z, angle, width, height, depth, axis_x = 0, axis_y = 0, scale = 1) {
            side <- get_side(piece_side)
            width <- scale * width
            height <- scale * height
            depth <- scale * depth
            df <- private$df
            whd <- switch(side,
                          top = get_scaling_factors("face", width=width, height=height, depth=depth),
                          face = get_scaling_factors("base", width=width, height=height, depth=depth),
                          base = get_scaling_factors("back", width=width, height=height, depth=depth),
                          back = get_scaling_factors("top", width=width, height=height, depth=depth),
                          list(width = depth, height = height, depth = width))
            df$height <- whd$height * df$height
            df$width <- whd$width * df$width
            df$depth <- whd$depth * df$depth
            R <- R_z(-180) %*% R_x(-90) %*% side_R(side) %*% AA_to_R(angle, axis_x, axis_y)
            pc <- Point3D$new(x, y, z)
            xyz_scaled <- Point3D$new(x = df$x, y = df$y, z = df$z)$translate(-0.5, -0.5, -0.5)$dilate(whd)
            xyz <- xyz_scaled$rotate(R)$translate(pc)
            df$x <- xyz$x
            df$y <- xyz$y
            df$z <- xyz$z
            #### Doesn't yet handle cases where df has its own angle, axis_x, axis_y values...
            df <- cbind(df, R_to_AA(R))
            df
        })
)
