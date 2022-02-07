# pieces that are composites of other pieces
# earlier pieces (in df) are "beneath" later pieces (as viewed from `ref_side`)
CompositePiece <- R6Class("pp_composite",
    public = list(
        initialize = function(df = tibble(), envir = list(), ref_side = "top") {
            private$df <- df
            private$envir <- envir
            private$ref_side <- ref_side
        }),
    active = list(
        grob_fn = function() function(piece_side, suit, rank, cfg) {
            df <- private$relative_df(piece_side)
            pmap_piece(df, suit=suit, envir = private$envir, draw = FALSE)
        },
        obj_fn = function() function(piece_side, suit, rank, cfg,
                                     x, y, z, angle, axis_x, axis_y,
                                     width, height, depth,
                                     filename, scale, res) {
            abort("Generally can't save Wavefront OBJ files for 'composite' pieces")
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
            df <- private$relative_df(piece_side)
            relative_side <- get_relative_side(piece_side, private$ref_side)
            df <- scale_df(df, relative_side, width, height, depth)
            df <- translate_df(df, relative_side, x, y, z, angle, width, height, depth)
            #### in future need to re-order elements using `op_sort()`?
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

            df <- private$transform_df(piece_side, x, y, z, angle, width, height, depth, axis_x, axis_y, scale)
            l <- pmap_piece(df, suit = suit, envir = private$envir, .f = piece3d,
                            res = res, alpha = alpha, lit = lit, shininess = shininess, textype = textype)
            as.integer(unlist(l))
        }),
    private = list(
        df = NULL, envir = NULL, ref_side = NULL,
        relative_df = function(piece_side) {
            side = get_relative_side(piece_side, private$ref_side)
            switch(side,
                   face = private$df,
                   back = back_df(private$df),
                   abort(paste("`CompositePiece()$relative_df()` can't handle relative side", side)))
        },
        transform_df = function(piece_side, x, y, z, angle, width, height, depth, axis_x = 0, axis_y = 0, scale = 1) {
            side <- get_side(piece_side)
            relative_side <- get_relative_side(piece_side, private$ref_side)

            df <- scale_df(private$df, relative_side, width, height, depth, scale)
            whd <- get_scaling_factors(relative_side, width=scale*width,
                                       height=scale*height, depth=scale*depth)
            R <- side_R_rev(private$ref_side) %*% side_R(side) %*% AA_to_R(angle, axis_x, axis_y)
            pc <- Point3D$new(x, y, z)
            xyz_scaled <- Point3D$new(x = df$x, y = df$y, z = df$z)$translate(-0.5, -0.5, -0.5)$dilate(whd)
            xyz <- xyz_scaled$rotate(R)$translate(pc)
            df$x <- xyz$x
            df$y <- xyz$y
            df$z <- xyz$z
            #### Doesn't yet handle cases where components in df have their own angle, axis_x, axis_y values
            df <- cbind(df, R_to_AA(R))
            df
        })
)

# Get "relative" side of `piece_side` given "reference" `ref_side`
get_relative_side <- function(piece_side, ref_side = "top") {
    side <- get_side(piece_side)
    switch(ref_side,
           face = switch(side,
                         face = "face",
                         right = "right",
                         back = "back",
                         left = "left",
                         top = "top",
                         base = "base"),
           right = switch(side,
                          face = "left",
                          right = "face",
                          back = "right",
                          left = "back",
                          top = "top",
                          base = "base"),
           back = switch(side,
                         face = "back",
                         right = "left",
                         back = "face",
                         left = "right",
                         top = "top",
                         base = "base"),
           left = switch(side,
                         face = "right",
                         right = "back",
                         back = "left",
                         left = "face",
                         top = "top",
                         base = "base"),
           top = switch(side,
                        face = "top",
                        right = "right",
                        back = "base",
                        left = "left",
                        top = "face",
                        base = "back"),
           base = switch(side,
                         face = "top",
                         right = "left",
                         back = "base",
                         left = "right",
                         top = "back",
                         base = "face"))
}

scale_df <- function(df, relative_side, width, height, depth, scale = 1) {
    whd <- get_scaling_factors(relative_side, width=scale*width,
                               height=scale*height, depth=scale*depth)
    df$height <- whd$height * df$height
    df$width <- whd$width * df$width
    df$depth <- whd$depth * df$depth
    df
}

# Simple case with no `axis_x` or `axis_y`
translate_df <- function(df, relative_side, x, y, z, angle, width, height, depth, scale = 1) {
    whd <- get_scaling_factors(relative_side, width=scale*width,
                               height=scale*height, depth=scale*depth)
    R <- R_z(angle)
    pc <- Point3D$new(x, y, z)
    xyz_scaled <- Point3D$new(x = df$x, y = df$y, z = df$z)$translate(-0.5, -0.5, -0.5)$dilate(whd)
    xyz <- xyz_scaled$rotate(R)$translate(pc)
    df$x <- xyz$x
    df$y <- xyz$y
    df$z <- xyz$z
    df$angle <- angle
    df
}

back_df = function(df) {
    df$z <- 1 - df$z
    pieces <- sapply(strsplit(df$piece_side, "_"), function(x) x[1])
    sides <- sapply(strsplit(df$piece_side, "_"), function(x) x[2])
    opposites <- sapply(sides, function(side) {
                            switch(side, face = "back", back = "face",
                                   left = "right", right = "left",
                                   top = "base", base = "top")
                    })
    df$piece_side <- paste(pieces, opposites, sep = "_")
    rev_df(df)
}
rev_df <- function(df) {
    if (nrow(df) > 1L) {
        df[nrow(df):1, ]
    } else {
        df
    }
}
