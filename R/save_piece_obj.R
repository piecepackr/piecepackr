#' Save Wavefront OBJ files of board game pieces
#'
#' `save_piece_obj()` saves Wavefront OBJ files (including associated MTL and texture image) of board game pieces.
#' @inheritParams pieceGrob
#' @param axis_x First coordinate of the axis unit vector.
#' @param axis_y Second coordinate of the axis unit vector.
#' @param filename Name of Wavefront OBJ object.  By default `tempfile(fileext = ".obj")`.
#' @param res Resolution of the faces.
#' @return A data frame with named elements "obj", "mtl", "png" with the created filenames.
#' @examples
#'     if (all(capabilities(c("cairo", "png")))) {
#'       cfg <- game_systems("sans3d")$dominoes
#'       files <- save_piece_obj("tile_face", suit = 3+1, rank=6+1, cfg = cfg)
#'       print(files)
#'     }
#' @seealso See \code{\link{geometry_utils}} for a discussion of the 3D rotation parameterization.
#' @export
save_piece_obj <- function(piece_side = "tile_face", suit = 1, rank = 1,
                           cfg = getOption("piecepackr.cfg", pp_cfg()),
                           ...,
                           x = 0, y = 0, z = 0,
                           angle = 0, axis_x = 0, axis_y = 0,
                           width = NA, height = NA, depth = NA,
                           envir = getOption("piecepackr.envir"),
                           filename = tempfile(fileext = "%03d.obj"),
                           scale = 1, res = 72) {
    opt <- options(piecepackr.op_scale = 0)
    on.exit(options(opt), add = TRUE)
    if (is_angle(angle))
        angle <- as.double(angle, "degrees")

    nn <- max(lengths(list(piece_side, suit, rank, x, y, z, angle, axis_x, axis_y, width, height, depth)))
    piece_side <- rep(piece_side, length.out = nn)
    suit <- rep(suit, length.out = nn)
    rank <- rep(rank, length.out = nn)
    x <- rep(x, length.out = nn)
    y <- rep(y, length.out = nn)
    z <- rep(z, length.out = nn)
    angle <- rep(angle, length.out = nn)
    axis_x <- rep(axis_x, length.out = nn)
    axis_y <- rep(axis_y, length.out = nn)
    width <- rep(width, length.out = nn)
    height <- rep(height, length.out = nn)
    depth <- rep(depth, length.out = nn)
    scale <- rep(scale, length.out = nn)

    cfg <- get_cfg(cfg, envir)
    cfg <- rep(c(cfg), length.out = nn)

    filename <- rep(filename, length.out = nn)
    if (all(grepl("%[#0+=-]*[0-9.]*[diouxX]", filename)))
        filename <- sprintf(filename, seq_along(filename))
    filename <- normalizePath(filename, mustWork = FALSE)
    stopifnot(!any(duplicated(filename)))

    l <- lapply(seq.int(nn), function(i) {
        df <- save_piece_obj_helper(piece_side[i], suit[i], rank[i], cfg[[i]],
                                    x[i], y[i], z[i],
                                    angle[i], axis_x[i], axis_y[i],
                                    width[i], height[i], depth[i],
                                    filename[i], scale[i], res)
        df
    })
    do.call(rbind, l)
}

save_piece_obj_helper <- function(piece_side = "tile_face", suit = 1, rank = 1,
                           cfg = getOption("piecepackr.cfg", pp_cfg()),
                           x = 0, y = 0, z = 0,
                           angle = 0, axis_x = 0, axis_y = 0,
                           width = NA, height = NA, depth = NA,
                           filename = tempfile(fileext = ".obj"), scale = 1, res = 72) {
    cfg <- as_pp_cfg(cfg)
    suit <- ifelse(is.na(suit), 1, suit)
    rank <- ifelse(is.na(rank), 1, rank)
    if (is.na(angle)) angle <- 0
    if (is.na(axis_x)) axis_x <- 0
    if (is.na(axis_y)) axis_y <- 0
    if (is.na(width)) width <- cfg$get_width(piece_side, suit, rank)
    if (is.na(height)) height <- cfg$get_height(piece_side, suit, rank)
    if (is.na(depth)) depth <- cfg$get_depth(piece_side, suit, rank)
    if (is.na(z)) z <- 0.5 * depth
    width <- scale * width
    height <- scale * height
    depth <- scale * depth

    l <- cfg$save_obj(piece_side, suit, rank,
                      x, y, z,
                      angle, axis_x, axis_y,
                      width, height, depth,
                      filename, res)
    as.data.frame(l)
}

write_mtl <- function(mtl_filename, png_filename) {
    writeLines(c("newmtl material_0", paste("map_Kd", png_filename)),
               mtl_filename)
}

# 1,2,3,4 -> 1,4,3,2
rev_shift <- function(x) c(x[1], rev(x[-1]))

# (2-sided token) rotation matrix to move from "face" to `side`
side_R <- function(side) {
    switch(side,
           back = R_y(180),
           base = R_x(-90),
           left = R_y(-90) %*% R_z(-90),
           right = R_y(90) %*% R_z(90),
           top = R_x(90) %*% R_z(180),
           diag(3))
}
# (2-sided token) rotation matrix to return to "face" from `side`
side_R_rev <- function(side) {
    switch(side,
           back = R_y(-180),
           base = R_x(90),
           left = R_z(90) %*% R_y(90),
           right = R_z(-90) %*% R_y(-90),
           top = R_z(-180) %*% R_x(-90),
           diag(3))
}

get_scaling_factors <- function(side, width, height, depth) {
    w <- switch(side,
                left = depth,
                right = depth,
                width)
    h <- switch(side,
                back = height,
                face = height,
                left = width,
                right = width,
                depth)
    d <- switch(side,
                base = height,
                top = height,
                left = height,
                right = height,
                depth)
    list(width = w, height = h, depth = d)
}

# two-sided objects
save_2s_obj <- function(piece_side = "tile_face", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)
    piece <- get_piece(piece_side)
    side <- get_side(piece_side)
    opt <- cfg$get_piece_opt(paste0(piece, "_face"), suit, rank)
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)

    # geometric vertices
    whd <- get_scaling_factors(side, width, height, depth)
    pc <- as_coord3d(x, y, z)
    R <- side_R(side) %*% AA_to_R(angle, axis_x, axis_y)
    xyz <- Token2S$new(shape, whd, pc, R)$xyz

    # texture coordinates
    back <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, !opt$back, width = opt$shape_w, height = opt$shape_h)
    xy_npc <- as_coord2d(shape$npc_coords)
    xy_npc_back <- as_coord2d(back$npc_coords)
    xy_vt_f <- xy_npc$scale(0.4, 1)$translate(as_coord2d(0.025, 0))
    xy_vt_b <- xy_npc_back$scale(0.4, 1)$translate(as_coord2d(0.575, 0))
    xy_vt_e <- list(x = c(0.52, 0.48, 0.48, 0.52), y = c(0, 0, 1, 1))
    vt <- list(x = c(xy_vt_f$x, xy_vt_b$x, xy_vt_e$x),
               y =  c(xy_vt_f$y, xy_vt_b$y, xy_vt_e$y))

    # textured face elements
    nv <- length(xyz) / 2
    f <- list()
    f[[1]] <- list(v = seq(nv), vt = seq(nv)) # top
    #### #217 "coin_arrangement"
    f[[2]] <- list(v = rev_shift(nv + seq(nv)), vt = rev_shift(nv + seq(nv))) # bottom
    # sides
    for (i in seq(nv)) {
        ir <- i %% nv + 1
        il <- ir %% nv + 1
        f[[2 + i]] <- list(v = c(ir + nv, il + nv, il, ir),
                           vt = 2 * nv + 1:4)
    }

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = vt, f = f)
    write_2s_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

write_2s_texture <- function(piece_side = "tile_face", suit = 1, rank = 1, cfg = pp_cfg(),
                             ...,
                             filename = tempfile(fileext = ".png"), res = 72) {

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
    piece <- get_piece(piece_side)
    piece_face <- paste0(piece, "_face")
    piece_back <- paste0(piece, "_back")
    height <- cfg$get_height(piece_face, suit, rank)
    width <- cfg$get_width(piece_face, suit, rank)
    edge_color <- cfg$get_piece_opt(piece_face, suit, rank)$edge_color

    args <- list(filename = filename, height = height, width = 2.5 * width,
                 units = "in", res = res, bg = "transparent")
    if (capabilities("cairo"))
        args$type <- "cairo"
    do.call(grDevices::png, args)

    # front
    pushViewport(viewport(x = 0.225, width = 0.45))
    draw_piece_and_bleed(piece_face, suit, rank, cfg)
    popViewport()

    # edge
    pushViewport(viewport(x = 0.5, width = 0.1))
    grid.rect(gp = gpar(col = "transparent", fill = edge_color))
    popViewport()

    # back
    pushViewport(viewport(x = 0.775, width = 0.45))
    draw_piece_and_bleed(piece_back, suit, rank, cfg)
    popViewport()
    grDevices::dev.off()

    invisible(filename)
}

#' Alternative Wavefront OBJ file generators
#'
#' These are alternative Wavefront OBJ generators intended to be used as a `obj_fn` attribute
#' in a [pp_cfg()] \dQuote{configuration list}.
#' `save_ellipsoid_obj()` saves an ellipsoid with a color equal to that piece's `background_color`.
#' `save_peg_doll_obj()` saves a \dQuote{peg doll} style doll with a color equal to that piece's `edge_color`
#' with a \dQuote{pawn belt} around it's waste from that suit's and rank's `belt_face`.
#' @seealso See [pp_cfg()] for a discussion of \dQuote{configuration lists}.
#'          Wavefront OBJ file generators are used by [save_piece_obj()] and (by default)
#'          [piece3d()] (`rgl` wrapper), [piece()] (`rayrender` wrapper),
#'          and [piece_mesh()] (`rayvertex` wrapper).
#' @inheritParams save_piece_obj
#' @param subdivide Increasing this value makes for a smoother ellipsoid (and larger OBJ file and slower render).
#'                  See [rgl::ellipse3d()].
#' @rdname obj_fns
#' @export
save_ellipsoid_obj <- function(piece_side = "bit_face", suit = 1, rank = 1,
                               cfg = getOption("piecepackr.cfg", pp_cfg()),
                               ...,
                               x = 0, y = 0, z = 0,
                               angle = 0, axis_x = 0, axis_y = 0,
                               width = NA, height = NA, depth = NA,
                               filename = tempfile(fileext = ".obj"), subdivide=3) {
    assert_suggested("rgl")
    if (is_angle(angle))
        angle <- as.double(angle, "degrees")

    cfg <- as_pp_cfg(cfg)
    piece <- get_piece(piece_side)
    side <- get_side(piece_side)

    # geometric vertices
    R <- side_R(side) %*% AA_to_R(angle, axis_x, axis_y)
    whd <- get_scaling_factors(side, width, height, depth)
    e <- rgl::ellipse3d(diag(3), t=0.5, subdivide=subdivide, smooth=FALSE)
    xyz <- as.data.frame(t(e$vb))
    names(xyz) <- c("x", "y", "z", "u")
    xyz <- as_coord3d(xyz)$
        scale(whd$width, whd$height, whd$depth)$
        translate(as_coord3d(x, y, z))

    # texture coordinates
    xy_vt <- list(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1))

    # face elements
    f <- lapply(seq.int(ncol(e$ib)), function(x) list(v = e$ib[, x], vt = 1:4))

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = xy_vt, f = f)

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
    opt <- cfg$get_piece_opt(paste0(piece, "_face"), suit, rank)
    grDevices::png(png_filename, bg=opt$background_color)
    grid.newpage()
    grDevices::dev.off()

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

#' @rdname obj_fns
#' @export
save_peg_doll_obj <- function(piece_side = "pawn_top", suit = 1, rank = 1,
                              cfg = getOption("piecepackr.cfg", pp_cfg()),
                              ...,
                              x = 0, y = 0, z = 0,
                              angle = 0, axis_x = 0, axis_y = 0,
                              width = NA, height = NA, depth = NA,
                              filename = tempfile(fileext = ".obj"), res = 72) {

    assert_suggested("rgl")
    if (is_angle(angle))
        angle <- as.double(angle, "degrees")

    cfg <- as_pp_cfg(cfg)
    piece <- get_piece(piece_side)
    side <- get_side(piece_side)

    R <- side_R(side) %*% AA_to_R(angle, axis_x, axis_y)
    whd <- get_scaling_factors(side, width, height, depth)
    pc <- as_coord3d(x, y, z)
    # normalized peg doll is laying down face up
    width <- whd$width
    height <- whd$height
    depth <- whd$depth
    if (!nigh(width, depth)) warn("Base of peg doll is not circular")
    # base
    circle <- as_coord2d(pp_shape("convex72", back=TRUE)$npc_coords)$translate(as_coord2d(-0.5, -0.5))
    xyz_base <- data.frame(x=circle$x, y=-0.5, z=circle$y)
    vt_base <- as.data.frame(circle$clone()$scale(0, 0.2)$translate(as_coord2d(0.5, 0.1)))[, c("x", "y")]
    n_base <- nrow(xyz_base)
    f_base <- list(list(v = seq(n_base), vt = seq(n_base)))

    # body below
    belt <- cfg$get_height("belt_face", suit, rank) / height
    head <- width / height
    neck <- 0.1 * head
    y_below <- (1 - head - neck - belt) / 2
    xyz_below <- data.frame(x=circle$x, y=y_below - 0.5, z=circle$y)
    vt_body <- data.frame(x = c(0, 0, 1, 1), y = c(0.1, 0, 0, 0.1))

    f_below <- list()
    for (i in seq(n_base)) {
        ir <- i %% n_base + 1
        il <- ir %% n_base + 1
        f_below[[i]] <- list(v = c(n_base + ir, n_base + il, il, ir),
                             vt = n_base + 1:4)
    }
    # belt
    xyz_belt <- data.frame(x=circle$x, y=y_below - 0.5 + belt, z=circle$y)
    vt_belt <- data.frame(x = rep(seq(0, 1, length.out = nrow(xyz_belt) + 1L), 2),
                          y = rep(c(0.75, 0.25), each=nrow(xyz_belt) + 1L))
    f_belt <- list()
    for (i in seq(n_base)) {
        if (i <= n_base / 2) {
            ir <- i
            il <- ir + 1
            v <- c(n_base + ir, n_base + il, il, ir)
            vt <- c(0.5 * n_base + 2 - ir, 0.5*n_base + 2 - il, 1.5*n_base + 3 - il, 1.5*n_base + 3 -ir)
            f_belt[[i]] <- list(v = n_base + v, vt = n_base + 4 + vt)
        } else {
            ir <- i
            il <- ir + 1
            ilv <- ifelse(il > n_base, 1, il)
            v <- c(n_base + ir, n_base + ilv, ilv, ir)
            vt <- c(1.5 * n_base + 2 - ir, 1.5*n_base + 2 - il, 2.5*n_base + 3 - il, 2.5*n_base + 3 -ir)
            f_belt[[i]] <- list(v = n_base + v, vt = n_base + 4 + vt)
        }
    }

    # body above
    xyz_above <- data.frame(x=circle$x, y=2 * y_below + belt - 0.5, z=circle$y)

    f_above <- list()
    for (i in seq(n_base)) {
        ir <- i %% n_base + 1
        il <- ir %% n_base + 1
        f_above[[i]] <- list(v = 2 * n_base + c(n_base + ir, n_base + il, il, ir),
                             vt = n_base + 1:4)
    }
    # neck
    xyz_neck1 <- data.frame(x=0.95*circle$x, y=0.5-head-0.95*neck, z=0.95*circle$y)
    xyz_neck2 <- data.frame(x=0.85*circle$x, y=0.5-head-0.7*neck, z=0.85*circle$y)
    xyz_neck3 <- data.frame(x=0.70*circle$x, y=0.5-head-0.4*neck, z=0.70*circle$y)
    xyz_neck4 <- data.frame(x=0.50*circle$x, y=0.5-head, z=0.50*circle$y)
    xyz_neck5 <- data.frame(x=0.50*circle$x, y=0.5-0.9*head, z=0.50*circle$y)
    xyz_neck <- rbind(xyz_neck1, xyz_neck2, xyz_neck3, xyz_neck4, xyz_neck5)

    f_neck <- vector("list", 5 * n_base)
    for (i in seq(n_base)) {
        ir <- i %% n_base + 1
        il <- ir %% n_base + 1
        f_neck[[i]] <- list(v = 3 * n_base + c(n_base + ir, n_base + il, il, ir),
                            vt = 3 * n_base + 4 + 2 + 1:4)
        f_neck[[n_base + i]] <- list(v = 4 * n_base + c(n_base + ir, n_base + il, il, ir),
                                     vt = 3 * n_base + 4 + 2 + 1:4)
        f_neck[[2 * n_base + i]] <- list(v = 5 * n_base + c(n_base + ir, n_base + il, il, ir),
                                         vt = 3 * n_base + 4 + 2 + 1:4)
        f_neck[[3 * n_base + i]] <- list(v = 6 * n_base + c(n_base + ir, n_base + il, il, ir),
                                         vt = 3 * n_base + 4 + 2 + 1:4)
        f_neck[[4 * n_base + i]] <- list(v = 7 * n_base + c(n_base + ir, n_base + il, il, ir),
                                         vt = 3 * n_base + 4 + 2 + 1:4)
    }

    # head
    e <- rgl::ellipse3d(diag(3), t=0.5, subdivide=4, smooth=FALSE)
    xyz <- as.data.frame(t(e$vb))
    names(xyz) <- c("x", "y", "z", "w")
    xyz_head <- as_coord3d(xyz)$
        scale(y = width/height)$
        translate(as_coord3d(x = 0, y = 0.5 - 0.5*width/height))
    xyz_head <- as.data.frame(xyz_head)[, c("x", "y", "z")]

    vt_head <- data.frame(x = c(0, 0, 1, 1), y = c(1, 0.90, 0.90, 1))

    f_head <- lapply(seq.int(ncol(e$ib)), function(x) list(v = 9 * n_base + e$ib[, x],
                                                           vt = 3 * n_base + 4 + 2 + 1:4))

    # write obj
    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    xyz <- rbind(xyz_base, xyz_below, xyz_belt, xyz_above, xyz_neck, xyz_head)
    xyz <- as_coord3d(xyz)$
        scale(whd$width, whd$height, whd$depth)$
        transform(R)$
        translate(pc)

    xy_vt <- rbind(vt_base, vt_body, vt_belt, vt_head)

    faces <- c(f_base, f_below, f_belt, f_above, f_neck, f_head)

    write_obj(filename, v = xyz, vt = xy_vt, f = faces)
    write_peg_doll_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

write_peg_doll_texture <- function(piece_side = "pawn_face", suit = 1, rank = 1, cfg = pp_cfg(),
                                   ...,
                                   filename = tempfile(fileext = ".png"), res = 72) {

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
    height <- cfg$get_height("belt_face", suit, rank)
    width <- cfg$get_width("belt_face", suit, rank)
    piece <- get_piece(piece_side)
    opt <- cfg$get_piece_opt(paste0(piece, "_face"), suit, rank)
    args <- list(filename = filename, height = 2 * height, width = width,
                 units = "in", res = res, bg = "transparent")
    if (capabilities("cairo"))
        args$type <- "cairo"
    do.call(grDevices::png, args)
    pushViewport(viewport(y=0.875, height=0.25))
    grid.rect(gp = gpar(col=NA_character_, fill=opt$edge_color))
    popViewport()
    pushViewport(viewport(y=0.500, height=0.50))
    grid.piece("belt_face", suit = suit, rank = rank, cfg = cfg, op_scale = 0)
    popViewport()
    pushViewport(viewport(y=0.125, height=0.25))
    grid.rect(gp = gpar(col=NA_character_, fill=opt$edge_color))
    popViewport()

    grDevices::dev.off()
    invisible(filename)
}

save_die_obj <- function(piece_side = "die_face", suit = 1, rank = 1, cfg = pp_cfg(),
                          ...,
                          x = 0, y = 0, z = 0,
                          angle = 0, axis_x = 0, axis_y = 0,
                          width = NA, height = NA, depth = NA,
                          filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    xyz <- die_xyz(suit, rank, cfg,
                   x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth)

    xy_vt <- list(x = rep(c(0, 0.5, 1), 4),
                  y = rep(c(1, 2/3, 1/3, 0), each = 3))

    # textured face elements
    f <- list()
    f[[1]] <- list(v = 1:4, vt = c(1, 4, 5, 2))
    f[[2]] <- list(v = c(8, 4, 3, 7), vt = c(2, 5, 6, 3))
    f[[3]] <- list(v = c(1, 4, 8, 5), vt = c(4, 7, 8, 5))
    f[[4]] <- list(v = c(6, 5, 8, 7), vt = c(5, 8, 9, 6))
    f[[5]] <- list(v = c(1, 5, 6, 2), vt = c(7, 10, 11, 8))
    f[[6]] <- list(v = c(3, 2, 6, 7), vt = c(8, 11, 12, 9))

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = xy_vt, f = f)
    write_die_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

write_pyramid_texture <- function(piece_side = "pyramid_face", suit = 1, rank = 1, cfg = pp_cfg(),
                             ...,
                             filename = tempfile(fileext = ".png"), res = 72) {

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
    height <- cfg$get_height("pyramid_face", suit, rank)
    width <- cfg$get_width("pyramid_face", suit, rank)

    args <- list(filename = filename, height = height, width = 4 * width,
                 units = "in", res = res, bg = "transparent")
    if (capabilities("cairo"))
        args$type <- "cairo"
    do.call(grDevices::png, args)

    pushViewport(viewport(x = 0.125, width = 0.25))
    draw_piece_and_bleed("pyramid_face", suit, rank, cfg)
    popViewport()

    pushViewport(viewport(x = 0.375, width = 0.25))
    draw_piece_and_bleed("pyramid_left", suit, rank, cfg)
    popViewport()

    pushViewport(viewport(x = 0.625, width = 0.25))
    draw_piece_and_bleed("pyramid_back", suit, rank, cfg)
    popViewport()

    pushViewport(viewport(x = 0.875, width = 0.25))
    draw_piece_and_bleed("pyramid_right", suit, rank, cfg)
    popViewport()

    grDevices::dev.off()
    invisible(filename)
}

write_die_texture <- function(piece_side = "die_face", suit = 1, rank = 1, cfg = pp_cfg(),
                              ..., filename = tempfile(fileext = ".png"), res = 72) {

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
    width <- cfg$get_width("die_face", suit, rank)

    args <- list(filename = filename, height = 3 * width, width = 2 * width,
                 units = "in", res = res, bg = "transparent")
    if (capabilities("cairo"))
        args$type <- "cairo"
    do.call(grDevices::png, args)

    rs <- get_die_face_info(suit, cfg$die_arrangement)
    pushViewport(viewport(x = 0.25, width = 0.5, y = 5/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[1], rank = rs$rank[1], cfg = cfg,
               angle = rs$angle[1], op_scale = 0)
    popViewport()
    pushViewport(viewport(x = 0.75, width = 0.5, y = 5/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[2], rank = rs$rank[2], cfg = cfg,
               angle = rs$angle[2], op_scale = 0)
    popViewport()
    pushViewport(viewport(x = 0.25, width = 0.5, y = 3/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[3], rank = rs$rank[3], cfg = cfg,
               angle = rs$angle[3], op_scale = 0)
    popViewport()
    pushViewport(viewport(x = 0.75, width = 0.5, y = 3/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[4], rank = rs$rank[4], cfg = cfg,
               angle = rs$angle[4], op_scale = 0)
    popViewport()
    pushViewport(viewport(x = 0.25, width = 0.5, y = 1/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[5], rank = rs$rank[5], cfg = cfg,
               angle = rs$angle[5], op_scale = 0)
    popViewport()
    pushViewport(viewport(x = 0.75, width = 0.5, y = 1/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[6], rank = rs$rank[6], cfg = cfg,
               angle = rs$angle[6], op_scale = 0)
    popViewport()

    grDevices::dev.off()
    invisible(filename)
}

draw_piece_and_bleed <- function(piece_side, suit, rank, cfg) {
    g <- pieceGrob(piece_side, suit, rank, cfg, default.units = "npc", op_scale = 0)
    bleed_color <- cfg$get_piece_opt(piece_side, suit, rank)$bleed_color
    b <- pp_shape()$polyclip(g, "minus", gp=gpar(col=NA, fill=bleed_color))
    grid.draw(b)
    grid.draw(g)
}

# pyramid top up
save_pt_obj <- function(piece_side = "pyramid_top", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)

    # vertices
    xyz <- pt_xyz(x, y, z,
                  angle, axis_x, axis_y,
                  width, height, depth)

    # texture coordinates
    xy_vt <- list(x = seq(0, 1, 0.125), y = rep(c(0, 1), length.out = 9))

    # textured face elements
    f <- list()
    f[[1]] <- list(v = c(2, 3, 1), vt = c(3, 5, 4)) # left
    f[[2]] <- list(v = c(3, 4, 1), vt = c(5, 7, 6)) # back
    f[[3]] <- list(v = c(4, 5, 1), vt = c(7, 9, 8)) # right
    f[[4]] <- list(v = c(5, 2, 1), vt = c(1, 3, 2)) # front

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = xy_vt, f = f)
    write_pyramid_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

# pyramid side down
save_ps_obj <- function(piece_side = "pyramid_face", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)

    # geometric vertices
    xyz <- ps_xyz(x, y, z,
                  angle, axis_x, axis_y,
                  width, height, depth)

    # texture coordinates
    xy_vt <- list(x = seq(0, 1, 0.125), y = rep(c(0, 1), length.out = 9))

    # textured face elements
    f <- list()
    if (piece_side == "pyramid_face") {
        f[[1]] <- list(v = c(2, 5, 3), vt = c(3, 5, 4)) # left
        f[[2]] <- list(v = c(5, 4, 3), vt = c(5, 7, 6)) # back
        f[[3]] <- list(v = c(4, 1, 3), vt = c(7, 9, 8)) # right
        f[[4]] <- list(v = c(1, 2, 3), vt = c(1, 3, 2)) # front
    } else if (piece_side == "pyramid_left") {
        f[[1]] <- list(v = c(1, 2, 3), vt = c(3, 5, 4)) # left
        f[[2]] <- list(v = c(2, 5, 3), vt = c(5, 7, 6)) # back
        f[[3]] <- list(v = c(5, 4, 3), vt = c(7, 9, 8)) # right
        f[[4]] <- list(v = c(4, 1, 3), vt = c(1, 3, 2)) # front
    } else if (piece_side == "pyramid_back") {
        f[[1]] <- list(v = c(4, 1, 3), vt = c(3, 5, 4)) # left
        f[[2]] <- list(v = c(1, 2, 3), vt = c(5, 7, 6)) # back
        f[[3]] <- list(v = c(2, 5, 3), vt = c(7, 9, 8)) # right
        f[[4]] <- list(v = c(5, 4, 3), vt = c(1, 3, 2)) # front
    } else { # pyramid right
        f[[1]] <- list(v = c(5, 4, 3), vt = c(3, 5, 4)) # left
        f[[2]] <- list(v = c(4, 1, 3), vt = c(5, 7, 6)) # back
        f[[3]] <- list(v = c(1, 2, 3), vt = c(7, 9, 8)) # right
        f[[4]] <- list(v = c(2, 5, 3), vt = c(1, 3, 2)) # front
    }

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = xy_vt, f = f)
    write_pyramid_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

# v: v$x v$y v$z
# vt: vt$x vt$y
# f: list of lists of faces: f[[1]]$v, f[[1]]$vt
write_obj <- function(file, v, vt = NULL, f = NULL) {
    ext <- tools::file_ext(file)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", file)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", file)
    write_mtl(mtl_filename, basename(png_filename))

    cat(paste0("# Written by piecepackr v", packageDescription("piecepackr")$Version),
        "# https://github.com/piecepackr/piecepackr",
        paste("mtllib", basename(mtl_filename)),
        "# geometric vertices",
        paste("v", v$x, v$y, v$z),
        sep = "\n", file = file)
    if (!is.null(vt)) {
        cat("# texture coordinates",
            paste("vt", vt$x, vt$y),
            sep = "\n", file = file, append = TRUE)
    }
    if (!is.null(f)) {
        cat("# Textured polygonal face element",
            "usemtl material_0",
            sapply(f, function(x) paste("f", paste0(x$v, "/", x$vt, collapse = " "))),
            sep = "\n", file = file, append = TRUE)
    }
    invisible(NULL)
}

# two-sided objects
save_holed_board_obj_fn <- function(nrows = 4L, ncols = 4L, margin = 0) { # nolint
    force(nrows)
    force(ncols)
    force(margin)
    stopifnot(nrows == ncols)
    function(piece_side = "tile_face", suit = 1, rank = 1, cfg = pp_cfg(),
             ...,
             x = 0, y = 0, z = 0,
             angle = 0, axis_x = 0, axis_y = 0,
             width = NA, height = NA, depth = NA,
             filename = tempfile(fileext = ".obj"), res = 72) {

        current_dev <- grDevices::dev.cur()
        if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)

        cfg <- as_pp_cfg(cfg)
        piece <- get_piece(piece_side)
        side <- get_side(piece_side)
        opt <- cfg$get_piece_opt(paste0(piece, "_face"), suit, rank)
        shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)

        grDevices::pdf(NULL, width = width, height = height)
        # snpc coords are npc if square
        xyi_npc <- xyi_holed_board(opt, nrows, ncols, margin)
        grDevices::dev.off()

        # Interior holes as integers
        xc <- rep(seq.int(ncols), times = nrows)
        yc <- rep(seq.int(nrows), each = ncols)

        # geometric vertices
        whd <- get_scaling_factors(side, width, height, depth)
        pc <- as_coord3d(x, y, z)
        R <- side_R(side) %*% AA_to_R(angle, axis_x, axis_y)

        xyz_scaled <- as_coord3d(xyi_npc)$
            translate(as_coord3d(-0.5, -0.5, 0.5))$
            scale(whd$width, whd$height, whd$depth)
        xyz_f <- xyz_scaled$transform(R)$translate(pc)

        xyz_scaled <- as_coord3d(xyi_npc)$
            translate(as_coord3d(-0.5, -0.5, -0.5))$
            scale(whd$width, whd$height, whd$depth)
        xyz_b <- xyz_scaled$transform(R)$translate(pc)

        xyz <- as_coord3d(x = c(xyz_f$x, xyz_b$x),
                          y = c(xyz_f$y, xyz_b$y),
                          z = c(xyz_f$z, xyz_b$z))

        # texture coordinates
        xy_npc_top <- as_coord2d(xyi_npc)
        xy_npc_bot <- as_coord2d(xyi_npc)
        xy_vt_f <- xy_npc_top$scale(0.4, 1)$translate(as_coord2d(0.025, 0))
        xy_vt_b <- xy_npc_bot$scale(0.4, 1)$translate(as_coord2d(0.575, 0))
        xy_vt_e <- list(x = c(0.52, 0.48, 0.48, 0.52), y = c(0, 0, 1, 1))
        vt <- list(x = c(xy_vt_f$x, xy_vt_b$x, xy_vt_e$x),
                   y =  c(xy_vt_f$y, xy_vt_b$y, xy_vt_e$y))

        # textured face elements
        nv <- length(xyz) / 2
        f <- list()

        # Outside shape
        xyio <- xyi_npc[ which(xyi_npc$id == 0), ]
        nvo <- nrow(xyio)

        # Within a circle which vertices are left, top, right, or bottom?
        nvc <- nrow(xyi_npc[ which(xyi_npc$id == 1), ])
        ivc_left <- seq.int(0.25 * nvc + 1, length.out = 0.5 * nvc + 1)
        ivc_top <- c(seq.int(0.50 * nvc + 1, length.out = 0.5 * nvc), 1)
        ivc_right <- c(seq.int(0.75 * nvc + 1, length.out = 0.25 * nvc),
                       seq.int(1, length.out = 0.25 * nvc + 1))
        ivc_bot <- seq.int(1, length.out = 0.5 * nvc + 1)

        # Left-most
        idc_left <- order(xc, yc)[seq.int(nrows)] # left-most circles
        id_left <- which(xyio$x < 0.5)
        # cycle elements so first one is "top" using `cycle_elements(x, n)`
        id_leftm <- which.max(xyio[id_left, ]$y)
        id_left <- cycle_elements(id_left, id_leftm - 1)
        for (idc in idc_left) {
            id_left <- c(id_left, nvo + nvc * (idc - 1) + ivc_left)
        }
        f[[1L]] <- list(v = id_left, vt = id_left) # top
        f[[2L]] <- list(v = rev_shift(nv + id_left), vt = rev_shift(nv + id_left)) # bottom

        # Right-most
        idc_right <- order(-xc, -yc)[seq.int(nrows)]
        id_right <- which(xyio$x > 0.5)
        id_rightm <- which.min(xyio[id_right, ]$y)
        id_right <- cycle_elements(id_right, id_rightm - 1)
        for (idc in idc_right) {
            id_right <- c(id_right, nvo + nvc * (idc - 1) + ivc_right)
        }
        f[[3L]] <- list(v = id_right, vt = id_right) # top
        f[[4L]] <- list(v = rev_shift(nv + id_right), vt = rev_shift(nv + id_right)) # bottom

        # Top-most
        idc_top <- order(-yc, xc)[seq.int(ncols)]
        id_top <- which(xyio$y > 0.5)
        id_topm <- which.max(xyio[id_top, ]$x)
        id_top <- cycle_elements(id_top, id_topm - 1)
        for (idc in idc_top) {
            id_top <- c(id_top, nvo + nvc * (idc - 1) + ivc_top)
        }
        f[[5L]] <- list(v = id_top, vt = id_top) # top
        f[[6L]] <- list(v = rev_shift(nv + id_top), vt = rev_shift(nv + id_top)) # bottom

        # Bottom-most
        idc_bot <- order(yc, -xc)[seq.int(ncols)]
        id_bot <- which(xyio$y < 0.5)
        id_botm <- which.min(xyio[id_bot, ]$x)
        id_bot <- cycle_elements(id_bot, id_botm - 1)
        for (idc in idc_bot) {
            id_bot <- c(id_bot, nvo + nvc * (idc - 1) + ivc_bot)
        }
        f[[7L]] <- list(v = id_bot, vt = id_bot) # top
        f[[8L]] <- list(v = rev_shift(nv + id_bot), vt = rev_shift(nv + id_bot)) # bottom

        # Interior
        ivc_lr <- seq.int(1, length.out = 0.25 * nvc + 1)
        ivc_ll <- seq.int(0.25 * nvc + 1, length.out = 0.25 * nvc + 1)
        ivc_ul <- seq.int(0.50 * nvc + 1, length.out = 0.25 * nvc + 1)
        ivc_ur <- c(seq.int(0.75 * nvc + 1, length.out = 0.25 * nvc), 1)
        if (nrows > 1L && ncols > 1L) {
            for (ir in seq.int(nrows - 1L)) {
                idc_ul <- which(yc == ir + 1L & xc == 1L)
                id_r <- nvo + nvc * (idc_ul - 1) + ivc_lr
                idc_ll <- which(yc == ir & xc == 1L)
                id_r <- c(id_r, nvo + nvc * (idc_ll - 1) + ivc_ur)
                if (ncols > 2L) {
                    idc_b <- order(yc, xc)[ncols * (ir - 1) + seq.int(2, ncols - 1)]
                    for (idc in idc_b) {
                        id_r <- c(id_r, nvo + nvc * (idc - 1) + ivc_top)
                    }
                }
                idc_lr <- which(yc == ir & xc == ncols)
                id_r <- c(id_r, nvo + nvc * (idc_lr - 1) + ivc_ul)
                idc_ur <- which(yc == ir + 1L & xc == ncols)
                id_r <- c(id_r, nvo + nvc * (idc_ur - 1) + ivc_ll)
                if (ncols > 2L) {
                    idc_t <- order(yc, -xc)[ncols * ir + seq.int(2, ncols - 1)]
                    for (idc in idc_t) {
                        id_r <- c(id_r, nvo + nvc * (idc - 1) + ivc_bot)
                    }
                }
                f[[length(f) + 1L]] <- list(v = id_r, vt = id_r) # top
                f[[length(f) + 1L]] <- list(v = rev_shift(nv + id_r), 
                                            vt = rev_shift(nv + id_r)) # bottom
            }
        }

        # Board edges
        np <- 0
        for (id in unique(xyi_npc$id)) {
            no <- length(which(xyi_npc$id == id))
            for (i in seq.int(no)) {
                ir <- i %% no + 1
                il <- ir %% no + 1
                f[[length(f) + 1L]] <- list(v = c(ir + nv + np, il + nv + np, il + np, ir + np),
                                            vt = 2 * nv + 1:4)
            }
            np <- np + no
        }

        ext <- tools::file_ext(filename)
        mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
        png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

        write_obj(filename, v = xyz, vt = vt, f = f)

        grDevices::png(png_filename, bg=opt$background_color)
        grid.newpage()
        grDevices::dev.off()

        invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
    }
}
