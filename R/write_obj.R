#' Save Wavefront OBJ files of board game pieces
#'
#' \code{save_piece_obj} saves Wavefront OBJ files (including associated MTL and texture image) of board game pieces.
#' @inheritParams pieceGrob
#' @param axis_x First coordinate of the axis unit vector.
#' @param axis_y Second coordinate of the axis unit vector.
#' @param filename Name of Wavefront OBJ object.
#' @param res Resolution of the faces.
#' @return A list with named elements "obj", "mtl", "png" with the created filenames.
#' @examples
#'     cfg <- game_systems("dejavu3d")$piecepack
#'     files <- save_piece_obj("tile_face", suit = 3, rank = 3, cfg = cfg)
#'     print(files)
#' @seealso See \code{\link{geometry_utils}} for a discussion of the 3D rotation parameterization.
#' @export
save_piece_obj <- function(piece_side = "tile_face", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
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

    cfg$save_obj(piece_side, suit, rank,
                 x, y, z,
                 angle, axis_x, axis_y,
                 width, height, depth,
                 filename, res)
}

write_2s_texture <- function(piece_side = "tile_face", suit = 1, rank = 1, cfg = pp_cfg(),
                             ...,
                             filename = tempfile(fileext = ".png"), res = 72) {

    piece <- get_piece(piece_side)
    piece_face <- paste0(piece, "_face")
    piece_back <- paste0(piece, "_back")
    height <- cfg$get_height(piece_face, suit, rank)
    width <- cfg$get_width(piece_face, suit, rank)
    edge_color <- cfg$get_piece_opt(piece_face, suit, rank)$edge_color

    grDevices::png(filename, height = height, width = 2.5 * width,
        units = "in", res = res, bg = "transparent")

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

write_mtl <- function(mtl_filename, png_filename) {
    writeLines(c("newmtl material_0", paste("map_Kd", png_filename)),
               mtl_filename)
}

# 1,2,3,4 -> 1,4,3,2
rev_shift <- function(x) c(x[1], rev(x[-1]))

# 2-sided token angle
side_R <- function(side) {
    switch(side,
           back = R_y(180),
           base = R_x(-90),
           left = R_y(-90) %*% R_z(-90),
           right = R_y(90) %*% R_z(90),
           top = R_x(90) %*% R_z(180),
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
write_2s_obj <- function(piece_side = "tile_face", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)
    piece <- get_piece(piece_side)
    side <- get_side(piece_side)
    opt <- cfg$get_piece_opt(paste0(piece, "_face"), suit, rank)
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    # geometric vertices
    R <- side_R(side) %*% AA_to_R(angle, axis_x, axis_y)
    whd <- get_scaling_factors(side, width, height, depth)
    pc <- Point3D$new(x, y, z)
    xyz <- Token2S$new(shape, whd, pc, R)$xyz

    # texture coordinates
    back <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, !opt$back)
    xy_npc <- Point2D$new(shape$npc_coords)
    xy_npc_back <- Point2D$new(back$npc_coords)
    xy_vt_f <- xy_npc$dilate(width = 0.4, height = 1)$translate(x = 0.025)
    xy_vt_b <- xy_npc_back$dilate(width = 0.4, height = 1)$translate(x = 0.575)
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
        f[[2 + i]] <- list(v = c(ir + nv, il + nv, il, ir), vt = 2 * nv + 1:4)
    }

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = vt, f = f)
    write_2s_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

write_die_obj <- function(piece_side = "die_face", suit = 1, rank = 1, cfg = pp_cfg(),
                          ...,
                          x = 0, y = 0, z = 0,
                          angle = 0, axis_x = 0, axis_y = 0,
                          width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    pc <- Point3D$new(x, y, z)

    xs <- c(0, 0, 1, 1, 0, 0, 1, 1) - 0.5
    ys <- c(1, 0, 0, 1, 1, 0, 0, 1) - 0.5
    zs <- rep(c(1, 0), each = 4) - 0.5

    # figure out rotation to reach each die face
    rs <- get_die_face_info(suit, cfg$die_arrangement) #### also allow customization of angle #175
    i <- which(rs$rank == rank)
    dR <- switch(i,
                 diag(3),
                 R_y(-90) %*% R_z(90),
                 R_x(90) %*% R_z(-90),
                 R_x(180),
                 R_y(90) %*% R_z(90),
                 R_x(-90) %*% R_z(90),
                 diag(3))
    R <- dR %*% AA_to_R(angle, axis_x, axis_y)
    xyz <- Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(R)$translate(pc)

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

    height <- cfg$get_height("pyramid_face", suit, rank)
    width <- cfg$get_width("pyramid_face", suit, rank)

    grDevices::png(filename, height = height, width = 4 * width,
        units = "in", res = res, bg = "transparent")

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
    width <- cfg$get_width("die_face", suit, rank)

    grDevices::png(filename, height = 3 * width, width = 2 * width,
                   units = "in", res = res, bg = "transparent")

    rs <- get_die_face_info(suit, cfg$die_arrangement) #### also angle
    pushViewport(viewport(x = 0.25, width = 0.5, y = 5/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[1], rank = rs$rank[1], cfg = cfg)
    popViewport()
    pushViewport(viewport(x = 0.75, width = 0.5, y = 5/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[2], rank = rs$rank[2], cfg = cfg)
    popViewport()
    pushViewport(viewport(x = 0.25, width = 0.5, y = 3/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[3], rank = rs$rank[3], cfg = cfg)
    popViewport()
    pushViewport(viewport(x = 0.75, width = 0.5, y = 3/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[4], rank = rs$rank[4], cfg = cfg)
    popViewport()
    pushViewport(viewport(x = 0.25, width = 0.5, y = 1/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[5], rank = rs$rank[5], cfg = cfg)
    popViewport()
    pushViewport(viewport(x = 0.75, width = 0.5, y = 1/6, height = 1/3))
    grid.piece("die_face", suit = rs$suit[6], rank = rs$rank[6], cfg = cfg)
    popViewport()

    grDevices::dev.off()
    invisible(filename)
}

draw_piece_and_bleed <- function(piece_side, suit, rank, cfg) {
    g <- pieceGrob(piece_side, suit, rank, cfg)
    bleed_color <- cfg$get_piece_opt(piece_side, suit, rank)$bleed_color
    b <- pp_shape()$polyclip(g, "minus", gp=gpar(col=NA, fill=bleed_color))
    grid.draw(b)
    grid.draw(g)
}

# pyramid top up
write_pt_obj <- function(piece_side = "pyramid_top", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)

    # vertices
    pc <- Point3D$new(x, y, z)
    xy_npc <- Point2D$new(rect_xy)
    xy <- xy_npc$translate(-0.5, -0.5)
    xyz_t <- Point3D$new(x = 0.0, y = 0.0, z = 0.5)
    xyz_b <- Point3D$new(xy, z = -0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    xyz <- Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(angle, axis_x, axis_y)$translate(pc)

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
write_ps_obj <- function(piece_side = "pyramid_face", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)

    # geometric vertices
    pc <- Point3D$new(x, y, z)
    xy_npc <- Point2D$new(pyramid_xy)
    xy <- xy_npc$translate(-0.5, -0.5)
    xyz_b <- Point3D$new(xy, z = -0.5)
    theta <- 2 * asin(0.5 * width / height)
    xyz_t <- Point3D$new(x = c(-0.5, 0.5), y = 0.5 - cos(theta), z = 0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    xyz <- Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(angle, axis_x, axis_y)$translate(pc)

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
