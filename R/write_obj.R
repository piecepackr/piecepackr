#' Save Wavefront OBJ files of board game pieces
#'
#' \code{save_piece_obj} saves Wavefront OBJ files (including associated MTL and texture image) of board game pieces.
#' @inheritParams pieceGrob
#' @param axis_x Ignored for now.
#' @param axis_y Ignored for now.
#' @param filename Name of Wavefront OBJ object.
#' @param res Resolution of the faces.
#' @return A list with named elements "obj", "mtl", "png" with the created filenames.
#' @examples
#'     cfg <- game_systems("dejavu3d")$piecepack
#'     files <- save_piece_obj("tile_face", suit = 3, rank = 3, cfg = cfg)
#'     print(files)
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

    if (grepl("tile|coin|pawn|die|matchstick|bit|board|card|saucer", piece_side)) {
        f <- write_2s_obj
    } else if (piece_side == "pyramid_top") {
        f <- write_pt_obj
    } else if (grepl("pyramid", piece_side)) {
        f <- write_ps_obj
    } else {
        stop("Don't know how to export ", piece_side, " to Wavefront OBJ format.")
    }
    f(piece_side, suit, rank, cfg,
      x = x, y = y, z = z, angle = angle, axis_x = axis_x, axis_y = axis_y,
      width = width, height = height, depth = depth,
      filename = filename, res = res)
}

write_2s_texture <- function(piece_side = "tile_face", suit = 1, rank = 1, cfg = pp_cfg(),
                             ...,
                             filename = tempfile(fileext = ".png"), res = 72) {
    opt <- cfg$get_piece_opt(piece_side, suit, rank)

    height <- cfg$get_height(piece_side, suit, rank)
    width <- cfg$get_width(piece_side, suit, rank)
    grDevices::png(filename, height = height, width = 2.5 * width,
        units = "in", res = res, bg = "transparent")

    # front
    pushViewport(viewport(x = 0.225, width = 0.45))
    grid.rect(gp = gpar(col = "transparent", fill = opt$background_color))
    popViewport()
    pushViewport(viewport(x = 0.225, width = 0.40))
    grid.piece(piece_side, suit, rank, cfg)
    popViewport()

    # edge
    pushViewport(viewport(x = 0.5, width = 0.1))
    grid.rect(gp = gpar(col = "transparent", fill = opt$edge_color))
    popViewport()

    # back
    opp_piece_side <- if (grepl("_face", piece_side)) {
        gsub("face", "back", piece_side)
    } else {
        gsub("back", "face", piece_side)
    }
    if (piece_side == "die_face") {
        opp_piece_side <- "die_face"
        rank <- 7 - rank
    }
    opp_opt <- cfg$get_piece_opt(opp_piece_side, suit, rank)
    pushViewport(viewport(x = 0.775, width = 0.45))
    grid.rect(gp = gpar(col = "transparent", fill = opp_opt$edge_color))
    popViewport()
    pushViewport(viewport(x = 0.775, width = 0.40))
    grid.piece(opp_piece_side, suit, rank, cfg)
    popViewport()
    grDevices::dev.off()

    invisible(filename)
}

write_mtl <- function(mtl_filename, png_filename) {
    writeLines(c("newmtl material_0", paste("map_Kd", png_filename)),
               mtl_filename)
}

write_2s_obj <- function(piece_side = "tile_face", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_2s_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)
    write_mtl(mtl_filename, basename(png_filename))

    opt <- cfg$get_piece_opt(piece_side, suit, rank)

    # 1st half "top" vertices
    # 2nd half "bottom" vertices
    pc <- Point3D$new(x, y, z)
    xy_npc <- Point$new(get_shape_xy(opt$shape, opt$shape_t, opt$shape_r))
    xy <- xy_npc$translate(-0.5, -0.5)
    xyz_t <- Point3D$new(xy, z = 0.5)
    xyz_b <- Point3D$new(xy, z = -0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    xyz <- Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(angle, axis_x, axis_y)$translate(pc)

    nv <- length(xyz) / 2

    # geometric vertices
    v <- paste("v", xyz$x, xyz$y, xyz$z)
    cat("# Written by piecepackr3d",
        paste("mtllib", basename(mtl_filename)),
        "# geometric vertices", v,
        sep = "\n", file = filename)

    # texture coordinates, nb. obj has y axis in opposite direction
    xy_vt_t <- xy_npc$dilate(width = 0.4, height = 1)$translate(x = 0.025)
    xy_vt_b <- xy_npc$dilate(width = 0.4, height = 1)$translate(x = 0.575)
    xy_vt_e <- list(x = c(0.52, 0.48, 0.48, 0.52), y = c(1, 0, 0, 1))
    vt_t <- paste("vt", xy_vt_t$x, xy_vt_t$y)
    vt_b <- paste("vt", xy_vt_b$x, xy_vt_b$y)
    vt_e <- paste("vt", xy_vt_e$x, xy_vt_e$y)
    cat("# texture coordinates", vt_t, vt_b, vt_e,
        sep = "\n", file = filename, append = TRUE)

    # faces
    f_t <- paste("f", paste0(seq(nv), "/", seq(nv), collapse = " "))
    f_b <- paste("f", paste0(nv + rev(seq(nv)), "/", nv + seq(nv), collapse = " "))
    cat("# Textured polygonal face element", "usemtl material_0", f_t, f_b,
        sep = "\n", file = filename, append = TRUE)
    # sides
    for (ii in seq(nv)) {
        ir <- ii %% nv + 1
        il <- ir %% nv + 1
        cat(paste("f", paste0(c(ir, il, il + nv, ir + nv), "/", 2 * nv + 1:4, collapse = " ")),
            sep = "\n", file = filename, append = TRUE)
    }
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
    grid.piece("pyramid_face", suit, rank, cfg)
    popViewport()

    pushViewport(viewport(x = 0.375, width = 0.25))
    grid.piece("pyramid_left", suit, rank, cfg)
    popViewport()

    pushViewport(viewport(x = 0.625, width = 0.25))
    grid.piece("pyramid_back", suit, rank, cfg)
    popViewport()

    pushViewport(viewport(x = 0.875, width = 0.25))
    grid.piece("pyramid_right", suit, rank, cfg)
    popViewport()
    grDevices::dev.off()

    invisible(filename)
}

write_pt_obj <- function(piece_side = "pyramid_top", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_pyramid_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)
    write_mtl(mtl_filename, basename(png_filename))

    pc <- Point3D$new(x, y, z)
    xy_npc <- Point$new(rect_xy)
    xy <- xy_npc$translate(-0.5, -0.5)
    xyz_t <- Point3D$new(x = 0.0, y = 0.0, z = 0.5)
    xyz_b <- Point3D$new(xy, z = -0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    xyz <- Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(angle, axis_x, axis_y)$translate(pc)

    # geometric vertices
    v <- paste("v", xyz$x, xyz$y, xyz$z)
    cat("# Written by piecepackr3d",
        paste("mtllib", basename(mtl_filename)),
        "# geometric vertices", v,
        sep = "\n", file = filename)

    # texture coordinates, nb. obj has y axis in opposite direction
    xy_vt <- list(x = seq(0, 1, 0.125), y = rep(c(0, 1), length.out = 9))
    cat("# texture coordinates", paste("vt", xy_vt$x, xy_vt$y),
        sep = "\n", file = filename, append = TRUE)

    cat("# Textured polygonal face element", "usemtl material_0",
        "f 2/3 3/5 1/4", # left
        "f 3/5 4/7 1/6", # back
        "f 4/7 5/9 1/8", # right
        "f 5/1 2/3 1/2", # front
        sep = "\n", file = filename, append = TRUE)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

write_ps_obj <- function(piece_side = "pyramid_face", suit = 1, rank = 1, cfg = pp_cfg(),
                         ...,
                         x = 0, y = 0, z = 0,
                         angle = 0, axis_x = 0, axis_y = 0,
                         width = NA, height = NA, depth = NA,
                         filename = tempfile(fileext = ".obj"), res = 72) {

    cfg <- as_pp_cfg(cfg)

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_pyramid_texture(piece_side, suit, rank, cfg, filename = png_filename, res = res)
    write_mtl(mtl_filename, basename(png_filename))

    pc <- Point3D$new(x, y, z)
    xy_npc <- Point$new(pyramid_xy)
    xy <- xy_npc$translate(-0.5, -0.5)
    xyz_b <- Point3D$new(xy, z = -0.5)
    theta <- 2 * asin(0.5 * width / height)
    xyz_t <- Point3D$new(x = c(-0.5, 0.5), y = 0.5 - cos(theta), z = 0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    xyz <- Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(angle, axis_x, axis_y)$translate(pc)

    # geometric vertices
    v <- paste("v", xyz$x, xyz$y, xyz$z)
    cat("# Written by piecepackr3d",
        paste("mtllib", basename(mtl_filename)),
        "# geometric vertices", v,
        sep = "\n", file = filename)

    # texture coordinates, nb. obj has y axis in opposite direction
    xy_vt <- list(x = seq(0, 1, 0.125), y = rep(c(0, 1), length.out = 9))
    cat("# texture coordinates", paste("vt", xy_vt$x, xy_vt$y),
        sep = "\n", file = filename, append = TRUE)

    if (piece_side == "pyramid_face") {
        cat("# Textured polygonal face element", "usemtl material_0",
            "f 2/3 5/5 3/4", # left
            "f 5/5 4/7 3/6", # back
            "f 4/7 1/9 3/8", # right
            "f 1/1 2/3 3/2", # front
            sep = "\n", file = filename, append = TRUE)
    } else if (piece_side == "pyramid_left") {
        cat("# Textured polygonal face element", "usemtl material_0",
            "f 1/3 2/5 3/4", # left
            "f 2/5 5/7 3/6", # back
            "f 5/7 4/9 3/8", # right
            "f 4/1 1/3 3/2", # front
            sep = "\n", file = filename, append = TRUE)
    } else if (piece_side == "pyramid_back") {
        cat("# Textured polygonal face element", "usemtl material_0",
            "f 4/3 1/5 3/4", # left
            "f 1/5 2/7 3/6", # back
            "f 2/7 5/9 3/8", # right
            "f 5/1 4/3 3/2", # front
            sep = "\n", file = filename, append = TRUE)
    } else { # pyramid right
        cat("# Textured polygonal face element", "usemtl material_0",
            "f 5/3 4/5 3/4", # left
            "f 4/5 1/7 3/6", # back
            "f 1/7 2/9 3/8", # right
            "f 2/1 5/3 3/2", # front
            sep = "\n", file = filename, append = TRUE)
    }
    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}
