# https://mathworld.wolfram.com/RegularOctahedron.html

## d8 Top
d8TopGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                      x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                      angle=0, type="normal",
                      width=NA, height=NA, depth=NA,
                      op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)

    x <- convertX(x, "in", valueOnly = TRUE)
    y <- convertY(y, "in", valueOnly = TRUE)
    z <- convertX(z, "in", valueOnly = TRUE)
    width <- convertX(width, "in", valueOnly = TRUE)
    height <- convertY(height, "in", valueOnly = TRUE)
    depth <- convertX(depth, "in", valueOnly = TRUE)

    #### allow limited 3D rotation #281
    axis_x <- 0
    axis_y <- 0
    ####
    xyz <- d8t_xyz(x, y, z, angle, axis_x, axis_y, width, height, depth)
    p <- Polygon$new(convex_xy(6, 0))
    edge_types <- paste0("d8_", c("right", "back", "left", "opposite_left", "opposite_back", "opposite_right"))
    order <- p$op_edge_order(op_angle)
    df <- tibble(index = 1:6, edge = edge_types, level = c(2, 1, 2, 1, 2, 1))[order, ]
    df <- df[order(df$level), ]
    gl <- gList()
    for (i in 1:nrow(df)) {
        edge <- df$edge[i]
        edge_rank <- d8_edge_rank(edge, rank)
        opt <- cfg$get_piece_opt("die_face", suit, edge_rank)
        gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
        xyz_polygon <- switch(edge,
               d8_left = xyz[c(1,5,2)],
               d8_back = xyz[c(1,6,5)],
               d8_right = xyz[c(1,3,6)],
               d8_opposite_left = xyz[c(4,2,5)],
               d8_opposite_back = xyz[c(4,3,2)],
               d8_opposite_right = xyz[c(4,6,3)]
        )
        xy_polygon <- as_coord2d(xyz_polygon, alpha = degrees(op_angle), scale = op_scale)
        xy_vp <- xy_vp_convex(xyz_polygon, op_scale, op_angle)
        gl[[i]] <- at_ps_grob("die_face", suit, edge_rank, cfg, xy_vp, xy_polygon, name = edge)
    }
    # face
    xyz_polygon <- xyz[1:3]
    xy_polygon <- as_coord2d(xyz_polygon, alpha = degrees(op_angle), scale = op_scale)
    xy_vp <- xy_vp_convex(xyz_polygon, op_scale, op_angle)
    gl[[nrow(df)+1]] <- at_ps_grob("die_face", suit, rank, cfg, xy_vp, xy_polygon, name = "d8_face")

    # pre-compute grobCoords
    coords_xyl <- as.list(convex_hull2d(as_coord2d(xyz, alpha = degrees(op_angle), scale = op_scale)))

    gTree(scale = 1,
          coords_xyl = coords_xyl,
          children=gl, cl=c("projected_rpg_die", "coords_xyl"))
}

d8t_xyz <- function(x, y, z,
                    angle, axis_x, axis_y,
                    width, height, depth) {
    xyz_t <- as_coord3d(convex_xy(3, 90), z = 0.0)$
        translate(as_coord3d(-0.5, -0.5, 0.5))
    xyz_b <- convex_xy(3, -90)
    xyz_b <- as_coord3d(x = xyz_b$x[c(1, 3, 2)], y = xyz_b$y[c(1, 3, 2)], z = 0.0)$
        translate(as_coord3d(-0.5, -0.5, -0.5))
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    as_coord3d(xs, ys, zs)$
        scale(width, height, depth)$
        transform(AA_to_R(angle, axis_x, axis_y))$
        translate(as_coord3d(x, y, z))
}

save_d8_obj <- function(piece_side, suit, rank, cfg,
                        x, y, z, angle, axis_x, axis_y,
                        width, height, depth,
                        filename, scale, res) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    xyz <- d8_xyz(suit, rank, cfg,
                  x, y, z,
                  angle, axis_x, axis_y,
                  width, height, depth)

    cxy <- convex_xy(3, 90)
    xy_vt <- list(x = rep(cxy$x / 2, 8) + rep(rep(0:1 / 2, each = 3), 4),
                  y = rep(cxy$y / 4, 8) + rep(rep(3:0 / 4, each = 3), each = 2))

    # textured face elements: face, left, right, back, opposite, opposite left, opposite right, opposite back
    f <- list()
    fn_vt <- function(r) switch(r, 1:3, 4:6, 7:9, 10:12, 13:15, 16:18, 19:21, 22:24)
    f[[1]] <- list(v = 1:3,
                   vt = fn_vt(rank))
    f[[2]] <- list(v = 4:6,
                   vt = fn_vt(d8_edge_rank("d8_left", rank)))
    f[[3]] <- list(v = 7:9,
                   vt = fn_vt(d8_edge_rank("d8_right", rank)))
    f[[4]] <- list(v = 10:12,
                   vt = fn_vt(d8_edge_rank("d8_back", rank)))
    f[[5]] <- list(v = 13:15,
                   vt = fn_vt(d8_edge_rank("d8_opposite", rank)))
    f[[6]] <- list(v = 16:18,
                   vt = fn_vt(d8_edge_rank("d8_opposite_left", rank)))
    f[[7]] <- list(v = 19:21,
                   vt = fn_vt(d8_edge_rank("d8_opposite_right", rank)))
    f[[8]] <- list(v = 22:24,
                   vt = fn_vt(d8_edge_rank("d8_opposite_back", rank)))

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = xy_vt, f = f)
    write_d8_texture("die_face", suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

d8_edge_rank <- function(edge, rank) {
    switch(edge,
           d8_left = d8_rank(rank + 2),
           d8_back = d8_rank(rank + 4),
           d8_right = d8_rank(rank + 6),
           d8_opposite = 9 - rank,
           d8_opposite_left = d8_rank(9 - rank + 2),
           d8_opposite_back = d8_rank(9 - rank + 4),
           d8_opposite_right = d8_rank(9 - rank + 6)
    )
}

d8_rank <- function(x) {
    (x - 1) %% 8 + 1
}

d8_xyz <- function(suit, rank, cfg,
                   x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth) {

    xyz_t <- as_coord3d(convex_xy(3, 90), z = 0.0)$
        translate(as_coord3d(-0.5, -0.5, 0.5))
    xyz_b <- convex_xy(3, -90)
    xyz_b <- as_coord3d(x = xyz_b$x[c(1,3,2)], y = xyz_b$y[c(1,3,2)], z = 0.0)$
        translate(as_coord3d(-0.5, -0.5, -0.5))

    # face
    xs_f <- xyz_t$x
    ys_f <- xyz_t$y
    zs_f <- xyz_t$z

    # left
    xs_l <- c(xyz_t$x[1], xyz_b$x[2], xyz_t$x[2])
    ys_l <- c(xyz_t$y[1], xyz_b$y[2], xyz_t$y[2])
    zs_l <- c(xyz_t$z[1], xyz_b$z[2], xyz_t$z[2])

    # right
    xs_r <- c(xyz_t$x[1], xyz_t$x[3], xyz_b$x[3])
    ys_r <- c(xyz_t$y[1], xyz_t$y[3], xyz_b$y[3])
    zs_r <- c(xyz_t$z[1], xyz_t$z[3], xyz_b$z[3])

    # back
    xs_b <- c(xyz_t$x[1], xyz_b$x[3], xyz_b$x[2])
    ys_b <- c(xyz_t$y[1], xyz_b$y[3], xyz_b$y[2])
    zs_b <- c(xyz_t$z[1], xyz_b$z[3], xyz_b$z[2])

    # opposite
    xs_o <- xyz_b$x
    ys_o <- xyz_b$y
    zs_o <- xyz_b$z

    # opposite left
    xs_ol <- c(xyz_b$x[1], xyz_t$x[2], xyz_b$x[2])
    ys_ol <- c(xyz_b$y[1], xyz_t$y[2], xyz_b$y[2])
    zs_ol <- c(xyz_b$z[1], xyz_t$z[2], xyz_b$z[2])

    # opposite right
    xs_or <- c(xyz_b$x[1], xyz_b$x[3], xyz_t$x[3])
    ys_or <- c(xyz_b$y[1], xyz_b$y[3], xyz_t$y[3])
    zs_or <- c(xyz_b$z[1], xyz_b$z[3], xyz_t$z[3])

    # opposite back
    xs_ob <- c(xyz_b$x[1], xyz_t$x[3], xyz_t$x[2])
    ys_ob <- c(xyz_b$y[1], xyz_t$y[3], xyz_t$y[2])
    zs_ob <- c(xyz_b$z[1], xyz_t$z[3], xyz_t$z[2])

    xs <- c(xs_f, xs_l, xs_r, xs_b, xs_o, xs_ol, xs_or, xs_ob)
    ys <- c(ys_f, ys_l, ys_r, ys_b, ys_o, ys_ol, ys_or, ys_ob)
    zs <- c(zs_f, zs_l, zs_r, zs_b, zs_o, zs_ol, zs_or, zs_ob)

    as_coord3d(xs, ys, zs)$
        scale(width, height, depth)$
        transform(AA_to_R(angle, axis_x, axis_y))$
        translate(as_coord3d(x, y, z))
}

write_d8_texture <- function(piece_side = "die_face", suit = 1, rank = 1, cfg = pp_cfg(),
                              ..., filename = tempfile(fileext = ".png"), res = 72) {

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    width <- cfg$get_width("die_face", suit, rank)

    args <- list(filename = filename, height = 4 * width, width = 2 * width,
                 units = "in", res = res, bg = "transparent")
    if (capabilities("cairo"))
        args$type <- "cairo"
    do.call(grDevices::png, args)

    xs <- rep(1:2 / 2 - 1/4, 4)
    ys <- rep(4:1 / 4 - 1/8, each = 2)
    for (i in 1:8) {
        pushViewport(viewport(x = xs[i], width = 1/2, y = ys[i], height = 1/4))
        draw_piece_and_bleed("die_face", suit, i, cfg)
        popViewport()
    }

    grDevices::dev.off()
    invisible(filename)
}
