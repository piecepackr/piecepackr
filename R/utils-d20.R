# https://mathworld.wolfram.com/RegularIcosahedron.html
# https://www.blocklayer.com/pyramid-calculator gives us pyramid side angle 37.4 degrees
# a = from vertex to adjacent vertex
# inradius = (3 * sqrt(3) + sqrt(15)) / 12) * a ~=  0.75576 * a

## d20 Top
d20TopGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
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

    xyz <- d20_xyz(suit, rank, cfg, x, y, z, angle, axis_x, axis_y, width, height, depth)
    l_xyz <- list()
    dfc <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
    for (i in 1:20) {
        idx <- seq.int(3 * i - 2, length.out = 3)
        dfc <- rbind(dfc, as.data.frame(mean(xyz[idx])))
    }

    op_ref <- as_coord2d(mean(xyz))$
        translate(degrees(op_angle + 180), radius = 10 * radius(xyz))
    xy_dists <- purrr::pmap_dbl(dfc, function(x, y, z, ...) abs(as_coord2d(x, y) - op_ref))
    dfc$rank <- 1:20
    dfc$xy_dists <- xy_dists
    dfc <- dfc[order(round(dfc$z, 2), -xy_dists), ]

    gl <- gList()
    for (i in 1:nrow(dfc)) {
        edge_rank <- dfc$rank[i]
        edge <- paste("d20_", edge_rank)
        opt <- cfg$get_piece_opt("die_face", suit, edge_rank)
        gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
        idx <- seq.int(3 * edge_rank - 2, length.out = 3)
        xyz_polygon <- xyz[idx]
        xy_polygon <- as_coord2d(xyz_polygon, alpha = degrees(op_angle), scale = op_scale)
        xy_vp <- xy_vp_convex(xyz_polygon, op_scale, op_angle)
        gl[[i]] <- at_ps_grob("die_face", suit, edge_rank, cfg, xy_vp, xy_polygon, name = edge)
    }

    # pre-compute grobCoords
    coords_xyl <- as.list(convex_hull2d(as_coord2d(xyz, alpha = degrees(op_angle), scale = op_scale)))

    gTree(scale = 1,
          coords_xyl = coords_xyl,
          children=gl, cl=c("projected_rpg_die", "coords_xyl"))
}

# if `rank` on top figure out rotation so one on top instead
# currently just a fixed arrangement
d20_rot_from_top <- function(rank) {
    a1 <- 180 - arccos(-sqrt(5)/3) # 180 - dihedral angle
    a2 <- 37.4 # equilateral pentagonal pyramid side (face) angle
    # a <- 50
    stopifnot(rank <= 20)
    switch(rank,
           diag(3), # 01
           R_x(180) %*% R_z(180) %*% R_x(a1) %*% R_z(120), # 02
           R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2), # 03
           R_z(120) %*% R_x(180) %*% R_z(-120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2), # 04
           R_z(120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120), # 05
           R_x(180) %*% R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(120), # 06
           R_z(180) %*% R_x(a1) %*% R_z(-120), # 07
           R_x(180) %*% R_z(180) %*% R_x(a1), # 08
           R_z(120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(-120), # 09
           R_z(120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120) %*% R_x(180), # 10
           R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(-120), # 11
           R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(120) %*% R_x(180), # 12
           R_z(180) %*% R_x(a1), # 13
           R_x(180) %*% R_z(180) %*% R_x(a1) %*% R_z(-120), # 14
           R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(120), # 15
           R_x(180) %*% R_z(120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120), # 16
           R_z(120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2), # 17
           R_x(180) %*% R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2), # 18
           R_z(180) %*% R_x(a1) %*% R_z(120), # 19
           R_x(180)) # 20
}

# if one on top figure out rotation so `rank` on top instead
d20_rot_to_top <- function(rank) {
    a1 <- 180 - arccos(-sqrt(5)/3) # 180 - dihedral angle
    a2 <- 37.4 # equilateral pentagonal pyramid side (face) angle
    stopifnot(rank <= 20)
    switch(rank,
           diag(3), # 01
           R_z(-120) %*% R_x(-a1) %*% R_z(-180) %*% R_x(-180), # 02
           R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120), # 03
           R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(120) %*% R_x(-180) %*% R_z(-120), # 04
           R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(-120), # 05
           R_z(-120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120) %*% R_x(-180), # 06
           R_z(120) %*% R_x(-a1) %*% R_z(-180), # 07
           R_x(-a1) %*% R_z(-180) %*% R_x(-180), # 08
           R_z(120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(-120), # 09
           R_x(-180) %*% R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(-120), # 10
           R_z(120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120), # 11
           R_x(-180) %*% R_z(-120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120), # 12
           R_x(-a1) %*% R_z(180), # 13
           R_z(120) %*% R_x(-a1) %*% R_z(-180) %*% R_x(-180), # 14
           R_z(-120) %*% R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120), # 15
           R_z(-120) %*% R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(-120) %*% R_x(-180), # 16
           R_x(a2) %*% R_z(144) %*% R_x(-a2) %*% R_z(-120), # 17
           R_x(a2) %*% R_z(-144) %*% R_x(-a2) %*% R_z(120) %*% R_x(-180), # 18
           R_z(-120) %*% R_x(-a1) %*% R_z(-180), # 19
           R_x(-180)) # 20
}

d20_xyz <- function(suit, rank, cfg,
                    x, y, z,
                    angle, axis_x, axis_y,
                    width, height, depth) {
    R_rank <- d20_rot_to_top(rank)
    xs <- numeric(0)
    ys <- numeric(0)
    zs <- numeric(0)
    xyz_t <- as_coord3d(convex_xy(3, 90), z = 0.0)$
        translate(as_coord3d(-0.5, -0.5, 0.5))$
        scale(width, height, depth)
    for (i in 1:20) {
        R_i <- d20_rot_from_top(i)
        xyz_i <- xyz_t$clone()$transform(R_i %*% R_rank)
        xs <- append(xs, xyz_i$x)
        ys <- append(ys, xyz_i$y)
        zs <- append(zs, xyz_i$z)
    }

    as_coord3d(xs, ys, zs)$
        transform(AA_to_R(angle, axis_x, axis_y))$
        translate(as_coord3d(x, y, z))
}

save_d20_obj <- function(piece_side, suit, rank, cfg,
                        x, y, z, angle, axis_x, axis_y,
                        width, height, depth,
                        filename, scale, res) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)

    xyz <- d20_xyz(suit, rank, cfg,
                   x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth)

    cxy <- convex_xy(3, 90)
    xy_vt <- list(x = rep(cxy$x / 4, 20) + rep(rep(0:3/4, each = 3), 5),
                  y = rep(cxy$y / 5, 20) + rep(rep(4:0/5, each = 3), each = 4))

    # textured face elements
    f <- list()
    for (i in 1:20) {
        idx <- seq.int(3 * i - 2, length.out = 3)
        f[[i]] <- list(v = idx, vt = idx)
    }

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = xy_vt, f = f)
    write_d20_texture("die_face", suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

write_d20_texture <- function(piece_side = "die_face", suit = 1, rank = 1, cfg = pp_cfg(),
                              ..., filename = tempfile(fileext = ".png"), res = 72) {

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    width <- cfg$get_width("die_face", suit, rank)

    args <- list(filename = filename, height = 5 * width, width = 4 * width,
                 units = "in", res = res, bg = "transparent")
    if (capabilities("cairo"))
        args$type <- "cairo"
    do.call(grDevices::png, args)

    xs <- rep(1:4 / 4 - 1/8, 5)
    ys <- rep(5:1 / 5 - 1/10, each = 4)
    for (i in 1:20) {
        pushViewport(viewport(x = xs[i], width = 1/4, y = ys[i], height = 1/5))
        draw_piece_and_bleed("die_face", suit, i, cfg)
        popViewport()
    }

    grDevices::dev.off()
    invisible(filename)
}
