# We'll use the alpha-90-beta-90 angle kites as described in
# "Mathematical Analysis of Trapezohedron with 2n congruent right kite faces" by Harish Chandra Rajpoot
# alpha = to_degrees(2 * atan(sqrt(tanpi(0.2) * tanpi(0.1)))) ~= 51.8272923729877490473
# beta = 180 - alpha ~= 128.1727076270122438473
# a = sqrt(tanpi(0.2) * tanpi(0.1)) * b ~= 0.48586827175664565326 * b := m_b * b
# inner radius = b/2 => die "depth" = b
#
# https://en.wikipedia.org/wiki/Right_kite
# diagonal that is a line of symmetry
# p = sqrt(a^2 + b^2) = sqrt(1 + m_b^2) * b
# p := 5 / 8 = 0.625
# => b = 0.625 / sqrt(1 + m_b^2) ~= 0.5621585749837085810299
# => a = m_b * b ~= 0.2731350152805132047718
# other diagonal
# q = 2ab / sqrt(a^2 + b^2) = (2 * m_b / sqrt(1 + m_b^2)) * b ~= 0.4913446110983896164548
# segments extending from intersection of diagonal to vertices in clockwise order d1, d2, d3, d4 then
# d1 * d3 = d2 * d4 = (p - d3) * d3 = (q / 2) * (q / 2) => d3^2 - p * d3 + q^2 / 4 = 0
# => d3 = (p - sqrt(p^2 - q^2)) / 2   OR  d3 = (p + sqrt(p^2 - q^2)) / 2  (latter seems too big!)
# => d3 ~ 0.1193643785156578984719 => d3 / p = 0.1909830056250526320039
# => kite "radius" = 0.5 - d3 / p ~= 0.3090169943749473402406
#
# Other links
# https://rechneronline.de/pi/right-kite.php
# http://www.dicecollector.com/JM/D10.HTM
# https://rpg.stackexchange.com/questions/114672/is-there-a-standard-d10-dimension-ratio
# https://math.stackexchange.com/questions/2464000/pentagonal-trapezohedron-with-face-perpendicular-to-side
# https://www.youtube.com/watch?v=eYO19tYYqXk
# https://www.athenopolis.net/2018/03/the-dimensions-of-ten-sided-die.html

d10TopGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                      x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                      angle=0, type="normal",
                      width=NA, height=NA, depth=NA,
                      op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt("die_face", suit, rank)

    x <- convertX(x, "in", valueOnly = TRUE)
    y <- convertY(y, "in", valueOnly = TRUE)
    z <- convertX(z, "in", valueOnly = TRUE)
    width <- convertX(width, "in", valueOnly = TRUE)
    height <- convertY(height, "in", valueOnly = TRUE)
    depth <- convertX(depth, "in", valueOnly = TRUE)

    #### allow limited 3D rotation #281
    axis_x <- 0
    axis_y <- 0

    xyz <- d10_xyz(suit, rank, cfg, x, y, z, angle, axis_x, axis_y, width, height, depth)
    l_xyz <- list()
    dfc <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
    for (i in 1:10) {
        idx <- seq.int(4 * i - 3, length.out = 4)
        dfc <- rbind(dfc, as.data.frame(mean(xyz[idx])))
    }

    op_ref <- as_coord2d(mean(xyz))$
        translate(degrees(op_angle + 180), radius = 10 * radius(xyz))
    xy_dists <- purrr::pmap_dbl(dfc, function(x, y, z, ...) abs(as_coord2d(x, y) - op_ref))
    dfc$edge <- paste0("d10_", c("face", "left1", "left2", "right1", "right2",
                                 "opposite", "opposite_left1", "opposite_left2", "opposite_right1", "opposite_right2"))
    dfc$rank <- vapply(dfc$edge, d10_edge_rank, numeric(1), rank = rank, USE.NAMES = FALSE)
    dfc$xy_dists <- xy_dists
    dfc$idx <- 1:10
    dfc <- dfc[order(round(dfc$z, 2), -xy_dists), ]

    gl <- gList()
    fn_idx <- function(idx) switch(idx, 1:4, 5:8, 9:12, 13:16, 17:20, 21:24, 25:28, 29:32, 33:36, 37:40)
    if (nigh(opt$shape_t, 90)) {
        idx_vp <- 1:4
    } else if (nigh(opt$shape_t, 0)) {
        idx_vp <- c(2:4, 1)
    } else if (nigh(opt$shape_t, 180)) {
        idx_vp <- c(4, 1:3)
    } else if (nigh(opt$shape_t, 270)) {
        idx_vp <- c(3:4, 1:2)
    }
    for (i in 1:nrow(dfc)) {
        edge_rank <- dfc$rank[i]
        opt <- cfg$get_piece_opt("die_face", suit, edge_rank)
        gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
        xyz_polygon <- xyz[fn_idx(dfc$idx[i])]
        xy_polygon <- as_coord2d(xyz_polygon, alpha = degrees(op_angle), scale = op_scale)
        xy_vp <- xy_vp_kite(xyz_polygon, op_scale, op_angle)[idx_vp]
        gl[[i]] <- at_ps_grob("die_face", suit, edge_rank, cfg, xy_vp, xy_polygon, name = dfc$edge[i])
    }

    # pre-compute grobCoords
    coords_xyl <- as.list(convex_hull2d((xy_polygon)))

    gTree(scale = 1,
          coords_xyl = coords_xyl,
          children=gl, cl=c("projected_rpg_die", "coords_xyl"))
}

# Assumes "acute" angle on top (`theta = 90`)
xy_vp_kite <- function(xyz_polygon, op_scale, op_angle) {
    p_mid <- mean(xyz_polygon[c(2,4)])
    p_left <- xyz_polygon[2]
    p_right <- xyz_polygon[4]

    up_diff <- xyz_polygon[1] - p_mid
    down_diff <- xyz_polygon[3] - p_mid
    p_ul <- p_left  + up_diff
    p_ur <- p_right + up_diff
    p_ll <- p_left + down_diff
    p_lr <- p_right + down_diff

    x <- c(p_ul$x, p_ll$x, p_lr$x, p_ur$x)
    y <- c(p_ul$y, p_ll$y, p_lr$y, p_ur$y)
    z <- c(p_ul$z, p_ll$z, p_lr$z, p_ur$z)

    as_coord2d(as_coord3d(x, y, z),
               alpha = degrees(op_angle),
               scale = op_scale)
}

d10_xyz <- function(suit, rank, cfg,
                    x, y, z,
                    angle, axis_x, axis_y,
                    width, height, depth) {
    opt <- cfg$get_piece_opt("die_face", suit, rank)
    xy <- kite_xy(r = 0.5 - 0.1909830056250526320039)
    if (nigh(opt$shape_t, 90) || nigh(opt$shape_t, 270)) {
        xyz_t <- as_coord3d(xy$x, xy$y, 0.0)$
            translate(as_coord3d(-0.5, -0.5, 0.5))$
            scale(width, height, depth)
        xyz_b <- as_coord3d(xy$x, xy$y, 0.0)$
            translate(as_coord3d(-0.5, -0.5, -0.5))$
            rotate("z-axis", degrees(180))$
            scale(width, height, depth)
    } else {
        xyz_t <- as_coord3d(xy$x, xy$y, z = 0.0)$
            translate(as_coord3d(-0.5, -0.5, 0.5))$
            scale(height, width, depth)
        xyz_b <- as_coord3d(xy$x, xy$y, z = 0.0)$
            translate(as_coord3d(-0.5, -0.5, -0.5))$
            rotate("z-axis", degrees(180))$
            scale(height, width, depth)
    }
    xyz_b <- xyz_b[c(1, 4, 3, 2)]

    xyz_l2 <- kite_xyz_missing_right(xyz_t[1], xyz_b[3], xyz_b[2]) # left2 face
    xyz_r2 <- kite_xyz_missing_left(xyz_t[1], xyz_b[4], xyz_b[3]) # right2 face
    xyz_ol2 <- kite_xyz_missing_right(xyz_b[1], xyz_t[3], xyz_t[2]) # opposite left2 face
    xyz_or2 <- kite_xyz_missing_left(xyz_b[1], xyz_t[4], xyz_t[3]) # opposite right2 face

    # top face
    x_face <- xyz_t$x
    y_face <- xyz_t$y
    z_face <- xyz_t$z

    # left1
    x_left1 <- c(xyz_l2$x[c(1, 4)], xyz_ol2$x[4], xyz_t$x[2])
    y_left1 <- c(xyz_l2$y[c(1, 4)], xyz_ol2$y[4], xyz_t$y[2])
    z_left1 <- c(xyz_l2$z[c(1, 4)], xyz_ol2$z[4], xyz_t$z[2])

    # left2
    x_left2 <- xyz_l2$x
    y_left2 <- xyz_l2$y
    z_left2 <- xyz_l2$z

    # right1
    x_right1 <- c(xyz_t$x[c(1, 4)], xyz_or2$x[2], xyz_r2$x[2])
    y_right1 <- c(xyz_t$y[c(1, 4)], xyz_or2$y[2], xyz_r2$y[2])
    z_right1 <- c(xyz_t$z[c(1, 4)], xyz_or2$z[2], xyz_r2$z[2])

    # right2
    x_right2 <- xyz_r2$x
    y_right2 <- xyz_r2$y
    z_right2 <- xyz_r2$z

    # opposite (bottom)
    x_opposite <- xyz_b$x
    y_opposite <- xyz_b$y
    z_opposite <- xyz_b$z

    # opposite left1
    x_opposite_left1 <- c(xyz_ol2$x[c(1, 4)], xyz_l2$x[4], xyz_b$x[2])
    y_opposite_left1 <- c(xyz_ol2$y[c(1, 4)], xyz_l2$y[4], xyz_b$y[2])
    z_opposite_left1 <- c(xyz_ol2$z[c(1, 4)], xyz_l2$z[4], xyz_b$z[2])

    # opposite left2
    x_opposite_left2 <- xyz_ol2$x
    y_opposite_left2 <- xyz_ol2$y
    z_opposite_left2 <- xyz_ol2$z

    # opposite right1
    x_opposite_right1 <- c(xyz_b$x[c(1, 4)], xyz_r2$x[2], xyz_or2$x[2])
    y_opposite_right1 <- c(xyz_b$y[c(1, 4)], xyz_r2$y[2], xyz_or2$y[2])
    z_opposite_right1 <- c(xyz_b$z[c(1, 4)], xyz_r2$z[2], xyz_or2$z[2])

    # opposite right2
    x_opposite_right2 <- xyz_or2$x
    y_opposite_right2 <- xyz_or2$y
    z_opposite_right2 <- xyz_or2$z

    xs <- c(x_face, x_left1, x_left2, x_right1, x_right2,
            x_opposite, x_opposite_left1, x_opposite_left2, x_opposite_right1, x_opposite_right2)
    ys <- c(y_face, y_left1, y_left2, y_right1, y_right2,
            y_opposite, y_opposite_left1, y_opposite_left2, y_opposite_right1, y_opposite_right2)
    zs <- c(z_face, z_left1, z_left2, z_right1, z_right2,
            z_opposite, z_opposite_left1, z_opposite_left2, z_opposite_right1, z_opposite_right2)

    pc <- as_coord3d(x, y, z)
    R <- R_z(opt$shape_t - 90) %*% AA_to_R(angle, axis_x, axis_y)
    as_coord3d(xs, ys, zs)$transform(R)$translate(pc)
}

kite_xyz_missing_right <- function(p_top, p_left, p_bottom) {
    p_mid <- p_bottom + 0.1909830056250526320039 * (p_top - p_bottom)
    p_right <- p_mid + (p_mid - p_left)
    as_coord3d(x = c(p_top$x, p_left$x, p_bottom$x, p_right$x),
               y = c(p_top$y, p_left$y, p_bottom$y, p_right$y),
               z = c(p_top$z, p_left$z, p_bottom$z, p_right$z))
}

kite_xyz_missing_left <- function(p_top, p_bottom, p_right) {
    p_mid <- p_bottom + 0.1909830056250526320039 * (p_top - p_bottom)
    p_left <- p_mid + (p_mid - p_right)
    as_coord3d(x = c(p_top$x, p_left$x, p_bottom$x, p_right$x),
               y = c(p_top$y, p_left$y, p_bottom$y, p_right$y),
               z = c(p_top$z, p_left$z, p_bottom$z, p_right$z))
}

save_d10_obj <- function(piece_side, suit, rank, cfg,
                        x, y, z, angle, axis_x, axis_y,
                        width, height, depth,
                        filename, scale, res) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)

    xyz <- d10_xyz(suit, rank, cfg,
                   x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth)

    xy <- kite_xy(r = 0.5 - 0.1909830056250526320039)
    xy_vt <- list(x = rep(xy$x / 2, 10) + rep(rep(0:1/2, each = 4), 5),
                  y = rep(xy$y / 5, 10) + rep(rep(4:0/5, each = 4), each = 2))

    # textured face elements: face, left1, left2, right1, right2
    #                      opposite, opposite left, opposite right, opposite back
    f <- list()
    fn_vt <- function(r) switch(r, 1:4, 5:8, 9:12, 13:16, 17:20, 21:24, 25:28, 29:32, 33:36, 37:40)
    f[[1]] <- list(v = 1:4,
                   vt = fn_vt(rank))
    f[[2]] <- list(v = 5:8,
                   vt = fn_vt(d10_edge_rank("d10_left1", rank)))
    f[[3]] <- list(v = 9:12,
                   vt = fn_vt(d10_edge_rank("d10_left2", rank)))
    f[[4]] <- list(v = 13:16,
                   vt = fn_vt(d10_edge_rank("d10_right1", rank)))
    f[[5]] <- list(v = 17:20,
                   vt = fn_vt(d10_edge_rank("d10_right2", rank)))
    f[[6]] <- list(v = 21:24,
                   vt = fn_vt(d10_edge_rank("d10_opposite", rank)))
    f[[7]] <- list(v = 25:28,
                   vt = fn_vt(d10_edge_rank("d10_opposite_left1", rank)))
    f[[8]] <- list(v = 29:32,
                   vt = fn_vt(d10_edge_rank("d10_opposite_left2", rank)))
    f[[9]] <- list(v = 33:36,
                   vt = fn_vt(d10_edge_rank("d10_opposite_right1", rank)))
    f[[10]] <- list(v = 37:40,
                    vt = fn_vt(d10_edge_rank("d10_opposite_right2", rank)))

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = xy_vt, f = f)
    write_d10_texture("die_face", suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

d10_edge_rank <- function(edge, rank) {
    switch(edge,
           d10_face = rank,
           d10_left1 = d10_rank(rank + 2),
           d10_left2 = d10_rank(rank + 4),
           d10_right1 = d10_rank(rank + 8),
           d10_right2 = d10_rank(rank + 6),
           d10_opposite = 11 - rank,
           d10_opposite_left1 = d10_rank(11 - rank + 2),
           d10_opposite_left2 = d10_rank(11 - rank + 4),
           d10_opposite_right1 = d10_rank(11 - rank + 8),
           d10_opposite_right2 = d10_rank(11 - rank + 6)
    )
}

d10_rank <- function(x) {
    (x - 1) %% 10 + 1
}

write_d10_texture <- function(piece_side = "die_face", suit = 1, rank = 1, cfg = pp_cfg(),
                              ..., filename = tempfile(fileext = ".png"), res = 72) {

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    height <- cfg$get_height("die_face", suit, rank)
    width <- cfg$get_width("die_face", suit, rank)

    if (nigh(opt$shape_t, 90) || nigh(opt$shape_t, 270)) {
        args <- list(filename = filename, height = 5 * height, width = 2 * width,
                     units = "in", res = res, bg = "transparent")
    } else {
        args <- list(filename = filename, height = 5 * width, width = 2 * height,
                     units = "in", res = res, bg = "transparent")
    }
    angle <- 90 - opt$shape_t
    if (capabilities("cairo"))
        args$type <- "cairo"
    do.call(grDevices::png, args)

    xs <- rep(1:2 / 2 - 1/4, 5)
    ys <- rep(5:1 / 5 - 1/10, each = 2)
    for (i in 1:10) {
        pushViewport(viewport(x = xs[i], width = inch(width), y = ys[i], height = inch(height), angle = angle))
        draw_piece_and_bleed("die_face", suit, i, cfg)
        popViewport()
    }

    grDevices::dev.off()
    invisible(filename)
}
