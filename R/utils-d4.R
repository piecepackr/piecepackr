d4Grob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, border=TRUE, flip=FALSE, scale=1,
          name=NULL, gp=gpar(), vp=NULL, cl=c("d4_die", "basic_piece_side"))
}

#' @export
makeContent.d4_die <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

    # Top (rank)
    gp_text <- gpar(col=opt$ps_color, fill=opt$ps_color,
                   fontsize = opt$ps_fontsize, fontfamily = opt$ps_fontfamily,
                   fontface = opt$ps_fontface)
    r <- 0.2
    top_grob <- textGrob(label = opt$ps_text, gp = gp_text,
                         x = 0.5, y = 0.5 + r, hjust = 0.5, vjust = 0.5,
                         name = "top_symbol")

    left <- switch(opt$ps_text, `1` = 2, `2` = 4, `3` = 4, `4` = 2)
    right <- switch(opt$ps_text, `1` = 3, `2` = 3, `3` = 1, `4` = 1)
    # Left/right
    left_grob <- textGrob(label = left, gp = gp_text,
                          x = 0.5, y = 0.5 + r, hjust = 0.5, vjust = 0.5,
                          vp = viewport(angle = 120),, name = "left_symbol")
    right_grob <- textGrob(label = right, gp = gp_text,
                          x = 0.5 , y = 0.5 + r, hjust = 0.5, vjust = 0.5,
                          vp = viewport(angle = 240), name = "right_symbol")

    # Border
    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }
    if (x$flip)
        gl <- gList(top_grob, left_grob, right_grob, background_grob, border_grob)
    else
        gl <- gList(background_grob, top_grob, left_grob, right_grob, border_grob)

    setChildren(x, gl)
}

# https://mathworld.wolfram.com/RegularTetrahedron.html

## d4 Top
d4TopGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
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
    xy_b <- npc_to_in(as_coord2d(convex_xy(3, 90)), x, y, width, height, angle)
    p <- Polygon$new(xy_b$x, xy_b$y)
    edge_types <- paste0("d4_", c("left", "face", "right"))
    order <- p$op_edge_order(op_angle)
    df <- tibble(index = 1:3, edge = edge_types)[order, ]
    gl <- gList()
    for (i in 1:3) {
        l_edge <- d4_edge_info(df$edge[i], rank, angle)
        opt <- cfg$get_piece_opt("die_face", suit, l_edge$rank)
        gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
        edge <- p$edges[df$index[i]]

        ex <- c(x, edge$p1$x, edge$p2$x)
        ey <- c(y, edge$p1$y, edge$p2$y)
        ez <- c(z + 0.5 * depth, z - 0.5 * depth, z - 0.5 * depth)
        xyz_polygon <- as_coord3d(x = ex, y = ey, z = ez)[cycle_d4(1:3, l_edge$vp_rot)]
        xy_polygon <- as_coord2d(xyz_polygon, alpha = degrees(op_angle), scale = op_scale)
        xy_vp <- xy_vp_convex(xyz_polygon, op_scale, op_angle)

        gl[[i]] <- at_ps_grob("die_face", suit, l_edge$rank, cfg, xy_vp, xy_polygon,
                              name = df$edge[i])
    }

    #### allow limited 3D rotation #281
    axis_x <- 0
    axis_y <- 0
    ####
    # pre-compute grobCoords
    coords_xyl <- d4t_grobcoords_xyl(x, y, z,
                                     angle, axis_x, axis_y,
                                     width, height, depth,
                                     op_scale, op_angle)

    gTree(scale = 1,
          coords_xyl = coords_xyl,
          children=gl, cl=c("projected_rpg_die", "coords_xyl"))
}

#' @export
makeContent.projected_rpg_die <- function(x) {
    for (i in seq_along(x$children))
        x$children[[i]]$scale <- x$scale
    x
}

# We need to widen/lengthen viewport for "convex" shapes
# Assuming top vertex in viewport at (0.5, 1)
xy_vp_convex <- function(xyz_polygon, op_scale, op_angle) {
    n <- length(xyz_polygon)

    xy <- convex_xy(n, 90)

    # widen viewport since "convex" shape doesn't reach edges of viewport
    i_left <- which.min(xy$x)
    i_right <- n + 2 - i_left

    p_mid_widest <- mean(xyz_polygon[c(i_left, i_right)])
    m_width <- 1 / (xy$x[i_right] - xy$x[i_left])
    p_left <- p_mid_widest + m_width * (xyz_polygon[i_left] - p_mid_widest)
    p_right <- p_mid_widest + m_width * (xyz_polygon[i_right] - p_mid_widest)

    up_diff <- xyz_polygon[1] - p_mid_widest
    p_ul <- p_left  + up_diff
    p_ur <- p_right + up_diff

    # lengthen viewport since (odd) "convex" shapes don't reach bottom of viewport
    i_bottom <- which.min(xy$y)
    i_top <- which.max(xy$y)
    m_height <- 1 / (1 - xy$y[i_left])
    p_ll <- p_ul - m_height * up_diff
    p_lr <- p_ur - m_height * up_diff

    x <- c(p_ul$x, p_ll$x, p_lr$x, p_ur$x)
    y <- c(p_ul$y, p_ll$y, p_lr$y, p_ur$y)
    z <- c(p_ul$z, p_ll$z, p_lr$z, p_ur$z)

    as_coord2d(as_coord3d(x, y, z), alpha = degrees(op_angle), scale = op_scale)
}

d4t_grobcoords_xyl <- function(x, y, z,
                              angle, axis_x, axis_y,
                              width, height, depth,
                              op_scale, op_angle) {
    xyz <- d4t_xyz(x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth)
    as.list(convex_hull2d(as_coord2d(xyz, alpha = degrees(op_angle), scale = op_scale)))
}

d4t_xyz <- function(x, y, z,
                    angle, axis_x, axis_y,
                    width, height, depth) {
    xy <- as_coord2d(convex_xy(3, 90))$translate(x = -0.5, y = -0.5)
    xyz_t <- as_coord3d(x = 0.0, y = 0.0, z = 0.5)
    xyz_b <- as_coord3d(xy, z = -0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    as_coord3d(xs, ys, zs)$
        scale(width, height, depth)$
        transform(AA_to_R(angle, axis_x, axis_y))$
        translate(x, y, z)
}

save_d4_obj <- function(piece_side, suit, rank, cfg,
                        x, y, z, angle, axis_x, axis_y,
                        width, height, depth,
                        filename, scale, res) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    xyz <- d4_xyz(suit, rank, cfg,
                  x, y, z,
                  angle, axis_x, axis_y,
                  width, height, depth)

    cxy <- convex_xy(3, 90)
    xy_vt <- list(x = rep(c(0.5 * cxy$x, 0.5 * cxy$x + 0.5), 2),
                  y = c(rep(0.5 * cxy$y + 0.5, 2), rep(0.5 * cxy$y, 2)))

    # textured face elements: face, left, right, bottom
    f <- list()
    fn_vt <- function(r) switch(r, 1:3, 4:6, 7:9, 10:12)
    f[[1]] <- list(v = 1:3,
                   vt = fn_vt(rank))
    f[[2]] <- list(v = 4:6,
                   vt = fn_vt(d4_edge_info("d4_left", rank)$rank))
    f[[3]] <- list(v = 7:9,
                   vt = fn_vt(d4_edge_info("d4_right", rank)$rank))
    f[[4]] <- list(v = 10:12,
                   vt = fn_vt(d4_edge_info("d4_bottom", rank)$rank))

    ext <- tools::file_ext(filename)
    mtl_filename <- gsub(paste0("\\.", ext, "$"), ".mtl", filename)
    png_filename <- gsub(paste0("\\.", ext, "$"), ".png", filename)

    write_obj(filename, v = xyz, vt = xy_vt, f = f)
    write_d4_texture("die_face", suit, rank, cfg, filename = png_filename, res = res)

    invisible(list(obj = filename, mtl = mtl_filename, png = png_filename))
}

d4_edge_info <- function(edge, rank, angle = 0) {
    if (edge == "d4_left") {
        edge_angle <- (angle - 120) %% 360
        edge_rank <- switch(rank, 4, 4, 2, 2)
        vp_rot <- switch(rank, 120, -120, 120, -120)
    } else if (edge == "d4_right") {
        edge_angle <- (angle + 120) %% 360
        edge_rank <- switch(rank, 3, 1, 1, 3)
        vp_rot <- switch(rank, 120, -120, 120, -120)
    } else if (edge == "d4_bottom") {
        edge_angle <- angle
        edge_rank <- switch(rank, 2, 3, 4, 1)
        vp_rot <- switch(rank, 120, 0, 120, 0)
    } else { # d4_face
        edge_angle <- angle
        edge_rank <- rank
        vp_rot <- 0
    }
    list(angle = edge_angle, rank = edge_rank, vp_rot = vp_rot)
}

cycle_d4 <- function(x, vp_rot) {
    if (vp_rot == 120)
        cycle_elements(x, 1)
    else if (vp_rot == -120)
        cycle_elements(x, -1)
    else
        x
}

d4_xyz <- function(suit, rank, cfg,
                   x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth) {

    xyz_b <- as_coord3d(convex_xy(3, 90), z = 0)$translate(-0.5, -0.5, -0.5)
    top <- as_coord3d(0, 0, 0.5)

    edge_types <- paste0("d4_", c("left", "face", "right"))

    # face
    xs_face <- c(top$x, xyz_b[2:3]$x)
    ys_face <- c(top$y, xyz_b[2:3]$y)
    zs_face <- c(top$z, xyz_b[2:3]$z)

    # left
    lr <- d4_edge_info("d4_left", rank)$vp_rot
    xs_left <- cycle_d4(c(top$x, xyz_b[1:2]$x), lr)
    ys_left <- cycle_d4(c(top$y, xyz_b[1:2]$y), lr)
    zs_left <- cycle_d4(c(top$z, xyz_b[1:2]$z), lr)

    # right
    rr <- d4_edge_info("d4_right", rank)$vp_rot
    xs_right <- cycle_d4(c(top$x, xyz_b[c(3, 1)]$x), rr)
    ys_right <- cycle_d4(c(top$y, xyz_b[c(3, 1)]$y), rr)
    zs_right <- cycle_d4(c(top$z, xyz_b[c(3, 1)]$z), rr)

    # bottom
    br <- d4_edge_info("d4_bottom", rank)$vp_rot
    xs_bottom <- cycle_d4(rev(xyz_b$x), br)
    ys_bottom <- cycle_d4(rev(xyz_b$y), br)
    zs_bottom <- cycle_d4(rev(xyz_b$z), br)

    xs <- c(xs_face, xs_left, xs_right, xs_bottom)
    ys <- c(ys_face, ys_left, ys_right, ys_bottom)
    zs <- c(zs_face, zs_left, zs_right, zs_bottom)

    as_coord3d(xs, ys, zs)$
        scale(width, height, depth)$
        transform(AA_to_R(angle, axis_x, axis_y))$
        translate(x, y, z)
}

write_d4_texture <- function(piece_side = "die_face", suit = 1, rank = 1, cfg = pp_cfg(),
                              ..., filename = tempfile(fileext = ".png"), res = 72) {

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    width <- cfg$get_width("die_face", suit, rank)

    args <- list(filename = filename, height = 2 * width, width = 2 * width,
                 units = "in", res = res, bg = "transparent")
    if (capabilities("cairo"))
        args$type <- "cairo"
    do.call(grDevices::png, args)

    pushViewport(viewport(x = 0.25, width = 0.5, y = 0.75, height = 0.5))
    draw_piece_and_bleed("die_face", suit, 1, cfg)
    popViewport()
    pushViewport(viewport(x = 0.75, width = 0.5, y = 0.75, height = 0.5))
    draw_piece_and_bleed("die_face", suit, 2, cfg)
    popViewport()
    pushViewport(viewport(x = 0.25, width = 0.5, y = 0.25, height = 0.5))
    draw_piece_and_bleed("die_face", suit, 3, cfg)
    popViewport()
    pushViewport(viewport(x = 0.75, width = 0.5, y = 0.25, height = 0.5))
    draw_piece_and_bleed("die_face", suit, 4, cfg)
    popViewport()

    grDevices::dev.off()
    invisible(filename)
}
