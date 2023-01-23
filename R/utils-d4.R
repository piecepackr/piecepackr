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
    xy <- Point2D$new(x, y)
    xy_b <- Point2D$new(convex_xy(3, 90))$npc_to_in(x, y, width, height, angle)
    p <- Polygon$new(xy_b)
    edge_types <- paste0("d4_", c("left", "face", "right"))
    order <- p$op_edge_order(op_angle)
    df <- tibble(index = 1:3, edge = edge_types)[order, ]
    gl <- gList()
    for (i in 1:3) {
        if (df$edge[i] == "d4_right") {
            edge_angle <- (angle + 120) %% 360
            edge_rank <- switch(rank, 3, 1, 1, 3)
            vp_rot <- switch(rank, 120, -120, 120, -120)
        } else if (df$edge[i] == "d4_left") {
            edge_angle <- (angle - 120) %% 360
            edge_rank <- switch(rank, 4, 4, 2, 2)
            vp_rot <- switch(rank, 120, -120, 120, -120)
        } else { # d4_face
            edge_angle <- angle
            edge_rank <- rank
            vp_rot <- 0
        }
        opt <- cfg$get_piece_opt("die_face", suit, edge_rank)
        gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
        edge <- p$edges[df$index[i]]

        ex <- c(x, edge$p1$x, edge$p2$x)
        ey <- c(y, edge$p1$y, edge$p2$y)
        ez <- c(z + 0.5 * depth, z - 0.5 * depth, z - 0.5 * depth)
        xyz_polygon <- Point3D$new(x = ex, y = ey, z = ez)
        xy_polygon <- xyz_polygon$project_op(op_angle, op_scale)

        xy_vp <- xy_vp_d4(xyz_polygon, op_scale, op_angle, edge_angle, vp_rot)

        gl[[i]] <- at_ps_grob("die_face", suit, edge_rank, cfg, xy_vp, xy_polygon,
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
          children=gl, cl=c("projected_d4_top", "coords_xyl"))
}

#' @export
makeContent.projected_d4_top <- function(x) {
    for (i in 1:3)
        x$children[[i]]$scale <- x$scale
    x
}

# We need to widen/lengthen and possibly rotate viewport..
xy_vp_d4 <- function(xyz_polygon, op_scale, op_angle, edge_angle, vp_rot) {
    p_midbottom <- xyz_polygon[2:3]$c

    # Widen viewport since "convex3" shape doesn't reach edges of viewport
    p_ll <- xyz_polygon[2]
    p_lr <- xyz_polygon[3]
    # p_ll <- p_midbottom + 1.018 * (p_ll - p_midbottom)
    # p_lr <- p_midbottom + 1.018 * (p_lr - p_midbottom)
    p_ll <- p_midbottom + 1.183568 * (p_ll - p_midbottom)
    p_lr <- p_midbottom + 1.183568 * (p_lr - p_midbottom)

    up_diff <- xyz_polygon[1] - p_midbottom
    p_ul <- p_ll + up_diff
    p_ur <- p_lr + up_diff

    # Adjust viewport down since "convex3" shape doesn't reach bottom of viewport
    p_ll <- p_ul - 4.1/3 * up_diff
    p_lr <- p_ur - 4.1/3 * up_diff

    x <- c(p_ul$x, p_ll$x, p_lr$x, p_ur$x)
    y <- c(p_ul$y, p_ll$y, p_lr$y, p_ur$y)
    z <- c(p_ul$z, p_ll$z, p_lr$z, p_ur$z)

    # Rotate viewport to xy-plane, rotate viewport, rotate back
    p <- Point3D$new(x, y, z)
    a <- 70.530 # manually estimated
    R <- R_z(-edge_angle) %*% R_x(-a) %*% R_z(vp_rot) %*% R_x(a) %*% R_z(edge_angle)
    p <- p$translate(-p$c)$rotate(R)$translate(p$c)
    p$project_op(op_angle, op_scale)
}

d4t_grobcoords_xyl <- function(x, y, z,
                              angle, axis_x, axis_y,
                              width, height, depth,
                              op_scale, op_angle) {
    xyz <- d4t_xyz(x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth)
    as.list(as.data.frame(xyz$project_op(op_angle, op_scale)$convex_hull))
}

d4t_xyz <- function(x, y, z,
                    angle, axis_x, axis_y,
                    width, height, depth) {
    pc <- Point3D$new(x, y, z)
    xy_npc <- Point2D$new(convex_xy(3, 90))
    xy <- xy_npc$translate(-0.5, -0.5)
    xyz_t <- Point3D$new(x = 0.0, y = 0.0, z = 0.5)
    xyz_b <- Point3D$new(xy, z = -0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(angle, axis_x, axis_y)$translate(pc)
}

d4_obj <- function(piece_side, suit, rank, cfg,
                            x, y, z, angle, axis_x, axis_y,
                            width, height, depth,
                            filename, scale, res) {
    abort("Can't save Wavefront OBJ files for d4 dice yet")
}
