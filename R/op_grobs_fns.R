basicOpGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                            x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                            angle=0, type="normal",
                            width=NA, height=NA, depth=NA,
                            op_scale=0, op_angle=45) {
            grob <- cfg$get_grob(piece_side, suit, rank, type)
            xy_p <- op_xy(x, y, z+0.5*depth, op_angle, op_scale)
            cvp <- viewport(xy_p$x, xy_p$y, width, height, angle=angle)
            grob <- grid::editGrob(grob, name="piece_side", vp=cvp)

            shadow_fn <- cfg$get_shadow_fn(piece_side, suit, rank)
            edge <- shadow_fn(piece_side, suit, rank, cfg,
                                x, y, z, angle, width, height, depth,
                                op_scale, op_angle)
            edge <- grid::editGrob(edge, name="other_faces")

            grobTree(edge, grob, cl="basic_projected_piece")
}

op_xy <- function(x, y, z, op_angle=45, op_scale=0) {
    x <- x + op_scale * z * cos(to_radians(op_angle))
    y <- y + op_scale * z * sin(to_radians(op_angle))
    list(x=x, y=y)
}

## edge-side grobs aka "shadow" effect
basicShadowGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                            x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                            angle=0, width=NA, height=NA, depth=NA,
                            op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    piece <- get_piece(piece_side)
    side <- ifelse(opt$back, "back", "face") #### allow limited 3D rotation

    x <- as.numeric(convertX(x, "in"))
    y <- as.numeric(convertY(y, "in"))
    z <- as.numeric(convertX(z, "in"))
    width <- as.numeric(convertX(width, "in"))
    height <- as.numeric(convertY(height, "in"))
    depth <- as.numeric(convertX(depth, "in"))

    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)
    R <- side_R(side) %*% AA_to_R(angle, axis_x = 0, axis_y = 0)
    whd <- get_scaling_factors(side, width, height, depth)
    pc <- Point3D$new(x, y, z)
    token <- Token2S$new(shape, whd, pc, R)

    gl <- gList()
    # opposite side
    opp_side <- ifelse(opt$back, "face", "back")
    #### Improved oblique projection 3D effect for dice #173
    opp_piece_side <- if (piece == "die") piece_side else paste0(piece, "_", opp_side)
    opp_opt <- cfg$get_piece_opt(opp_piece_side, suit, rank)
    gp_opp <- gpar(col=opp_opt$border_color, fill=opp_opt$background_color, lex=opp_opt$border_lex)
    xyz_opp <- if (opt$back) token$xyz_face else token$xyz_back
    xy_opp <- xyz_opp$project_op(op_angle, op_scale)

    grob_opposite <- polygonGrob(x = xy_opp$x, y = xy_opp$y, default.units = "in",
                          gp = gp_opp, name="opposite_piece_side")

    # edges
    edges <- token$op_edges(op_angle)
    for (i in seq_along(edges)) {
        name <- paste0("edge", i)
        gl[[i]] <- edges[[i]]$op_grob(op_angle, op_scale, name=name)
    }
    gp_edge <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    grob_edge <- gTree(children=gl, gp=gp_edge, name="token_edges")
    grobTree(grob_opposite, grob_edge)
}

basicEllipsoid <- function(piece_side, suit, rank, cfg=pp_cfg(),
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                           angle=0, type="normal",
                           width=NA, height=NA, depth=NA,
                           op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)

    x <- as.numeric(convertX(x, "in"))
    y <- as.numeric(convertY(y, "in"))
    z <- as.numeric(convertX(z, "in"))
    width <- as.numeric(convertX(width, "in"))
    height <- as.numeric(convertY(height, "in"))
    depth <- as.numeric(convertX(depth, "in"))

    xyz <- ellipse_xyz()$dilate(width, height, depth)$translate(x, y, z)
    xy <- xyz$project_op(op_angle, op_scale)
    hull <- grDevices::chull(as.matrix(xy))
    x <- xy$x[hull]
    y <- xy$y[hull]

    gp <- gpar(col=opt$border_color, fill=opt$background_color, lex=opt$border_lex)
    polygonGrob(x = x, y = y, default.units = "in", gp = gp)
}

ellipse_xyz <- function() {
    xy <- expand.grid(x=seq(0.0, 1.0, 0.05), y = seq(0.0, 1.0, 0.05))
    xy <- xy[which(xy$x^2 + xy$y^2 <= 1), ]
    z <- sqrt(1 - xy$x^2 - xy$y^2)
    ppp <- data.frame(x=xy$x, y=xy$y, z=z)
    ppn <- data.frame(x=xy$x, y=xy$y, z=-z)
    pnp <- data.frame(x=xy$x, y=-xy$y, z=z)
    pnn <- data.frame(x=xy$x, y=-xy$y, z=-z)
    nnp <- data.frame(x=-xy$x, y=-xy$y, z=z)
    npp <- data.frame(x=-xy$x, y=xy$y, z=z)
    npn <- data.frame(x=-xy$x, y=xy$y, z=-z)
    nnn <- data.frame(x=-xy$x, y=-xy$y, z=-z)
    df <- 0.5 * rbind(ppp, ppn, pnp, pnn, nnp, npp, npn, nnn)
    Point3D$new(df)
}

basicPyramidTop <- function(piece_side, suit, rank, cfg=pp_cfg(),
                            x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                            angle=0, type="normal",
                            width=NA, height=NA, depth=NA,
                            op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)

    x <- as.numeric(convertX(x, "in"))
    y <- as.numeric(convertY(y, "in"))
    z <- as.numeric(convertX(z, "in"))
    width <- as.numeric(convertX(width, "in"))
    height <- as.numeric(convertY(height, "in"))
    depth <- as.numeric(convertX(depth, "in"))
    xy_b <- Point2D$new(rect_xy)$npc_to_in(x, y, width, height, angle)
    p <- Polygon$new(xy_b)
    edge_types <- paste0("pyramid_", c("left", "back", "right", "face"))
    order <- p$op_edge_order(op_angle)
    df <- tibble(index = 1:4, edge = edge_types)[order, ]
    gl <- gList()
    for (i in 1:4) {
        opt <- cfg$get_piece_opt(df$edge[i], suit, rank)
        gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
        edge <- p$edges[df$index[i]]
        ex <- c(edge$p1$x, edge$p2$x, x)
        ey <- c(edge$p1$y, edge$p2$y, y)
        ez <- c(z - 0.5 * depth, z - 0.5 * depth, z + 0.5 * depth)
        exy <- Point3D$new(x = ex, y = ey, z = ez)$project_op(op_angle, op_scale)
        gl[[i]] <- polygonGrob(x = exy$x, y = exy$y, gp = gp, default.units = "in")
    }
    # Check angle and op_angle and if possible draw one of the pyramid faces
    if (nigh((angle - op_angle) %% 90, 0)) {
        base_mid <- exy[1]$midpoint(exy[2])
        xy_mid <- base_mid$midpoint(exy[3])
        vheight <- base_mid$distance_to(exy[3])
        vp <- viewport(x = xy_mid$x, y = xy_mid$y, default.units = "in", angle = op_angle - 90,
                       width = width, height = vheight)
        gl[[4]] <- grobTree(cfg$get_grob(df$edge[i], suit, rank, "picture"), vp = vp)
    }
    gTree(children=gl, cl="projected_pyramid_top")
}

basicPyramidSide <- function(piece_side, suit, rank, cfg=pp_cfg(),
                            x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                            angle=0, type="normal",
                            width=NA, height=NA, depth=NA,
                            op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)

    x <- as.numeric(convertX(x, "in"))
    y <- as.numeric(convertY(y, "in"))
    z <- as.numeric(convertX(z, "in"))
    width <- as.numeric(convertX(width, "in"))
    height <- as.numeric(convertY(height, "in"))
    depth <- as.numeric(convertX(depth, "in"))
    xy_b <- Point2D$new(pyramid_xy)$npc_to_in(x, y, width, height, angle)
    p <- Polygon$new(xy_b)

    theta <- 2 * asin(0.5 * width / height)
    yt <- 1 - cos(theta)
    xy_t <- Point2D$new(x = 0:1, y = yt)$npc_to_in(x, y, width, height, angle)

    gl <- gList()

    # opposite edge
    opposite_edge <- switch(piece_side,
                            "pyramid_face" = "pyramid_back",
                            "pyramid_back" = "pyramid_face",
                            "pyramid_left" = "pyramid_right",
                            "pyramid_right" = "pyramid_left")
    opt <- cfg$get_piece_opt(opposite_edge, suit, rank)
    gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
    exy <- Point3D$new(x = xy_b$x, y = xy_b$y, z = z - 0.5 * depth)$project_op(op_angle, op_scale)
    gl[[1]] <- polygonGrob(x = exy$x, y = exy$y, gp = gp, default.units = "in")
    gl[[2]] <- nullGrob()
    gl[[3]] <- nullGrob()

    # side edges
    edge_types <- paste0("pyramid_", switch(piece_side,
                         "pyramid_face" = c("right", "bottom", "left"),
                         "pyramid_left" = c("face", "bottom", "back"),
                         "pyramid_back" = c("left", "bottom", "right"),
                         "pyramid_right" = c("back", "bottom", "face")
                         ))

    order <- p$op_edge_order(op_angle)
    df <- tibble(index = 1:3, edge = edge_types)[order, ]
    gli <- 2

    for (i in 1:3) {
        edge_ps <- df$edge[i]
        index <- df$index[i]
        if (edge_ps == "pyramid_bottom") next
        opt <- cfg$get_piece_opt(edge_ps, suit, rank)
        gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
        edge <- p$edges[index]
        if (index == 1) {
            ex <- c(edge$p1$x, edge$p2$x, edge$p2$x)
            ey <- c(edge$p1$y, edge$p2$y, xy_t$y[1])
            ez <- c(z - 0.5 * depth, z - 0.5 * depth, z + 0.5 * depth)
        } else { # index equals 3
            ex <- c(edge$p1$x, edge$p2$x, edge$p1$x)
            ey <- c(edge$p1$y, edge$p2$y, xy_t$y[2])
            ez <- c(z - 0.5 * depth, z - 0.5 * depth, z + 0.5 * depth)
        }
        exy <- Point3D$new(x = ex, y = ey, z = ez)$project_op(op_angle, op_scale)
        gl[[gli]] <- polygonGrob(x = exy$x, y = exy$y, gp = gp, default.units = "in")
        gli <- gli + 1
    }

    # face
    x_f <- xy_b$x
    y_f <- c(xy_b$y[1], xy_t$y)
    z_f <- c(z - 0.5 * depth, z + 0.5 * depth, z + 0.5 * depth)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
    exy <- Point3D$new(x = x_f, y = y_f, z = z_f)$project_op(op_angle, op_scale)
    gl[[4]] <- polygonGrob(x = exy$x, y = exy$y, gp = gp, default.units = "in")

    # Check angle and op_angle and if possible draw one of the pyramid faces
    diff_angle <- (op_angle - angle) %% 360
    if ((nigh(diff_angle, 90) || nigh(diff_angle, 270)) && nigh(angle %% 90, 0)) {
        base_mid <- exy[2]$midpoint(exy[3])
        xy_mid <- base_mid$midpoint(exy[1])
        vheight <- base_mid$distance_to(exy[1])
        vp <- viewport(x = xy_mid$x, y = xy_mid$y, default.units = "in", angle = angle,
                       width = width, height = vheight)
        gl[[4]] <- grobTree(cfg$get_grob(piece_side, suit, rank, "picture"), vp = vp)
    }

    gTree(children=gl, cl="projected_pyramid_side")
}
