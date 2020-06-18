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
            edge <- grid::editGrob(edge, name="edge")

            grobTree(edge, grob, cl="basic_projected_piece")
}

op_xy <- function(x, y, z, op_angle=45, op_scale=0) {
    x <- x + op_scale * z * cos(to_radians(op_angle))
    y <- y + op_scale * z * sin(to_radians(op_angle))
    list(x=x, y=y)
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
    if (near((angle - op_angle) %% 90, 0)) {
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
    if ((near(diff_angle, 90) || near(diff_angle, 270)) && near(angle %% 90, 0)) {
        base_mid <- exy[2]$midpoint(exy[3])
        xy_mid <- base_mid$midpoint(exy[1])
        vheight <- base_mid$distance_to(exy[1])
        vp <- viewport(x = xy_mid$x, y = xy_mid$y, default.units = "in", angle = angle,
                       width = width, height = vheight)
        gl[[4]] <- grobTree(cfg$get_grob(piece_side, suit, rank, "picture"), vp = vp)
    }

    gTree(children=gl, cl="projected_pyramid_side")
}

## edge-side grobs aka "shadow" effect
basicShadowGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                            x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                            angle=0, width=NA, height=NA, depth=NA,
                            op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)

    x <- as.numeric(convertX(x, "in"))
    y <- as.numeric(convertY(y, "in"))
    z <- as.numeric(convertX(z, "in"))
    width <- as.numeric(convertX(width, "in"))
    height <- as.numeric(convertY(height, "in"))
    depth <- as.numeric(convertX(depth, "in"))

    shape <- opt$shape
    if (shape == "circle") {
        circleShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    } else if (shape == "halma") {
        genericShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    } else if (shape %in% c("oval", "roundrect")) {
        convexCurvedShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    } else {
        polygonShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    }
}

genericShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    xy <- get_shape_xy(opt$shape, opt$shape_t, opt$shape_r)
    xy_c <- Point2D$new(xy)$npc_to_in(x, y, width, height, angle)

    gl <- gList()

    # bottom
    xy_l <- Point3D$new(xy_c, z = z - 0.5 * depth)$project_op(op_angle, op_scale)
    gp <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
    gl[[1]] <- polygonGrob(x=xy_l$x, y=xy_l$y, default.units="in", gp=gp)

    # edges
    p <- Polygon$new(xy_c)
    fm <- p$op_edges(op_angle)$face_matrix(z, depth)
    e <- Point3D$new(fm[, 1], fm[, 2], fm[, 3])$project_op(op_angle, op_scale)
    for (i in seq(nrow(fm) / 4)) {
        index <- 4 * (i-1) + 1
        xf <- e$x[index:(index+3)]
        yf <- e$y[index:(index+3)]
        gl[[i+1]] <- polygonGrob(x=xf, y=yf, default.units="in")
    }
    gp <- gpar(col=NA, fill=opt$edge_color)
    gTree(children=gl, gp=gp, cl="generic_edge")
}

circleShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    n_points <- 36
    thetas <- seq(op_angle+90, op_angle+270, length.out=n_points)
    r <- min(0.5*width, 0.5*height)

    xy_c <- Point2D$new(x, y)$translate_polar(thetas, r)
    xy_l <- Point3D$new(xy_c, z = z - 0.5 * depth)$project_op(op_angle, op_scale)
    xy_u <- Point3D$new(xy_c, z = z + 0.5 * depth)$project_op(op_angle, op_scale)

    x <- c(xy_l$x, rev(xy_u$x))
    y <- c(xy_l$y, rev(xy_u$y))
    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    polygonGrob(x=x, y=y, default.units="in", gp=gp)
}

convexCurvedShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    xy <- get_shape_xy(opt$shape, opt$shape_t, opt$shape_r)
    p <- Polygon$new(Point2D$new(xy)$npc_to_in(x, y, width, height, angle))

    # We'll project points onto a line to figure out which are visible at that 'op_angle'.
    # The line we want to project on is at a right angle to 'op_angle'.
    n <- length(p$vertices)
    projections <- numeric(n)
    proj_vec <- Vector$new(to_x(op_angle - 90, 1), to_y(op_angle - 90, 1))
    for (ii in seq(n)) {
        projections[ii] <- proj_vec$dot(p$vertices[ii])
    }
    i_min <- which.min(projections)
    i_max <- which.max(projections)
    if (i_min < i_max) {
        indices <- seq(i_min, i_max)
    } else {
        indices <- c(seq(i_min, n), seq(1, i_max))
    }
    xy_c <- p$vertices[indices]

    xy_l <- Point3D$new(xy_c, z = z - 0.5 * depth)$project_op(op_angle, op_scale)
    xy_u <- Point3D$new(xy_c, z = z + 0.5 * depth)$project_op(op_angle, op_scale)

    x <- c(xy_l$x, rev(xy_u$x))
    y <- c(xy_l$y, rev(xy_u$y))
    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    polygonGrob(x=x, y=y, default.units="in", gp=gp)
}

polygonShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    xy <- get_shape_xy(opt$shape, opt$shape_t, opt$shape_r)
    xy_c <- Point2D$new(xy)$npc_to_in(x, y, width, height, angle)
    p <- Polygon$new(xy_c)
    fm <- p$op_edges(op_angle)$face_matrix(z, depth)
    e <- Point3D$new(fm[, 1], fm[, 2], fm[, 3])$project_op(op_angle, op_scale)

    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    gl <- gList()
    for (i in seq(nrow(fm) / 4)) {
        index <- 4 * (i-1) + 1
        xf <- e$x[index:(index+3)]
        yf <- e$y[index:(index+3)]
        gl[[i]] <- polygonGrob(x=xf, y=yf, default.units="in", name=paste0("edge", i))
    }
    gTree(children=gl, gp=gp, cl="prism_edge")
}
