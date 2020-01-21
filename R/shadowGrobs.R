basicShadowGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                            x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                            angle=0, width=NA, height=NA, depth=NA,
                            op_scale=0, op_angle=45, default.units="npc") {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    if (is.na(angle)) angle <- 0
    if (is.na(width)) width <- inch(cfg$get_width(piece_side, suit, rank))
    if (is.na(height)) height <- inch(cfg$get_height(piece_side, suit, rank))
    if (is.na(depth)) depth <- inch(cfg$get_depth(piece_side, suit, rank))
    if (!is.unit(width)) width <- unit(width, default.units)
    if (!is.unit(height)) height <- unit(height, default.units)
    if (!is.unit(depth)) depth <- unit(depth, default.units)

    x <- as.numeric(convertX(x, "in"))
    y <- as.numeric(convertY(y, "in"))
    z <- as.numeric(convertX(z, "in"))
    width <- as.numeric(convertX(width, "in"))
    height <- as.numeric(convertY(height, "in"))
    depth <- as.numeric(convertX(depth, "in"))

    shape <- opt$shape
    if (shape == "circle") {
        circleShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    } else if (shape %in% c("halma", "oval")) {
        genericShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    } else {
        polygonShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    }
}

genericShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)
    n_shadows <- max(round(100*depth), 3)
    zs <- z + depth * seq(-0.5, 0.5, length.out = n_shadows)
    xy_p <- Point3D$new(x, y, zs)$project_op(op_angle, op_scale)
    gl <- gList()
    vp <- viewport(xy_p$x[1], xy_p$y[1], width, height, angle=angle, default.units = "in")
    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    gl[[1]] <- shape_fn(gp=gp, vp=vp)
    gps <- gpar(col=NA, fill=opt$edge_color)
    for (ii in 2:n_shadows) {
        vps <- viewport(xy_p$x[ii], xy_p$y[ii], width, height, angle=angle, default.units = "in")
        gl[[ii]] <- shape_fn(gp=gps, vp=vps)
    }
    return(gl)
}

circleShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    n_points <- 36
    thetas <- seq(op_angle+90, op_angle+270, length.out=n_points)
    r <- min(0.5*width, 0.5*height)

    xy_c <- Point$new(x, y)$translate_polar(thetas, r)
    xy_l <- Point3D$new(xy_c, z = z - 0.5 * depth)$project_op(op_angle, op_scale)
    xy_u <- Point3D$new(xy_c, z = z + 0.5 * depth)$project_op(op_angle, op_scale)

    x <- c(xy_l$x, rev(xy_u$x))
    y <- c(xy_l$y, rev(xy_u$y))
    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    polygonGrob(x=x, y=y, default.units="in", gp=gp)
}

polygonShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    xy <- get_shape_xy(opt$shape, opt$shape_t, opt$shape_r)
    xy_c <- Point$new(xy)$npc_to_in(x, y, width, height, angle)
    p <- Polygon$new(xy_c)
    fm <- p$op_edges(op_angle)$face_matrix(z, depth)
    e <- Point3D$new(fm[, 1], fm[, 2], fm[, 3])$project_op(op_angle, op_scale)

    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    gl <- gList()
    for (i in seq(nrow(fm) / 4)) {
        index <- 4 * (i-1) + 1
        xf <- e$x[index:(index+3)]
        yf <- e$y[index:(index+3)]
        gl[[i]] <- polygonGrob(x=xf, y=yf, default.units="in", gp=gp)
    }
    gl
}
