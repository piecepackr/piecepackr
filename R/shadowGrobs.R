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
    shape <- opt$shape
    if (shape == "circle") {
        circleShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    } else if (shape == "rect") {
        rectShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    } else if (grepl("convex", shape)) {
        convexShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    } else {
        genericShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle)
    }
}

genericShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)
    n_shadows <- max(round(100*as.numeric(convertX(depth, "in"))), 3)
    ns <- n_shadows-1
    zs <- rep((1/ns)*depth, ns)
    for (ii in 2:ns) # cumsum doesn't work with units
        zs[ii] <- zs[ii] + zs[ii-1]
    zs <- unit.c(z-0.5*depth, z-0.5*depth + zs)
    xp <- op_x(x, y, zs, op_angle, op_scale)
    yp <- op_y(x, y, zs, op_angle, op_scale)
    gl <- gList()
    vp <- viewport(xp[1], yp[1], width, height, angle=angle)
    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    gl[[1]] <- shape_fn(gp=gp, vp=vp)
    for (ii in 2:n_shadows) {
        vps <- viewport(xp[ii], yp[ii], width, height, angle=angle)
        gps <- gpar(col=NA, fill=opt$edge_color)
        gl[[ii]] <- shape_fn(gp=gps, vp=vps)
    }
    return(gl)
}

circleShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    n_points <- 36
    thetas <- seq(op_angle+90, op_angle+270, length.out=n_points)
    r <- min(0.5*width, 0.5*height)
    xl <- op_x(x, y, z-0.5*depth, op_angle, op_scale)
    yl <- op_y(x, y, z-0.5*depth, op_angle, op_scale)
    xu <- op_x(x, y, z+0.5*depth, op_angle, op_scale)
    yu <- op_y(x, y, z+0.5*depth, op_angle, op_scale)
    xcl <- xl + to_x(thetas, r)
    ycl <- yl + to_y(thetas, r)
    xcu <- rev(xu + to_x(thetas, r))
    ycu <- rev(yu + to_y(thetas, r))
    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    polygonGrob(x=unit.c(xcl, xcu), y=unit.c(ycl, ycu), gp=gp)
}

rectShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    # non-rotated, non-projected corners of the bottom of the solid
    xs <- as.numeric(convertX(unit.c(x-0.5*width, x-0.5*width, x+0.5*width, x+0.5*width), "in"))
    ys <- as.numeric(convertY(unit.c(y-0.5*height, y+0.5*height, y+0.5*height, y-0.5*height), "in"))
    x <- as.numeric(convertX(x, "in"))
    y <- as.numeric(convertY(y, "in"))
    z <- as.numeric(convertX(z, "in"))
    depth <- as.numeric(convertX(depth, "in"))
    # rotated, non-projected corners of the bottom of the solid
    thetas <- to_t(xs-x, ys-y) + angle
    rs <- to_r(xs-x, ys-y)
    xs <- x + to_x(thetas, rs)
    ys <- y + to_y(thetas, rs)
    # rotated, projected corners of the bottom and top of the solid
    xl <- op_x(xs, ys, z-0.5*depth, op_angle, op_scale)
    yl <- op_y(xs, ys, z-0.5*depth, op_angle, op_scale)
    xu <- op_x(xs, ys, z+0.5*depth, op_angle, op_scale)
    yu <- op_y(xs, ys, z+0.5*depth, op_angle, op_scale)

    # face angles
    fas <- c(180, 90, 0, -90) + angle + 180

    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    gl <- gList()
    for (ii in 1:4) {
        xf <- c(xl[ii],xl[ii%%4+1],xu[ii%%4+1],xu[ii])
        yf <- c(yl[ii],yl[ii%%4+1],yu[ii%%4+1],yu[ii])
        da <- dist_angles(fas[ii], op_angle)
        if ((da < 90)) {
            gl[[ii]] <- polygonGrob(x=xf, y=yf, default.units="in", gp=gp)
        }
    }
    gl
}

convexShadowGrob <- function(opt, x, y, z, angle, width, height, depth, op_scale, op_angle) {
    if (as.numeric(convertX(width, "in")) != as.numeric(convertY(height, "in")))
        return(genericShadowGrob(opt, x, y, z, angle, width, height, depth, op_scale, op_angle))

    n_vertices <- get_n_vertices(opt$shape)
    r <- convertX(min(0.5*width, 0.5*height), "in")
    thetas <- seq(0, 360, length.out=n_vertices+1) + opt$shape_t + angle
    # rotated, non-projected vertices at the bottom of the solid
    xs <- x + to_x(thetas, r)
    ys <- y + to_y(thetas, r)
    # rotated, projected vertices at the bottom and top of the solid
    xl <- op_x(xs, ys, z-0.5*depth, op_angle, op_scale)
    yl <- op_y(xs, ys, z-0.5*depth, op_angle, op_scale)
    xu <- op_x(xs, ys, z+0.5*depth, op_angle, op_scale)
    yu <- op_y(xs, ys, z+0.5*depth, op_angle, op_scale)

    # face angles
    fas <- thetas + 360 / n_vertices / 2 + 180

    gp <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    gl <- gList()
    for (ii in 1:n_vertices) {
        xf <- unit.c(xl[ii],xl[ii+1],xu[ii+1],xu[ii])
        yf <- unit.c(yl[ii],yl[ii+1],yu[ii+1],yu[ii])
        da <- dist_angles(fas[ii], op_angle)
        if ((da < 90)) {
            gl[[ii]] <- polygonGrob(x=xf, y=yf, default.units="in", gp=gp)
        }
    }
    gl
}

dist_angles <- function(angle1, angle2) {
    d <- abs(angle1 - angle2) %% 360
    ifelse(d > 180, 360-d, d)
}
