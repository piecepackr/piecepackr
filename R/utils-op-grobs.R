op_xy <- function(x, y, z, op_angle=45, op_scale=0) {
    x <- x + op_scale * z * cos(to_radians(op_angle))
    y <- y + op_scale * z * sin(to_radians(op_angle))
    list(x=x, y=y)
}

## Two-sided token
basicTokenGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                            x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                            angle=0, type="normal",
                            width=NA, height=NA, depth=NA,
                            op_scale=0, op_angle=45) {

            side <- get_side(piece_side)

            if (side %in% c("face", "back")) {

                grob <- cfg$get_grob(piece_side, suit, rank, type)
                xy_p <- op_xy(x, y, z+0.5*depth, op_angle, op_scale)
                cvp <- viewport(xy_p$x, xy_p$y, width, height, angle=angle)
                grob <- grid::editGrob(grob, name="piece_side", vp=cvp)

                edge <- basicTokenEdge(piece_side, suit, rank, cfg,
                                       x, y, z, angle, width, height, depth,
                                       op_scale, op_angle)
                edge <- grid::editGrob(edge, name="other_faces")

                gl <- gList(edge, grob)

                gTree(scale = 1, type = type,
                      children = gl, cl="basic_projected_token")

            } else {
                generalTokenGrob(piece_side, suit, rank, cfg,
                                 x, y, z,
                                 angle, type,
                                 width, height, depth,
                                 op_scale, op_angle)
            }

}

#' @export
makeContent.basic_projected_token <- function(x) {
    gp <- gpar(cex = x$scale, lex = x$scale)
    x$children$other_faces <- update_gp(x$children$other_faces, gp)

    if (hasName(x$children$piece_side, "scale"))
        x$children$piece_side$scale <- x$scale
    else if (x$type == "normal")
        x$children$piece_side <- update_gp(x$children$piece_side, gp)

    x
}

#' @export
grobCoords.basic_projected_token <- function(x, closed, ...) {
    if (is.null(x$children$other_faces$coords_xyl))
        NextMethod()
    else
        xylists_to_grobcoords(x$children$other_faces$coords_xyl, x$name, closed)
}

## Token edge-side grobs
basicTokenEdge <- function(piece_side, suit, rank, cfg=pp_cfg(),
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                           angle=0, width=NA, height=NA, depth=NA,
                           op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    piece <- get_piece(piece_side)
    side <- ifelse(opt$back, "back", "face") #### allow limited 3D rotation #281

    x <- convertX(x, "in", valueOnly = TRUE)
    y <- convertY(y, "in", valueOnly = TRUE)
    z <- convertX(z, "in", valueOnly = TRUE)
    width <- convertX(width, "in", valueOnly = TRUE)
    height <- convertY(height, "in", valueOnly = TRUE)
    depth <- convertX(depth, "in", valueOnly = TRUE)

    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)
    R <- side_R(side) %*% AA_to_R(angle, axis_x = 0, axis_y = 0)
    whd <- get_scaling_factors(side, width, height, depth)
    pc <- Point3D$new(x, y, z)
    token <- Token2S$new(shape, whd, pc, R)

    gl <- gList()
    # opposite side
    #### Could use transformation grob (flipped)
    opp_side <- ifelse(opt$back, "face", "back")
    opp_piece_side <- paste0(piece, "_", opp_side)
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

    # pre-compute grobCoords
    if (shape$convex)
        coords_xyl <- as.list(as.data.frame(token$xyz$project_op(op_angle, op_scale)$convex_hull))
    else
        coords_xyl <- NULL

    gTree(coords_xyl = coords_xyl,
          children = gList(grob_opposite, grob_edge))
}

generalTokenGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                           angle=0, type="normal",
                           width=NA, height=NA, depth=NA,
                           op_scale=0, op_angle=45) {

    cfg <- as_pp_cfg(cfg)
    piece <- get_piece(piece_side)
    side <- get_side(piece_side)
    opt <- cfg$get_piece_opt(paste0(piece, "_face"), suit, rank)
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    x <- convertX(x, "in", valueOnly = TRUE)
    y <- convertY(y, "in", valueOnly = TRUE)
    z <- convertX(z, "in", valueOnly = TRUE)
    width <- convertX(width, "in", valueOnly = TRUE)
    height <- convertY(height, "in", valueOnly = TRUE)
    depth <- convertX(depth, "in", valueOnly = TRUE)

    #### Generalize axis_x, axis_y #281
    axis_x <- 0
    axis_y <- 0

    # geometric vertices
    R <- side_R(side) %*% AA_to_R(angle, axis_x, axis_y)
    whd <- get_scaling_factors(side, width, height, depth)
    pc <- Point3D$new(x, y, z)
    token <- Token2S$new(shape, whd, pc, R)

    gl <- gList()
    edges <- token$op_edges(op_angle)
    for (i in seq_along(edges)) {
        name <- paste0("edge", i)
        gl[[i]] <- edges[[i]]$op_grob(op_angle, op_scale, name=name)
    }
    gp_edge <- gpar(col=opt$border_color, fill=opt$edge_color, lex=opt$border_lex)
    grob_edge <- gTree(children=gl, gp=gp_edge, name="token_edges")


    side_visible <- token$visible_side(op_angle)
    piece_side <- paste0(piece, "_", side_visible)
    xy_vp <- token$op_xy_vp(op_angle, op_scale, side_visible)
    xy_polygon <- token$xyz_side(side_visible)$project_op(op_angle, op_scale)
    ps_grob <- at_ps_grob(piece_side, suit, rank, cfg, xy_vp, xy_polygon)

    gl <- gList(grob_edge, ps_grob)

    # pre-compute grobCoords
    if (shape$convex)
        coords_xyl <- as.list(as.data.frame(token$xyz$project_op(op_angle, op_scale)$convex_hull))
    else
        coords_xyl <- NULL


    gTree(scale = 1, type = type,
          coords_xyl = coords_xyl,
          children = gl, cl = "general_projected_token")
}

#' @export
makeContent.general_projected_token <- function(x) {
    gp <- gpar(cex = x$scale, lex = x$scale)
    x$children$token_edges <- update_gp(x$children$token_edges, gp)
    x$children$piece_side$scale <- x$scale
    x
}

#' @export
grobCoords.general_projected_token <- function(x, closed, ...) {
    if (is.null(x$coords_xyl))
        NextMethod()
    else
        xylists_to_grobcoords(x$coords_xyl, x$name, closed)
}

## Die
basicDieGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
                     x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                     angle=0, type="normal",
                     width=NA, height=NA, depth=NA,
                     op_scale=0, op_angle=45) {

    grob <- cfg$get_grob(piece_side, suit, rank, type)
    xy_p <- op_xy(x, y, z+0.5*depth, op_angle, op_scale)
    cvp <- viewport(xy_p$x, xy_p$y, width, height, angle=angle)
    grob <- grid::editGrob(grob, name="top_face", vp=cvp)

    x <- convertX(x, "in", valueOnly = TRUE)
    y <- convertY(y, "in", valueOnly = TRUE)
    z <- convertX(z, "in", valueOnly = TRUE)
    width <- convertX(width, "in", valueOnly = TRUE)
    height <- convertY(height, "in", valueOnly = TRUE)
    depth <- convertX(depth, "in", valueOnly = TRUE)

    #### allow limited 3D rotation #281
    axis_x <- 0
    axis_y <- 0

    edge <- basicDieEdge(piece_side, suit, rank, cfg,
                         x, y, z,
                         angle, axis_x, axis_y,
                         width, height, depth,
                         op_scale, op_angle)
    edge <- grid::editGrob(edge, name="other_faces")

    gl <- gList(edge, grob)

    # pre-compute grobCoords
    coords_xyl <- die_grobcoords_xyl(suit, rank, cfg,
                                     x, y, z,
                                     angle, axis_x, axis_y,
                                     width, height, depth,
                                     op_scale, op_angle)

    gTree(scale = 1, type = type,
          coords_xyl = coords_xyl,
          children = gl, cl=c("basic_projected_die", "coords_xyl"))
}

#' @export
grobCoords.coords_xyl <- function(x, closed, ...) {
    xylists_to_grobcoords(x$coords_xyl, x$name, closed)
}

#' @export
makeContent.basic_projected_die <- function(x) {
    x$children$other_faces$children[[1]]$scale <- x$scale
    x$children$other_faces$children[[2]]$scale <- x$scale

    if (hasName(x$children$top_face, "scale")) {
        x$children$top_face$scale <- x$scale
    } else if (x$type == "normal") {
        gp <- gpar(cex = x$scale, lex = x$scale)
        x$children$top_face <- update_gp(x$children$top_face, gp)
    }
    x
}

#### What if shape is "roundrect"?
die_grobcoords_xyl <- function(suit, rank, cfg,
                              x, y, z,
                              angle, axis_x, axis_y,
                              width, height, depth,
                              op_scale, op_angle) {

    xyz <- die_xyz(suit, rank, cfg,
                   x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth)
    as.list(as.data.frame(xyz$project_op(op_angle, op_scale)$convex_hull))
}

basicDieEdge <- function(piece_side, suit, rank, cfg=pp_cfg(),
                     x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                     angle=0, axis_x=0, axis_y=0,
                     width=NA, height=NA, depth=NA,
                     op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)


    lf <- get_die_faces(suit, rank, cfg,
                        x, y, z,
                        angle, axis_x, axis_y,
                        width, height, depth)

    gl <- gList()

    indices_visible <- visible_die_faces(lf, op_angle)

    #### For "round" dice add a grob for corners #298
    for (i in indices_visible) {
        xy <- lf$f_xyz[[i]]$project_op(op_angle, op_scale)
        #### For "round" dice figure out coordinates for border lines #298
        rank <- lf$die_face_info$rank[i]
        name <- paste0("die_side", i)
        gl[[i]] <- at_ps_grob(piece_side, suit, rank, cfg, xy, xy, name = name)
    }
    gTree(children=gl, name="die_sides", cl="basic_projected_die_edge")
}

## Ellipsoid
basicEllipsoid <- function(piece_side, suit, rank, cfg=pp_cfg(),
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=unit(0, "npc"),
                           angle=0, type="normal",
                           width=NA, height=NA, depth=NA,
                           op_scale=0, op_angle=45) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)

    x <- convertX(x, "in", valueOnly = TRUE)
    y <- convertY(y, "in", valueOnly = TRUE)
    z <- convertX(z, "in", valueOnly = TRUE)
    width <- convertX(width, "in", valueOnly = TRUE)
    height <- convertY(height, "in", valueOnly = TRUE)
    depth <- convertX(depth, "in", valueOnly = TRUE)

    xyz <- ellipse_xyz()$dilate(width, height, depth)$translate(x, y, z)
    xy <- xyz$project_op(op_angle, op_scale)
    xy_hull <- xy$convex_hull

    gp <- gpar(col=opt$border_color, fill=opt$background_color, lex=opt$border_lex)
    polygonGrob(x = xy_hull$x, y = xy_hull$y,
                default.units = "in", gp = gp)
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

## Pyramid Top
basicPyramidTop <- function(piece_side, suit, rank, cfg=pp_cfg(),
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

        ex <- c(x, edge$p1$x, edge$p2$x)
        ey <- c(y, edge$p1$y, edge$p2$y)
        ez <- c(z + 0.5 * depth, z - 0.5 * depth, z - 0.5 * depth)
        xyz_polygon <- Point3D$new(x = ex, y = ey, z = ez)
        xy_polygon <- xyz_polygon$project_op(op_angle, op_scale)
        xy_vp <- xy_vp_ps(xyz_polygon, op_scale, op_angle)

        piece_side <- df$edge[i]
        gl[[i]] <- at_ps_grob(piece_side, suit, rank, cfg, xy_vp, xy_polygon, name = piece_side)
    }

    #### allow limited 3D rotation #281
    axis_x <- 0
    axis_y <- 0
    # pre-compute grobCoords
    coords_xyl <- pt_grobcoords_xyl(x, y, z,
                                    angle, axis_x, axis_y,
                                    width, height, depth,
                                    op_scale, op_angle)

    gTree(scale = 1,
          coords_xyl = coords_xyl,
          children=gl, cl=c("projected_pyramid_top", "coords_xyl"))
}

pt_grobcoords_xyl <- function(x, y, z,
                              angle, axis_x, axis_y,
                              width, height, depth,
                              op_scale, op_angle) {
    xyz <- pt_xyz(x, y, z,
                  angle, axis_x, axis_y,
                  width, height, depth)
    as.list(as.data.frame(xyz$project_op(op_angle, op_scale)$convex_hull))
}


## Pyramid side
basicPyramidSide <- function(piece_side, suit, rank, cfg=pp_cfg(),
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

    xy_b <- Point2D$new(pyramid_xy)$npc_to_in(x, y, width, height, angle)
    p <- Polygon$new(xy_b)
    xy_tip <- xy_b[1]

    theta <- 2 * asin(0.5 * width / height)
    yt <- 1 - cos(theta)
    xy_t <- Point2D$new(x = 0:1, y = yt)$npc_to_in(x, y, width, height, angle)

    gl <- gList()

    ## opposite edge
    opposite_edge <- switch(piece_side,
                            "pyramid_face" = "pyramid_back",
                            "pyramid_back" = "pyramid_face",
                            "pyramid_left" = "pyramid_right",
                            "pyramid_right" = "pyramid_left")
    xyz_polygon <- Point3D$new(x = xy_b$x[c(1, 3:2)],
                               y = xy_b$y[c(1, 3:2)],
                               z = z - 0.5 * depth)
    xy_polygon <- xyz_polygon$project_op(op_angle, op_scale)
    xy_vp <- xy_vp_ps(xyz_polygon, op_scale, op_angle)
    gl[[1]] <- at_ps_grob(opposite_edge, suit, rank, cfg, xy_vp, xy_polygon,
                          name = opposite_edge)

    ## side edges
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
        if (index == 1) { # right side viewed top (left side viewed on side)
            ex <- c(edge$p1$x, edge$p2$x, xy_t$x[1])
            ey <- c(edge$p1$y, edge$p2$y, xy_t$y[1])
            ez <- c(z - 0.5 * depth, z - 0.5 * depth, z + 0.5 * depth)
        } else { # left side viewed top (right side viewed on side)
            ex <- c(edge$p2$x, xy_t$x[2], edge$p1$x)
            ey <- c(edge$p2$y, xy_t$y[2], edge$p1$y)
            ez <- c(z - 0.5 * depth, z + 0.5 * depth, z - 0.5 * depth)
        }
        xyz_polygon <- Point3D$new(x = ex, y = ey, z = ez)
        xy_polygon <- xyz_polygon$project_op(op_angle, op_scale)
        xy_vp <- xy_vp_ps(xyz_polygon, op_scale, op_angle)
        gl[[gli]] <- at_ps_grob(edge_ps, suit, rank, cfg, xy_vp, xy_polygon, name = edge_ps)
        gli <- gli + 1
    }

    ## edge facing up
    x_f <- c(xy_tip$x, xy_t$x)
    y_f <- c(xy_tip$y, xy_t$y)
    z_f <- c(z - 0.5 * depth, z + 0.5 * depth, z + 0.5 * depth)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gp <- gpar(col = opt$border_color, lex = opt$border_lex, fill = opt$background_color)
    xyz_polygon <- Point3D$new(x = x_f, y = y_f, z = z_f)
    xy_polygon <- xyz_polygon$project_op(op_angle, op_scale)
    xy_vp <- xy_vp_ps(xyz_polygon, op_scale, op_angle)
    gl[[4]] <- at_ps_grob(piece_side, suit, rank, cfg, xy_vp, xy_polygon, name = piece_side)

    #### allow limited 3D rotation #281
    axis_x <- 0
    axis_y <- 0
    # pre-compute grobCoords
    coords_xyl <- ps_grobcoords_xyl(x, y, z,
                                    angle, axis_x, axis_y,
                                    width, height, depth,
                                    op_scale, op_angle)

    gTree(scale = 1,
          coords_xyl = coords_xyl,
          children=gl, cl=c("projected_pyramid_side", "coords_xyl"))
}

ps_grobcoords_xyl <- function(x, y, z,
                              angle, axis_x, axis_y,
                              width, height, depth,
                              op_scale, op_angle) {
    xyz <- ps_xyz(x, y, z,
                  angle, axis_x, axis_y,
                  width, height, depth)
    as.list(as.data.frame(xyz$project_op(op_angle, op_scale)$convex_hull))
}

makeContent.projected_pyramid_side <- function(x) {
    for (i in 1:4) {
        grob <- x$children[[i]]
        if (inherits(grob, c("polygon", "grob")))
            grob <- update_gp(grob, gp = gpar(cex = x$scale, lex = x$scale))
            x$children[[i]] <- grob
    }
    x
}

# compute `xy_vp` for pyramid sides
xy_vp_ps <- function(xyz_polygon, op_scale, op_angle) {

    p_midbottom <- xyz_polygon[2:3]$c
    p_diff <- p_midbottom$diff(xyz_polygon[1])
    p_ul <- xyz_polygon[2]$translate(p_diff)
    p_ur <- xyz_polygon[3]$translate(p_diff)

    x <- c(p_ul$x, xyz_polygon$x[2:3], p_ur$x)
    y <- c(p_ul$y, xyz_polygon$y[2:3], p_ur$y)
    z <- c(p_ul$z, xyz_polygon$z[2:3], p_ur$z)

    Point3D$new(x, y, z)$project_op(op_angle, op_scale)
}
