# Get xyz value for vertices

die_xyz <- function(suit, rank, cfg,
                    x, y, z,
                    angle, axis_x, axis_y,
                    width, height, depth) {
    xs <- c(0, 0, 1, 1, 0, 0, 1, 1) - 0.5
    ys <- c(1, 0, 0, 1, 1, 0, 0, 1) - 0.5
    zs <- rep(c(1, 0), each = 4) - 0.5

    dfi <- get_die_face_info(suit, cfg$die_arrangement)
    dR <- get_die_rotation(suit, rank, cfg)
    R <- dR %*% AA_to_R(angle, axis_x, axis_y)

    as_coord3d(xs, ys, zs)$
        scale(width, height, depth)$
        transform(R)$
        translate(x, y, z)
}

# pyramid top
pt_xyz <- function(x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth) {
    xy <- as_coord2d(rect_xy)$translate(-0.5, -0.5)
    xyz_t <- as_coord3d(x = 0.0, y = 0.0, z = 0.5)
    xyz_b <- as_coord3d(xy, z = -0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    as_coord3d(xs, ys, zs)$
        scale(width, height, depth)$
        transform(AA_to_R(angle, axis_x, axis_y))$
        translate(as_coord3d(x, y, z))
}

# pyramid (on its) side
ps_xyz <- function(x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth) {
    xy <- as_coord2d(pyramid_xy)$translate(-0.5, -0.5)
    xyz_b <- as_coord3d(xy, z = -0.5)
    theta <- 2 * asin(0.5 * width / height)
    xyz_t <- as_coord3d(x = c(-0.5, 0.5), y = 0.5 - cos(theta), z = 0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    as_coord3d(xs, ys, zs)$
        scale(width, height, depth)$
        transform(AA_to_R(angle, axis_x, axis_y))$
        translate(x, y, z)
}
