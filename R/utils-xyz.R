# Get xyz value for vertices

die_xyz <- function(suit, rank, cfg,
                    x, y, z,
                    angle, axis_x, axis_y,
                    width, height, depth) {

    pc <- Point3D$new(x, y, z)
    xs <- c(0, 0, 1, 1, 0, 0, 1, 1) - 0.5
    ys <- c(1, 0, 0, 1, 1, 0, 0, 1) - 0.5
    zs <- rep(c(1, 0), each = 4) - 0.5

    dfi <- get_die_face_info(suit, cfg$die_arrangement)
    dR <- get_die_rotation(suit, rank, cfg)
    R <- dR %*% AA_to_R(angle, axis_x, axis_y)

    Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(R)$translate(pc)

}

# pyramid top
pt_xyz <- function(x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth) {
    pc <- Point3D$new(x, y, z)
    xy_npc <- Point2D$new(rect_xy)
    xy <- xy_npc$translate(-0.5, -0.5)
    xyz_t <- Point3D$new(x = 0.0, y = 0.0, z = 0.5)
    xyz_b <- Point3D$new(xy, z = -0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(angle, axis_x, axis_y)$translate(pc)
}

# pyramid (on its) side
ps_xyz <- function(x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth) {
    pc <- Point3D$new(x, y, z)
    xy_npc <- Point2D$new(pyramid_xy)
    xy <- xy_npc$translate(-0.5, -0.5)
    xyz_b <- Point3D$new(xy, z = -0.5)
    theta <- 2 * asin(0.5 * width / height)
    xyz_t <- Point3D$new(x = c(-0.5, 0.5), y = 0.5 - cos(theta), z = 0.5)
    xs <- c(xyz_t$x, xyz_b$x)
    ys <- c(xyz_t$y, xyz_b$y)
    zs <- c(xyz_t$z, xyz_b$z)
    Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(angle, axis_x, axis_y)$translate(pc)
}
