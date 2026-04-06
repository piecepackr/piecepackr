# Get xyz value for vertices

die_xyz <- function(suit, rank, cfg, x, y, z, angle, axis_x, axis_y, width, height, depth) {
	xs <- c(0, 0, 1, 1, 0, 0, 1, 1) - 0.5
	ys <- c(1, 0, 0, 1, 1, 0, 0, 1) - 0.5
	zs <- rep(c(1, 0), each = 4) - 0.5

	dfi <- get_die_face_info(suit, cfg$die_arrangement)
	dR <- get_die_rotation(suit, rank, cfg)
	R <- dR %*% AA_to_R(angle, axis_x, axis_y)

	as_coord3d(xs, ys, zs)$scale(width, height, depth)$transform(R)$translate(x, y, z)
}

rounded_die_xyz <- function(suit, rank, cfg, x, y, z, angle, axis_x, axis_y, width, height, depth) {
	stopifnot(width == height, width == depth)
	opt <- cfg$get_piece_opt("die_face", suit, rank)
	# Sphere radius
	# 3D Euclidean distance to (0.5 - opt$shape_r, 0.5, 0.5)
	s <- sqrt((0.5 - opt$shape_r)^2 + 0.50)
	xy <- expand.grid(x = seq(0.0, 1.0, 0.05), y = seq(0.0, 1.0, 0.05))
	xy <- xy[which(xy$x^2 + xy$y^2 <= 1), ]
	nz <- sqrt(1 - xy$x^2 - xy$y^2)
	ppp <- data.frame(x = s * xy$x, y = s * xy$y, z = s * nz)
	ppn <- data.frame(x = s * xy$x, y = s * xy$y, z = -s * nz)
	pnp <- data.frame(x = s * xy$x, y = -s * xy$y, z = s * nz)
	pnn <- data.frame(x = s * xy$x, y = -s * xy$y, z = -s * nz)
	nnp <- data.frame(x = -s * xy$x, y = -s * xy$y, z = s * nz)
	npp <- data.frame(x = -s * xy$x, y = s * xy$y, z = s * nz)
	npn <- data.frame(x = -s * xy$x, y = s * xy$y, z = -s * nz)
	nnn <- data.frame(x = -s * xy$x, y = -s * xy$y, z = -s * nz)
	df <- rbind(ppp, ppn, pnp, pnn, nnp, npp, npn, nnn)
	# Flatten: clamp coords beyond each die face to the face boundary
	df$x <- pmin(pmax(df$x, -0.5), 0.5)
	df$y <- pmin(pmax(df$y, -0.5), 0.5)
	df$z <- pmin(pmax(df$z, -0.5), 0.5)
	dR <- get_die_rotation(suit, rank, cfg)
	R <- dR %*% AA_to_R(angle, axis_x, axis_y)
	as_coord3d(df)$scale(width, height, depth)$transform(R)$translate(x, y, z)
}

ellipse_xyz <- function() {
	xy <- expand.grid(x = seq(0.0, 1.0, 0.05), y = seq(0.0, 1.0, 0.05))
	xy <- xy[which(xy$x^2 + xy$y^2 <= 1), ]
	z <- sqrt(1 - xy$x^2 - xy$y^2)
	ppp <- data.frame(x = xy$x, y = xy$y, z = z)
	ppn <- data.frame(x = xy$x, y = xy$y, z = -z)
	pnp <- data.frame(x = xy$x, y = -xy$y, z = z)
	pnn <- data.frame(x = xy$x, y = -xy$y, z = -z)
	nnp <- data.frame(x = -xy$x, y = -xy$y, z = z)
	npp <- data.frame(x = -xy$x, y = xy$y, z = z)
	npn <- data.frame(x = -xy$x, y = xy$y, z = -z)
	nnn <- data.frame(x = -xy$x, y = -xy$y, z = -z)
	df <- 0.5 * rbind(ppp, ppn, pnp, pnn, nnp, npp, npn, nnn)
	as_coord3d(df)
}


# pyramid top
pt_xyz <- function(x, y, z, angle, axis_x, axis_y, width, height, depth) {
	xy <- as_coord2d(rect_xy)$translate(-0.5, -0.5)
	xyz_t <- as_coord3d(x = 0.0, y = 0.0, z = 0.5)
	xyz_b <- as_coord3d(xy, z = -0.5)
	xs <- c(xyz_t$x, xyz_b$x)
	ys <- c(xyz_t$y, xyz_b$y)
	zs <- c(xyz_t$z, xyz_b$z)
	as_coord3d(xs, ys, zs)$scale(width, height, depth)$transform(AA_to_R(
		angle,
		axis_x,
		axis_y
	))$translate(as_coord3d(x, y, z))
}

# pyramid (on its) side
ps_xyz <- function(x, y, z, angle, axis_x, axis_y, width, height, depth) {
	xy <- as_coord2d(pyramid_xy)$translate(-0.5, -0.5)
	xyz_b <- as_coord3d(xy, z = -0.5)
	theta <- 2 * asin(0.5 * width / height)
	xyz_t <- as_coord3d(x = c(-0.5, 0.5), y = 0.5 - cos(theta), z = 0.5)
	xs <- c(xyz_t$x, xyz_b$x)
	ys <- c(xyz_t$y, xyz_b$y)
	zs <- c(xyz_t$z, xyz_b$z)
	as_coord3d(xs, ys, zs)$scale(width, height, depth)$transform(AA_to_R(
		angle,
		axis_x,
		axis_y
	))$translate(x, y, z)
}
