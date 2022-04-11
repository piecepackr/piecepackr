#### also allow customization of angle #175
get_die_face_info <- function(suit, arrangement = "counter_down") {
    angle <- rep(0, length.out=6)
    suit <- rep(suit, length.out=6)

    if (grepl(da_token, arrangement)) {
        s <- strsplit(arrangement, ",")[[1]]
        angle <- sub("[1-6]", "", s)
        angle <- sub("\\^", "0", angle)
        angle <- sub("<", "90", angle)
        angle <- sub(">", "-90", angle)
        angle <- as.numeric(sub("v", "180", angle))
        angle <- ifelse(is.na(angle), 0, angle)
        rank <- as.integer(sub("[\\^<>v]", "", s))
    } else if (arrangement == "opposites_sum_to_5") {
        rank <- c(1, 2, 3, 6, 5, 4)
    } else if (arrangement == "counter_up") {
        rank <- 6:1
    } else if (arrangement == "counter_down") {
        rank <- 1:6
    } else {
        msg <- paste(sprintf('`die_arrangement` = "%s" is unrecognized', arrangement))
        abort(msg , class = "piecepackr_cfg")
    }
    suit <- suit[rank]
    list(rank = rank, suit = suit, angle = angle)
}

da_token <- "^([1-6]([\\^<>v])?,){5}[1-6]([\\^<>v])?$"

# figure out rotation to reach each die face
get_die_rotation <- function(suit, rank, cfg) {
    rs <- get_die_face_info(suit, cfg$die_arrangement)
    i <- which(rs$rank == rank)
    stopifnot(i <= 6)
    dR <- switch(i,
                 diag(3),
                 R_y(-90) %*% R_z(90),
                 R_x(90) %*% R_z(-90),
                 R_x(180),
                 R_y(90) %*% R_z(90),
                 R_x(-90) %*% R_z(90))
    dR %*% R_z(-rs$angle[i])
}


get_die_faces <- function(suit, rank, cfg,
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
    xyz <- Point3D$new(xs, ys, zs)$dilate(width, height, depth)$rotate(R)$translate(pc)

    # textured face elements
    f <- list()
    f[[1]] <- xyz[die_cycle(1:4, dfi$angle[1])]
    f[[2]] <- xyz[die_cycle(c(8, 4, 3, 7), dfi$angle[2])]
    f[[3]] <- xyz[die_cycle(c(1, 4, 8, 5), dfi$angle[3])]
    f[[4]] <- xyz[die_cycle(c(6, 5, 8, 7), dfi$angle[4])]
    f[[5]] <- xyz[die_cycle(c(1, 5, 6, 2), dfi$angle[5])]
    f[[6]] <- xyz[die_cycle(c(3, 2, 6, 7), dfi$angle[6])]

    list(f_xyz = f, die_face_info = dfi)
}

die_cycle <- function(indices, angle) {
    if (angle == 90) {
        cycle_elements(indices, -1)
    } else if (angle == 180) {
        cycle_elements(indices, -2)
    } else if (angle == -90) {
        cycle_elements(indices,  1)
    } else {
        indices
    }
}

visible_die_faces <- function(die_faces, op_angle = 45) {
    indices <- 1:6

    i_top <- which.max(sapply(1:6, function(i) die_faces$f_xyz[[i]]$c$z))
    i_bot <- which.min(sapply(1:6, function(i) die_faces$f_xyz[[i]]$c$z))
    indices <- setdiff(indices, c(i_top, i_bot))

    r <- 10 * die_faces$f_xyz[[1]]$width
    op_ref <- Point2D$new(0, 0)$translate_polar(180 + op_angle, r)
    op_line <- Line$new(op_angle, op_ref)
    depths <- sapply(indices, function(i) die_faces$f_xyz[[i]]$c$z)
    dists <- sapply(indices, function(i) op_line$distance_to(die_faces$f_xyz[[i]]$c))
    indices <- indices[order(round(depths, 6), -dists)] # `round()` avoids weird sorting errors
    utils::tail(indices, 2L)
}
