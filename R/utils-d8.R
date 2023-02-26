# https://mathworld.wolfram.com/RegularOctahedron.html

## d8 Top
d8TopGrob <- function(piece_side, suit, rank, cfg=pp_cfg(),
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

    rlang::abort("We don't support oblique projection for d8 yet")

    #### allow limited 3D rotation #281
    axis_x <- 0
    axis_y <- 0
    ####
}

save_d8_obj <- function(piece_side, suit, rank, cfg,
                        x, y, z, angle, axis_x, axis_y,
                        width, height, depth,
                        filename, scale, res) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    rlang::abort("We don't support Wavefront OBJ export for d8 yet")
}
