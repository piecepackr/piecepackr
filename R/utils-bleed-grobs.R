basicGrobWithBleed <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    grob <- cfg$get_grob(piece_side, suit, rank)
    gTree(opt=opt, grob=grob,
          bleed=inch(0.125), scale=1,
          name=NULL, gp=gpar(), vp=NULL, cl="basic_grob_with_bleed")
}

#' @export
makeContent.basic_grob_with_bleed <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    width <- convertWidth(unit(1, "npc"), "in") - 2 * x$bleed
    height <- convertHeight(unit(1, "npc"), "in") - 2 * x$bleed

    cvp <- viewport(width=width, height=height)
    grob <- grid::editGrob(x$grob, vp=cvp, name="piece")
    if (hasName(grob, "scale"))
        grob$scale <- x$scale

    gp_bleed <- gpar(col = NA, lwd = 0, fill = opt$bleed_color)
    bleed <- rectGrob(gp = gp_bleed, name = "bleed")

    gl <- gList(bleed, grob)

    setChildren(x, gl)
}
