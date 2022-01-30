basicGrobWithBleed <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    grob <- cfg$get_grob(piece_side, suit, rank)
    width <- inch(cfg$get_width(piece_side, suit, rank))
    height <- inch(cfg$get_height(piece_side, suit, rank))
    gTree(opt=opt, grob=grob, width=width, height=height,
          name=NULL, gp=gpar(), vp=NULL, cl="basic_grob_with_bleed")
}

#' @export
makeContent.basic_grob_with_bleed <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    grob <- x$grob
    cvp <- viewport(width=x$width, height=x$height)
    grob <- grid::editGrob(grob, vp=cvp, name="piece")

    gp_bleed <- gpar(col = NA, lwd = 0, fill = opt$bleed_color)
    bleed <- shape$polyclip(grob, op = "minus",
                            gp = gp_bleed, name = "bleed")

    gl <- gList(bleed, grob)

    setChildren(x, gl)
}
