# Grob for Fudge dice

fudgeGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, border=TRUE, flip=FALSE, scale=1,
          name=NULL, gp=gpar(), vp=NULL, cl=c("fudge_die", "basic_piece_side"))
}

#' @export
makeContent.fudge_die <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

    # Primary symbol
    gp_ps <- gpar(col=NA, fill=opt$ps_color)
    if (opt$ps_text == "+") {
        ps_grob <- rectGrob(width = c(0.12, 0.7), height = c(0.7, 0.12),
                            default.units = "npc", gp=gp_ps)
    } else if (opt$ps_text == "-") {
        ps_grob <- rectGrob(width = 0.7, height = 0.12,
                            default.units = "npc", gp=gp_ps)
    } else {
        ps_grob <- nullGrob()
    }
    ps_grob <- editGrob(ps_grob, name = "primary_symbol")

    # Border
    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }
    if (x$flip)
        gl <- gList(ps_grob, background_grob, border_grob)
    else
        gl <- gList(background_grob, ps_grob, border_grob)

    setChildren(x, gl)
}
