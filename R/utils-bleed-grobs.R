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
    stopifnot(all(hasName(x$grob, c("name", "vp"))))
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    width <- convertWidth(unit(1, "npc"), "in") - 2 * x$bleed
    height <- convertHeight(unit(1, "npc"), "in") - 2 * x$bleed

    cvp <- viewport(width=width, height=height)
    grob <- grid::editGrob(x$grob, vp=cvp, name="piece")
    if (hasName(grob, "scale"))
        grob$scale <- x$scale

    width <- convertWidth(width, "in", valueOnly = TRUE)
    height <- convertHeight(height, "in", valueOnly = TRUE)
    bleed <- convertWidth(x$bleed, "in", valueOnly = TRUE)

    # if easy extend mats and/or gridlines into bleed zone...
    if (inherits(grob, "basic_piece_side") &&
        (is_color_invisible(opt$border_color) || nigh(opt$border_lex, 0)) &&
        (!is_color_invisible(opt$mat_color) && !nigh(opt$mat_width, 0)) &&
        opt$shape == "rect") {
        bleed <- rect_mat_bleed(opt, width, height, bleed)
    } else if (inherits(grob, "basic_piece_side") &&
        (is_color_invisible(opt$border_color) || nigh(opt$border_lex, 0)) &&
        (is_color_invisible(opt$mat_color) || nigh(opt$mat_width, 0)) &&
        (!is_color_invisible(opt$gridline_color) && !nigh(opt$gridline_lex, 0)) &&
        (opt$shape %in% c("rect", "roundrect"))) {
        bleed <- rect_gl_bleed(opt)
    } else {
        gp_bleed <- gpar(col = NA, lwd = 0, fill = opt$bleed_color)
        bleed <- rectGrob(gp = gp_bleed, name = "bleed")
    }

    gl <- gList(bleed, grob)

    setChildren(x, gl)
}

rect_mat_bleed <- function(opt, width, height, bleed) {
    gp_bg <- gpar(col = NA, lwd = 0, fill = opt$background_color)
    gp_mat <- gpar(col = NA, lwd = 0, fill = opt$mat_color)

    mat_width <- rep(opt$mat_width, length.out=4)
    if (!nigh(mat_width[1], 0)) {
        mat_width[1] <- (bleed + mat_width[1] * height) / (height + 2 * bleed)
    }
    if (!nigh(mat_width[2], 0)) {
        mat_width[2] <- (bleed + mat_width[2] * width) / (width + 2 * bleed)
    }
    if (!nigh(mat_width[3], 0)) {
        mat_width[3] <- (bleed + mat_width[3] * height) / (height + 2 * bleed)
    }
    if (!nigh(mat_width[4], 0)) {
        mat_width[4] <- (bleed + mat_width[4] * width) / (width + 2 * bleed)
    }
    mat <- rectMatGrobFn(mat_width)(gp = gp_mat)
    t <- 1 - mat_width[1]
    r <- 1 - mat_width[2]
    b <- mat_width[3]
    l <- mat_width[4]
    bg <- polygonGrob(x = c(l, l, r, r), y = c(t, b, b, t), name="background", gp=gp_bg)

    gList(bg, mat)
}

rect_gl_bleed <- function(opt) {
    gp_bg <- gpar(col = NA, lwd = 0, fill = opt$background_color)
    gp_gl <- gpar(col = opt$gridline_color,
                  lwd = 8 * opt$gridline_lex,
                  lineend = "butt")
    bg <- rectGrob(gp = gp_bg, name = "background")
    gl_v <- linesGrob(x=0.5, name="line1", gp = gp_gl)
    gl_h <- linesGrob(y=0.5, name="line2", gp = gp_gl)

    gList(bg, gl_v, gl_h)
}
