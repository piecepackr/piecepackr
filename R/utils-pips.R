#' @importFrom tibble tibble
xya_pips_cards <- function(n_pips) {
    if (n_pips == 0) {
        return(tibble(x = numeric(0), y = numeric(0), angle = numeric(0)))
    }
    switch(n_pips,
           tibble(x = 0.5, y = 0.5, angle = 0), # 1
           tibble(x = 0.5, y = c(0.2, 0.8), angle = c(180, 0)), # 2
           tibble(x = 0.5, y = c(0.2, 0.5, 0.8), angle = c(180, 0, 0)), # 3
           tibble(x = rep(c(0.25, 0.75), 2),
                  y = rep(c(0.2, 0.8), each=2), # 4
                  angle = rep(c(180, 0), each=2)),
           tibble(x = c(rep(c(0.25, 0.75), 2), 0.5), # 5
                  y = c(rep(c(0.2, 0.8), each=2), 0.5),
                  angle = c(rep(180, 2), rep(0, 3))),
           tibble(x = rep(c(0.25, 0.75), 3),
                  y = rep(c(0.2, 0.5, 0.8), each=2), # 6
                  angle = c(rep(180, 2), rep(0, 4))),
           tibble(x = c(rep(c(0.25, 0.75), 3), 0.5), # 7
                  y = c(rep(c(0.2, 0.5, 0.75), each=2), 0.55),
                  angle = c(rep(180, 2), rep(0, 5))),
           tibble(x = rep(c(0.25, 0.75), 4),  # 8
                  y = rep(c(0.2, 0.4, 0.6, 0.8), each=2),
                  angle = c(rep(180, 4), rep(0, 4))),
           tibble(x = c(rep(c(0.25, 0.75), 4), 0.5), # 9
                  y = c(rep(c(0.2, 0.4, 0.6, 0.8), each=2), 0.7),
                  angle = c(rep(180, 4), rep(0, 5))),
           tibble(x = c(0.5, rep(c(0.25, 0.75), 4), 0.5), # 10
                  y = c(0.3, rep(c(0.2, 0.4, 0.6, 0.8), each=2), 0.7),
                  angle = c(rep(180, 5), rep(0, 5))),
           stop("Don't know pip pattern for ", n_pips, " pips")
           )
}

xya_pips_dominoes <- function(n_pips, die = FALSE) {
    if (n_pips == 0) {
        return(tibble(x = numeric(0), y = numeric(0), angle = numeric(0)))
    }
    if (die)
        high <- 0.75
    else
        high <- 0.78
    low <- 1 - high
    switch(n_pips,
           tibble(x = 0.5, y = 0.5, angle = 0), # 1
           tibble(x = c(low, high), y = c(high, low), angle = c(0, 180)), # 2
           tibble(x = c(low, 0.5, high),
                  y = c(high, 0.5, low),
                  angle = c(0, 0, 180)), # 3
           tibble(x = rep(c(low, high), 2), # 4
                  y = rep(c(low, high), each=2),
                  angle = rep(c(180, 0), each=2)),
           tibble(x = c(rep(c(low, high), 2), 0.5), # 5
                  y = c(rep(c(low, high), each=2), 0.5),
                  angle = c(rep(180, 2), rep(0, 3))),
           tibble(x = rep(c(low, high), 3),
                  y = rep(c(low, 0.5, high), each=2), # 6
                  angle = c(rep(180, 2), rep(0, 4))),
           tibble(x = c(rep(c(low, high), 3), 0.5), # 7
                  y = c(rep(c(low, 0.5, high), each=2), 0.50),
                  angle = c(rep(180, 2), rep(0, 5))),
           tibble(x = c(0.5, rep(c(low, high), 3), 0.5), # 8
                  y = c(low, rep(c(low, 0.5, high), each=2), high),
                  angle = c(rep(180, 3), rep(0, 5))),
           tibble(x = c(rep(c(low, 0.5, high), 3)), # 9
                  y = c(rep(c(low, 0.5, high), each=3)),
                  angle = c(rep(180, 3), rep(0, 6))),
           tibble(x = c(0.5, rep(c(0.20, 0.80), 4), 0.5), # 10
                  y = c(0.2, rep(c(0.2, 0.4, 0.6, 0.8), each=2), 0.8),
                  angle = c(rep(180, 5), rep(0, 5))),
           tibble(x = c(0.5, rep(c(0.20, 0.80), 4), 0.5, 0.5), # 11
                  y = c(0.2, rep(c(0.2, 0.4, 0.6, 0.8), each=2), 0.5, 0.8),
                  angle = c(rep(180, 5), rep(0, 6))),
           tibble(x = c(rep(c(0.20, 0.5, 0.80), 4)), # 12
                  y = c(rep(c(0.2, 0.4, 0.6, 0.8), each=3)),
                  angle = c(rep(180, 6), rep(0, 6))),
           tibble(x = c(rep(c(0.2, 0.4, 0.6, 0.8), 3), 0.5), # 13
                  y = c(rep(c(0.2, 0.4, 0.6), each=4), 0.8),
                  angle = c(rep(180, 8), rep(0, 5))),
           tibble(x = c(rep(c(0.2, 0.4, 0.6, 0.8), 3), 0.3, 0.7), # 14
                  y = c(rep(c(0.2, 0.4, 0.6), each=4), 0.8, 0.8),
                  angle = c(rep(180, 8), rep(0, 6))),
           tibble(x = c(rep(c(0.2, 0.4, 0.6, 0.8), 3), 0.3, 0.5, 0.7), # 15
                  y = c(rep(c(0.2, 0.4, 0.6), each=4), 0.8, 0.8, 0.8),
                  angle = c(rep(180, 8), rep(0, 7))),
           tibble(x = c(rep(c(0.2, 0.4, 0.6, 0.8), 4)), # 16
                  y = c(rep(c(0.2, 0.4, 0.6, 0.8), each=4)),
                  angle = c(rep(180, 8), rep(0, 8))),
           tibble(x = c(rep(c(0.140, 0.340, 0.660, 0.860), 4), 0.5), # 17
                  y = c(rep(c(0.2, 0.4, 0.6, 0.8), each=4), 0.5),
                  angle = c(rep(180, 8), rep(0, 9))),
           tibble(x = c(rep(c(0.140, 0.340, 0.660, 0.860), 4), 0.5, 0.5), # 18
                  y = c(rep(c(0.2, 0.4, 0.6, 0.8), each=4), 0.3, 0.7),
                  angle = c(rep(180, 8), rep(0, 8), 180, 0)),
           stop("Don't know pip pattern for ", n_pips, " pips")
           )
}

#### make.Content
cardGrobFn <- function(rank_offset = 0, type = "card", grob_type = "text") {
    force(type)
    force(rank_offset)
    force(grob_type)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

        background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")
        mat_grob <- shape$mat(opt$mat_width, gp = gpar(fill = opt$mat_color), name = "mat")

        gp_rank <- gpar(col = opt$ps_color, fontsize = opt$ps_fontsize,
                        fontfamily = opt$ps_fontfamily, fontface = opt$ps_fontface,
                        lineheight = 0.8)
        rank_grob <- textGrob(label = opt$ps_text, gp = gp_rank,
                              x = c(0.1, 0.9), y = c(0.9, 0.1), rot = c(0, 180),
                              hjust = 0.5, vjust = 0.5, name = "rank")
        gp_suit <- gpar(col = opt$dm_color, fontsize = opt$dm_fontsize,
                        fontfamily = opt$dm_fontfamily, fontface = opt$dm_fontface)
        suit_grob <- textGrob(label = opt$dm_text, gp = gp_suit,
                              x = c(0.1, 0.9), y = c(0.8, 0.2), rot = c(0, 180),
                              hjust = 0.5, vjust = 0.5, name = "suit")

        # Pips
        tfn <- pippedGrobFn(rank_offset, type, grob_type, border = FALSE, mat = FALSE)
        pip_grob <- tfn(piece_side, suit, rank, cfg)
        pip_grob <- grid::editGrob(pip_grob,
                          vp=viewport(height = 1.0, width = 0.80, gp=gpar(cex=1.8)), name = "pips")

        # Border
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")

        grobTree(background_grob, pip_grob, rank_grob, suit_grob, mat_grob, border_grob, cl="card_side")
    }
}

#### make.Content
jokerCardGrobFn <- function(star = TRUE) {
    force(star)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

        background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")
        mat_grob <- shape$mat(opt$mat_width, gp = gpar(fill = opt$mat_color), name = "mat")

        gp_rank <- gpar(col = opt$ps_color, fontsize = opt$ps_fontsize,
                        fontfamily = opt$ps_fontfamily, fontface = opt$ps_fontface,
                        lineheight = 0.8)
        rank_grob <- textGrob(label = opt$ps_text, gp = gp_rank,
                              x = c(0.1, 0.9), y = c(0.9, 0.1), rot = c(0, 180),
                              hjust = 0.5, vjust = 0.5, name = "rank")

        # Joker
        joker_grob <- jokerGrob(opt, star)

        # Border
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")

        grobTree(background_grob, joker_grob, rank_grob,
                 mat_grob, border_grob, cl="joker_card_side")
    }
}

jokerGrob <- function(opt, star = TRUE) {
    gp_meeple <- gpar(col = opt$ps_color, lwd = 4)
    vp_meeple <- viewport(height=inch(1.3), width=inch(1.2), y = 0.4)
    meeple_grob <- pp_shape("meeple")$shape(gp=gp_meeple, vp=vp_meeple)
    gp_triangle <- gpar(fill = opt$ps_color, col = NA_character_)
    y_triangle <- unit(0.4, "npc") + inch(0.5 * 1.3) + inch(0.5 * 0.8) + inch(-0.10)
    vp_triangle <- viewport(height=inch(0.8), width=inch(0.45), y = y_triangle)
    triangle_grob <- pp_shape("pyramid")$shape(gp=gp_triangle, vp=vp_triangle)
    y_circle <- y_triangle + inch(0.5 * 0.8) + inch(0.5 * 0.2) + inch(-0.02)
    vp_circle <- viewport(height=inch(0.2), width=inch(0.2), y = y_circle)
    circle_grob <- circleGrob(vp = vp_circle, gp = gpar(col = opt$ps_color, lwd = 4))
    if (star) {
        star_grob <- pp_shape("concave5")$shape(gp = gp_triangle, vp = vp_circle)
    } else {
        star_grob <- NULL
    }
    grobTree(meeple_grob, triangle_grob, circle_grob, star_grob,
             vp = viewport(width=0.8, height=0.9), cl = "joker")
}

#### make.Content
faceCardGrobFn <- function(label = "", placement = "high") {
    force(label)
    force(placement)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

        background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")
        mat_grob <- shape$mat(opt$mat_width, gp = gpar(fill = opt$mat_color), name = "mat")

        gp_rank <- gpar(col = opt$ps_color, fontsize = opt$ps_fontsize,
                        fontfamily = opt$ps_fontfamily, fontface = opt$ps_fontface,
                        lineheight = 0.8)
        rank_grob <- textGrob(label = opt$ps_text, gp = gp_rank,
                              x = c(0.1, 0.9), y = c(0.9, 0.1), rot = c(0, 180),
                              hjust = 0.5, vjust = 0.5, name = "rank")
        gp_suit <- gpar(col = opt$dm_color, fontsize = opt$dm_fontsize,
                        fontfamily = opt$dm_fontfamily, fontface = opt$dm_fontface)
        suit_grob <- textGrob(label = opt$dm_text, gp = gp_suit,
                              x = c(0.1, 0.9), y = c(0.8, 0.2), rot = c(0, 180),
                              hjust = 0.5, vjust = 0.5, name = "suit")

        # Face
        top_grob <- faceGrob(opt, label, placement)
        top_grob <- grid::editGrob(top_grob,
                          vp=viewport(y=0.75, height = 0.5, width = 0.80), name = "top")
        bot_grob <- faceGrob(opt, label, placement)
        bot_grob <- grid::editGrob(top_grob,
                          vp=viewport(y=0.25, height = 0.5, width = 0.80, angle = 180), name = "bottom")

        # Border
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")

        grobTree(background_grob, top_grob, bot_grob, rank_grob,
                 suit_grob, mat_grob, border_grob, cl="face_card_side")
    }
}

faceGrob <- function(opt, label = "", placement = "high") {
    if (placement == "high") {
        y <- 0.7
        x_meeple <- 0.5
        y_meeple <- 0.3
        in_meeple <- 1
        cex <- 1.5
    } else {
        y <- 0.25
        x_meeple <- 0.57
        y_meeple <- 0.42
        in_meeple <- 0.6
        cex <- 3.0
    }
    rot <- ifelse(label %in% c("\u046a", "\u050a"), 180, 0)
    gp_label <- gpar(col = opt$ps_color, fontsize = opt$ps_fontsize,
                    fontfamily = opt$ps_fontfamily, fontface = opt$ps_fontface,
                    lineheight = 0.8, cex = cex)
    label_grob <- textGrob(label = label, gp = gp_label, x = 0.5, y = y,
                          hjust = 0.5, vjust = 0.5, rot = rot, name = "label")
    gp_suit <- gpar(col = opt$dm_color, fontsize = opt$dm_fontsize,
                    fontfamily = opt$dm_fontfamily, fontface = opt$dm_fontface,
                    cex = in_meeple)
    suit_grob <- textGrob(label = opt$dm_text, gp = gp_suit, x = x_meeple, y = y_meeple,
                          hjust = 0.5, vjust = 0.5, name = "suit")
    gp_meeple <- gpar(col = opt$ps_color, lwd = 4)
    vp_meeple <- viewport(height=inch(in_meeple), width=inch(in_meeple), x = x_meeple, y = y_meeple)
    meeple_grob <- pp_shape("meeple")$shape(gp = gp_meeple, vp = vp_meeple)
    grobTree(meeple_grob, label_grob, suit_grob, cl="face")
}

#### Support roundrect shape
#### make.Content
dominoGrobFn <- function(rank_offset = 0, type = "domino", grob_type = "circle") {
    force(rank_offset)
    force(type)
    force(grob_type)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

        mat_grob <- shape$mat(opt$mat_width, gp = gpar(fill = opt$mat_color), name = "mat")

        # Top (Rank)
        tfn <- pippedGrobFn(rank_offset, type, grob_type, border = FALSE, mat = FALSE)
        top_grob <- tfn("tile_face", rank, rank, cfg)
        top_grob <- grid::editGrob(top_grob,
                          vp=viewport(height = 0.5, y = 0.75, angle = 180), name="top_rank")

        # Bottom (Suit)
        bfn <- pippedGrobFn(rank_offset, type, grob_type, border = FALSE, mat = FALSE)
        bot_grob <- bfn("tile_face", suit, suit, cfg)
        bot_grob <- grid::editGrob(bot_grob,
                          vp=viewport(height = 0.5, y = 0.25), name="bottom_suit")

        gp_gl <- gpar(col = opt$gridline_color, lex = opt$gridline_lex)
        gl_grob <- segmentsGrob(x0=0.1, x1=0.9, y0 = 0.5, y1 = 0.5,  gp=gp_gl, name="gridlines")

        # Border
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name="border")

        grobTree(top_grob, bot_grob, gl_grob, mat_grob, border_grob, cl="domino_side")
    }
}

pippedGrobFn <- function(rank_offset = 0, type = "die", grob_type = "circle",
                         border = TRUE, mat = TRUE) {
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        gTree(opt = opt, n_pips = rank + rank_offset, type = type, grob_type = grob_type, border = border, mat = mat,
              name = NULL, gp = gpar(), vp = NULL, cl = "pipped")
    }
}

#' @export
makeContent.pipped <- function(x) {
    opt <- x$opt
    if (x$type == "card") {
        xya <- xya_pips_cards(x$n_pips)
    } else if (x$type == "die") {
        xya <- xya_pips_dominoes(x$n_pips, die = TRUE)
    } else {
        xya <- xya_pips_dominoes(x$n_pips)
    }
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name="background")

    # Mat
    if (x$mat) {
        mat_grob <- shape$mat(opt$mat_width, gp = gpar(fill = opt$mat_color), name = "mat")
    } else {
        mat_grob <- NULL
    }

    # Pips
    if (nrow(xya) > 0) {
        if (x$grob_type == "circle") {
            gp_pip <- gpar(col=opt$dm_color, fill=opt$dm_color)
            if (x$type != "card" && x$n_pips > 9) r <- 0.06 else r <- 0.08
            pip_grob <- circleGrob(x=xya$x, y=xya$y, r=r, gp=gp_pip, name="pips")
        } else {
            gp_pip <- gpar(col=opt$dm_color, fontsize=opt$dm_fontsize,
                          fontfamily=opt$dm_fontfamily, fontface=opt$dm_fontface)
            pip_grob <- textGrob(opt$dm_text, x=xya$x, y=xya$y, rot = xya$angle,
                               gp = gp_pip, hjust = 0.5, vjust = 0.5, name="pips")
        }
    } else {
        pip_grob <- nullGrob(name="pips")
    }

    # Border
    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name="border")
    } else {
        border_grob <- NULL
    }

    gl <- gList(background_grob, mat_grob, pip_grob, border_grob)
    setChildren(x, gl)
}
