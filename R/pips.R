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

xya_pips_dominoes <- function(n_pips) {
    if (n_pips == 0) {
        return(tibble(x = numeric(0), y = numeric(0), angle = numeric(0)))
    }
    switch(n_pips,
           tibble(x = 0.5, y = 0.5, angle = 0), # 1
           tibble(x = c(0.25, 0.75), y = c(0.75, 0.25), angle = c(0, 180)), # 2
           tibble(x = c(0.25, 0.5, 0.75),
                  y = c(0.75, 0.5, 0.25),
                  angle = c(0, 0, 180)), # 3
           tibble(x = rep(c(0.25, 0.75), 2), # 4
                  y = rep(c(0.25, 0.75), each=2),
                  angle = rep(c(180, 0), each=2)),
           tibble(x = c(rep(c(0.25, 0.75), 2), 0.5), # 5
                  y = c(rep(c(0.25, 0.75), each=2), 0.5),
                  angle = c(rep(180, 2), rep(0, 3))),
           tibble(x = rep(c(0.25, 0.75), 3),
                  y = rep(c(0.25, 0.5, 0.75), each=2), # 6
                  angle = c(rep(180, 2), rep(0, 4))),
           tibble(x = c(rep(c(0.25, 0.75), 3), 0.5), # 7
                  y = c(rep(c(0.25, 0.5, 0.75), each=2), 0.50),
                  angle = c(rep(180, 2), rep(0, 5))),
           tibble(x = c(0.5, rep(c(0.25, 0.75), 3), 0.5), # 8
                  y = c(0.25, rep(c(0.25, 0.5, 0.75), each=2), 0.75),
                  angle = c(rep(180, 3), rep(0, 5))),
           tibble(x = c(rep(c(0.25, 0.5, 0.75), 3)), # 9
                  y = c(rep(c(0.25, 0.5, 0.75), each=3)),
                  angle = c(rep(180, 3), rep(0, 6))),
           tibble(x = c(0.5, rep(c(0.25, 0.75), 4), 0.5), # 10
                  y = c(0.2, rep(c(0.2, 0.4, 0.6, 0.8), each=2), 0.8),
                  angle = c(rep(180, 5), rep(0, 5))),
           tibble(x = c(0.5, rep(c(0.25, 0.75), 4), 0.5, 0.5), # 11
                  y = c(0.2, rep(c(0.2, 0.4, 0.6, 0.8), each=2), 0.5, 0.8),
                  angle = c(rep(180, 5), rep(0, 6))),
           tibble(x = c(rep(c(0.25, 0.5, 0.75), 4)), # 12
                  y = c(rep(c(0.2, 0.4, 0.6, 0.8), each=3)),
                  angle = c(rep(180, 6), rep(0, 6))),
           stop("Don't know pip pattern for ", n_pips, " pips")
           )
}

cardGrobFn <- function(rank_offset = 0, card = TRUE, type = "suit") {
    force(rank_offset)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)

        background_grob <- shape_fn(gp=gpar(col=NA, fill=opt$background_color))
        mat_grob <- matGrob(opt$mat_color, opt$shape, opt$shape_t, opt$mat_width)

        gp_rank <- gpar(col = opt$ps_color, fontsize = opt$ps_fontsize,
                        fontfamily = opt$ps_fontfamily, fontface = opt$ps_fontface,
                        lineheight = 0.8)
        rank_grob <- textGrob(label = opt$ps_text, gp = gp_rank,
                              x = c(0.1, 0.9), y = c(0.9, 0.1), rot = c(0, 180),
                              hjust = 0.5, vjust = 0.5)
        gp_suit <- gpar(col = opt$dm_color, fontsize = opt$dm_fontsize,
                        fontfamily = opt$dm_fontfamily, fontface = opt$dm_fontface)
        suit_grob <- textGrob(label = opt$dm_text, gp = gp_suit,
                              x = c(0.1, 0.9), y = c(0.8, 0.2), rot = c(0, 180),
                              hjust = 0.5, vjust = 0.5)

        # Pips
        tfn <- pippedGrobFn(rank_offset, card, type, border = FALSE, mat = FALSE)
        pip_grob <- grobTree(tfn(piece_side, suit, rank, cfg),
                          vp=viewport(height = 1.0, width = 0.80, gp=gpar(cex=1.8)))

        # Border
        border_grob <- shape_fn(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex))

        gl <- gList(background_grob, pip_grob, rank_grob, suit_grob, mat_grob, border_grob)
        gTree(children=gl, name=piece_side)
    }
}

dominoGrobFn <- function(rank_offset = 0, card = FALSE, type = "circle") {
    force(rank_offset)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)

        # Mat
        mat_grob <- matGrob(opt$mat_color, opt$shape, opt$shape_t, opt$mat_width)

        # Top (Rank)
        tfn <- pippedGrobFn(rank_offset, card, type, border = FALSE, mat = FALSE)
        top_grob <- grobTree(tfn("tile_face", rank, rank, cfg),
                          vp=viewport(height = 0.5, y = 0.75, angle = 180))

        # Bottom (Suit)
        bfn <- pippedGrobFn(rank_offset, card, type, border = FALSE, mat = FALSE)
        bot_grob <- grobTree(bfn("tile_face", suit, suit, cfg),
                          vp=viewport(height = 0.5, y = 0.25))

        gp_gl <- gpar(col = opt$gridline_color, lex = opt$gridline_lex)
        gl_grob <- segmentsGrob(x0=0.1, x1=0.9, y0 = 0.5, y1 = 0.5,  gp=gp_gl)

        # Border
        border_grob <- shape_fn(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex))

        gl <- gList(top_grob, bot_grob, gl_grob, mat_grob, border_grob)
        gTree(children=gl, name=piece_side)
    }
}

pippedGrobFn <- function(rank_offset = 0, card = FALSE, type = "circle",
                         border = TRUE, mat = TRUE) {
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        if (card) {
            xya <- xya_pips_cards(rank + rank_offset)
        } else {
            xya <- xya_pips_dominoes(rank + rank_offset)
        }
        opt <- cfg$get_piece_opt(piece_side, suit, rank)

        shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)

        # Background
        background_grob <- shape_fn(gp=gpar(col=NA, fill=opt$background_color))

        # Mat
        if (mat)
            mat_grob <- matGrob(opt$mat_color, opt$shape, opt$shape_t, opt$mat_width)
        else
            mat_grob <- nullGrob()

        # Pips
        if (nrow(xya) > 0) {
            if (type == "circle") {
                gp_pip <- gpar(col=opt$dm_color, fill=opt$dm_color)
                if (!card && (rank + rank_offset) > 9) r <- 0.06 else r <- 0.08
                pip_grob <- circleGrob(x=xya$x, y=xya$y, r=r, gp=gp_pip)
            } else {
                gp_pip <- gpar(col=opt$dm_color, fontsize=opt$dm_fontsize,
                              fontfamily=opt$dm_fontfamily, fontface=opt$dm_fontface)
                pip_grob <- textGrob(opt$dm_text, x=xya$x, y=xya$y, rot = xya$angle,
                                   gp = gp_pip, hjust = 0.5, vjust = 0.5)
            }
        } else {
            pip_grob <- nullGrob()
        }

        # Border
        if (border) {
            border_grob <- shape_fn(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex))
        } else {
            border_grob <- nullGrob()
        }

        gl <- gList(background_grob, mat_grob, pip_grob, border_grob)
        gTree(children=gl, name=piece_side)
    }
}
