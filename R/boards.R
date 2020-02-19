cellBoardGrob <- function(nrows = 8, ncols = 8, # nolint
                            background_color = "white",
                            checker_color = "black",
                            gridline_color = NA_character_, gridline_lex = 1) {
    x <- seq(0.5 / ncols, 1 - 0.5 / ncols, length.out = ncols)
    y <- seq(0.5 / nrows, 1 - 0.5 / nrows, length.out = nrows)
    gl <- gList(rectGrob(gp = gpar(col = NA, fill = background_color)))
    col <- gridline_color
    fill <- c(checker_color, NA_character_)
    for (i in seq(nrows)) {
        lwd <- 1
        gp <- gpar(col = col, fill = cycle_elements(fill, i-1), lwd = lwd, lineend = "butt", lex = gridline_lex)
        gl[[i + 1]] <- rectGrob(x, y[i], width = 1 / ncols, height = 1 / nrows, gp = gp, default.units = "npc")
    }
    gl
}

checkeredBoardGrobFn <- function(nrows = 8, ncols = 8) { # nolint
    force(nrows)
    force(ncols)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)

        shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)

        background_grob <- shape_fn(gp=gpar(col=NA, fill=opt$background_color))

        background_color <- opt$background_color
        checker_color <- opt$gridline_color
        gridline_color <- NA_character_
        cell_grob <- cellBoardGrob(nrows, ncols, background_color, checker_color, gridline_color)

        border_grob <- shape_fn(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex))
        gl <- gList(background_grob, cell_grob, border_grob)

        gTree(children=gl, name=piece_side)
    }
}

linedBoardGrobFn <- function(nrows = 8, ncols = 8) { # nolint
    force(nrows)
    force(ncols)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)

        shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)

        background_grob <- shape_fn(gp=gpar(col=NA, fill=opt$background_color))

        background_color <- opt$background_color
        checker_color <- NA_character_
        cell_grob <- cellBoardGrob(nrows, ncols, background_color, checker_color,
                                   opt$gridline_color, opt$gridline_lex)

        border_grob <- shape_fn(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex))
        gl <- gList(background_grob, cell_grob, border_grob)

        gTree(children=gl, name=piece_side)
    }
}

cycle_elements <- function(x, n = 1) {
    l <- length(x)
    if (l < 2 || n == l || n == 0) {
        x
    } else if (n < l) {
        c(x[(n+1):l], x[1:n])
    } else {
        cycle_elements(cycle_elements(x, l), n-l)
    }
}
