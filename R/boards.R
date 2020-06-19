cellsGrob <- function(nrows = 8, ncols = 8, # nolint
                      name = NULL, gp = gpar(), vp = NULL) {
    if (is.null(gp$fill)) gp$fill <- NA_character_
    if (is.null(gp$col)) gp$col <- NA_character_
    if (is.null(gp$lineend)) gp$lineend <- "butt"

    fill <- gp$fill
    gp$fill <- NULL

    x <- seq(0.5 / ncols, 1 - 0.5 / ncols, length.out = ncols)
    y <- seq(0.5 / nrows, 1 - 0.5 / nrows, length.out = nrows)
    gl <- gList()
    fill <- c(fill, NA_character_)
    for (i in seq(nrows)) {
        lwd <- 1
        gp_cell <- gpar(fill = cycle_elements(fill, i-1))
        gl[[i]] <- rectGrob(x, y[i], width = 1 / ncols, height = 1 / nrows, default.units = "npc",
                            name = paste("cell.", i), gp = gp_cell)
    }
    gTree(children=gl, name = name, gp = gp, vp = vp, cl = "cells")
}

#### make.Content
checkeredBoardGrobFn <- function(nrows = 8, ncols = 8) { # nolint
    force(nrows)
    force(ncols)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

        background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

        cell_grob <- cellsGrob(nrows, ncols, gp=gpar(fill=opt$gridline_color), name = "cells")

        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")

        grobTree(background_grob, cell_grob, border_grob,
                 name=piece_side, cl = "checkered_board")
    }
}

#### make.Content
linedBoardGrobFn <- function(nrows = 8, ncols = 8) { # nolint
    force(nrows)
    force(ncols)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

        background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

        gp_cell <- gpar(col=opt$gridline_color, lex=opt$gridline_lex)
        cell_grob <- cellsGrob(nrows, ncols, gp=gp_cell, name = "cells")

        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex), name = "border")

        grobTree(background_grob, cell_grob, border_grob,
                 name=piece_side, cl = "lined_board")
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
