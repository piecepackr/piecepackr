cellsGrob <- function(nrows = 8, ncols = 8, # nolint
                      name = NULL, gp = gpar(), vp = NULL) {
    x <- seq(0.5 / ncols, 1 - 0.5 / ncols, length.out = ncols)
    y <- seq(0.5 / nrows, 1 - 0.5 / nrows, length.out = nrows)
    gl <- gList()
    fill <- c(gp$fill %||% NA_character_, NA_character_)
    for (i in seq(nrows)) {
        lwd <- 1
        gp_cell <- gpar(fill = cycle_elements(fill, i-1))
        gl[[i]] <- rectGrob(x, y[i], width = 1 / ncols, height = 1 / nrows, default.units = "npc",
                            name = paste("cell.", i), gp = gp_cell)
    }
    gp$col <- gp$col %||% NA_character_
    gp$fill <- NULL
    gp$lineend <- gp$lineend %||% "butt"
    gTree(children=gl, name = name, gp = gp, vp = vp, cl = "cells")
}

checkeredBoardGrobFn <- function(nrows = 8, ncols = 8, margin = 0) { # nolint
    force(nrows)
    force(ncols)
    force(margin)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        gTree(opt=opt, border=TRUE, scale = 1,
              nrows=nrows, ncols=ncols, margin=margin,
              name=piece_side,
              cl = c("checkered_board", "basic_piece_side"))
    }
}

#' @export
makeContent.checkered_board <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

    vp_cell <- viewport(width = x$ncols / (x$ncols + 2 * x$margin),
                        height = x$nrows / (x$nrows + 2 * x$margin))
    cell_grob <- cellsGrob(x$nrows, x$ncols, gp=gpar(fill=opt$gridline_color),
                           name = "cells", vp = vp_cell)

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }

    gl <- gList(background_grob, cell_grob, border_grob)
    setChildren(x, gl)
}

linedBoardGrobFn <- function(nrows = 8, ncols = 8, margin = 0) { # nolint
    force(nrows)
    force(ncols)
    force(margin)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)
        gTree(opt=opt, border=TRUE, scale = 1,
              nrows=nrows, ncols=ncols, margin=margin,
              name=piece_side,
              cl = c("lined_board", "basic_piece_side"))
    }
}

#' @export
makeContent.lined_board <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

    vp_cell <- viewport(width = x$ncols / (x$ncols + 2 * x$margin),
                        height = x$nrows / (x$nrows + 2 * x$margin))
    gp_cell <- gpar(col=opt$gridline_color, lex=opt$gridline_lex)
    cell_grob <- cellsGrob(x$nrows, x$ncols, gp=gp_cell, name = "cells", vp = vp_cell)

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex), name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }

    gl <- gList(background_grob, cell_grob, border_grob)
    setChildren(x, gl)
}

cycle_elements <- function(x, n = 1) {
    l <- length(x)
    if (l < 2 || abs(n) == l || n == 0) {
        x
    } else if (n < -l) {
        cycle_elements(cycle_elements(x, -l), n+l)
    } else if (n < 0) {
        n <- -n
        c(x[(l-n+1):l], x[1:(l-n)])
    } else if (n < l) {
        c(x[(n+1):l], x[1:n])
    } else {
        cycle_elements(cycle_elements(x, l), n-l)
    }
}

alquerqueBoardGrobFn <- function() {
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)

        gTree(opt = opt,
              scale = 1, border = TRUE,
              name=piece_side,
              cl = c("alquerque_board", "basic_piece_side"))
    }
}

#' @export
makeContent.alquerque_board <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

    gp_line <- gpar(col=NA, fill=opt$gridline_color)
    inner_grob <- alquerqueInnerGrob(gp=gp_line, name = "lines_and_dots")
    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex), name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }

    gl <- gList(background_grob, inner_grob, border_grob)
    setChildren(x, gl)
}

morrisBoardGrobFn <- function(n_pieces = 12) {
    force(n_pieces)
    function(piece_side, suit, rank, cfg=pp_cfg()) {
        cfg <- as_pp_cfg(cfg)
        opt <- cfg$get_piece_opt(piece_side, suit, rank)

        gTree(opt = opt, n_pieces = n_pieces,
              scale = 1, border = TRUE,
              name=piece_side,
              cl = c("morris_board", "basic_piece_side"))

    }
}

#' @export
makeContent.morris_board <- function(x) {
    n_pieces <- x$n_pieces
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

    gp_line <- gpar(col=NA, fill=opt$gridline_color)
    if (n_pieces < 3) {
        inner_grob <- morrisBoard3Grob(gp=gp_line, name = "lines_and_dots", diag = FALSE)
    } else if (n_pieces < 5) {
        inner_grob <- morrisBoard3Grob(gp=gp_line, name = "lines_and_dots")
    } else if (n_pieces < 7) {
        inner_grob <- morrisBoard6Grob(gp=gp_line, name = "lines_and_dots")
    } else if (n_pieces == 7) {
        inner_grob <- morrisBoard6Grob(gp=gp_line, name = "lines_and_dots", cross = TRUE)
    } else if (n_pieces < 11) {
        inner_grob <- morrisBoard9Grob(gp=gp_line, name = "lines_and_dots")
    } else {
        inner_grob <- morrisBoard9Grob(gp=gp_line, name = "lines_and_dots", diag = TRUE)
    }

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex), name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }

    gl <- gList(background_grob, inner_grob, border_grob)
    setChildren(x, gl)
}

alquerqueInnerGrob <- function(gp = gpar(), name = NULL, diag = TRUE) {
    w <- 1/64
    wd <- w / sqrt(2) / 2
    vlines <- rectGrob(x = c(0, 1, 2, 3, 4) / 4, y = 0.5, width = w, height = 1 + w)
    hlines <- rectGrob(x = 0.5, y = c(0, 1, 2, 3, 4) / 4, height = w, width = 1 + w)
    tl_br1 <- polygonGrob(x = c(1 - wd, 1 + wd, 1 + wd, 0 + wd, 0 - wd, 0 - wd, 1 - wd),
                         y = c(0 - wd, 0 - wd, 0 + wd, 1 + wd, 1 + wd, 1 - wd, 0 - wd))
    tl_br2 <- polygonGrob(x = c(1/2 - wd, 1/2 + wd, 1/2 + wd, 0 + wd, 0 - wd, 0 - wd, 1/2 - wd),
                         y = c(0 - wd, 0 - wd, 0 + wd, 1/2 + wd, 1/2 + wd, 1/2 - wd, 0 - wd))
    tl_br3 <- polygonGrob(x = c(1 - wd, 1 + wd, 1 + wd, 1/2 + wd, 1/2 - wd, 1/2 - wd, 1 - wd),
                         y = c(1/2 - wd, 1/2 - wd, 1/2 + wd, 1 + wd, 1 + wd, 1 - wd, 1/2 - wd))
    tr_bl1 <- polygonGrob(x = c(0 + wd, 0 - wd, 0 - wd, 1 - wd, 1 + wd, 1 + wd, 0 + wd),
                         y = c(0 - wd, 0 - wd, 0 + wd, 1 + wd, 1 + wd, 1 - wd, 0 - wd))
    tr_bl2 <- polygonGrob(x = c(0 + wd, 0 - wd, 0 - wd, 1/2 - wd, 1/2 + wd, 1/2 + wd, 0 + wd),
                         y = c(1/2 - wd, 1/2 - wd, 1/2 + wd, 1 + wd, 1 + wd, 1 - wd, 1/2 - wd))
    tr_bl3 <- polygonGrob(x = c(1/2 + wd, 1/2 - wd, 1/2 - wd, 1 - wd, 1 + wd, 1 + wd, 1/2 + wd),
                         y = c(0 - wd, 0 - wd, 0 + wd, 1/2 + wd, 1/2 + wd, 1/2 - wd, 0 - wd))
    dots <- circleGrob(x = rep(0:4, 5) / 4, y = rep(0:4, each = 5) / 4, r = 1.5 * w)

    grobTree(vlines, hlines, tl_br1, tl_br2, tl_br3, tr_bl1, tr_bl2, tr_bl3, dots,
             name = name, cl = "alquerque_board_inner", gp = gp,
             vp = viewport(width = 4/5, height = 4/5))
}

morrisBoard3Grob <- function(gp = gpar(), name = NULL, diag = TRUE) {
    w <- 1/64
    wd <- w / sqrt(2) / 2
    vlines <- rectGrob(x = c(0, 1, 2) / 2, y = 0.5, width = w, height = 1 + w)
    hlines <- rectGrob(x = 0.5, y = c(0, 1, 2) / 2, height = w, width = 1 + w)
    if (diag) {
        tl_br <- polygonGrob(x = c(1 - wd, 1 + wd, 1 + wd, 0 + wd, 0 - wd, 0 - wd, 1 - wd),
                             y = c(0 - wd, 0 - wd, 0 + wd, 1 + wd, 1 + wd, 1 - wd, 0 - wd))
        tr_bl <- polygonGrob(x = c(0 + wd, 0 - wd, 0 - wd, 1 - wd, 1 + wd, 1 + wd, 0 + wd),
                             y = c(0 - wd, 0 - wd, 0 + wd, 1 + wd, 1 + wd, 1 - wd, 0 - wd))
    } else {
        tl_br <- NULL
        tr_bl <- NULL
    }
    dots <- circleGrob(x = rep(c(0, 1/2, 1), 3), y = rep(c(0, 1/2, 1), each = 3), r = 1.5 * w)

    grobTree(vlines, hlines, tl_br, tr_bl, dots,
             name = name, cl = "morris_board_inner", gp = gp,
             vp = viewport(width = 2/3, height = 2/3))
}
morrisBoard6Grob <- function(gp = gpar(), name = NULL, cross = FALSE) {
    w <- 1/64
    wd <- w / sqrt(2) / 2
    dots <- circleGrob(x = rep.int(c(0, 1, 2, 3, 4), c(3, 3, 4, 3, 3)) / 4,
                       y = c(0, 2, 4, 1, 2, 3, 0, 1, 3, 4, 1, 2, 3, 0, 2, 4) / 4,
                       r = 1.5 * w)
    if (cross) {
        vlines <- rectGrob(x = c(0, 1, 2,  3, 4) / 4,
                           y = c(2, 2, 2, 2, 2) / 4,
                           width = w,
                           height = c(4, 2, 4, 2, 4) / 4 + w)
        hlines <- rectGrob(x =  c(2, 2, 2, 2, 2) / 4,
                           y =  c(0, 1, 2, 3, 4) / 4,
                           height = w,
                           width = c(4, 2, 4, 2, 4) / 4 + w)
    } else {
        vlines <- rectGrob(x = c(0, 1, 2, 2, 3, 4) / 4,
                           y = c(2, 2, 0.5, 3.5, 2, 2) / 4,
                           width = w,
                           height = c(4, 2, 1, 1, 2, 4) / 4 + w)
        hlines <- rectGrob(x =  c(2, 2, 0.5, 3.5, 2, 2) / 4,
                           y =  c(0, 1, 2, 2, 3, 4) / 4,
                           height = w,
                           width = c(4, 2, 1, 1, 2, 4) / 4 + w)
    }

    grobTree(vlines, hlines, dots,
             name = name, cl = "morris_board_inner", gp = gp,
             vp = viewport(width = 4/5, height = 4/5))
}
morrisBoard9Grob <- function(gp = gpar(), name = NULL, diag = FALSE) {
    w <- 1/64
    wd <- w / sqrt(2) / 2
    vlines <- rectGrob(x = c(0, 1, 2, 3, 3, 4, 5, 6) / 6,
                       y = c(3, 3, 3, 1, 5, 3, 3, 3) / 6,
                       width = w,
                       height = c(6, 4, 2, 2, 2, 2, 4, 6) / 6 + w)
    hlines <- rectGrob(x =  c(3, 3, 3, 1, 5, 3, 3, 3) / 6,
                       y =  c(0, 1, 2, 3, 3, 4, 5, 6) / 6,
                       height = w,
                       width =  c(6, 4, 2, 2, 2, 2, 4, 6) / 6 + w)
    dots <- circleGrob(x = rep.int(c(0, 1, 2, 3, 4, 5, 6), c(3, 3, 3, 6, 3, 3, 3)) / 6,
                       y = c(0, 3, 6, 1, 3, 5, 2, 3, 4, 0, 1, 2, 4, 5, 6, 2, 3, 4, 1, 3, 5, 0, 3, 6) / 6,
                       r = 1.5 * w)
    if (diag) {
        ul <- polygonGrob(x = c(2/6 - wd, 2/6 + wd, 2/6 + wd, 0 + wd, 0 - wd, 0 - wd, 2/6 - wd),
                          y = c(0 - wd, 0 - wd, 0 + wd, 2/6 + wd, 2/6 + wd, 2/6 - wd, 0 - wd) + 4/6)
        br <- polygonGrob(x = c(2/6 - wd, 2/6 + wd, 2/6 + wd, 0 + wd, 0 - wd, 0 - wd, 2/6 - wd) + 4/6,
                          y = c(0 - wd, 0 - wd, 0 + wd, 2/6 + wd, 2/6 + wd, 2/6 - wd, 0 - wd))
        ur <- polygonGrob(x = c(0 + wd, 0 - wd, 0 - wd, 2/6 - wd, 2/6 + wd, 2/6 + wd, 0 + wd) + 4/6,
                          y = c(0 - wd, 0 - wd, 0 + wd, 2/6 + wd, 2/6 + wd, 2/6 - wd, 0 - wd) + 4/6)
        bl <- polygonGrob(x = c(0 + wd, 0 - wd, 0 - wd, 2/6 - wd, 2/6 + wd, 2/6 + wd, 0 + wd),
                          y = c(0 - wd, 0 - wd, 0 + wd, 2/6 + wd, 2/6 + wd, 2/6 - wd, 0 - wd))
    } else {
        ul <- NULL
        ur <- NULL
        br <- NULL
        bl <- NULL
    }

    grobTree(vlines, hlines, dots, ul, ur, br, bl,
             name = name, cl = "morris_board_inner", gp = gp,
             vp = viewport(width=6/7, height=6/7))
}
