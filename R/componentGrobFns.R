#' Piece Grob Functions
#'
#' Default functions \code{grid.piece} uses to create \code{grid} 
#' graphical \code{grob} objects.
#' 
#' @rdname pieceGrobFns
#' @name pieceGrobFns
#' @inheritParams grid.piece
#' @export
basicPieceGrobFn <- function(piece_side, i_s, i_r, cfg) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, i_s, i_r)

    shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)

    # Background
    background_grob <- shape_fn(gp=gpar(col=NA, fill=opt$background_col))

    # Gridlines, Mat
    gl_grob <- gridlinesGrob(opt$gridline_col, opt$shape, opt$shape_t)

    mat_grob <- matGrob(opt$mat_col, opt$shape, opt$shape_t, opt$mat_width)

    # Primary symbol
    gp_ps <- gpar(col=opt$ps_col, fontsize=opt$ps_fontsize, 
                  fontfamily=opt$ps_fontfamily, fontface=opt$ps_fontface)
    ps_grob <- textGrob(opt$ps_text, x=opt$ps_x, y=opt$ps_y, gp=gp_ps)

    # Directional mark
    gp_dm <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, 
                  fontfamily=opt$dm_fontfamily, fontface=opt$ps_fontface)
    dm_grob <- textGrob(opt$dm_text, x=opt$dm_x, y=opt$dm_y, gp=gp_dm)

    # Border 
    border_grob <- shape_fn(gp=gpar(col=opt$border_col, fill=NA))
    gl <- gList(background_grob, gl_grob, mat_grob, ps_grob,
                dm_grob, border_grob)

    gTree(children=gl)
}

#' @rdname pieceGrobFns
#' @export
pyramidTopGrob <- function(piece_side, i_s, i_r, cfg) {
    cfg <- as.list(cfg)
    cfg$scale <- 0.3 * get_scale(cfg)
    g1 <- pieceGrob("pyramid_face", i_s, i_r, cfg)
    vp1 <- viewport(y=0.75, width=1.0, height=0.5, angle=180)
    g2 <- pieceGrob("pyramid_back", i_s, i_r, cfg)
    vp2 <- viewport(y=0.25, width=1.0, height=0.5, angle=0)
    g3 <- pieceGrob("pyramid_left", i_s, i_r, cfg)
    vp3 <- viewport(x=0.25, width=1.0, height=0.5, angle=-90)
    g4 <- pieceGrob("pyramid_right", i_s, i_r, cfg)
    vp4 <- viewport(x=0.75, width=1.0, height=0.5, angle=90)
    gl <- gList(gTree(g1, vp=vp1, name="face"),
        gTree(g2, vp=vp2, name="back"),
        gTree(g3, vp=vp3, name="left"),
        gTree(g4, vp=vp4, name="right"))
    gTree(children=gl)
}

dieLayoutGrobRF <- function(piece_side, i_s, i_r, cfg) {
    piecepackDieGrob(i_s, cfg)
}

dieLayoutGrobLF <- function(piece_side, i_s, i_r, cfg) {
    piecepackDieGrob(i_s, cfg, flip=TRUE)
}

suitdieLayoutGrobRF <- function(piece_side, i_s, i_r, cfg) {
    suitdieGrob(cfg)
}

suitdieLayouGrobtLF <- function(piece_side, i_s, i_r, cfg) {
    suitdieGrob(cfg, flip=TRUE)
}

suitrankdieLayoutGrobRF <- function(piece_side, i_s, i_r, cfg) {
    i_s <- get_suit_die_suits(cfg)
    piecepackDieGrob(i_s, cfg)
}
suitrankdieLayoutGrobLF <- function(piece_side, i_s, i_r, cfg) {
    i_s <- get_suit_die_suits(cfg)
    piecepackDieGrob(i_s, cfg, flip=TRUE)
}

## Layout functions
x_die_layoutRF <- c(1/4, 2/4, 2/4, 3/4, 3/4, 4/4) - 1/8
x_die_layoutLF <- c(4/4, 3/4, 3/4, 2/4, 2/4, 1/4) - 1/8
y_die_layout <- c(1/3, 1/3, 2/3, 2/3, 3/3, 3/3) - 1/6

piecepackDieGrob <- function(i_s, cfg, flip=FALSE) {
    cfg <- as_pp_cfg(cfg)
    angle <- rep(c(0, -90), 3)
    if (flip) {
        x <- x_die_layoutLF
        angle <- -angle
    } else {
        x <- x_die_layoutRF
    }
    arrangement <- cfg$die_arrangement
    i_s <- rep(i_s, length.out=6)
    if (arrangement == "opposites_sum_to_5") {
        i_r <- c(1, 2, 3, 6, 5, 4)
        i_s <- i_s[i_r]
    } else if (arrangement == "counter_up") {
        i_r <- 6:1
        i_s <- rev(i_s)
    } else {
        i_r <- 1:6
    }
    gl <- gList()
    for (ii in 1:6) {
        vp <- viewport(x=x[ii], y=y_die_layout[ii], angle=angle[ii])
        gl[[ii]] <- pieceGrob("die_face", i_s[ii], i_r[ii], cfg, vp=vp)
    }
    gTree(children=gl)
}

suitdieGrob <- function(cfg, flip=FALSE) {
    angle <- rep(c(-90, 0), 3)
    if (flip) {
        x <- x_die_layoutLF
        angle <- -angle
    } else {
        x <- x_die_layoutRF
    }
    i_s <- get_suit_die_suits(cfg)
    gl <- gList()
    for (ii in 1:6) {
        vp <- viewport(x=x[ii], y=y_die_layout[ii], angle=angle[ii])
        gl[[ii]] <- pieceGrob("suitdie_face", i_s[ii], NA, cfg, vp=vp)
    }
    gTree(children=gl)
}

get_suit_die_suits <- function(cfg) {
    n_suits <- get_n_suits(cfg)
    if (n_suits < 4) {
        rep(n_suits:1, length.out=6)
    } else if (n_suits == 4) {
        c(5,6,4:1)
    } else if (n_suits > 4) {
        6:1
    } 
}

pawnLayoutGrob <- function(piece_side, i_s, i_r, cfg) {
    gl <- gList()
    suppressWarnings({
        ph <- cfg$get_pp_height("pawn_face")
        pb <- 3/8 ####
        denominator <- 2*(ph + pb)
        yf <- 0.5*ph / denominator
        height = 0.5*ph / denominator
        vp <- viewport(y=1/2-yf, height=0.5*height)
        gl[[1]] <- pieceGrob("pawn_face", i_s, NA, cfg, vp=vp, name="pawn_face")
        vp <- viewport(y=1/2+yf, height=0.5*height, angle=180)
        gl[[2]] <- pieceGrob("pawn_back", i_s, NA, cfg, vp=vp, name="pawn_back")
    })
    opt <- cfg$get_piece_opt("pawn_face", i_s, 0)
    border_col <- opt$border_col
    gl[[3]] <- linesGrob(y=0.5, gp=gpar(col=border_col, fill=NA, lty="dashed"))
    gl[[4]] <- rectGrob(gp=gpar(col=border_col, fill=NA))
    ll <- 0.07
    gl[[5]] <- segmentsGrob(0.5, 0, 0.5, ll, gp=gpar(col=border_col))
    gl[[6]] <- segmentsGrob(0.5, 1, 0.5, 1-ll, gp=gpar(border_col))
    gTree(children=gl)
}

pyramidLayoutGrob <- function(piece_side, i_s, i_r, cfg) {
    gl <- gList()
    suppressWarnings({
        t <- c(72, 36, 0, -36, -72)
        r <- 0.5
        x <- to_x(t, r)
        y <- 0.5 + 0.5*to_y(t, r)
        pieces <- c("pyramid_face", "pyramid_right", "pyramid_back", "pyramid_left", "pyramid_face")
        angles <- c(90+72, 90+36, 90, 90-36, 90-72)
        for(ii in 1:5) {
            cs <- pieces[ii]
            vp <- viewport(width=inch(cfg$get_pp_width(cs, i_r)), 
                           height=inch(cfg$get_pp_height(cs, i_r)), 
                           angle=angles[ii], x=x[ii], y=y[ii])
            grob <- pieceGrob(cs, i_s, i_r, cfg, vp=vp)
            gl[[ii]] <- grob
        }
    })
    gTree(children=gl)
}
