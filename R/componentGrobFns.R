#' Piece Grob Functions
#'
#' Default functions \code{grid.piece} uses to create \code{grid} 
#' graphical \code{grob} objects.
#' 
#' @rdname pieceGrobFns
#' @name pieceGrobFns
#' @param cfg Piecepack configuration list or \code{pp_cfg} object, 
#' @inheritParams grid.piece
#' @export
basicPieceGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)

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

    gTree(children=gl, name=piece_side)
}

#' @rdname pieceGrobFns
#' @export
pyramidTopGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as.list(cfg)
    cfg$scale <- 0.3 * get_scale(cfg)
    g1 <- pieceGrob("pyramid_face",  suit, rank, cfg, 
                    y=0.75, width=1.0, height=0.5, angle=180, name="face")
    g2 <- pieceGrob("pyramid_back",  suit, rank, cfg, 
                    y=0.25, width=1.0, height=0.5, angle=  0, name="back")
    g3 <- pieceGrob("pyramid_left",  suit, rank, cfg, 
                    x=0.25, width=1.0, height=0.5, angle=-90, name="left")
    g4 <- pieceGrob("pyramid_right", suit, rank, cfg,
                    x=0.75, width=1.0, height=0.5, angle= 90, name="right")
    gTree(children=gList(g1, g2, g3, g4), name="pyramid_top")
}

dieLayoutGrobRF <- function(piece_side, suit, rank, cfg) {
    piecepackDieGrob(suit, cfg)
}

dieLayoutGrobLF <- function(piece_side, suit, rank, cfg) {
    piecepackDieGrob(suit, cfg, flip=TRUE)
}

suitdieLayoutGrobRF <- function(piece_side, suit, rank, cfg) {
    suitdieGrob(cfg)
}

suitdieLayouGrobtLF <- function(piece_side, suit, rank, cfg) {
    suitdieGrob(cfg, flip=TRUE)
}

suitrankdieLayoutGrobRF <- function(piece_side, suit, rank, cfg) {
    suit <- get_suit_die_suits(cfg)
    piecepackDieGrob(suit, cfg)
}
suitrankdieLayoutGrobLF <- function(piece_side, suit, rank, cfg) {
    suit <- get_suit_die_suits(cfg)
    piecepackDieGrob(suit, cfg, flip=TRUE)
}

## Layout functions
x_die_layoutRF <- c(1/4, 2/4, 2/4, 3/4, 3/4, 4/4) - 1/8
x_die_layoutLF <- c(4/4, 3/4, 3/4, 2/4, 2/4, 1/4) - 1/8
y_die_layout <- c(1/3, 1/3, 2/3, 2/3, 3/3, 3/3) - 1/6

piecepackDieGrob <- function(suit, cfg, flip=FALSE) {
    cfg <- as_pp_cfg(cfg)
    angle <- rep(c(0, -90), 3)
    if (flip) {
        x <- x_die_layoutLF
        angle <- -angle
    } else {
        x <- x_die_layoutRF
    }
    arrangement <- cfg$die_arrangement
    suit <- rep(suit, length.out=6)
    if (arrangement == "opposites_sum_to_5") {
        rank <- c(1, 2, 3, 6, 5, 4)
        suit <- suit[rank]
    } else if (arrangement == "counter_up") {
        rank <- 6:1
        suit <- rev(suit)
    } else {
        rank <- 1:6
    }
    gl <- gList()
    for (ii in 1:6) {
        vp <- viewport(x=x[ii], y=y_die_layout[ii], angle=angle[ii])
        gl[[ii]] <- pieceGrob("die_face", suit[ii], rank[ii], cfg, vp=vp)
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
    suit <- get_suit_die_suits(cfg)
    gl <- gList()
    for (ii in 1:6) {
        vp <- viewport(x=x[ii], y=y_die_layout[ii], angle=angle[ii])
        gl[[ii]] <- pieceGrob("suitdie_face", suit[ii], NA, cfg, vp=vp)
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

pawnLayoutGrob <- function(piece_side, suit, rank, cfg) {
    gl <- gList()
    suppressWarnings({
        ph <- cfg$get_pp_height("pawn_face")
        pb <- 3/7 * ph
        denominator <- 2*(ph + pb)
        yf <- 0.5*ph / denominator
        height = ph / denominator
        gl[[1]] <- pieceGrob("pawn_face", suit, NA, cfg, 
                             y=1/2-yf, height=height, name="pawn_face")
        gl[[2]] <- pieceGrob("pawn_back", suit, NA, cfg, 
                             y=1/2+yf, height=height, angle=180, name="pawn_back")
    })
    opt <- cfg$get_piece_opt("pawn_face", suit, 0)
    border_col <- opt$border_col
    gl[[3]] <- linesGrob(y=0.5, gp=gpar(col=border_col, fill=NA, lty="dashed"))
    gl[[4]] <- rectGrob(gp=gpar(col=border_col, fill=NA))
    ll <- 0.07
    gl[[5]] <- segmentsGrob(0.5, 0, 0.5, ll, gp=gpar(col=border_col))
    gl[[6]] <- segmentsGrob(0.5, 1, 0.5, 1-ll, gp=gpar(border_col))
    gTree(children=gl, name="pawn_layout")
}

pyramidLayoutGrob <- function(piece_side, suit, rank, cfg) {
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
            vp <- viewport(width=inch(cfg$get_pp_width(cs, rank)), 
                           height=inch(cfg$get_pp_height(cs, rank)), 
                           angle=angles[ii], x=x[ii], y=y[ii])
            grob <- pieceGrob(cs, suit, rank, cfg, vp=vp)
            gl[[ii]] <- grob
        }
    })
    gTree(children=gl)
}
