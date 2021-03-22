#' Piece Grob Functions
#'
#' \code{basicPieceGrob}, \code{pyramidTopGrob}, and \code{previewLayoutGrob}
#'  are the default \dQuote{grob} functions that \code{grid.piece} uses
#' to create \code{grid} graphical \code{grob} objects.  \code{picturePieceGrobFn}
#' is a function that returns a \dQuote{grob} function that imports graphics from files
#' found in its \code{directory} argument.
#'
#' @rdname basicPieceGrobs
#' @name basicPieceGrobs
#' @param cfg Piecepack configuration list or \code{pp_cfg} object.
#' @inheritParams grid.piece
#' @examples
#'
#'  if (require("grid") && capabilities("cairo")) {
#'     cfg <- pp_cfg(list(invert_colors=TRUE))
#'
#'     pushViewport(viewport(width=unit(2, "in"), height=unit(2, "in")))
#'     grid.draw(basicPieceGrob("tile_face", suit=1, rank=3))
#'     popViewport()
#'
#'     grid.newpage()
#'     pushViewport(viewport(width=unit(0.75, "in"), height=unit(0.75, "in")))
#'     grid.draw(basicPieceGrob("coin_back", suit=2, rank=0, cfg=cfg))
#'     popViewport()
#'
#'     grid.newpage()
#'     pushViewport(viewport(width=unit(6, "in"), height=unit(6, "in")))
#'     grid.draw(previewLayoutGrob("preview_layout", suit=5, rank=0, cfg=cfg))
#'     popViewport()
#'
#'     grid.newpage()
#'     pushViewport(viewport(width=unit(0.75, "in"), height=unit(0.75, "in")))
#'     grid.draw(pyramidTopGrob("pyramid_top", suit=3, rank=5))
#'     popViewport()
#'
#'     \donttest{
#'         directory <- tempdir()
#'         save_piece_images(cfg, directory=directory, format="svg", angle=0)
#'         cfg2 <- pp_cfg(list(grob_fn=picturePieceGrobFn(directory)))
#'
#'         grid.newpage()
#'         pushViewport(viewport(width=unit(0.75, "in"), height=unit(0.75, "in")))
#'         grid.draw(pyramidTopGrob("pyramid_top", suit=3, rank=5, cfg=cfg2))
#'         popViewport()
#'     }
#'  }
#' @export
basicPieceGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, name=NULL, gp=gpar(), vp=NULL, cl="basic_piece_side")
}

#' @export
makeContent.basic_piece_side <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color), name = "background")

    gl_grob <- shape$gridlines(gp = gpar(col = opt$gridline_color, lex = opt$gridline_lex), name = "gridlines")
    mat_grob <- shape$mat(opt$mat_width, gp = gpar(fill = opt$mat_color), name = "mat")

    # Primary symbol
    gp_ps <- gpar(col=opt$ps_color, fontsize=opt$ps_fontsize,
                  fontfamily=opt$ps_fontfamily, fontface=opt$ps_fontface)
    ps_grob <- textGrob(opt$ps_text, x=opt$ps_x, y=opt$ps_y, hjust = 0.5, vjust = 0.5,
                        gp = gp_ps, name = "primary_symbol")

    # Directional mark
    gp_dm <- gpar(col=opt$dm_color, fontsize=opt$dm_fontsize,
                  fontfamily=opt$dm_fontfamily, fontface=opt$ps_fontface)
    dm_grob <- textGrob(opt$dm_text, x=opt$dm_x, y=opt$dm_y, hjust = 0.5, vjust = 0.5,
                        gp = gp_dm, name = "directional_mark")

    gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
    border_grob <- shape$shape(gp=gp_border, name = "border")
    gl <- gList(background_grob, gl_grob, mat_grob, ps_grob, dm_grob, border_grob)

    setChildren(x, gl)
}

#' @rdname basicPieceGrobs
#' @param directory Directory that `picturePieceGrobFn` will look in for piece graphics.
#' @param filename_fn Function that takes arguments `directory`, `piece_side`, `suit`,
#'        `rank`, and optionally `cfg` and returns the (full path) filename of the image that the
#'        function returned by `picturePieceGrobFn` should import.
#' @export
picturePieceGrobFn <- function(directory, filename_fn=find_pp_file) {
    function(piece_side, suit, rank, cfg) {
        if (hasName(formals(find_pp_file), "cfg"))
            f <- filename_fn(directory, piece_side, suit, rank, cfg)
        else
            f <- filename_fn(directory, piece_side, suit, rank)
        file2grob(f)
    }
}

find_pp_file <- function(directory, piece_side, suit, rank, cfg) {
    for (format in c("svgz", "svg", "png", "jpg", "jpeg")) {
        f <- piece_filename_helper(directory, piece_side, format, suit, rank, cfg)
        if (file.exists(f)) {
            return(f)
        }
    }
    stop(paste("Couldn't find suitable", piece_side, "image in", directory))
}

piece_filename_helper <- function(directory, piece_side, format, suit, rank, cfg) {
    if (piece_side %in% intersect(cfg$lacks_rank, cfg$lacks_suit)) {
        piece_filename(directory, piece_side, format, 0)
    } else if (piece_side %in% cfg$lacks_rank) {
        piece_filename(directory, piece_side, format, 0, suit=suit)
    } else if (piece_side %in% cfg$lacks_suit) {
        piece_filename(directory, piece_side, format, 0, rank=rank)
    } else {
        piece_filename(directory, piece_side, format, 0, suit=suit, rank=rank)
    }
}

#' @rdname basicPieceGrobs
#' @export
pyramidTopGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    g1 <- pieceGrob("pyramid_face",  suit, rank, cfg, use_pictureGrob=TRUE,
                    y=0.75, width=1.0, height=0.5, angle=180, name="face")
    g2 <- pieceGrob("pyramid_back",  suit, rank, cfg, use_pictureGrob=TRUE,
                    y=0.25, width=1.0, height=0.5, angle=  0, name="back")
    g3 <- pieceGrob("pyramid_left",  suit, rank, cfg, use_pictureGrob=TRUE,
                    x=0.25, width=1.0, height=0.5, angle=-90, name="left")
    g4 <- pieceGrob("pyramid_right", suit, rank, cfg, use_pictureGrob=TRUE,
                    x=0.75, width=1.0, height=0.5, angle= 90, name="right")
    grobTree(g3, g4, g1, g2, cl="pyramid_top")
}

#' @rdname basicPieceGrobs
#' @export
previewLayoutGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)

    t_width <- cfg$get_width("tile_face")
    c_width <- cfg$get_width("coin_face")
    d_width <- cfg$get_width("die_face")
    s_width <-  cfg$get_width("saucer_face")
    p_height <- cfg$get_height("pawn_face")
    preview_width <- 3*t_width

    gl <- gList()
    cs_tiles <- rep("tile_face", 6)
    suit_tiles <- 1:6
    rank_tiles <- rep(2, 6)
    if (cfg$n_suits < 5) {
        cs_tiles[5] <- "tile_back"
        suit_tiles[5] <- NA
        rank_tiles[5] <- NA
    }
    if (cfg$n_suits < 6) {
        cs_tiles[6] <- "tile_back"
        suit_tiles[6] <- NA
        rank_tiles[6] <- NA
    }
    x_tiles <- c(1, 5, 5, 1, 3, 3)
    y_tiles <- c(5, 5, 3, 3, 5, 3)
    gl[["tiles"]] <- pieceGrob(cs_tiles, suit_tiles, rank_tiles, cfg, x_tiles, y_tiles,
                   default.units="in", name="tiles")

    cs_coins <- rep(c("coin_face", "coin_back"), each=3)
    suit_coins <- c(rep(NA, 3), 4:2)
    rank_coins <- c(1:3, rep(NA, 3))
    x_coins <- rep(1:3, 2)*c_width - 0.5*c_width
    y_coins <- 0.5*t_width + rep(c(0.5, -0.5), each=3)*c_width
    if (cfg$n_suits > 4) suit_coins[5] <- 5
    if (cfg$n_suits > 5) suit_coins[6] <- 6

    gl[["coins"]] <- pieceGrob(cs_coins, suit_coins, rank_coins, cfg, x_coins, y_coins,
                   default.units="in", name="coins")

    gl[["saucers"]] <- pieceGrob(c("saucer_face", "saucer_back"), c(1, NA), NA, cfg,
                   t_width+1.5*d_width, 0.5*t_width + c(0.5, -0.5)*s_width,
                   default.units="in", name="saucers")

    gl[["pawns"]] <- pieceGrob(c("pawn_face", "pawn_back"), 2, NA, cfg,
                   t_width+3*d_width, 0.5*t_width + c(0.5, -0.5)*p_height,
                   angle=c(0, 180), default.units="in", name="pawns")

    gl[["suitrankdie"]] <- pieceGrob("suitrankdie_layoutRF", NA, NA, cfg,
                   preview_width-2*d_width, 0.5*t_width,
                   default.units="in", name="suitrankdie")

    gTree(children=gl, cl="preview_layout")
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

get_die_face_info <- function(suit, arrangement = "counter_down") { #### also angle #175
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
    list(rank = rank, suit = suit)
}

piecepackDieGrob <- function(suit, cfg, flip=FALSE,
                             arrangement=cfg$die_arrangement) {
    cfg <- as_pp_cfg(cfg)
    angle <- rep(c(0, -90), 3)
    if (flip) {
        x <- x_die_layoutLF
        angle <- -angle
    } else {
        x <- x_die_layoutRF
    }
    rs <- get_die_face_info(suit, arrangement)
    gl <- gList()
    for (ii in 1:6) {
        gl[[ii]] <- pieceGrob("die_face", rs$suit[ii], rs$rank[ii], cfg,
                            x=x[ii], y=y_die_layout[ii],
                            width=1/4, height=1/3, angle=angle[ii])
    }
    gTree(children=gl, name="die_layout")
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
    gTree(children=gl, name="suitdie_layout")
}

get_suit_die_suits <- function(cfg) {
    n_suits <- cfg$n_suits
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
        ph <- cfg$get_height("pawn_face")
        pb <- 3/7 * ph
        denominator <- 2 * (ph + pb)
        yf <- 0.5*ph / denominator
        height <- ph / denominator
        gl[[1]] <- pieceGrob("pawn_face", suit, NA, cfg,
                             y=1/2-yf, height=height, name="pawn_face")
        gl[[2]] <- pieceGrob("pawn_back", suit, NA, cfg,
                             y=1/2+yf, height=height, angle=180, name="pawn_back")
    })
    opt <- cfg$get_piece_opt("pawn_face", suit, 0)
    gp <- gpar(col=opt$border_color, fill=NA)
    gl[[3]] <- linesGrob(y=0.5, gp=gpar(col=opt$border_color, fill=NA, lty="dashed"))
    gl[[4]] <- rectGrob(gp=gp)
    ll <- 0.07
    gl[[5]] <- segmentsGrob(0.5, 0, 0.5, ll, gp=gp)
    gl[[6]] <- segmentsGrob(0.5, 1, 0.5, 1-ll, gp=gp)
    gTree(children=gl, name="pawn_layout")
}

pyramidLayoutGrob <- function(piece_side, suit, rank, cfg) {
    w <- cfg$get_width("pyramid_face", rank=rank)
    h <- cfg$get_height("pyramid_face", rank=rank)
    d <- sqrt(h^2 + (0.5*w)^2)
    t <- c(72, 36, 0, -36, -72)
    r <- h
    x <- 0.5 * to_x(t, r)
    y <- d + 0.5 * to_y(t, r)
    pieces <- c("pyramid_face", "pyramid_right", "pyramid_back", "pyramid_left", "pyramid_face")
    angles <- c(90+72, 90+36, 90, 90-36, 90-72)
    df <- tibble(piece_side=pieces, x=x, y=y, suit=suit, rank=rank, angle=angles)
    g <- pmap_piece(df, cfg=cfg, default.units="inches", draw=FALSE)
    grobTree(g, name="pyramid_layout")
}
