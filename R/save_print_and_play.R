is_odd <- function(x) as.logical(x %% 2)

LETTER_WIDTH <- 8.5
LETTER_HEIGHT <- 11
A4_WIDTH <- 8.27
A4_HEIGHT <- 11.69
A5W <- 5 # 5.83"
A5H <- 7.5 # 8.27"

dev_onefile <- function(filename, family, paper) {
    dev <- switch(tools::file_ext(filename),
                  pdf = grDevices::cairo_pdf,
                  ps = grDevices::cairo_ps,
                  svg = grDevices::svg)
    if (paper == "letter") {
        dev(filename, onefile=TRUE, width=LETTER_HEIGHT, height=LETTER_WIDTH, family=family)
    } else if (paper == "A4") {
        dev(filename, onefile=TRUE, width=A4_HEIGHT, height=A4_WIDTH, family=family)
    } else if (paper == "A5") {
        dev(filename, onefile=TRUE, width=A4_HEIGHT/2, height=A4_WIDTH, family=family)
    } else {
        stop(paste("Don't know how to handle paper", paper))
    }
}

gappend <- function(ll, g) {
    ll[[length(ll)+1]] <- g
    ll
}

#' Save piecepack print-and-play (PnP) file
#'
#' Save piecepack print-and-play (PnP) file
#'
#' @param cfg Piecepack configuration list or `pp_cfg` object
#' @param output_filename Filename for print-and-play file
#' @param size PnP output size (currently either "letter", "A4", or "A5")
#' @param pieces Character vector of desired PnP pieces.
#'        Supports "piecepack", "matchsticks", "pyramids", "subpack", or "all".
#' @param arrangement Either "single-sided" or "double-sided".
#' @examples
#'   \donttest{
#'     is_mac <- tolower(Sys.info()[["sysname"]]) == "darwin"
#'     if (capabilities("cairo") && !is_mac) {
#'         cfg <- pp_cfg(list(invert_colors.suited=TRUE))
#'         save_print_and_play(cfg, "my_pnp_file.pdf")
#'         save_print_and_play(cfg, "my_pnp_file_ds.pdf", arrangement="double-sided")
#'         save_print_and_play(cfg, "my_pnp_file_A4.pdf", size="A4", pieces="all")
#'         save_print_and_play(cfg, "my_pnp_file_A5.pdf", size="A5")
#'         unlink("my_pnp_file.pdf")
#'         unlink("my_pnp_file_ds.pdf")
#'         unlink("my_pnp_file_A4.pdf")
#'         unlink("my_pnp_file_A5.pdf")
#'     }
#'   }
#' @export
save_print_and_play <- function(cfg = getOption("piecepackr.cfg", pp_cfg()),
                                output_filename="piecepack.pdf", size="letter",
                                pieces=c("piecepack", "matchsticks", "pyramids"),
                                arrangement="single-sided") {

    cfg <- as_pp_cfg(cfg)
    n_suits <- cfg$n_suits

    if (size == "letter") {
        xl <- inch(LETTER_HEIGHT / 2 - A5W / 2)
        xr <- inch(LETTER_HEIGHT / 2 + A5W / 2)
    } else if (size == "A4") {
        xl <- inch(A4_HEIGHT / 2 - A5W / 2)
        xr <- inch(A4_HEIGHT / 2 + A5W / 2)
    } else { # size is "A5"
        xl <- 0.5
        xr <- 0.5
    }
    if ("all" %in% pieces)
        pieces <- c("piecepack", "pyramids", "matchsticks", "subpack")

    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    dev_onefile(output_filename, cfg$fontfamily, size)

    vpl <- viewport(x=xl, width=inch(A5W), height=inch(A5H))
    vpr <- viewport(x=xr, width=inch(A5W), height=inch(A5H))
    gl <- list()
    pl <- list()

    if ("piecepack" %in% pieces || "subpack" %in% pieces) {
        psuits <- seq(cfg$n_suits+2L)
        lpgf <- lapply(psuits, a5_piecepack_grob, cfg=cfg, front=TRUE, arrangement=arrangement)
        lpgb <- lapply(psuits, a5_piecepack_grob, cfg=cfg, front=FALSE, arrangement=arrangement)
    }

    ## Front Matter
    gl <- gappend(gl, a5_title_grob(cfg, pieces))
    gl <- gappend(gl, a5_inst_grob(cfg, pieces))
    if (arrangement == "double-sided" && size != "A5") {
        gl <- gappend(gl, blank_grob)
        gl <- gappend(gl, blank_grob)
        pl[["Front Matter"]] <- 2
    } else {
        pl[["Front Matter"]] <- 1
    }

    if ("piecepack" %in% pieces) {
        n_pages <- n_suits
        if (is_odd(n_suits) && arrangement == "double-sided" && size != "A5")
            n_pages <- n_pages + 1
        for (suit in seq(n_pages)) {
            gl <- gappend(gl, lpgf[[suit]])
            gl <- gappend(gl, lpgb[[suit]])
        }
        pl$Piecepack <- n_pages
    }

    #### Fine-tune between pyramids, matchsticks, and misc.?
    #### Add misc, cards, dominoes accessories?
    if ("matchsticks" %in% pieces) {
        n_pages <- (n_suits-1)%/%4+1
        for (ii in seq(n_pages)) {
            ss <- seq(4*ii-3, 4*ii)
            if (arrangement == "double-sided" && size == "A5") {
                gl <- gappend(gl, a5_matchsticks_grob(ss, cfg, TRUE))
                gl <- gappend(gl, a5_matchsticks_grob(ss, cfg, FALSE))
                gl <- gappend(gl, a5_matchsticks_grob2(ss, cfg, TRUE))
                gl <- gappend(gl, a5_matchsticks_grob2(ss, cfg, FALSE))
            } else {
                gl <- gappend(gl, a5_matchsticks_grob(ss, cfg, TRUE))
                gl <- gappend(gl, a5_matchsticks_grob2(ss, cfg, TRUE))
                if (arrangement == "double-sided") {
                    gl <- gappend(gl, a5_matchsticks_grob2(ss, cfg, FALSE))
                    gl <- gappend(gl, a5_matchsticks_grob(ss, cfg, FALSE))
                }
            }
        }
        pl$Matchsticks <- n_pages
    }
    if ("pyramids" %in% pieces) {
        n_pages <- (n_suits-1)%/%4+1
        for (ii in seq(n_pages)) {
            ss <- seq(4*ii-3, 4*ii)
            s1 <- ss[1:2]
            s2 <- ss[3:4]
            if (arrangement == "double-sided" && size == "A5") {
                gl <- gappend(gl, a5_pyramids_grob(s1, cfg, TRUE))
                gl <- gappend(gl, blank_grob)
                gl <- gappend(gl, a5_pyramids_grob(s2, cfg, FALSE))
                gl <- gappend(gl, blank_grob)
            } else {
                gl <- gappend(gl, a5_pyramids_grob(s1, cfg, TRUE))
                gl <- gappend(gl, a5_pyramids_grob(s2, cfg, FALSE))
                if (arrangement == "double-sided") {
                    gl <- gappend(gl, blank_grob)
                    gl <- gappend(gl, blank_grob)
                }
            }
        }
        pl$Pyramids <- n_pages
    }
    if ("subpack" %in% pieces) {
        vpul <- viewport(x=0.25, y=0.75, width=inch(A5W/2), height=inch(A5H/2))
        vpur <- viewport(x=0.75, y=0.75, width=inch(A5W/2), height=inch(A5H/2))
        vpll <- viewport(x=0.25, y=0.25, width=inch(A5W/2), height=inch(A5H/2))
        vplr <- viewport(x=0.75, y=0.25, width=inch(A5W/2), height=inch(A5H/2))
        lsgf <- lapply(lpgf, as_picture, width=A5W, height=A5H)
        lsgb <- lapply(lpgb, as_picture, width=A5W, height=A5H)
        n_pages <- (n_suits-1)%/%4+1
        if (arrangement == "double-sided" && is_odd(n_pages) && size != "A5")
            n_pages <- n_pages + 1
        for (ii in seq(n_pages)) {
            ss <- seq(4*ii-3, 4*ii)
            ss[which(ss > cfg$n_suits+2L)] <- cfg$n_suits+2L
            gf <- grobTree(grobTree(lsgf[[ss[1]]], vp=vpul),
                     grobTree(lsgb[[ss[1]]], vp=vpur),
                     grobTree(lsgf[[ss[2]]], vp=vpll),
                     grobTree(lsgb[[ss[2]]], vp=vplr))
            gb <- grobTree(grobTree(lsgf[[ss[3]]], vp=vpul),
                     grobTree(lsgb[[ss[3]]], vp=vpur),
                     grobTree(lsgf[[ss[4]]], vp=vpll),
                     grobTree(lsgb[[ss[4]]], vp=vplr))
            gl <- gappend(gl, gf)
            gl <- gappend(gl, gb)
        }
        pl$Subpack <- n_pages
    }
    if (size == "A5") pl <- lapply(pl, function(x) 2*x)
    for (ii in seq(gl)) {
        if (is_odd(ii)) {
            grid.newpage()
            draw_a5_page(gl[[ii]], vpl)
        } else {
            if (size == "A5") grid.newpage()
            draw_a5_page(gl[[ii]], vpr)
        }
    }
    invisible(grDevices::dev.off())
    if (tools::file_ext(output_filename) == "pdf" && has_gs()) {
        add_pdf_metadata(output_filename, cfg, pl)
    }
    invisible(NULL)
}

a5_vp <- viewport(width=unit(A5W, "in"), height=unit(A5H, "in"))

#' @importFrom tibble tibble

gp_title <- gpar(fontsize=15, fontfamily="sans", fontface="bold")
gp_header <- gpar(fontsize=12, fontfamily="sans", fontface="bold")
gp_text <- gpar(fontsize=9, fontfamily="sans")

htg <- function(label, x, y, just="center", ...) {
    textGrob(label, x=inch(x), y=inch(y), just=just,
             gp=gp_header, ...)
}
tg <- function(label, x, y, just="center",...) {
    textGrob(label, x=inch(x), y=inch(y), just=just,
             gp=gp_text, ...)
}

a5_inst_grob <- function(cfg=pp_cfg(), pieces) {
    grob_title <- textGrob("Piece Anatomy", y=0.97, just="center", gp=gp_title, name="title")
    # Tile
    yt <- A5H-1.3
    xt <- 1.5
    df <- tibble(piece_side = c("tile_face", "tile_back"),
                         suit=1, x=xt+c(-0.5, 0.5), y=yt, width=1, height=1)
    grob_tile <- grobTree(htg("Tile", xt, yt+0.7),
                          tg("Face", xt-0.5, y=yt+0.6),
                          tg("Back", xt+0.5, y=yt+0.6),
                          pmap_piece(df, default.units="in", draw=FALSE,
                                     cfg=cfg,
                                     gp=gpar(cex=0.5, lex=0.5)))
                          # curveGrob(x1=xt[1]-0.8, y1=yt-1.1, x2=xt[2]+0.8, y2=yt-1.1,
                          #           curvature=0.2, square=FALSE, arrow=arrow(),
                          #           default.units="in"))
    # Coin
    cwr <- 0.75 / cfg$get_width("coin_face")
    cw <- 0.75
    yc <- yt - 0.5 - cw
    xc <- 0.5 + 2*cw
    df <- tibble(piece_side = rep(c("coin_back", "coin_face"), 2),
                         x=xc+c(1.7, 0.7,-0.7, -1.7)*cw,
                         y=rep(yc,4),
                         width=0.75, height=0.75,
                         angle=c(0,180,0,0))
    grob_coin <- grobTree(htg("Coin", xc, yc+0.5*cw+0.2),
                          tg("Back", xc+c(1.7, -0.7)*cw, yc+0.5*cw+0.1),
                          tg("Face", xc+c(0.7, -1.7)*cw, yc+0.5*cw+0.1),
                          pmap_piece(df, default.units="in",
                                     cfg=cfg, draw=FALSE, gp=gpar(lex=cwr, cex=cwr)),
                          textGrob("or", x=xc, y=yc, default.units="in"))
    # Saucer
    swr <- 0.75 / cfg$get_width("saucer_face")
    sw <- 0.75
    ys <- yt - 1.6 - sw
    xs <- 0.5 + 2*cw
    df <- tibble(piece_side = rep(c("saucer_back", "saucer_face"), 2),
                         x=xs+c(1.7, 0.7,-0.7, -1.7)*sw,
                         y=rep(ys,4),
                         width=0.75, height=0.75,
                         angle=c(0,180,0,0))
    grob_saucer <- grobTree(htg("(Pawn) Saucer", xs, ys+0.5*sw+0.25),
                            tg("Back", xs+c(1.7, -0.7)*sw, ys+0.5*sw+0.1),
                            tg("Face", xs+c(0.7, -1.7)*sw, ys+0.5*sw+0.1),
                            pmap_piece(df, default.units="in",
                                       cfg=cfg, draw=FALSE, gp=gpar(lex=swr, cex=swr)),
                          textGrob("or", x=xs, y=ys, default.units="in"))

    # Dice
    dwr <- 0.5 / cfg$get_width("die_face")
    dw <- 0.5
    yd <- 1.4
    d1 <- grobTree(piecepackDieGrob(1, cfg, arrangement="counter_down"),
                   vp=viewport(x=0.2, y=inch(yd),
                   width=inch(4*dw), height=inch(3*dw), gp=gpar(lex=dwr, cex=dwr)))
    d2 <- grobTree(piecepackDieGrob(1, cfg, arrangement="counter_up"),
                   vp=viewport(x=0.5, y=inch(yd),
                       width=inch(4*dw), height=inch(3*dw), gp=gpar(lex=dwr, cex=dwr)))
    d3 <- grobTree(piecepackDieGrob(1, cfg, arrangement="opposites_sum_to_5"),
                   vp=viewport(x=0.8, y=inch(yd),
                       width=inch(4*dw), height=inch(3*dw), gp=gpar(lex=dwr, cex=dwr)))
    grob_die <- grobTree(htg("Die", A5W/2, yd+1.2),
                         tg('"counter_down"\narrangement', 0.3*A5W, yd+0.9),
                         tg('"counter_up"\narrangement', 0.6*A5W, yd+0.9),
                         tg('"opposites_sum_to_5"\narrangement', 0.85*A5W, yd+0.9),
                         d1, d2, d3)
    # Pawn
    y_pawn <- 4.0
    grob_pawn <- grobTree(htg("Pawn", A5W-0.5, y_pawn+1.4),
                          tg("Back", A5W-0.85, y_pawn+0.5, rot=90),
                          tg("Face", A5W-0.85, y_pawn-0.5, rot=90),
                          tg("Stand", A5W-0.85, y_pawn-1.1, rot=90),
                          tg("Stand", A5W-0.85, y_pawn+1.0, rot=90),
                          pieceGrob("pawn_layout", 1, cfg=cfg,
                                     x=inch(A5W-0.5), y=inch(y_pawn)))
    # Belt
    yb <- yt - 2.0 - sw - 0.5
    xb <- 2
    grob_belt <- grobTree(htg("(Pawn) Belt (Face)", xb, yb+0.4),
                          pieceGrob("belt_face", x=xb, y=yb, cfg=cfg,
                                    default.units="in"))
    # Pyramids
    yy <- yt+0.10
    xy <- A5W-1.2
    df <- tibble(piece_side=c("pyramid_layout", "pyramid_top"),
                              suit=1, rank=1, x=xy+c(-0.5, 0.6), y=yy)
    grob_pyramid <- grobTree(htg("Pyramid", xy, yy+0.8),
                             htg("Top (View)", xy+0.7, yy+0.50),
                             tg("Face", xy+0.6, yy+0.33),
                             tg("Back", xy+0.6, yy-0.33),
                             tg("Left", xy+0.25, yy, rot=90),
                             tg("Right", xy+0.95, yy, rot=-90),
                             tg("Back", xy-0.0, yy, rot=-90),
                             tg("Face", xy-1.0, yy+0.60, rot=90),
                             tg("Face", xy-1.0, yy-0.60, rot=90),
                             tg("Right", xy-0.05, yy+0.45),
                             tg("Left", xy-0.05, yy-0.45),
                             pmap_piece(df, default.units="in",
                                        cfg=cfg, draw=FALSE))
    # Matchsticks
    df <- tibble(piece_side=c("matchstick_face", "matchstick_back"),
                 x=2.5, y=c(0.4, 0.2), rank=3, angle=90)
    grob_matchsticks <- grobTree(htg("Matchstick", 1, 0.3),
                                 tg("Face", 1.7, 0.4),
                                 tg("Back", 1.7, 0.2),
                                 pmap_piece(df, default.units="in",
                                            cfg=cfg, draw=FALSE))

    grobTree(grob_title, grob_tile, grob_coin, grob_saucer,
             grob_die, grob_pawn, grob_belt,
             grob_pyramid, grob_matchsticks,
             name="instructions", vp=a5_vp)
}

a5_title_grob <- function(cfg=pp_cfg(), pieces) {
    current_dev <- grDevices::dev.cur()
    cc_picture <- grImport2::readPicture(system.file("extdata/by-sa.svgz", package="piecepackr"))
    if (current_dev > 1) grDevices::dev.set(current_dev)

    # Title
    y_title <- unit(0.97, "npc")
    grob_title <- textGrob(cfg$title, y=y_title, just="center", gp=gp_title, name="title")

    # Description
    y_description <- y_title - grobHeight(grob_title) - unit(0.03, "npc")
    dtext <- paste(strwrap(cfg$description, 72), collapse="\n")
    grob_description <- textGrob(dtext, x=0.1, y=y_description, just=c(0,1),
                                  gp=gp_text, name="description")

    # License
    y_license <- y_description - grobHeight(grob_description) - unit(0.03, "npc")
    license <- paste(c("\u25cf This print-and-play layout was generated by piecepackr.",
                       "\thttps://github.com/trevorld/piecepackr",
                       "\u25cf It is licensed under a CC BY-SA 4.0 license.",
        "\thttps://creativecommons.org/licenses/by-sa/4.0"
                ), collapse="\n")
    grob_lh <- textGrob("License", x=0.1, y=y_license, just="left", gp=gp_header)
    grob_l <- textGrob(license, x=0.1, y=y_license-unit(0.02, "npc"), just=c(0,1), gp=gp_text)
    grob_cc <- grImport2::symbolsGrob(cc_picture, x=0.75, y=y_license-unit(0.065, "npc"), size=inch(0.9))

    grob_license <- grobTree(grob_lh, grob_l, grob_cc, name="license")

    # Copyright
    y_copyright <- y_license - grobHeight(grob_lh) - grobHeight(grob_l) - unit(0.03, "npc")
    copyright <- paste(c("\u00a9 2016-2019 Trevor L Davis. Some Rights Reserved.",
                         cfg$copyright),
                       collapse="\n")
    grob_ch <- textGrob("Copyright", x=0.1, y=y_copyright, just="left", gp=gp_header)
    grob_c <-  textGrob(copyright, x=0.1, y=y_copyright-unit(0.02, "npc"), just=c(0,1), gp=gp_text)
    grob_copyright <- grobTree(grob_ch, grob_c, name="copyright")

    # Credits
    y_credits <- y_copyright - grobHeight(grob_ch) - grobHeight(grob_c) - unit(0.03, "npc")
    credits <- paste(c('\u25cf The piecepack was invented by James "Kyle" Droscha. Public Domain.',
                       "\thttps://web.archive.org/web/2018/http://www.piecepack.org/Anatomy.html",
                       # "\thttp://www.piecepack.org/Anatomy.html",
                       "\u25cf Piecepack pyramids were invented by Tim Schutz. Public Domain.",
                       "\thttp://www.ludism.org/ppwiki/PiecepackPyramids",
                       "\u25cf Pawn saucers were invented by Karol M. Boyle. Public Domain.",
                       "\thttps://web.archive.org/web/2018/http://www.piecepack.org/Accessories.html",
                       # "\thttp://www.piecepack.org/Accessories.html",
                       "\u25cf Piecepack matchsticks were invented by Dan Burkey. Public Domain.",
                       "\thttp://www.ludism.org/ppwiki/PiecepackMatchsticks",
                       cfg$credit),
                     collapse="\n")
    grob_credits <- gTree(name="credits", children=gList(
        textGrob("Credits", x=0.1, y=y_credits, just="left", gp=gp_header),
        textGrob(credits, x=0.1, y=y_credits-unit(0.02, "npc"), just=c(0,1), gp=gp_text)
    ))

    grobTree(grob_title, grob_description, grob_license, grob_copyright,
             grob_credits, name="title_page", vp=a5_vp)
}

blank_grob <- textGrob("Intentionally left blank")

draw_a5_page <- function(grob, vp) {
    pushViewport(vp)
    grid.draw(grob)
    upViewport()
}

a5_matchsticks_grob <- function(suits=1:4, cfg=pp_cfg(), front=TRUE) {

    n_suits <- length(suits)
    y1t <- A5H - 0.5* MATCHSTICK_HEIGHTS[1]
    y1b <- A5H - 1.5* MATCHSTICK_HEIGHTS[1]
    x1s <- (0.5 + 0:(3*n_suits-1)) * MATCHSTICK_WIDTHS[1]
    xs <-  (0.5 + 0:(6*n_suits-1)) * MATCHSTICK_WIDTHS[2]
    y2  <- y1b - 0.5*MATCHSTICK_HEIGHTS[1] - 0.5*MATCHSTICK_HEIGHTS[2]
    y3  <- y2  - 0.5*MATCHSTICK_HEIGHTS[2] - 0.5*MATCHSTICK_HEIGHTS[3]
    y5  <- y3  - 0.5*MATCHSTICK_HEIGHTS[3] - 0.5*MATCHSTICK_HEIGHTS[5]
    y6  <- y5  - 0.5*MATCHSTICK_HEIGHTS[5] - 0.5*MATCHSTICK_HEIGHTS[6]

    x <- c(rep(x1s,each=2), rep(xs, 4))
    y <- c(rep(c(y1t, y1b), 3*n_suits), rep(c(y2, y3, y5, y6), each=6*n_suits))
    suit <- rep(rep(suits, each=6), 5)
    rank <- rep(c(1:3,5:6), each=6*n_suits)
    if (front) {
        piece_side <- "matchstick_face"
    } else {
        x <- A5W - x
        piece_side <- "matchstick_back"

    }
    df <- tibble(piece_side, x, y, suit, rank)

    pmap_piece(df, cfg=cfg, default.units="inches", draw=FALSE)
}

a5_matchsticks_grob2 <- function(suits=1:4, cfg=pp_cfg(), front=TRUE) {

    n_suits <- length(suits)
    y4s <- A5H - (0.5 + 0:3)*MATCHSTICK_HEIGHTS[4]
    x4s <- (0.5 + 0:5) * MATCHSTICK_WIDTHS[6]

    x <- rep(x4s, 4)
    y <- rep(c(y4s), each=6)
    suit <- rep(suits, each=6)
    rank <- 4
    if (front) {
        piece_side <- "matchstick_face"
    } else {
        x <- A5W - x
        piece_side <- "matchstick_back"

    }
    df <- tibble(piece_side, x, y, suit, rank)

    # add some extra dice if there is some space
    if (cfg$get_width("die_face") <= 0.6) {
        xd1 <- max(x4s) + 0.0 + c(0.6)*cfg$get_width("die_layoutLF")
        dfd <- tibble::tibble(piece_side="die_layoutLF",
                              x=xd1, y=unique(y),
                              suit=suits, rank=NA)
        df <- rbind(df, dfd)
    }
    if (cfg$get_width("die_face") <= 0.55) {
        xd2 <- xd1 + c(0.7)*cfg$get_width("die_layoutLF")
        dfd <- tibble::tibble(piece_side="die_layoutLF",
                              x=xd2, y=unique(y),
                              suit=suits, rank=NA)
        df <- rbind(df, dfd)
    }

    pmap_piece(df, cfg=cfg, default.units="inches", draw=FALSE)
}

a5_piecepack_grob <- function(suit, cfg=pp_cfg(), front=TRUE, arrangement="single-sided") {
    die_width <- cfg$get_width("die_width")
    pawn_width <- cfg$get_width("pawn_width")
    tile_width <- cfg$get_width("tile_back")
    xtl <- 1.5 * tile_width
    xtr <- 0.5 * tile_width
    ytb <- 0.5 * tile_width
    ytm <- 1.5 * tile_width
    ytt <- 2.5 * tile_width
    xc <-A5W - 0.25 * tile_width
    ycs <- rev((0.50 + seq(0, 5)) * 0.8)
    xdr <- c(0.5,1.5,2.5) * die_width
    ydt <- A5H - 0.5 * die_width
    ydb <- A5H - 1.5 * die_width
    xp <- A5W - 0.5 * cfg$get_height("pawn_layout")
    yp <- A5H - 0.5 * pawn_width
    xb <- A5W - 0.5 * cfg$get_width("belt_face")
    yb <- A5H - pawn_width - 0.5 * cfg$get_height("belt_face") - 0.25
    xsr <- A5W - 0.25 * tile_width
    ysb <- 2.75 * tile_width
    dft <- tibble(piece_side="tile_face", x=rep(c(xtr,xtl),3),
                  y=rep(c(ytt,ytm,ytb),each=2),
                  suit, rank=1:6, angle=0)
    dfc <- tibble(piece_side="coin_back", x=rep(xc,6),
                  y=ycs, suit, rank=1:6,
                  angle=ifelse(front, cfg$coin_arrangement, 0))
    dfd <- tibble(piece_side="die_face", x=rep(xdr,2),
                  y=rep(c(ydt,ydb),each=3),
                  suit, rank=1:6, angle=0)
    dfp <- tibble(piece_side="pawn_layout", x=xp,y=yp,
                  suit, rank=NA, angle=90)
    dfb <- tibble(piece_side="belt_face", x=xb, y=yb,
                  suit, rank=NA, angle=0)
    dfs <- tibble(piece_side="saucer_face", x=xsr, y=ysb,
                  suit, rank=NA,
                  angle=ifelse(front, 0, cfg$coin_arrangement))
    df <- rbind(dfc, dfd, dfp, dfb, dfs, dft)
    if (!front) {
        df$x <- A5W - df$x
        df$piece_side <- c(rep("coin_face", 6), rep("die_face", 6),
                           "pawn_layout", "belt_face", "saucer_back", rep("tile_back", 6))
        if (arrangement == "double-sided") {
            df <- df[df$piece_side %in% c("tile_back", "coin_face", "saucer_back"), ]
        }
    }
    pmap_piece(df, cfg=cfg, default.units="inches", draw=FALSE)
}

a5_pyramids_grob <- function(suit=1:2, cfg=pp_cfg(), front=TRUE) {
    grobTree(pyramid_grob_helper(suit[1], cfg, 0),
            pyramid_grob_helper(suit[2], cfg, A5W/2))
}

pyramid_grob_helper <- function(suit, cfg=pp_cfg(), xleft=0) {
    rank <- c(2,4,5,1,3, 6)
    plh <- PYRAMID_LAYOUT_HEIGHTS[rank]
    y_up <- cumsum(plh) - 0.5*plh
    x_up <- 0.5*PYRAMID_LAYOUT_WIDTHS[rank]
    x <- xleft+c(x_up[1:3], A5W/2 - x_up[4:6])
    y <- c(y_up[1:3], c(plh[5] + plh[6] + 0.5*plh[4], plh[6]+0.5*plh[5], 0.5*plh[6]))
    piece_side <- "pyramid_layout"
    angle <- c(rep(0, 3), rep(180,3))
    df <- tibble::tibble(piece_side, x, y, suit, rank, angle)
    # space for a pawn layout on top
    dfp <- tibble::tibble(piece_side="pawn_layout",
                          x = xleft + 0.25 * A5W, y = A5H - 0.5 * cfg$get_width("pawn_layout"),
                          suit, rank=1, angle=90)
    df <- rbind(dfp, df)
    pmap_piece(df, cfg=cfg, default.units="inches", draw=FALSE)
}

#' @importFrom utils packageDescription
add_pdf_metadata <- function(output_filename, cfg=pp_cfg(), pl=list()) {
    temp_pdf <- tempfile(fileext=".pdf")
    on.exit(unlink(temp_pdf))
    temp_txt <- tempfile(fileext=".txt")
    on.exit(unlink(temp_txt))
    file.copy(output_filename, temp_pdf)

    starting_pages <- c(1, 1+cumsum(sapply(pl, identity)))
    ns <- names(pl)
    txt <- character(0)
    for (ii in seq(pl)) {
        line <- sprintf("[/Page %s /View [/XYZ null null null] /Title (%s) /OUT pdfmark",
                   starting_pages[ii], ns[ii])
        txt <- append(txt, line)
    }
    title <- sprintf(" /Title (%s)\n", cfg$title)
    creator <- sprintf(" /Creator (piecepackr v%s)\n", packageDescription("piecepackr")$Version)
    subject <- sprintf(" /Subject (%s)\n", cfg$description)
    keywords <- " /Keywords (piecepack)\n"
    line <- sprintf("[%s%s%s%s /DOCINFO pdfmark",
                    ifelse(length(title), title, ""), creator,
                    ifelse(length(subject), subject, ""), keywords)
    txt <- append(txt, line)
    writeLines(txt, temp_txt)

    args <- c("-q", "-o", shQuote(output_filename), "-sDEVICE=pdfwrite", shQuote(temp_txt), shQuote(temp_pdf))
    system2(gs(), args)
}
