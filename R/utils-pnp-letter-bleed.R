print_and_play_paper_bleed <- function(cfg, size, pieces, arrangement, quietly) {
    n_suits <- cfg$n_suits
    n_ranks <- cfg$n_ranks

    stopifnot(n_ranks <= 6)
    if ("matchsticks" %in% pieces)
        abort('"matchsticks" `pieces` not currently supported for `bleed = TRUE`')
    if ("pyramids" %in% pieces)
        abort('"pyramids" `pieces` not currently supported for `bleed = TRUE`')
    if ("subpack" %in% pieces)
        abort('"subpack" `pieces` not currently supported for `bleed = TRUE`')

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

    vpl <- viewport(x=xl, width=inch(A5W), height=inch(A5H))
    vpr <- viewport(x=xr, width=inch(A5W), height=inch(A5H))
    gl <- list()
    pl <- list()

    ## Front Matter
    gl <- gappend(gl, a5_title_grob(cfg, pieces, quietly))
    gl <- gappend(gl, a5_inst_grob_bleed(cfg, pieces))
    if (arrangement == "double-sided" && size != "A5") {
        gl <- gappend(gl, blank_grob)
        gl <- gappend(gl, blank_grob)
        pl[["Front Matter"]] <- 2
    } else {
        pl[["Front Matter"]] <- 1
    }

    ## Piecepack
    if ("piecepack" %in% pieces) {
        n_pages_tiles <- n_suits
        if (is_odd(n_pages_tiles) && arrangement == "double-sided" && size != "A5")
            n_pages_tiles <- n_pages_tiles + 1
        for (suit in seq(n_pages_tiles)) {
            gl <- gappend(gl, a5_tile_grob(suit, cfg, TRUE, arrangement))
            gl <- gappend(gl, a5_tile_grob(suit, cfg, FALSE, arrangement))
        }
        pl$Tiles <- n_pages_tiles

        n_pages_coins <- (n_suits-1)%/%4+1
        for (ii in seq(n_pages_coins)) {
            ss <- seq(4*ii-3, 4*ii)
            gl <- gappend(gl, a5_coin_grob(ss, cfg, TRUE, arrangement))
            gl <- gappend(gl, a5_coin_grob(ss, cfg, FALSE, arrangement))
        }
        if (is_odd(n_pages_coins) && arrangement == "double-sided" && size != "A5") {
            n_pages_coins <- n_pages_coins + 1
            ss <- seq(4*n_pages_coins-3, 4*n_pages_coins)
            gl <- gappend(gl, a5_coin_grob(ss, cfg, TRUE, arrangement))
            gl <- gappend(gl, a5_coin_grob(ss, cfg, FALSE, arrangement))
        }
        pl$Coins <- n_pages_coins

        n_pages_dice <- (n_suits-1)%/%4+1
        for (ii in seq(n_pages_dice)) {
            ss <- seq(4*ii-3, 4*ii)
            if (arrangement == "double-sided" && size == "A5") {
                gl <- gappend(gl, a5_die_grob(ss, cfg, TRUE, arrangement))
                gl <- gappend(gl, blank_grob)
                gl <- gappend(gl, a5_pawn_grob(ss, cfg, TRUE, arrangement))
                gl <- gappend(gl, blank_grob)
            } else {
                gl <- gappend(gl, a5_die_grob(ss, cfg, TRUE, arrangement))
                gl <- gappend(gl, a5_pawn_grob(ss, cfg, FALSE, arrangement))
                if (arrangement == "double-sided") {
                    gl <- gappend(gl, blank_grob)
                    gl <- gappend(gl, blank_grob)
                }
            }
        }
        if (arrangement == "double-sided")
            n_pages_dice <- 2 * n_pages_dice
        pl$`Dice and Pawns` <- n_pages_dice
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

    pl
}

a5_inst_grob_bleed <- function(cfg, pieces) {
    textGrob("Bleed instructions")
}

a5_tile_grob <- function(i_suit, cfg, front, arrangement) {

    tile_width <- cfg$get_width("tile_back")
    xtr <- 0.5 * tile_width + 1/8 + 1/8
    xtl <- xtr + tile_width + 1/4

    ytb <- 0.5 * tile_width + 1/8 + 1/8
    ytm <- ytb + tile_width + 1/4
    ytt <- ytm + tile_width + 1/4

    df <- tibble::tibble(piece_side = "tile_face",
                         x=rep(c(xtr,xtl),3),
                         y=rep(c(ytt,ytm,ytb),each=2),
                         suit = i_suit,
                         rank = 1:6)

    if (!front) {
        df$piece_side = "tile_back"
        df$x <- A5W - df$x
        # Rotate tile backs to partially hide direction of face if tile back are not fully symmetric
        df$angle <- 90 * ((df$suit + df$rank) %% 4)
    }
    #### A5 size
    if (!front) {
        vline <- linesGrob(x = 0, gp=gpar(lty = "dashed"))
    } else {
        vline <- nullGrob()
    }

    cm_grob <- pmap_piece(df, cropmarkGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    ps_grob <- pmap_piece(df, pieceGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    gList(cm_grob, ps_grob, vline)
}

a5_coin_grob <- function(suit, cfg, front, arrangement) {
    coin_width <- cfg$get_width("coin_back")
    xt1 <- 0.5 * coin_width + 1/8 + 1/8
    xt2 <- xt1 + coin_width + 1/4 + 1/8
    xt3 <- xt2 + coin_width + 1/4 + 1/8
    xt4 <- xt3 + coin_width + 1/4 + 1/8

    yt1 <- 0.5 * coin_width + 1/8 + 1/8
    yt2 <- yt1 + coin_width + 1/4 + 1/8
    yt3 <- yt2 + coin_width + 1/4 + 1/8
    yt4 <- yt3 + coin_width + 1/4 + 1/8
    yt5 <- yt4 + coin_width + 1/4 + 1/8
    yt6 <- yt5 + coin_width + 1/4 + 1/8

    df <- tibble::tibble(piece_side = "coin_back",
                         x=rep(c(xt1,xt2,xt3,xt4),each=6),
                         y=rep(c(yt1,yt2,yt3,yt4,yt5,yt6),4),
                         suit = rep(suit, each=6),
                         rank = rep(1:6, 4))

    if (!front) {
        df$piece_side = "coin_face"
        df$x <- A5W - df$x
    } else {
        # Rotate coin faces to desired direction
        df$angle <- cfg$coin_arrangement
    }
    #### A5 size
    if (!front) {
        vline <- linesGrob(x = 0, gp=gpar(lty = "dashed"))
    } else {
        vline <- nullGrob()
    }

    cm_grob <- pmap_piece(df, cropmarkGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    ps_grob <- pmap_piece(df, pieceGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    gList(cm_grob, ps_grob, vline)
}

a5_die_grob <- function(suit, cfg, front, arrangement) {
    die_width <- cfg$get_width("die_face")
    xt1 <- 0.5 * 3/4 + 1/8 + 1/8
    xt2 <- xt1 + 3/4 + 1/4 + 1/8
    xt3 <- xt2 + 3/4 + 1/4 + 1/8
    xt4 <- xt3 + 3/4 + 1/4 + 1/8

    yt1 <- 0.5 * 3/4 + 1/8 + 1/8
    yt2 <- yt1 + 3/4 + 1/4 + 1/8
    yt3 <- yt2 + 3/4 + 1/4 + 1/8
    yt4 <- yt3 + 3/4 + 1/4 + 1/8
    yt5 <- yt4 + 3/4 + 1/4 + 1/8
    yt6 <- yt5 + 3/4 + 1/4 + 1/8

    df <- tibble::tibble(piece_side = "die_face",
                         x=rep(c(xt1,xt2,xt3,xt4),each=6),
                         y=rep(c(yt1,yt2,yt3,yt4,yt5,yt6),4),
                         suit = rep(suit, each=6),
                         rank = rep(1:6, 4))

    if (!front) {
        df$x <- A5W - df$x
    }

    cm_grob1 <- pmap_piece(df, cropmarkGrob, cfg = cfg, default.units = "in",
                           width = die_width, height = die_width,
                           bleed = (3/4-die_width)/2 + 1/8, draw=FALSE)
    cm_grob2 <- pmap_piece(df, cropmarkGrob, cfg = cfg, default.units = "in",
                           width = 3/4, height = 3/4,
                           bleed = 1/8, draw=FALSE)
    ps_grob <- pmap_piece(df, pieceGrob, cfg = cfg, default.units = "in",
                          bleed = (3/4-die_width)/2 + 1/8, draw=FALSE)
    gList(cm_grob1, cm_grob2, ps_grob)
}

a5_pawn_grob <- function(suit, cfg, front, arrangement) {

    belt_width <- cfg$get_width("belt_face")
    pawn_height <- cfg$get_height("pawn_layout")

    x <- A5W * (1:4/ 4 - 1/8)
    yb <- A5H - (0.5 * belt_width + 1/8 + 1/8)

    yp <- 0.5 * pawn_height + 1/8 + 1/8

    ys <- 0.5 * A5H

    df_b <- tibble::tibble(piece_side = "belt_face",
                           x = x, suit = suit, y = yb, angle = 90)

    df_s <- tibble::tibble(piece_side = "saucer_face",
                           x = x, suit = suit, y = ys, angle = 0)

    df_p <- tibble::tibble(piece_side = "pawn_layout",
                           x = x, suit = suit, y = yp, angle = 0)

    df <- rbind(df_b, df_s, df_p)

    if (!front)
        df$x <- A5W - df$x

    cm_grob <- pmap_piece(df, cropmarkGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    ps_grob <- pmap_piece(df, pieceGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    gList(cm_grob, ps_grob)
}
