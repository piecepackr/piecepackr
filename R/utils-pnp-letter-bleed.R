print_and_play_paper_bleed <- function(cfg, size, pieces, arrangement, quietly, size_bleed) {
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
        xl <- inch(LETTER_HEIGHT / 2 - A5W / 2 + size_bleed$left)
        xr <- inch(LETTER_HEIGHT / 2 + A5W / 2 + size_bleed$left)
        y <- inch(LETTER_WIDTH / 2 + size_bleed$bottom)
    } else if (size == "A4") {
        xl <- inch(A4_HEIGHT / 2 - A5W / 2 + size_bleed$left)
        xr <- inch(A4_HEIGHT / 2 + A5W / 2 + size_bleed$left)
        y <- inch(A4_WIDTH / 2 + size_bleed$bottom)
    } else { # size is "A5"
        xl <- inch(A5W / 2 + size_bleed$left)
        xr <- inch(A5W / 2 + size_bleed$left)
        y <- inch(A5H / 2 + size_bleed$bottom)
    }

    vpl <- viewport(x=xl, y=y, width=inch(A5W), height=inch(A5H))
    vpr <- viewport(x=xr, y=y, width=inch(A5W), height=inch(A5H))
    gl <- list()
    pl <- list()

    ## Front Matter
    gl <- gappend(gl, a5_title_grob(cfg, pieces, quietly, bleed=TRUE))
    gl <- gappend(gl, a5_inst_grob_bleed(cfg, pieces, arrangement, size))
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
            gl <- gappend(gl, a5_tile_grob(suit, cfg, TRUE, arrangement, size))
            gl <- gappend(gl, a5_tile_grob(suit, cfg, FALSE, arrangement, size))
        }
        pl$Tiles <- n_pages_tiles

        n_pages_coins <- (n_suits-1)%/%4+1
        for (ii in seq(n_pages_coins)) {
            ss <- seq(4*ii-3, 4*ii)
            gl <- gappend(gl, a5_coin_grob(ss, cfg, TRUE, arrangement, size))
            gl <- gappend(gl, a5_coin_grob(ss, cfg, FALSE, arrangement, size))
        }
        if (is_odd(n_pages_coins) && arrangement == "double-sided" && size != "A5") {
            n_pages_coins <- n_pages_coins + 1
            ss <- 1:4
            gl <- gappend(gl, a5_coin_grob(ss, cfg, TRUE, arrangement, size))
            gl <- gappend(gl, a5_coin_grob(ss, cfg, FALSE, arrangement, size))
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

a5_inst_grob_bleed <- function(cfg, pieces, arrangement, size) {
    y_inst <- unit(1, "npc") - unit(0.2, "in")
    inst <- c("\u25cf See https://www.ludism.org/ppwiki/MakingPiecepacks for general advice")

    # Settings
    components <- paste(paste0('"', pieces, '"'), collapse=", ")
    if (TRUE)
        arrangement <- sprintf('\t\u25cb "%s" arrangement with "bleed" zones', arrangement)
    inst <- c(inst,
              "\u25cf This print-and-play layout was generated for:",
              sprintf('\t\u25cb %s components', components),
              arrangement,
              sprintf('\t\u25cb "%s" paper size', size))

    # Tiles
    tile_crop <- c('\t\u25cb Use "crop" marks as guide to where to cut them out')
    if (size == "a5")
        inst <- c(inst,
                  '\u25cf For each suit we have a page of tile "faces" then a page of tile "backs"',
                  tile_crop,
                  '\t\u25cb May place whole sheet on one side and next sheet on the opposite side',
                  '\t\t of target material before cutting out')
    else
        inst <- c(inst,
                  '\u25cf We have a page of tiles for each suit: "faces" on left and "backs" on right',
                  tile_crop,
                  '\t\u25cb Central "gutter" line may help place sheet on both sides of target material',
                  '\t\t  Line up "gutter" line on the center of edge of target material, fold,',
                  '\t\t  and cut tiles out.',
                  '\t\u25cb Alternatively place whole sheet on one side and another on the opposite side')

    # Coins
    coin_crop <- c('\t\u25cb For those lacking circular cutting tools one may use "crop" marks',
                  '\t\t as guide to cut out (inferior) square coins',
                  '\t\u25cb Otherwise use "crop" marks to help center placement of circular cutting tool')
    if (size == "a5")
        inst <- c(inst,
                  "\u25cf We have a page of coin faces then a page of coin backs (per 4 suits)",
                  coin_crop)
    else
        inst <- c(inst,
                  '\u25cf We have a page of coins (per 4 suits): "backs" on left and "faces" on right',
                  coin_crop,
                  '\t\u25cb Central "gutter" line may help place sheet on both sides of target material',
                  '\t\t  Line up "gutter" line on the center of edge of target material, fold,',
                  '\t\t  and cut coins out.',
                  '\t\u25cb Alternatively place whole sheet on one side and a copy on the opposite side')

    # Dice and Pawns
    dice_crop <- c('\t\u25cb Depending on size of target dice use inner or outer "crop" marks',
                   '\t\t as guide to cut out dice',
                   '\t\u25cb Alternatively use "crop" marks to help center placement of circular cutting tool',
                   '\t\u25cb Use "crop" marks to cut out pawn "belts"',
                   '\t\u25cb Use "crop" marks to cut out pawn "tokens" (depending on shape of token)',
                   '\t\t "gutter" line can be used to help place pawn "tokens" on both sides',
                   '\t\t  of target material ("gutter" line on center of edge of target material)')
    if (size == "a5")
        inst <- c(inst,
                  "\u25cf We have a page of dice faces then a page of pawn belts and pawn tokens (per 4 suits)",
                  dice_crop)
    else
        inst <- c(inst,
                  '\u25cf We have a page of dice faces, pawn belts, and pawn tokens (per 4 suits):',
                  '\t\u25cb Dice faces on left, pawn "belts" on top right, pawn "tokens" on bottom right',
                  dice_crop)

    inst <- paste(inst, collapse="\n")

    gTree(name="instructions", children=gList(
        # rectGrob(gp=gpar(fill="grey90")),
        textGrob("Instructions", x=unit(0.5, "cm"), y=y_inst, just="left", gp=gp_header),
        textGrob(inst, x=unit(0.5, "cm"), y=y_inst-unit(0.2, "in"), just=c(0,1), gp=gp_text)
    ))
}

a5_tile_grob <- function(i_suit, cfg, front, arrangement, size) {

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
    if (!front && size != "A5") {
        vline <- linesGrob(x = 0, gp=gpar(lty = "dashed"))
    } else {
        vline <- nullGrob()
    }

    cm_grob <- pmap_piece(df, cropmarkGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    ps_grob <- pmap_piece(df, pieceGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    gList(cm_grob, ps_grob, vline)
}

a5_coin_grob <- function(suit, cfg, front, arrangement, size) {
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
    if (!front && size != "A5") {
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
    pawn_height <- cfg$get_height("pawn_face")

    x <- A5W * (1:4/ 4 - 1/8)
    yb <- A5H - (0.5 * belt_width + 1/8 + 1/8)


    ys <- 0.5 * A5H

    df_b <- tibble::tibble(piece_side = "belt_face",
                           x = x, suit = suit, y = yb, angle = 90)

    ypb <- 0.5 * pawn_height + 1/8 + 1/8
    ypt <- (ypb + 0.5 * pawn_height + 1/8) + 1/2 + (1/8 + 0.5 * pawn_height) # 1/2" gutter = jumbo domino


    if (!front) {
        df_pb <- tibble::tibble(piece_side = "pawn_back",
                                x = x, suit = suit, y = ypb, angle = 180)
        df_pt <- tibble::tibble(piece_side = "pawn_face",
                                x = x, suit = suit, y = ypt, angle = 0)

        # df_s <- tibble::tibble(piece_side = "saucer_face",
        #                        x = x, suit = suit, y = ys, angle = 0)
        df <- rbind(df_b, df_pb, df_pt)

        df$x <- A5W - df$x

    } else {
        df_pb <- tibble::tibble(piece_side = "pawn_face",
                                x = x, suit = suit, y = ypb, angle = 180)
        df_pt <- tibble::tibble(piece_side = "pawn_back",
                                x = x, suit = suit, y = ypt, angle = 0)

        # df_s <- tibble::tibble(piece_side = "saucer_back",
        #                        x = x, suit = suit, y = ys, angle = 0)
        df <- rbind(df_b, df_pb, df_pt)
    }

    hline <- linesGrob(y = inch(0.5 * (ypb + ypt)), gp=gpar(lty = "dashed"))


    cm_grob <- pmap_piece(df, cropmarkGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    ps_grob <- pmap_piece(df, pieceGrob, cfg = cfg, default.units = "in", bleed = TRUE, draw=FALSE)
    gList(cm_grob, ps_grob, hline)
}
