cc_file <- pictureGrob(readPicture(system.file("extdata/by-sa-svg.svg", package="piecepack")))

is_odd <- function(x) { as.logical(x %% 2) }

inch <- function(x) { unit(x, "in") }

make_deck_header <- function(arg) {
    make_header_helper(arg$deck_title, arg)
}

make_die_viewports <- function(label, flip=FALSE) {
    if (flip) {
        addViewport(x=4/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle= 90, name=paste0(label, ".die.1"))
        addViewport(x=3/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0(label, ".die.2"))
        addViewport(x=3/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle= 90, name=paste0(label, ".die.3"))
        addViewport(x=2/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0(label, ".die.4"))
        addViewport(x=2/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle= 90, name=paste0(label, ".die.5"))
        addViewport(x=1/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0(label, ".die.6"))
    } else {
        addViewport(x=1/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=-90, name=paste0(label, ".die.1"))
        addViewport(x=2/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0(label, ".die.2"))
        addViewport(x=2/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=-90, name=paste0(label, ".die.3"))
        addViewport(x=3/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0(label, ".die.4"))
        addViewport(x=3/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=-90, name=paste0(label, ".die.5"))
        addViewport(x=4/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0(label, ".die.6"))
    }
}

make_4by3_viewports <- function(label) {
    for (i_row in 1:3) {
        i_l_rank <- 2 * (i_row-1) + 1
        i_r_rank <- 2 * (i_row-1) + 2
        addViewport(y=(4-i_row)/3-1/6, height=1/3, name=paste0(label, ".row.", i_row))
        downViewport(paste0(label, ".row.", i_row))
        addViewport(x=0.125, width=0.25, name=paste0(label, ".face.", i_l_rank))
        addViewport(x=0.375, width=0.25, name=paste0(label, ".back.", i_l_rank))
        addViewport(x=0.625, width=0.25, name=paste0(label, ".face.", i_r_rank))
        addViewport(x=0.875, width=0.25, name=paste0(label, ".back.", i_r_rank))
        upViewport()
    }
}

make_coinrow_viewports <- function(label) {
    for (i_r in 1:6) {
        addViewport(x=(2*i_r-1)/12-1/24, width=1/12, name=paste0(label, ".back.", i_r))
        addViewport(x=(2*i_r)/12-1/24, width=1/12, name=paste0(label, ".face.", i_r))
    }
}

draw_coin_row <- function(i_s, opts) {
    label <- stringi::stri_rand_strings(n=1, length=4)
    make_coinrow_viewports(label, flip=flip)
    for(i_r in 1:6) {
        seekViewport(paste0(label, ".back.", i_r))
        draw_coin_back(i_s, opts)
        seekViewport(paste0(label, ".face.", i_r))
        draw_coin_face(i_r, opts)
    }
}

draw_coin_4by3 <- function(i_s, opts) {
    label <- stringi::stri_rand_strings(n=1, length=4)
    make_4by3_viewports(label)
    for(i_r in 1:6) {
        seekViewport(paste0(label, ".back.", i_r))
        draw_coin_back(i_s, opts)
        seekViewport(paste0(label, ".face.", i_r))
        draw_coin_face(i_r, opts)
    }
}

draw_piecepack_die <- function(i_s, opts, flip=FALSE) {
    suppressWarnings({
        # label <- stringi::stri_rand_strings(n=1, length=4)
        label <- "piecepack"
        make_die_viewports(label, flip=flip)
        for(i_r in 1:6) {
            downViewport(paste0(label, ".die.", i_r))
            draw_piecepack_die_face(i_s, i_r, opts)
            upViewport()
        }
    })
}

draw_suit_die <- function(opts, flip=FALSE) {
    suppressWarnings({
        # label <- stringi::stri_rand_strings(n=1, length=4)
        label <- "suit"
        make_die_viewports(label, flip=flip)
        if (opts$n_suits == 4) {
            downViewport(paste0(label, ".die.1"))
            draw_suit_die_face(6, opts)
            upViewport()
            for (i_s in 1:4) {
                downViewport(paste0(label, ".die.", i_s+1))
                draw_suit_die_face(5-i_s, opts)
                upViewport()
            }
            downViewport(paste0(label, ".die.6"))
            draw_suit_die_face(5, opts)
            upViewport()
        } else if (opts$n_suits == 5) {
            for (i_s in 1:5) {
                downViewport(paste0(label, ".die.", i_s))
                draw_suit_die_face(6-i_s, opts)
                upViewport()
            }
            downViewport(paste0(label, ".die.6"))
            draw_suit_die_face(6, opts)
            upViewport()
        } else if (opts$n_suits == 6) {
            for (i_s in 1:6) {
                downViewport(paste0(label, ".die.", i_s))
                draw_suit_die_face(7-i_s, opts)
                upViewport()
            }
        } else {
            stop(paste("Don't know how to draw suit die for", opts$n_suits, "suits"))
        }
    })
}

draw_rank_die <- function(opts, flip=FALSE) {
    suppressWarnings({
        label <- stringi::stri_rand_strings(n=1, length=4)
        make_die_viewports(label, flip=flip)
        for (i_r in 1:6) {
            seekViewport(paste0(label, ".die.", i_r))
            draw_piecepack_die_face(opts$i_unsuit + 1, i_r, opts)
        }
    })
}

draw_suitrank_die <- function(opts, flip=FALSE) {
    suppressWarnings({
        # label <- stringi::stri_rand_strings(n=1, length=8)
        label <- "suitrank"
        make_die_viewports(label, flip=flip)
        if (opts$n_suits == 4) {
            downViewport(paste0(label, ".die.1"))
            draw_piecepack_die_face(6, 1, opts)
            upViewport()
            downViewport(paste0(label, ".die.2"))
            draw_piecepack_die_face(5, 2, opts)
            upViewport()
            for (i_r in 3:6) {
                downViewport(paste0(label, ".die.", i_r))
                draw_piecepack_die_face(5-(i_r-2), i_r, opts)
                upViewport()
            }
        } else if (opts$n_suits == 5) {
            downViewport(paste0(label, ".die.6"))
            draw_piecepack_die_face(6, 6, opts)
            upViewport()
            for (i_r in 1:5) {
                downViewport(paste0(label, ".die.", i_r))
                draw_piecepack_die_face(6-i_r, i_r, opts)
                upViewport()
            }
        } else if (opts$n_suits == 6) {
            for (i_r in 1:6) {
                downViewport(paste0(label, ".die.", i_r))
                draw_piecepack_die_face(7-i_r, i_r, opts)
                upViewport()
            }
        } else {
            stop(paste("Don't know how to draw suit/rank die for", opts$n_suits, "suits"))
        }
    })
}

make_header_helper <- function(title, arg) {
    header_height <- 0.8
    y_header <- win_height - header_height/2
    addViewport(y=inch(y_header), width=inch(6.0), height=inch(header_height), name="header")
    seekViewport("header")
    width_image = 0.14
    addViewport(y=0.85, height=0.2, name="title")
    addViewport(x=width_image/2, y=0.4, width=width_image, height=0.5, name="l_cc_image")
    addViewport(x=1-width_image/2, y=0.4, width=width_image, height=0.5, name="r_cc_image")
    addViewport(x=0.5, y=0.4, width=1-2*width_image, height=0.8, name="text")
    seekViewport("text")
    gp <- gpar(fontsize=9, fontfamily=arg$header_font)
    # grid.text(arg$program, x=0.0, y=0.8, just="left", gp=gp)
    # grid.text(arg$copyright, x=0.0, y=0.6, just="left", gp=gp)
    # grid.text(arg$license1, x=0.0, y=0.4, just="left", gp=gp)
    # grid.text(arg$license2, x=0.0, y=0.2, just="left", gp=gp)
    grid.text(arg$program, x=0.5, y=0.8, just="center", gp=gp)
    grid.text(arg$copyright, x=0.5, y=0.6, just="center", gp=gp)
    grid.text(arg$license1, x=0.5, y=0.4, just="center", gp=gp)
    grid.text(arg$license2, x=0.5, y=0.2, just="center", gp=gp)
    seekViewport("l_cc_image")
    grid.draw(cc_file)
    seekViewport("r_cc_image")
    grid.draw(cc_file)
    seekViewport("title")
    gp <- gpar(fontsize=15, fontfamily=arg$header_font, fontface="bold")
    grid.text(title, just="center", gp=gp)
}

make_preview_header <- function(arg) {
    make_header_helper(arg$title, arg)
}

seekViewport <- function(...) { suppressWarnings(grid::seekViewport(...)) }

pp_pdf <- function(filename, family, paper) {
    if (paper == "letter") {
        cairo_pdf(filename, onefile=TRUE, width=8.5, height=11, family=family)
    } else if (paper == "A4") {
        cairo_pdf(filename, onefile=TRUE, width=8.3, height=11.7, family=family)
    } else {
        stop(paste("Don't know how to handle paper", paper))
    }
}

#' @export
make_collection_preview <- function(arg) {
    dir.create(arg$pdf_preview_dir, recursive=TRUE, showWarnings=FALSE)

    decks <- arg$decks
    fp <- file.path(arg$pdf_preview_dir, paste0(arg$filename, ".pdf"))
    pp_pdf(fp, arg$font, arg$paper)

    n_pages <- ceiling(length(decks) / 6)

    for (ii in 1:n_pages) {

        jj <- (ii - 1) * 6 + 1

        l_logos <- list()
        for(kk in 0:5) {
            deck <- decks[jj+kk]
            if(is.na(deck))
                l_logos[[kk+1]] <- nullGrob()
            else
                l_logos[[kk+1]] <- pictureGrob(readPicture(file.path(arg$svg_preview_dir, paste0(deck, ".svg")), warn=FALSE))
        }
        l_squares <- lapply(seq(along=l_logos), function(x) { rectGrob(gp=gpar(lty="dashed", col="grey", fill=NA)) })
        grid.newpage()
        vp <- viewport(x=inch(4.25), y=inch(5.0), width=inch(8), height=inch(8)) 
        pushViewport(vp)
        gridExtra::grid.arrange(grobs=l_logos, ncol=2, newpage=FALSE, padding=0)
        gridExtra::grid.arrange(grobs=l_squares, ncol=2, newpage=FALSE, padding=0)
        upViewport()
        make_preview_header(arg)
    }

    if (is_odd(n_pages)) {
        grid.newpage()
        grid.text("This page intentionally left blank")
    }
    invisible(dev.off())
}

coin_width <- 3/4
die_width <- 1/2
tile_width <- 2
belt_width <- 1.5
saucer_width <- 7/8
chip_width <- 5/8
win_width <- 8
win_height <- 10.5

mainViewport <- function() {
    addViewport(width=inch(win_width), height=inch(win_height), name="main")
    downViewport("main")
}

draw_suit_page <- function(i_s, opts) {
    grid.newpage()

    # Build viewports
    mainViewport()
    addViewport(y=inch(1.5*tile_width), width=inch(4*tile_width), height=inch(3*tile_width), name="tiles")
    downViewport("tiles")
    make_4by3_viewports("tile")
    seekViewport("main")
    xpawn <- 0.75/2
    pheight <- 4.5
    pwidth <- 0.75
    ypawn <- win_height - pheight/2 
    addViewport(x=inch(xpawn), y=inch(ypawn), width=inch(pwidth), height=inch(pheight), name="lpawn")
    addViewport(x=inch(win_width-xpawn), y=inch(ypawn), width=inch(pwidth), height=inch(pheight), name="rpawn")

    xdie <- pwidth + 3*die_width/2 
    # ydie <- win_width - 4*die_width/2 - 0.125
    ydie <- 3*tile_width + 4*die_width/2
    addViewport(x=inch(xdie) , y=inch(ydie), width=inch(2), height=inch(1.5), angle=-90, name="ldie")
    addViewport(x=inch(win_width-xdie) , y=inch(ydie), width=inch(2), height=inch(1.5),  angle=90, name="rdie")

    ysaucer <- 3*tile_width + saucer_width/2
    addViewport(y=inch(ysaucer), height=inch(saucer_width), width=inch(4*saucer_width), name="saucers")
    seekViewport("saucers")
    addViewport(x=1/4-1/8, width=0.25, name="lsaucer.face")
    addViewport(x=2/4-1/8, width=0.25, name="lsaucer.back")
    addViewport(x=3/4-1/8, width=0.25, name="rsaucer.face")
    addViewport(x=4/4-1/8, width=0.25, name="rsaucer.back")
    seekViewport("main")
    # addViewport(y=inch(ycoin), width=inch(7.5), height=inch(0.625), name="coinrow")
    # ycoin <- 6+ 3*coin_width
    ycoin <- ysaucer + saucer_width/2 + 3*coin_width/2
    addViewport(y=inch(ycoin), width=inch(4 * coin_width), height=inch(3 * coin_width), name="coins")
    seekViewport("main")


    # ydie2 <- ybelt - 1.5*die_width
    ydie2 <- ydie + 2*die_width 
    addViewport(x=inch(xdie) , y=inch(ydie2), width=inch(2), height=inch(1.5), angle=-90, name="ldie2")
    addViewport(x=inch(win_width-xdie) , y=inch(ydie2), width=inch(2), height=inch(1.5),  angle=90, name="rdie2")

    # ybelt <- ycoin + 3*coin_width/2 + die_width/2
    # xbelt <- win_width/2 - belt_width/2
    ybelt <- ydie2 + 2*die_width + die_width/2
    xbelt <- pwidth + belt_width/2
    addViewport(x=inch(xbelt), y=inch(ybelt), width=inch(belt_width), height=inch(0.5), name="lpawnbelt")
    addViewport(x=inch(win_width-xbelt), y=inch(ybelt), width=inch(belt_width), height=inch(0.5), name="rpawnbelt")

    # Draw components
    for (i_r in 1:6) {
        seekViewport(paste0("tile.face.", i_r))
        draw_tile_face(i_s, i_r, opts)
        seekViewport(paste0("tile.back.", i_r))
        draw_tile_back(opts)
    }

    # coins
    seekViewport("coins")
    draw_coin_4by3(i_s, opts)

    # pawn and belt
    seekViewport("lpawnbelt")
    draw_pawn_belt(i_s, opts)
    seekViewport("rpawnbelt")
    draw_pawn_belt(i_s, opts)
    seekViewport("lpawn")
    draw_pawn(i_s, opts)
    seekViewport("rpawn")
    draw_pawn(i_s, opts)

    # die
    seekViewport("rdie")
    draw_piecepack_die(i_s, opts)
    seekViewport("ldie")
    draw_piecepack_die(i_s, opts, flip=TRUE)
    seekViewport("rdie2")
    draw_piecepack_die(i_s, opts)
    seekViewport("ldie2")
    draw_piecepack_die(i_s, opts, flip=TRUE)

    # pawn saucers
    seekViewport("lsaucer.face")
    draw_pawn_saucer(i_s, opts)
    seekViewport("lsaucer.back")
    draw_pawn_saucer(opts$i_unsuit, opts)
    seekViewport("rsaucer.face")
    draw_pawn_saucer(i_s, opts)
    seekViewport("rsaucer.back")
    draw_pawn_saucer(opts$i_unsuit, opts)

    # annotations
    seekViewport("main")
    # grid.text("die", x=inch(xdie+0.5), y=inch(ydie+0.9))
    # grid.text("coins", y=inch( ycoin + 0.4))
    # grid.text("pawn", x=inch(3.25), y=inch(ypawn+0.5))
    # grid.text("pawn belt", x=inch(3.25), y=inch(ybelt+0.4))
    # grid.text("tiles", y=inch( 6.4))
    make_deck_header(opts)

}

draw_accessories_page <- function(opts, odd=TRUE) {
    grid.newpage()

    # Build viewports
    mainViewport()
    y_joker <- 8.75
    addViewport(y=inch(y_joker), x=0.5, width=inch(4), height=inch(2), name="joker.tiles")
    downViewport("joker.tiles")
    addViewport(x=0.25, width=0.5, name="joker.tile.face")
    addViewport(x=0.75, width=0.5, name="joker.tile.back")
    seekViewport("main")
    # dice
    ydh <- y_joker - die_width/2 + die_width
    ydm <- ydh - 3 * die_width
    ydl <- ydm - 3 * die_width
    ydb <- ydl - 3 * die_width
    die_xl = 2*die_width
    die_xm = die_xl + 2*die_width
    die_xh = die_xm + 2*die_width
    die_right <- die_xl + 0.8
    addViewport(y=inch(ydh), x=inch(die_xl), width=inch(2), height=inch(1.5), name="lsuitdie")
    addViewport(y=inch(ydh), x=inch(win_width-die_xl), width=inch(2), height=inch(1.5), name="rsuitdie")
    addViewport(y=inch(ydm-die_width), width=inch(2), height=inch(1.5), name="suitdie3")
    addViewport(y=inch(ydm), x=inch(win_width-die_xl), width=inch(2), height=inch(1.5), name="suitrankdie1")
    addViewport(y=inch(ydm), x=inch(win_width-die_xm), width=inch(2), height=inch(1.5), name="suitrankdie2")
    addViewport(y=inch(ydm), x=inch(die_xl), width=inch(2), height=inch(1.5), name="suitrankdie3")
    addViewport(y=inch(ydm), x=inch(die_xm), width=inch(2), height=inch(1.5), name="suitrankdie4")
    addViewport(y=inch(ydl), x=inch(win_width-die_xl), width=inch(2), height=inch(1.5), name="rankdie1")
    addViewport(y=inch(ydl), x=inch(win_width-die_xm), width=inch(2), height=inch(1.5), name="rankdie2")
    addViewport(y=inch(ydl), x=inch(win_width-die_xh), width=inch(2), height=inch(1.5), name="rankdie3")
    addViewport(y=inch(ydl), x=inch(die_xh), width=inch(2), height=inch(1.5), name="rankdie4")
    addViewport(y=inch(ydb), x=inch(win_width-die_xl), width=inch(2), height=inch(1.5), name="rankdie1l")
    addViewport(y=inch(ydb), x=inch(win_width-die_xm), width=inch(2), height=inch(1.5), name="rankdie2l")
    addViewport(y=inch(ydb), x=inch(win_width-die_xh), width=inch(2), height=inch(1.5), name="rankdie3l")
    addViewport(y=inch(ydb), x=inch(die_xh), width=inch(2), height=inch(1.5), name="rankdie4l")

    die_x <- c(die_xl, die_xm, die_xm, die_xl, die_xh, die_xh)
    die_y <- c(ydl, ydl, ydb, ydb, ydl, ydb)
    for (i_s in 1:opts$n_suits) {
        addViewport(y=inch(die_y[i_s]), x=inch(die_x[i_s]), width=inch(2), height=inch(1.5), name=paste0("ppdie.", i_s))
        addViewport(y=inch((opts$n_suits + 1 - i_s)*chip_width - 0.5*chip_width), 
                    width=inch(12*chip_width), height=inch(chip_width), name=paste0("chips.", i_s))
        seekViewport(paste0("chips.", i_s))
        for (i_r in 1:6) {
            addViewport(x=(2*i_r-1)/12-1/24, width=1/12, name=paste0("chips.suit.", i_s, i_r))
            addViewport(x=(2*i_r)/12-1/24, width=1/12, name=paste0("chips.value.", i_s, i_r))
        }
        seekViewport("main")
    }
    saucer_y <- 4*chip_width + 0.5*saucer_width + 0.125
    addViewport(y=inch(saucer_y),width=inch(8*saucer_width), height=inch(saucer_width), name="pawnsaucers")
    seekViewport("pawnsaucers")
    addViewport(x=1/8-1/16, width=1/8, name="pawnsaucer.face.1")
    addViewport(x=2/8-1/16, width=1/8, name="pawnsaucer.back.1")
    addViewport(x=3/8-1/16, width=1/8, name="pawnsaucer.face.2")
    addViewport(x=4/8-1/16, width=1/8, name="pawnsaucer.back.2")
    addViewport(x=5/8-1/16, width=1/8, name="pawnsaucer.face.3")
    addViewport(x=6/8-1/16, width=1/8, name="pawnsaucer.back.3")
    addViewport(x=7/8-1/16, width=1/8, name="pawnsaucer.face.4")
    addViewport(x=8/8-1/16, width=1/8, name="pawnsaucer.back.4")
    seekViewport("main")

    # Draw components
    seekViewport("joker.tile.face")
    draw_tile_face(opts$i_unsuit, opts$n_ranks + 1, opts)
    seekViewport("joker.tile.back")
    draw_tile_back(opts)
    seekViewport("rsuitdie")
    draw_suit_die(opts)
    seekViewport("lsuitdie")
    draw_suit_die(opts, flip=TRUE)
    seekViewport("suitdie3")
    if (odd)
        draw_suit_die(opts)
    else
        draw_suit_die(opts, flip=TRUE)
    seekViewport("suitrankdie1")
     draw_suitrank_die(opts)
    seekViewport("suitrankdie2")
     draw_suitrank_die(opts)
    seekViewport("suitrankdie3")
     draw_suitrank_die(opts, flip=TRUE)
    seekViewport("suitrankdie4")
     draw_suitrank_die(opts, flip=TRUE)
    seekViewport("rankdie1")
    draw_rank_die(opts)
    seekViewport("rankdie2")
    draw_rank_die(opts)
    seekViewport("rankdie3")
    draw_rank_die(opts)
    if (opts$n_suits < 5) {
        seekViewport("rankdie4")
        draw_rank_die(opts, flip=TRUE)
    }
    seekViewport("rankdie1l")
    draw_rank_die(opts)
    seekViewport("rankdie2l")
    draw_rank_die(opts)
    seekViewport("rankdie3l")
    draw_rank_die(opts)
    if (opts$n_suits < 6) {
        seekViewport("rankdie4l")
        draw_rank_die(opts, flip=TRUE)
    }
    for (i_s in 1:opts$n_suits) {
        seekViewport(paste0("ppdie.", i_s))
        draw_piecepack_die(i_s, opts, flip=TRUE)
        if (opts$n_suits <= 4) {
            seekViewport(paste0("pawnsaucer.face.", i_s))
            draw_pawn_saucer(i_s, opts)
            seekViewport(paste0("pawnsaucer.back.", i_s))
            draw_pawn_saucer(opts$i_unsuit, opts)
        }

        for (i_r in 1:6) {
            seekViewport(paste0("chips.suit.", i_s, i_r))
            draw_chip_back(i_s, opts)
            seekViewport(paste0("chips.value.", i_s, i_r))
            draw_chip_face(i_s, i_r, opts)
        }
    }

    # Annotations
    seekViewport("main")
    # for (i_s in 1:4) {
    #     grid.text(paste0("ppdie", i_s))
    # }
    # grid.text("joker tile", x=inch(0.8), y=inch(8.5), rot=90)
    # grid.text("additional piecepack dice", x=inch(0.8), y=inch((ydm+ydl)/2), rot=90)
    # grid.text("pawn\nsaucers", x=inch(1), y=inch(3.8), rot=90)
    # grid.text('chips', x=inch(0.4), y=inch(2), rot=90)
    # grid.text("suit die", x=inch(die_right), y=inch(ydh-0.3), rot=90)
    # grid.text("suit/rank die", x=inch(die_right), y=inch(ydl-0.3), rot=90)
    # grid.text("rank die", x=inch(die_right), y=inch(ydm-0.3), rot=90)
    make_deck_header(opts)

}

#' @export
make_set <- function(opts, directory) {
    dir.create(directory, recursive=TRUE, showWarnings=FALSE)

    pdf_file <- file.path(directory, paste0(opts$deck_filename, ".pdf"))
    unlink(pdf_file)

    pp_pdf(pdf_file, opts$font, opts$paper)

    for (i_s in 1:opts$n_suits) {
        draw_suit_page(i_s, opts)
    }
    if (is_odd(opts$n_suits)) {
        draw_suit_page(opts$i_unsuit+1, opts)
    }

    # Accessories
    if ((opts$n_suits >= 4) && (opts$n_suits <= 6)) {
        draw_accessories_page(opts)
        draw_accessories_page(opts, odd=FALSE)
    }
    invisible(dev.off())
}

make_bookmarks_txt <- function(deck_filenames) {
    n_sets <- length(deck_filenames)
    n_preview <- ceiling(n_sets / 6)
    if (is_odd(n_preview))
        n_preview <- n_preview + 1
    txt <- "[/Page 1 /View [/XYZ null null null] /Title (Piecepack Sets Preview) /OUT pdfmark"
    next_page <- n_preview + 1
    for(ii in 1:n_sets) {
        new_txt <- sprintf("[/Page %s /View [/XYZ null null null] /Title (Piecepack Set #%s) /OUT pdfmark", next_page, ii)
        txt <- append(txt, new_txt)
        next_page <- next_page + n_pages(deck_filenames[ii])
    }
    writeLines(txt, "bookmarks.txt")
}

n_pages <- function(pdf_filename) {
    as.numeric(system(paste("pdfinfo", pdf_filename, " | grep Pages | sed 's/[^0-9]*//'"), intern=TRUE))
}

#' @export
make_collection <- function(arg) {
    dir.create(arg$pdf_collection_dir, recursive=TRUE, showWarnings=FALSE)
    deck_filenames <- file.path(arg$pdf_deck_dir, paste0(arg$decks, ".pdf"))
    n_sets <- length(deck_filenames)
    fp <- shQuote(file.path(arg$pdf_preview_dir, paste0(arg$filename, ".pdf")))
    of_un <- file.path(arg$pdf_collection_dir, paste0(arg$filename, "_o.pdf")) # unlink doesn't work with the shQuote'd version of file
    of <- shQuote(of_un)
    bf <- shQuote(file.path(arg$pdf_collection_dir, paste0(arg$filename, ".pdf")))
    command <- paste("pdfjoin -q -o", of, "--pdftitle", shQuote(arg$title), 
                     "--pdfauthor", shQuote(arg$author), 
                     "--pdfkeywords", shQuote(arg$keywords), 
                     "--pdfsubject", shQuote(arg$subject),
                     fp, paste(shQuote(deck_filenames), collapse=" "))
    cat(command, "\n")
    system(command)

    # add bookmarks
    make_bookmarks_txt(deck_filenames)
    bcommand <- paste("gs -q -o", bf, "-sDEVICE=pdfwrite", "bookmarks.txt", "-f", of)
    # embed fonts gs -q -dNOPAUSE -dBATCH -dPDFSETTINGS=/prepress -sDEVICE=pdfwrite -sOutputFile=output.pdf input.pdf
    # pdftocairo -pdf input.pdf output.pdf
    cat(bcommand, "\n")
    system(bcommand)
    unlink(of_un)
}

get_collections <- function() {
    sub(".json$", "", list.files("svg", pattern=".json$"))
}
