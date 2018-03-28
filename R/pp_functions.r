cc_file <- pictureGrob(readPicture(system.file("extdata/by-sa-svg.svg", package="piecepack")))

make_deck_header <- function(arg) {
    vp <- viewport(y=unit(10.05, "in"), width=unit(8, "in"), height=unit(0.8, "in"))
    pushViewport(vp)
    vp <- viewport(x=0.3, y=0.3, height=0.5)
    pushViewport(vp)

    # CC image
    grid.draw(cc_file)
    
    upViewport()
    grid.text(arg$deck_title, x=0.5, y=0.9, just="center", gp=gpar(fontsize=20))
    grid.text(arg$program, x=0.4, y=0.6, just="left")
    grid.text(arg$copyright, x=0.4, y=0.4, just="left")
    grid.text(arg$license1, x=0.4, y=0.2, just="left")
    grid.text(arg$license2, x=0.4, y=0.0, just="left")
    upViewport()
}

make_preview_header <- function(arg) {
    vp <- viewport(y=unit(10.05, "in"), width=unit(8, "in"), height=unit(0.8, "in"))
    pushViewport(vp)
    vp <- viewport(x=0.3, y=0.3, height=0.5)
    pushViewport(vp)

    # CC image
    grid.draw(cc_file)
    
    upViewport()
    grid.text(arg$collection_title, x=0.5, y=0.9, just="center", gp=gpar(fontsize=20))
    grid.text(arg$program, x=0.4, y=0.6, just="left")
    grid.text(arg$copyright, x=0.4, y=0.4, just="left")
    grid.text(arg$license1, x=0.4, y=0.2, just="left")
    grid.text(arg$license2, x=0.4, y=0.0, just="left")
    upViewport()
}

seekViewport <- function(...) { suppressWarnings(grid::seekViewport(...)) }

#' @export
make_collection_preview <- function(arg) {
    suppressPackageStartupMessages(library('piecepack'))

    decks <- arg$decks
    fp <- paste0("previews/", arg$collection_filename, ".pdf") 
    cairo_pdf(fp, onefile=TRUE, width=8.5, height=11)

    for (ii in 1:(length(decks) / 6)) {

        jj <- (ii - 1) * 6 + 1

        l_logos <- list()
        for(kk in 0:5) {
            deck <- decks[jj+kk]
            if(is.na(deck))
                l_logos[[kk+1]] <- nullGrob()
            else
                l_logos[[kk+1]] <- pictureGrob(readPicture(file.path("svg", deck, "preview.svg"), warn=FALSE))
        }
        l_squares <- lapply(seq(along=l_logos), function(x) { rectGrob(gp=gpar(lty="dashed", col="grey", fill=NA)) })
        grid.newpage()
        vp <- viewport(x=unit(4.25, "in"), y=unit(5.0, "in"), width=unit(8, "in"), height=unit(8, "in")) 
        pushViewport(vp)
        gridExtra::grid.arrange(grobs=l_logos, ncol=2, newpage=FALSE, padding=0)
        gridExtra::grid.arrange(grobs=l_squares, ncol=2, newpage=FALSE, padding=0)
        upViewport()
        make_preview_header(arg)
    }

    dev.off()

}

#' @export
make_set <- function(opts) {
    suppressPackageStartupMessages(library('piecepack'))

    pdf_file <- paste0("pdf/", opts$deck_filename, ".pdf")
    unlink(pdf_file)

    cairo_pdf(pdf_file, onefile=TRUE, width=8.5, height=11, family=opts$font)

    l_piecepack_die <- list()
    for (i_s in 1:opts$n_suits) {

        grid.newpage()

        # Build viewports
        pushViewport(viewport(name="main"))
        addViewport(y=unit(5.25, "in"), width=unit(8, "in"), height=unit(2, "in"), name="tilerow.1")
        downViewport("tilerow.1")
        addViewport(x=0.125, width=0.25, name="tile.front.1")
        addViewport(x=0.375, width=0.25, name="tile.back.1")
        addViewport(x=0.625, width=0.25, name="tile.front.2")
        addViewport(x=0.875, width=0.25, name="tile.back.2")
        seekViewport("main")
        addViewport(y=unit(3.25, "in"), width=unit(8, "in"), height=unit(2, "in"), name="tilerow.2")
        downViewport("tilerow.2")
        addViewport(x=0.125, width=0.25, name="tile.front.3")
        addViewport(x=0.375, width=0.25, name="tile.back.3")
        addViewport(x=0.625, width=0.25, name="tile.front.4")
        addViewport(x=0.875, width=0.25, name="tile.back.4")
        seekViewport("main")
        addViewport(y=unit(1.25, "in"), width=unit(8, "in"), height=unit(2, "in"), name="tilerow.3")
        downViewport("tilerow.3")
        addViewport(x=0.125, width=0.25, name="tile.front.5")
        addViewport(x=0.375, width=0.25, name="tile.back.5")
        addViewport(x=0.625, width=0.25, name="tile.front.6")
        addViewport(x=0.875, width=0.25, name="tile.back.6")
        seekViewport("main")
        ycoin <- 7.0
        addViewport(y=unit(ycoin, "in"), width=unit(7.5, "in"), height=unit(0.625, "in"), name="coinrow")
        downViewport("coinrow")
        for (i_r in 1:6) {
            addViewport(x=(2*i_r-1)/12-1/24, width=1/12, name=paste0("coin.suit.", i_r))
            addViewport(x=(2*i_r)/12-1/24, width=1/12, name=paste0("coin.value.", i_r))
        }
        seekViewport("main")
        ypawn <- 8.8
        addViewport(x=unit(3.25, "in"), y=unit(ypawn, "in"), width=unit(0.75, "in"), height=unit(4.5, "in"), angle=90, name="pawn")
        ybelt <- 7.8
        addViewport(x=unit(3.25, "in"), y=unit(ybelt, "in"), width=unit(1.5, "in"), height=unit(0.5, "in"), name="pawnbelt")

        xdie <- 7.0
        ydie <- 8.5
        addViewport(x=unit(xdie, "in") , y=unit(ydie, "in"), width=unit(2, "in"), height=unit(1.5, "in"), name="die")
        seekViewport("die")
        addViewport(x=1/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=-90, name="piecepack_die.1")
        addViewport(x=2/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=  0, name="piecepack_die.2")
        addViewport(x=2/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=-90, name="piecepack_die.3")
        addViewport(x=3/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=  0, name="piecepack_die.4")
        addViewport(x=3/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=-90, name="piecepack_die.5")
        addViewport(x=4/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=  0, name="piecepack_die.6")


        # Draw components
        for (i_r in 1:6) {
            seekViewport(paste0("tile.front.", i_r))
            draw_tile_face(i_s, i_r, opts)
            seekViewport(paste0("tile.back.", i_r))
            draw_tile_back(opts)
        }

        # coins
        l_coins <- list()
        for(i_r in 1:6) {
            seekViewport(paste0("coin.suit.", i_r))
            draw_coin_back(i_s, opts)
            seekViewport(paste0("coin.value.", i_r))
            draw_coin_value(i_r, opts)
        }

        # die
        for(i_r in 1:6) {
            seekViewport(paste0("piecepack_die.", i_r))
            draw_piecepack_die_face(i_s, i_r, opts)
        }

        # pawn and belt
        seekViewport("pawn")
        draw_pawn(i_s, opts)
        seekViewport("pawnbelt")
        draw_pawn_belt(i_s, opts)

        # annotations
        seekViewport("main")
        grid.text("die", x=unit(xdie+0.5, "in"), y=unit(ydie+0.9, "in"))
        grid.text("coins", y=unit( ycoin + 0.4, "in"))
        grid.text("pawn", x=unit(3.25, "in"), y=unit(ypawn+0.5, "in"))
        grid.text("pawn belt", x=unit(3.25, "in"), y=unit(ybelt+0.4, "in"))
        grid.text("tiles", y=unit( 6.4, "in"))

        make_deck_header(opts)
    }

    # Accessories
    if (opts$n_suits == 4) {
        grid.newpage()

        # Build viewports
        pushViewport(viewport(name="main"))
        addViewport(y=unit(8.5, "in"), x=unit(3.0, "in"), width=unit(4, "in"), height=unit(2, "in"), name="joker.tiles")
        downViewport("joker.tiles")
        addViewport(x=0.25, width=0.5, name="joker.tile.face")
        addViewport(x=0.75, width=0.5, name="joker.tile.back")
        seekViewport("main")
        # dice
        ydh <- 8.20
        ydm <- 6.60
        ydl <- 5.00
        addViewport(y=unit(ydh, "in"), x=unit(7, "in"), width=unit(2, "in"), height=unit(1.5, "in"), name="suitdie")
        seekViewport("suitdie")
        addViewport(x=1/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=-90, name="suitdie.0")
        addViewport(x=2/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=  0, name="suitdie.1")
        addViewport(x=2/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=-90, name="suitdie.2")
        addViewport(x=3/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=  0, name="suitdie.3")
        addViewport(x=3/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=-90, name="suitdie.4")
        addViewport(x=4/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=  0, name="suitdie.5")
        seekViewport("main")
        addViewport(y=unit(ydm, "in"), x=unit(7, "in"), width=unit(2, "in"), height=unit(1.5, "in"), name="rankdie")
        seekViewport("rankdie")
        addViewport(x=1/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=-90, name="rankdie.1")
        addViewport(x=2/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=  0, name="rankdie.2")
        addViewport(x=2/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=-90, name="rankdie.3")
        addViewport(x=3/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=  0, name="rankdie.4")
        addViewport(x=3/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=-90, name="rankdie.5")
        addViewport(x=4/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=  0, name="rankdie.6")
        seekViewport("main")
        addViewport(y=unit(ydl, "in"), x=unit(7, "in"), width=unit(2, "in"), height=unit(1.5, "in"), name="suitrankdie")
        seekViewport("suitrankdie")
        addViewport(x=1/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=-90, name="suitrankdie.1")
        addViewport(x=2/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=  0, name="suitrankdie.2")
        addViewport(x=2/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=-90, name="suitrankdie.3")
        addViewport(x=3/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=  0, name="suitrankdie.4")
        addViewport(x=3/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=-90, name="suitrankdie.5")
        addViewport(x=4/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=  0, name="suitrankdie.6")
        seekViewport("main")
        addViewport(y=unit(3.8, "in"),width=unit(6, "in"), height=unit(0.75, "in"), name="pawnsaucers")
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

        die_x <- c(2, 4.5, 4.5, 2)
        die_y <- c(ydl, ydl, ydm, ydm)
        for (i_s in 1:4) {
            addViewport(y=unit(die_y[i_s], "in"), x=unit(die_x[i_s], "in"), width=unit(2, "in"), height=unit(1.5, "in"), name=paste0("ppdie.", i_s))
            seekViewport(paste0("ppdie.", i_s))
            addViewport(x=1/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=-90, name=paste0("ppdie.", i_s, 1))
            addViewport(x=2/4-1/8, y=1/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0("ppdie.", i_s, 2))
            addViewport(x=2/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=-90, name=paste0("ppdie.", i_s, 3))
            addViewport(x=3/4-1/8, y=2/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0("ppdie.", i_s, 4))
            addViewport(x=3/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=-90, name=paste0("ppdie.", i_s, 5))
            addViewport(x=4/4-1/8, y=3/3-1/6, width=1/4, height=1/3, angle=  0, name=paste0("ppdie.", i_s, 6))
            seekViewport("main")
            addViewport(y=unit(0.2 + i_s * 0.715, "in"), width=unit(7.5, "in"), height=unit(0.625, "in"), name=paste0("chips.", i_s))
            seekViewport(paste0("chips.", i_s))
            for (i_r in 1:6) {
                addViewport(x=(2*i_r-1)/12-1/24, width=1/12, name=paste0("chips.suit.", i_s, i_r))
                addViewport(x=(2*i_r)/12-1/24, width=1/12, name=paste0("chips.value.", i_s, i_r))
            }
            seekViewport("main")
        }

        # Draw components
        seekViewport("joker.tile.face")
        draw_joker_tile_face(opts)
        seekViewport("joker.tile.back")
        draw_tile_back(opts)
        seekViewport("suitdie.0")
        draw_suit_die_face(6, opts)
        for (i_s in 1:4) {
            seekViewport(paste0("suitdie.", i_s))
            draw_suit_die_face(5-i_s, opts)
        }
        seekViewport(paste0("suitdie.", 5))
        draw_suit_die_face(5, opts)
        for (i_r in 1:6) {
            seekViewport(paste0("rankdie.", i_r))
            draw_piecepack_die_face(opts$i_unsuit + 1, i_r, opts)
        }
        seekViewport("suitrankdie.1")
        draw_piecepack_die_face(6, 1, opts)
        seekViewport("suitrankdie.2")
        draw_piecepack_die_face(5, 2, opts)
        for (i_r in 3:6) {
            seekViewport(paste0("suitrankdie.", i_r))
            draw_piecepack_die_face(5-(i_r-2), i_r, opts)
        }
        for (i_s in 1:4) {
            seekViewport(paste0("pawnsaucer.face.", i_s))
            draw_pawn_saucer_suit(i_s, opts)
            seekViewport(paste0("pawnsaucer.back.", i_s))
            draw_pawn_saucer_suit(5, opts)
        }

        die_x <- c(2, 4.5, 4.5, 2)
        die_y <- c(ydl, ydl, ydm, ydm)
        for (i_s in 1:4) {
            for (i_r in 1:6) {
                seekViewport(paste0("ppdie.", i_s, i_r))
                draw_piecepack_die_face(i_s, i_r, opts)
                seekViewport(paste0("chips.suit.", i_s, i_r))
                draw_chip_back(i_s, opts)
                seekViewport(paste0("chips.value.", i_s, i_r))
                draw_chip_face(i_s, i_r, opts)
            }
        }

        # Annotations
        seekViewport("main")
        grid.text("joker tile", x=unit(0.8, "in"), y=unit(8.5, "in"), rot=90)
        grid.text("additional piecepack dice", x=unit(0.8, "in"), y=unit((ydm+ydl)/2, "in"), rot=90)
        grid.text("pawn\nsaucers", x=unit(1, "in"), y=unit(3.8, "in"), rot=90)
        grid.text('chips', x=unit(0.4, "in"), y=unit(2, "in"), rot=90)
        die_right <- 7.8
        grid.text("suit die", x=unit(die_right, "in"), y=unit(ydh-0.3, "in"), rot=90)
        grid.text("suit/rank die", x=unit(die_right, "in"), y=unit(ydl-0.3, "in"), rot=90)
        grid.text("rank die", x=unit(die_right, "in"), y=unit(ydm-0.3, "in"), rot=90)
        make_deck_header(opts)

        dev.off()
        invisible(NULL)

    }
}

make_bookmarks_txt <- function(deck_filenames) {
    n_sets <- length(deck_filenames)
    n_preview <- ceiling(n_sets / 6)
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
    suppressPackageStartupMessages(library("piecepack"))
    deck_filenames <- file.path("pdf", paste0(arg$decks, ".pdf"))
    n_sets <- length(deck_filenames)
    fp <- shQuote(file.path("previews", paste0(arg$collection_filename, ".pdf")))
    of_un <- file.path("collections", paste0(arg$collection_filename, "_o.pdf")) # unlink doesn't work with the shQuote'd version of file
    of <- shQuote(of_un)
    bf <- shQuote(file.path("collections", paste0(arg$collection_filename, ".pdf")))
    command <- paste("pdfjoin -q -o", of, "--pdftitle", shQuote(arg$collection_title), "--pdfauthor", shQuote("Trevor L Davis"), "--pdfkeywords", shQuote("piecepack"), fp, paste(shQuote(deck_filenames), collapse=" "))
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
