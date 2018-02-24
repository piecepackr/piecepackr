# Small Square Tile: 675 x 675 pixels
# Dice Sticker: 188 x 188 pixels
# Large Pro Box Top Wrap: 3225 x 5025 pixels
# Poker Tuck Box (72 cards): 2550 x 1950 pixels
# Small Pro Box: 3450 x 2700 pixels
# Medium Pro Box: 3450 x 5250 pixels

# 2.5" x 2.5" cards require 1/8" bleed on each side 0.05 in normalized units, extra 1/8" for safe zone
# 2" by 2" tiles require 1/8" bleed on each side 0.0625 in normalized units, extra 1/8" for safe zone
# 0.5" by 0.5" stickers require about 1/8 of sticker for bleed, extra 0.125 for safe zone, 20 pixels (about 0.106383)

# png <- to_png("templates/dice-sticker.png")
# png <- to_png("templates/small-square-tile.png")
# grid.newpage()
# to_crop <- 40
# grid.raster(png[(1 + to_crop):(675-to_crop),(1 + to_crop):(675-to_crop),])

.rotate_matrix <- function(x) { t(apply(x, 2, rev)) }

.rotate <- function(png) {
    do <- dim(png)
    new_png <- array(dim=do[c(2,1,3)])
    new_png[,,1] <- .rotate_matrix(png[,,1])
    new_png[,,2] <- .rotate_matrix(png[,,2])
    new_png[,,3] <- .rotate_matrix(png[,,3])
    new_png
}

.png_to_grid <- function(png) { rasterGrob(png) } 

.to_png <- function(f) { 
    bf <- basename(f)
    png <- png::readPNG(f) 
    if (substring(bf, 1, 2) == "s_") {
        to_crop <- 19
        png <- png[(1 + to_crop):(188-to_crop),(1 + to_crop):(188-to_crop),]
    }
    if (substring(bf, 1, 2) == "t_") {
        to_crop <- 40
        png <- png[(1 + to_crop):(675-to_crop),(1 + to_crop):(675-to_crop),]
    }
    png
}

# #' @export
# get_collection <- function(dir) { gsub("(.*)_.*_.*_.*_.*_.*_.*", "\\1", dir)}

# uglier, smaller file size
# cc_file <- .png_to_grid(.to_png(system.file("extdata/cc_license_88x31.png", package="piecepack")))
# prettier, larger file size
cc_file <- pictureGrob(readPicture(system.file("extdata/by-sa-svg.svg", package="piecepack")))
# ugly
# cc_file <- pictureGrob(readPicture(system.file("extdata/by-sa.eps.xml", package="piecepack")))

make_header <- function(collection) {
    arg <- rjson::fromJSON(file=file.path("png", paste0(collection, ".json")))
    vp <- viewport(y=unit(10.05, "in"), width=unit(8, "in"), height=unit(0.8, "in"))
    pushViewport(vp)
    vp <- viewport(x=0.3, y=0.3, height=0.5)
    pushViewport(vp)

    # CC image
    grid.draw(cc_file)
    
    upViewport()
    grid.text(arg$set_name, x=0.5, y=0.9, just="center", gp=gpar(fontsize=20))
    grid.text(arg$copyright, x=0.4, y=0.5, just="left")
    grid.text(arg$license1, x=0.4, y=0.3, just="left")
    grid.text(arg$license2, x=0.4, y=0.1, just="left")
    upViewport()
}

#' @export
get_directories <- function() {
    directories <- list.files("png", full.names=FALSE)
    directories <- grep(".json$", directories, value=TRUE, invert=TRUE)
    directories

}

#' @export
make_collection_preview <- function(collection) {
    suppressPackageStartupMessages(library('piecepack'))
    dirs <- get_directories()
    # dirs <- grep(collection, dirs, value=TRUE)   
    fp <- paste0("previews/", collection, ".pdf") 
    cairo_pdf(fp, onefile=TRUE, width=8.5, height=11)

    for (ii in 1:(length(dirs) / 6)) {

        jj <- (ii - 1) * 6 + 1

        l_logos <- list()
        for(kk in 0:5) {
            dir <- dirs[jj+kk]
            if(is.na(dir))
                l_logos[[kk+1]] <- nullGrob()
            else
                l_logos[[kk+1]] <- .png_to_grid(.to_png(file.path("png", dir, "preview.png")))
        }
        l_squares <- lapply(seq(along=l_logos), function(x) { rectGrob(gp=gpar(lty="dashed", col="grey", fill=NA)) })
        grid.newpage()
        vp <- viewport(x=unit(4.25, "in"), y=unit(5.0, "in"), width=unit(8, "in"), height=unit(8, "in")) 
        pushViewport(vp)
        grid.arrange(grobs=l_logos, ncol=2, newpage=FALSE, padding=0)
        grid.arrange(grobs=l_squares, ncol=2, newpage=FALSE, padding=0)
        upViewport()
        make_header(collection)
    }

    dev.off()

}

#' @export
make_set <- function(opts) {
    dir <- opts$deck_label
    pdf_file <- paste0("pdf/", dir, ".pdf")
    unlink(pdf_file)

    suppressPackageStartupMessages(library('piecepack'))
    x_c <- 0.5 + 0.5*cos(seq(0, 2*pi, length.out=100))
    y_c <- 0.5 + 0.5*sin(seq(0, 2*pi, length.out=100))
    x_r <- c(1, 1, 0, 0, 1, 1)
    y_r <- c(0.5, 0, 0, 1, 1, 0.5)
    l_circles <- lapply(1:12, function(x) { circleGrob(gp=gpar(col="grey", fill=NA))})
    l_inverse_circles <- lapply(1:12, function(x) { polygonGrob(x = c(x_c, x_r), y=c(y_c, y_r), gp=gpar(fill="white", col="white")) })
    l_squares <- lapply(1:12, function(x) { rectGrob(gp=gpar(col="grey", fill=NA)) })

    l_die_squares <- lapply(1:12, function(x) { nullGrob() })
    l_die_squares[[9]] <- rectGrob(gp=gpar(col="grey"))
    l_die_squares[[10]] <- rectGrob(gp=gpar(col="grey"))
    l_die_squares[[6]] <- rectGrob(gp=gpar(col="grey"))
    l_die_squares[[7]] <- rectGrob(gp=gpar(col="grey"))
    l_die_squares[[3]] <- rectGrob(gp=gpar(col="grey"))
    l_die_squares[[4]] <- rectGrob(gp=gpar(col="grey"))

    files <- list.files(file.path("png", dir), full.names=TRUE)
    pngs <- lapply(files, .to_png)
    files <- basename(files) 
    names(pngs) <- files
    raster_grobs <- lapply(pngs, .png_to_grid)
    names(raster_grobs) <- files

    cairo_pdf(pdf_file, onefile=TRUE, width=8.5, height=11, family="Symbola")

    l_piecepack_die <- list()
    for (i_s in 1:4) {

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
        addViewport(y=unit(7.0, "in"), width=unit(7.5, "in"), height=unit(0.625, "in"), name="coinrow")
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

        # Draw components
        for (i_r in 1:6) {
            seekViewport(paste0("tile.front.", i_r))
            draw_tile_front(i_s, i_r, opts)
            seekViewport(paste0("tile.back.", i_r))
            draw_tile_back(opts)
        }
        seekViewport("main")
        grid.text("tiles", y=unit( 6.4, "in"))

        # coins
        l_coins <- list()
        for(i_r in 1:6) {
            seekViewport(paste0("coin.suit.", i_r))
            draw_coin_suit(i_s, opts)
            seekViewport(paste0("coin.value.", i_r))
            draw_coin_value(i_r, opts)
        }
        seekViewport("main")
        grid.text("coins", y=unit( 7.4, "in"))

        # die
        l_die <- lapply(1:12, function(x) {nullGrob()})
        l_die[[9]] <- raster_grobs[[paste0("s_die_", i_s, "_1.png")]]
        l_die[[10]] <- raster_grobs[[paste0("s_die_", i_s, "_2.png")]]
        l_die[[6]] <- raster_grobs[[paste0("s_die_", i_s, "_3.png")]]
        l_die[[7]] <- raster_grobs[[paste0("s_die_", i_s, "_4.png")]]
        l_die[[3]] <- raster_grobs[[paste0("s_die_", i_s, "_5.png")]]
        l_die[[4]] <- raster_grobs[[paste0("s_die_", i_s, "_6.png")]]
        l_piecepack_die[[i_s]] <- l_die
        xdie <- 7.0
        ydie <- 8.5
        vp <- viewport(x=unit(xdie, "in") , y=unit(ydie, "in"), width=unit(2, "in"), height=unit(1.5, "in"))
        pushViewport(vp)
        grid.arrange(grobs=l_die_squares, ncol=4, newpage=FALSE, padding=0)
        grid.arrange(grobs=l_die, ncol=4, newpage=FALSE, padding=0)
        upViewport()
        grid.text("die", x=unit(xdie+0.5, "in"), y=unit(ydie+0.9, "in"))

        # pawn and belt
        seekViewport("pawn")
        draw_pawn(i_s, opts)
        seekViewport("pawnbelt")
        draw_pawn_belt(i_s, opts)
        seekViewport("main")
        grid.text("pawn", x=unit(3.25, "in"), y=unit(ypawn+0.5, "in"))
        grid.text("pawn belt", x=unit(3.25, "in"), y=unit(ybelt+0.4, "in"))

        make_header(get_collection())
    }

    grid.newpage()

    # Build viewports
    pushViewport(viewport(name="main"))
    addViewport(y=unit(8.5, "in"), x=unit(3.0, "in"), width=unit(4, "in"), height=unit(2, "in"), name="joker.tiles")
    downViewport("joker.tiles")
    addViewport(x=0.25, width=0.5, name="joker.tile.face")
    addViewport(x=0.75, width=0.5, name="joker.tile.back")
    seekViewport("main")

    # Draw components
    seekViewport("joker.tile.face")
    draw_joker_tile_face(opts)
    seekViewport("joker.tile.back")
    draw_tile_back(opts)
    seekViewport("main")
    grid.text("joker tile", x=unit(0.8, "in"), y=unit(8.5, "in"), rot=90)

    ydh <- 8.20
    ydm <- 6.60
    ydl <- 5.00

    # suit die
    l_die <- lapply(1:12, function(x) {nullGrob()})
    l_die[[9]] <- raster_grobs[[paste0("s_suit_die_null_.png")]]
    l_die[[10]] <- raster_grobs[[paste0("s_suit_die_joker_.png")]]
    l_die[[6]] <- raster_grobs[[paste0("s_suit_die_", 4, "_.png")]]
    l_die[[7]] <- raster_grobs[[paste0("s_suit_die_", 3, "_.png")]]
    l_die[[3]] <- raster_grobs[[paste0("s_suit_die_", 2, "_.png")]]
    l_die[[4]] <- raster_grobs[[paste0("s_suit_die_", 1, "_.png")]]
    vp <- viewport(y=unit(ydh, "in"), x=unit(7, "in"), width=unit(2, "in"), height=unit(1.5, "in"))
    pushViewport(vp)
    grid.arrange(grobs=l_die_squares, ncol=4, newpage=FALSE, padding=0)
    grid.arrange(grobs=l_die, ncol=4, newpage=FALSE, padding=0)
    upViewport()
    grid.text("suit die", x=unit(5.8, "in"), y=unit(ydh-0.3, "in"), rot=90)

    # rank die
    l_die <- lapply(1:12, function(x) {nullGrob()})
    l_die[[9]] <- raster_grobs[[paste0("s_rank_die_", "_1.png")]]
    l_die[[10]] <- raster_grobs[[paste0("s_rank_die_", "_2.png")]]
    l_die[[6]] <- raster_grobs[[paste0("s_rank_die_", "_3.png")]]
    l_die[[7]] <- raster_grobs[[paste0("s_rank_die_", "_4.png")]]
    l_die[[3]] <- raster_grobs[[paste0("s_rank_die_", "_5.png")]]
    l_die[[4]] <- raster_grobs[[paste0("s_rank_die_", "_6.png")]]
    vp <- viewport(y=unit(ydm, "in"), x=unit(7, "in"), width=unit(2, "in"), height=unit(1.5, "in"))
    pushViewport(vp)
    grid.arrange(grobs=l_die_squares, ncol=4, newpage=FALSE, padding=0)
    grid.arrange(grobs=l_die, ncol=4, newpage=FALSE, padding=0)
    upViewport()
    grid.text("rank die", x=unit(5.8, "in"), y=unit(ydm-0.3, "in"), rot=90)

    # combined suit/rank die
    l_die <- lapply(1:12, function(x) {nullGrob()})
    l_die[[9]] <- raster_grobs[[paste0("s_rank_die_", "_1.png")]]
    # l_die[[10]] <- raster_grobs[[paste0("s_suit_die_joker_.png")]]
    # l_die[[10]] <- raster_grobs[[paste0("s_rank_die_", "_a.png")]]
    l_die[[10]] <- raster_grobs[[paste0("s_suit_die_ace_.png")]]
    l_die[[6]] <- raster_grobs[[paste0("s_die_", 4, "_3.png")]]
    l_die[[7]] <- raster_grobs[[paste0("s_die_", 3, "_4.png")]]
    l_die[[3]] <- raster_grobs[[paste0("s_die_", 2, "_6.png")]]
    l_die[[4]] <- raster_grobs[[paste0("s_die_", 1, "_6.png")]]
    vp <- viewport(y=unit(ydl, "in"), x=unit(7, "in"), width=unit(2, "in"), height=unit(1.5, "in"))
    pushViewport(vp)
    grid.arrange(grobs=l_die_squares, ncol=4, newpage=FALSE, padding=0)
    grid.arrange(grobs=l_die, ncol=4, newpage=FALSE, padding=0)
    upViewport()
    grid.text("suit/rank die", x=unit(5.8, "in"), y=unit(ydl-0.3, "in"), rot=90)

    # pawn saucers
    l_saucers <- list()
    l_saucers[[1]] <- raster_grobs[[paste0("s_saucer_suit_", 1, "_.png")]]
    l_saucers[[2]] <- raster_grobs[[paste0("s_saucer_hidden__.png")]]
    l_saucers[[3]] <- raster_grobs[[paste0("s_saucer_suit_", 2, "_.png")]]
    l_saucers[[4]] <- raster_grobs[[paste0("s_saucer_hidden__.png")]]
    l_saucers[[5]] <- raster_grobs[[paste0("s_saucer_suit_", 3, "_.png")]]
    l_saucers[[6]] <- raster_grobs[[paste0("s_saucer_hidden__.png")]]
    l_saucers[[7]] <- raster_grobs[[paste0("s_saucer_suit_", 4, "_.png")]]
    l_saucers[[8]] <- raster_grobs[[paste0("s_saucer_hidden__.png")]]
    vp <- viewport(y=unit(3.8, "in"),width=unit(6, "in"), height=unit(0.75, "in"))
    pushViewport(vp)
    grid.arrange(grobs=l_saucers, nrow=1, newpage=FALSE, padding=0)
    grid.arrange(grobs=l_inverse_circles[1:8], nrow=1, newpage=FALSE, padding=0)
    grid.arrange(grobs=l_circles[1:8], nrow=1, newpage=FALSE, padding=0)
    upViewport()
    grid.text("pawn\nsaucers", x=unit(1, "in"), y=unit(3.8, "in"), rot=90)

    die_x <- c(2, 4.5, 4.5, 2)
    die_y <- c(ydl, ydl, ydm, ydm)
    for (i_s in 1:4) {

        # additional die
        vp <- viewport(y=unit(die_y[i_s], "in"), x=unit(die_x[i_s], "in"), width=unit(2, "in"), height=unit(1.5, "in"))
        pushViewport(vp)
        grid.arrange(grobs=l_die_squares, ncol=4, newpage=FALSE, padding=0)
        grid.arrange(grobs=l_piecepack_die[[i_s]], ncol=4, newpage=FALSE, padding=0)
        upViewport()

        # suit chips
        l_chips <- list()
        for (cc in grep(paste0("s_chip_value_", i_s), files, value=TRUE)) {
            l_chips[[cc]] <- raster_grobs[[cc]]
            l_chips[[paste0(cc, "_", i_s)]] <- raster_grobs[[paste0("s_chip_suit_", i_s, "_.png")]]
        }

        vp <- viewport(y=unit(0.2 + i_s * 0.715, "in"), width=unit(7.5, "in"), height=unit(0.625, "in"))
        pushViewport(vp)
        grid.arrange(grobs=l_chips, nrow=1, newpage=FALSE, padding=0)
        grid.arrange(grobs=l_inverse_circles, nrow=1, newpage=FALSE, padding=0)
        grid.arrange(grobs=l_circles, nrow=1, newpage=FALSE, padding=0)
        upViewport()
        # popViewport(vp)
    }

    # #### experiment
    # vp <- viewport(y=unit(die_y[i_s], "in"), x=unit(die_x[i_s]+1.5, "in"), width=unit(2, "in"), height=unit(1.5, "in"))
    # pushViewport(vp)
    # grid.arrange(grobs=l_die_squares, ncol=4, newpage=FALSE, padding=0)
    # grid.arrange(grobs=l_piecepack_die[[i_s]], ncol=4, newpage=FALSE, padding=0)
    # upViewport()

    grid.text("additional piecepack dice", x=unit(0.8, "in"), y=unit((ydm+ydl)/2, "in"), rot=90)
    grid.text('chips', x=unit(0.4, "in"), y=unit(2, "in"), rot=90)
    make_header(get_collection())

    dev.off()
    invisible(NULL)

}

make_bookmarks_txt <- function(n_sets) {
    n_preview <- ceiling(n_sets / 6)
    txt <- "[/Page 1 /View [/XYZ null null null] /Title (Piecepack Sets Preview) /OUT pdfmark"
    next_page <- n_preview + 1
    pages_per_pp <- 5
    for(ii in 1:n_sets) {
        new_txt <- sprintf("[/Page %s /View [/XYZ null null null] /Title (Piecepack Set #%s) /OUT pdfmark", next_page, ii)
        txt <- append(txt, new_txt)
        next_page <- next_page + pages_per_pp
    }
    writeLines(txt, "bookmarks.txt")
}

#' @export
make_collection <- function(collection) {
    suppressPackageStartupMessages(library("piecepack"))
    arg <- rjson::fromJSON(file=file.path("png", paste0(collection, ".json")))
    sets <- list.files("pdf", full.names=TRUE)
    n_sets <- length(sets)
    fp <- shQuote(file.path("previews", paste0(collection, ".pdf")))
    of_un <- file.path("collections", paste0(collection, "_o.pdf")) # unlink doesn't work with the shQuote'd version of file
    of <- shQuote(of_un)
    bf <- shQuote(file.path("collections", paste0(collection, ".pdf")))
    command <- paste("pdfjoin -o", of, "--pdftitle", shQuote(arg$set_name), "--pdfauthor", shQuote("Trevor L Davis"), "--pdfkeywords", shQuote("piecepack"), fp, paste(shQuote(sets), collapse=" "))
    cat(command, "\n")
    system(command)

    # add bookmarks
    make_bookmarks_txt(n_sets)
    bcommand <- paste("gs -o", bf, "-sDEVICE=pdfwrite", "bookmarks.txt", "-f", of)
    # embed fonts gs -q -dNOPAUSE -dBATCH -dPDFSETTINGS=/prepress -sDEVICE=pdfwrite -sOutputFile=output.pdf input.pdf
    # pdftocairo -pdf input.pdf output.pdf
    cat(bcommand, "\n")
    system(bcommand)
    unlink(of_un)
}

#' @export
get_collection <- function() {
    sub(".json$", "", list.files("png", pattern=".json$"))
}
