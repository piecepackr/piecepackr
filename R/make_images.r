bleed_col1 <- "grey"
bleed_col2 <- "yellow"

seg <- function(x, y, xend, yend, color="black", size=50, ...) {
    grid.segments(x0=x, y0=y, x1=xend, y1=yend, gp=gpar(col=color, lwd=size))
}
curve <- function(x, y, xend, yend, color="black", size=1, curvature=0.5, ...) {
    geom_curve(x=x, y=y, xend=xend, yend=yend, 
               color=color, size=size, curvature=curvature, ...)
}

ppsvg <- function(...) { svg(..., family=c("Symbola")) }

addViewport <- function(...) { 
    pushViewport(viewport(..., clip="on")) 
    upViewport()
}

grid.inversecircle <- function() {
    x_c <- 0.5 + 0.5*cos(seq(0, 2*pi, length.out=100))
    y_c <- 0.5 + 0.5*sin(seq(0, 2*pi, length.out=100))
    x_r <- c(1, 1, 0, 0, 1, 1)
    y_r <- c(0.5, 0, 0, 1, 1, 0.5)
    grid.polygon(x = c(x_c, x_r), y=c(y_c, y_r), gp=gpar(fill="white", col="white"))
}

make_preview <- function(arg) {
    dir <- arg$directory

    ppsvg(file.path(dir, "preview.svg"), width=4, height=4.67)
    # dev.new(width=4, height=4.67, unit="in")
    grid.newpage()

    # Build viewports
    pushViewport(viewport(name="main"))
    addViewport(y=unit(2.67, "in"), width=unit(4, "in"), height=unit(4, "in"), name="tiles")
    downViewport("tiles")
    addViewport(x=0.25, y=0.75, width=0.5, height=0.5, name="tile.1")
    addViewport(x=0.75, y=0.75, width=0.5, height=0.5, name="tile.2")
    addViewport(x=0.75, y=0.25, width=0.5, height=0.5, name="tile.3")
    addViewport(x=0.25, y=0.25, width=0.5, height=0.5, name="tile.4")
    seekViewport("main")
    addViewport(y=unit(0.335, "in"), width=unit(4, "in"), height=unit(0.67, "in"), name="coins")
    downViewport("coins")
    for (ii in 1:6) {
        addViewport(x=ii/6-1/12, width=1/6, name=paste0("coin.", ii))
    }

    # Draw components
    for (ii in 1:4) {
        seekViewport(paste0("tile.", ii))
        draw_tile_front(ii, 2, opts)
    }
    seekViewport(paste0("coin.1"))
    draw_coin_value(1, opts)
    seekViewport(paste0("coin.2"))
    draw_coin_value(2, opts)
    seekViewport(paste0("coin.3"))
    draw_coin_suit(4, opts)
    seekViewport(paste0("coin.4"))
    draw_coin_value(3, opts)
    seekViewport(paste0("coin.5"))
    draw_coin_suit(2, opts)
    seekViewport(paste0("coin.6"))
    draw_coin_suit(1, opts)

    dev.off()
}

#' @export
make_images <- function(arg) {
    if (dir.exists(arg$directory)) { unlink(arg$directory, recursive=TRUE) }
    dir.create(arg$directory)
    # make_tiles(arg)
    # make_stickers(arg)
    # make_pawns(arg)
    make_preview(arg)
    invisible(NULL)
}

sticker_filename <- function(component, suit_name, rank_name) {
    filename <- sprintf("s_%s_%s_%s.svg", component, suit_name, rank_name)
}

tile_filename <- function(component, suit_name, rank_name) {
    filename <- sprintf("t_%s_%s_%s.svg", component, suit_name, rank_name)
}
pawn_filename <- function(component, suit_name, rank_name="") {
    filename <- sprintf("p_%s_%s_%s.svg", component, suit_name, rank_name)
}

add_bleed_lines <- function(arg, component, add_safezone=TRUE) {
    offset <- switch(component, tile = 40 / 675, card=0.05, sticker=0.1)
    bl_alpha <- 0.5
    bl_size <- 2
    gp_bleed <- gpar(col=bleed_col1, lwd=bl_size, alpha=bl_alpha)
    gp_safe <- gpar(col=bleed_col2, lwd=bl_size, alpha=bl_alpha)
    grid.lines(y=offset, gp=gp_bleed)
    grid.lines(y=1-offset, gp=gp_bleed) 
    grid.lines(x=offset, gp=gp_bleed)
    grid.lines(x=1-offset, gp=gp_bleed)
    if (add_safezone) {
        grid.lines(y=2*offset, gp=gp_safe)
        grid.lines(y=1-2*offset, gp=gp_safe) 
        grid.lines(x=2*offset, gp=gp_safe)
        grid.lines(x=1-2*offset, gp=gp_safe)
    }
    invisible(NULL)
}

## Tiles
## Tile Back
dip = 300
dpi_tile <- 75
tile_in = 675/dpi_tile
gl_size <- 4
hl_size <- 0.5


add_hexlines <- function(arg, omit_direction=FALSE, hl_col=arg$suit_colors[5]) {
    # ho <- 0.33
    ho <- 0.25
    hl_size <- 4
    if (omit_direction %in% 1:2)  # upper left
        NULL
    else
        seg(0, 1 - ho, ho, 1, hl_col, hl_size) 
    if (omit_direction %in% 3:4)  # lower left
        NULL
    else
        seg(0, ho, ho, 0, hl_col, hl_size) 
    if (omit_direction %in% 5:6)  # lower right
        NULL
    else
        seg(1, ho, 1 - ho, 0, hl_col, hl_size) 
    if (omit_direction %in% 7:8)  # upper right
        NULL
    else
        seg(1, 1 - ho, 1 - ho, 1, hl_col, hl_size) 
}

v_offset <- 0.13
h_offset <- 0.13
s_size <- 6
s_offset_dir <- 0.15
s_size_dir <- 12

draw_pawn <- function(i_s, arg) {
    suit_color <- arg$suit_colors[i_s]
    suit_symbol <- arg$suit_symbols[i_s]
    if (arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }
    grid.rect(gp = gpar(fill=bcol))
    gp_tr <- gpar(col=scol, fontsize=40)
    grid.text(suit_symbol, y=0.65, rot=180, gp=gp_tr)
    grid.text(suit_symbol, y=0.35, rot=0, gp=gp_tr)
    grid.lines(y=0.5, gp=gpar(col=arg$suit_colors[5], lty="dashed"))
    grid.lines(y=0.1, gp=gpar(col=arg$suit_colors[5], lty="dashed"))
    grid.lines(y=0.9, gp=gpar(col=arg$suit_colors[5], lty="dashed"))
    grid.rect(gp = gpar(col=arg$suit_colors[5], fill=NA))

    invisible(NULL)
}

draw_pawn_belt <- function(i_s, arg) {
    suit_color <- arg$suit_colors[i_s]
    suit_symbol <- arg$suit_symbols[i_s]
    if (arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }
    grid.rect(gp = gpar(fill=bcol))
    gp_tr <- gpar(col=scol, fontsize=20)
    grid.text(suit_symbol, gp=gp_tr)
    grid.lines(y=0.9, gp=gpar(col=scol, lwd=8))
    grid.lines(y=0.1, gp=gpar(col=scol, lwd=8))
    grid.rect(gp = gpar(col=arg$suit_colors[5], fill=NA))

    invisible(NULL)
}

make_pawns <- function(arg) {
    # Pawn
    for (i_s in seq(along=arg$suit_names)) {

        filename <- file.path(arg$directory, pawn_filename("pawn", i_s))
        ppsvg(filename, width=0.75,  height=4.5)
        grid.newpage()
        draw_pawn(i_s, arg)
        dev.off()

        filename <- file.path(arg$directory, pawn_filename("belt", i_s))
        ppsvg(filename, width=1.5,  height=0.5)
        grid.newpage()
        draw_pawn_belt(i_s, arg)
        dev.off()

        # if (interactive()) { browser() }
    }
}

draw_joker_tile_face <- function(arg, draw_border = TRUE) {
    if (arg$inverted) {
        bcol <- arg$suit_colors[5]
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- arg$suit_colors[5]
    }
    #### Add option to use each of four suits instead
    symbol.1 <- arg$suit_symbols[5]
    symbol.2 <- arg$suit_symbols[5]
    symbol.3 <- arg$suit_symbols[5]
    symbol.4 <- arg$suit_symbols[5]
    grid.rect(gp = gpar(fill=bcol))
    if (arg$add_checkers) {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(fill=arg$suit_colors[5]))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(fill=arg$suit_colors[5]))
    }
    if (arg$add_bleed_lines) {
        add_bleed_lines(arg, "tile")
    }
    if (arg$add_hex_lines) {
        if (arg$inverted) 
            add_hexlines(arg, hl_col=arg$background)
        else 
            add_hexlines(arg)
         
    }
    gp_tr <- gpar(col=scol, fontsize=fs_ts)
    grid.text(symbol.1, x=0.25, y=.75, gp=gp_tr)
    grid.text(symbol.3, x=0.75, y=.25, gp=gp_tr)
    if (arg$add_checkers) {
        gp_tr <- gpar(col=bcol, fontsize=fs_ts)
        grid.text(symbol.4, x=0.25, y=.25, gp=gp_tr)
        grid.text(symbol.2, x=0.75, y=.75, gp=gp_tr)
    } else {
        grid.text(symbol.4, x=0.25, y=.25, gp=gp_tr)
        grid.text(symbol.2, x=0.75, y=.75, gp=gp_tr)
    }
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

fs_tr <- 72 #### font size tile rank
fs_ts <- 32  #### font size tile suit

draw_tile_back <- function(arg, draw_border = TRUE) {
    grid.rect(gp = gpar(fill=arg$background))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "tile")
    gp_gl <- gpar(col=arg$suit_colors[5], lwd=8, lineend="square")
    if (arg$add_hex_lines)
        add_hexlines(arg)
    # width <- c(0.02, 0.98) # avoid bleading over the edge
    # grid.lines(x=0.5, y=width, gp=gp_gl)
    # grid.lines(y=0.5, x=width, gp=gp_gl)
    grid.lines(x=0.5, gp=gp_gl)
    grid.lines(y=0.5, gp=gp_gl)
    if (draw_border)
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_tile_front <- function(i_s, i_r, arg, draw_border = TRUE) {
    suit_symbol <- arg$suit_symbols[i_s]
    suit_color <- arg$suit_colors[i_s]
    if (i_r == 2 && arg$use_suit_as_ace)
        rank_symbol <- suit_symbol
    else
        rank_symbol <- arg$rank_symbols[i_r]

    if (arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }
    grid.rect(gp = gpar(fill=bcol))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "tile")
    if (arg$add_checkers) {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(fill=arg$suit_colors[5]))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(fill=arg$suit_colors[5]))
    }
    if (arg$add_hex_lines) {
        add_hexlines(arg)
        gp_tr <- gpar(col=scol, fontsize=fs_tr)
        grid.text(rank_symbol, gp=gp_tr)

        gp_ts <- gpar(col=scol, fontsize=fs_ts)
        vp <- viewport(x=0.25, y=0.75, width=0.5, height=0.5)
        pushViewport(vp)
        grid.text(suit_symbol, gp=gp_ts)
        upViewport()
    } else {
        gp_tr <- gpar(col=scol, fontsize=fs_tr)
        grid.text(rank_symbol, gp=gp_tr)

        gp_ts <- gpar(col=scol, fontsize=fs_ts)
        vp <- viewport(x=0.25, y=0.75, width=0.5, height=0.5)
        pushViewport(vp)
        grid.text(suit_symbol, gp=gp_ts)
        upViewport()
    }
    if (draw_border)
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

make_tiles <- function(arg) {

    ## Joker tile face
    filename <- file.path(arg$directory, tile_filename("front", "joker", ""))
    ppsvg(filename)
    grid.newpage()
    draw_joker_tile_face(arg)
    dev.off()

    filename <- file.path(arg$directory, tile_filename("back", "", ""))
    ppsvg(filename)
    grid.newpage()
    draw_tile_back(arg)
    dev.off()

    # Tile front
    for (i_s in 1:4) {
        for (i_r in 1:6) {

            suit_name <- arg$suit_names[i_s]
            filename <- file.path(arg$directory, tile_filename("front", i_s, i_r))
            ppsvg(filename)
            grid.newpage()
            draw_tile_front(i_s, i_r, arg)
            dev.off()
        }
    }
}

add_directional_marker <- function(arg, back=TRUE) {
    # if (arg$inverted) {
    #     if (back) {
    #         o_neutral_col <- arg$background
    #         } else {
    #             o_neutral_col <- arg$suit_colors[1]
    #         }
    #     } else {
    #         o_neutral_col <- arg$suit_colors[1]
    #     }
    # }
    grid.lines(x=c(0.5, 0.5), y=c(0.90, 0.95), gp=gpar(col=arg$suit_colors[5], lwd=10))
}

od_fontsize <- 30 # orthodox die font size
oc_fontsize <- 30 # orthodox coin font size

draw_suit_die_face <- function(i_s, arg, draw_border = TRUE) {
    suit_symbol <- arg$suit_symbols[i_s]
    suit_color <- arg$suit_colors[i_s]
    if (arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }
    grid.rect(gp = gpar(fill=bcol))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    grid.text(suit_symbol, gp = gpar(col=scol, fontsize=od_fontsize) )
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_pawn_saucer_suit <- function(i_s, arg, draw_border = TRUE) {
    suit_symbol <- arg$suit_symbols[i_s]
    suit_color <- arg$suit_colors[i_s]
    if (i_s < 5 && arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }
    grid.circle(gp = gpar(fill=bcol))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    grid.text(suit_symbol, gp = gpar(col=scol, fontsize=oc_fontsize) )
    add_directional_marker(arg, back=TRUE)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_rankdie <- function(i_r, arg, draw_border = TRUE) {
    rank_symbol <- arg$rank_symbols[i_r]
    if (i_r == 2 && arg$use_suit_as_ace)
        rank_symbol <- arg$suit_symbols[5]
    color <- arg$suit_colors[5]
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    grid.rect(gp = gpar(fill=arg$background))
    grid.text(rank_symbol, gp = gpar(col=color, fontsize=od_fontsize) )
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_suitdie_null <- function(arg, draw_border = TRUE) {
    grid.rect(gp = gpar(fill=arg$background))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_coin_value <- function(i_r, arg, draw_border = TRUE) {
    if (i_r == 2 && arg$use_suit_as_ace)
        rank_symbol <- arg$suit_symbols[5]
    else
        rank_symbol <- arg$rank_symbols[i_r]
    grid.circle(gp = gpar(fill=arg$background))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    grid.text(rank_symbol, gp = gpar(col=arg$suit_colors[5], fontsize=oc_fontsize) )
    add_directional_marker(arg, back=FALSE)
    if (draw_border) {
        grid.circle(gp=gpar(col="grey", fill=NA))
    }
    invisible(NULL)
}

draw_die_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    suit_symbol <- arg$suit_symbols[i_s]
    suit_color <- arg$suit_colors[i_s]
    if (i_r == 2 && arg$use_suit_as_ace)
        rank_symbol <- suit_symbol
    else
        rank_symbol <- arg$rank_symbols[i_r]

    if (arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }

    grid.rect(gp = gpar(fill=bcol))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    symbol <- ifelse(i_r == 2 && arg$use_suit_as_ace, 
                     suit_symbol, rank_symbol)
    grid.text(symbol, gp = gpar(col=scol, fontsize=od_fontsize))
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_chip_value <- function(i_s, i_r, arg, draw_border = TRUE) {
    suit_symbol <- arg$suit_symbols[i_s]
    suit_color <- arg$suit_colors[i_s]
    rank_symbol <- arg$rank_symbols[i_r] # don't use ``use_suit_as_ace``

    if (arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }
    grid.circle(gp = gpar(fill=bcol))
    # grid.circle(r=cr, gp=gpar(col=scol, lwd=circle_lwd, fill=bcol))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    grid.text(rank_symbol, gp = gpar(col=scol, fontsize=oc_fontsize) )
    add_directional_marker(arg, back=FALSE)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_coin_suit <- function(i_s, arg, draw_border = TRUE) {
    suit_symbol <- arg$suit_symbols[i_s]
    suit_color <- arg$suit_colors[i_s]

    if (arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }
    grid.circle(gp = gpar(fill=bcol))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    grid.text(suit_symbol, gp = gpar(col=scol, fontsize=oc_fontsize) )
    add_directional_marker(arg, back=TRUE)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}
draw_chip_suit <- function(i_s, arg, draw_border = TRUE) {
    suit_symbol <- arg$suit_symbols[i_s]
    suit_color <- arg$suit_colors[i_s]

    if (arg$inverted) {
        bcol <- suit_color
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- suit_color
    }
    grid.circle(gp = gpar(fill=bcol))
    # grid.circle(r=cr, gp=gpar(col=scol, lwd=circle_lwd, fill=bcol))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    grid.text(suit_symbol, gp = gpar(col=scol, fontsize=oc_fontsize))
    add_directional_marker(arg, back=TRUE)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

make_stickers <- function(arg) {
    circle_lwd <- 22
    cr <- 0.45

    for (i_s in 1:4) {
        for (i_r in 1:6) {

            # # tile value
            # filename <- file.path(arg$directory, sticker_filename("tile_value", i_s, i_r))
            # ppsvg(filename)
            # grid.newpage()
            # grid.rect(gp = gpar(fill=bcol))
            # if (arg$add_bleed_lines)
            #     add_bleed_lines(arg, "sticker")
            # symbol <- ifelse(i_r == 2, ace_symbol, rank_symbol)
            # grid.text(symbol, gp = gpar(col=scol, fontsize=od_fontsize) )
            # dev.off()

            # die
            filename <- file.path(arg$directory, sticker_filename("die", i_s, i_r))
            ppsvg(filename)
            grid.newpage()
            draw_die_face(i_s, i_r, arg)
            dev.off()

            # chip value
            filename <- file.path(arg$directory, sticker_filename("chip_value", i_s, i_r))
            ppsvg(filename)
            grid.newpage()
            draw_chip_value(i_s, i_r, arg)
            dev.off()

        }
        # # tile suite
        # filename <- file.path(arg$directory, sticker_filename("tile_suite", i_s, ""))
        # ppsvg(filename)
        # grid.newpage()
        # grid.rect(gp = gpar(fill=bcol))
        # if (arg$add_bleed_lines)
        #     add_bleed_lines(arg, "sticker")
        # grid.text(suit_symbol, gp = gpar(col=scol, fontsize=od_fontsize) )
        # dev.off()

        # coin suit
        filename <- file.path(arg$directory, sticker_filename("coin_suit", i_s, ""))
        ppsvg(filename)
        grid.newpage()
        draw_coin_suit(i_s, arg)
        dev.off()

        # chip suit
        filename <- file.path(arg$directory, sticker_filename("chip_suit", i_s, ""))
        ppsvg(filename)
        grid.newpage()
        draw_chip_suit(i_s, arg)
        dev.off()

        # suit dice
        filename <- file.path(arg$directory, sticker_filename("suit_die", i_s, ""))
        ppsvg(filename)
        grid.newpage()
        draw_suit_die_face(i_s, arg)
        dev.off()

        # pawn saucer suit
        filename <- file.path(arg$directory, sticker_filename("saucer_suit", i_s, ""))
        ppsvg(filename)
        grid.newpage()
        draw_pawn_saucer_suit(i_s, arg)
        dev.off()
    }

    # pawn saucer hidden
    filename <- file.path(arg$directory, sticker_filename("saucer_hidden", "", ""))
    ppsvg(filename)
    grid.newpage()
    draw_pawn_saucer_suit(5, arg)
    dev.off()

    # suit dice
    filename <- file.path(arg$directory, sticker_filename("suit_die", "joker", ""))
    ppsvg(filename)
    grid.newpage()
    draw_suit_die_face(5, arg)
    dev.off()

    # suit/rank ace
    filename <- file.path(arg$directory, sticker_filename("suit_die", "ace", ""))
    ppsvg(filename)
    grid.newpage()
    draw_suit_die_face(5, arg)
    dev.off()

    # rank dice
    for (i_r in seq(along=arg$rank_symbols)) {
        filename <- file.path(arg$directory, sticker_filename("rank_die", "", i_r))
        ppsvg(filename)
        grid.newpage()
        draw_rankdie(i_r, arg)
        dev.off()

        # coin value
        filename <- file.path(arg$directory, sticker_filename("coin_value", "", i_r))
        ppsvg(filename)
        grid.newpage()
        draw_coin_value(i_r, arg)
        dev.off()
    }

    filename <- file.path(arg$directory, sticker_filename("suit_die", "null", ""))
    ppsvg(filename)
    grid.newpage()
    draw_suitdie_null(arg)
    dev.off()

    # # tile back
    # filename <- file.path(arg$directory, sticker_filename("tile_back", "", ""))
    # ppsvg(filename)
    # grid.newpage()
    # grid.rect(gp = gpar(fill=bcol))
    # if (arg$add_bleed_lines)
    #     add_bleed_lines(arg, "sticker")

    # grid.lines(x=0.5, gp=gpar(col=arg$suit_colors[5], lwd=8))
    # grid.lines(y=0.5, gp=gpar(col=arg$suit_colors[5], lwd=8))
    # dev.off()
}
