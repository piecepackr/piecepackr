# df <- read.csv("cache/df.csv")
# suit_info <- read.csv("cache/suit_info.csv")

# Small Square Deck: 825 x 825 pixels 
# Small Square Tile: 675 x 675 pixels
# Dice Sticker: 188 x 188 pixels
# Large Pro Box Top Wrap: 3225 x 5025 pixels
# Poker Tuck Box (72 cards): 2550 x 1950 pixels
# Small Pro Box: 3450 x 2700 pixels
# Medium Pro Box: 3450 x 5250 pixels

## 2.5" x 2.5" cards require 1/8" bleed on each side 0.05 in normalized units, extra 1/8" for safe zone
## 2" by 2" tiles require 1/8" bleed on each side 0.0625 in normalized units, extra 1/8" for safe zone
## 0.5" by 0.5" stickers require about 1/8 of sticker for bleed, 0.106383 in normalized units, extra 0.125 for safe zone

neutral_col1 <- "grey"
neutral_col2 <- "yellow"
# dpi_sticker <- 188
# sticker_size = 188 / dpi_sticker
sticker_px = 188

seg <- function(x, y, xend, yend, color="black", size=50, ...) {
    grid.segments(x0=x, y0=y, x1=xend, y1=yend, gp=gpar(col=color, lwd=size))
}
curve <- function(x, y, xend, yend, color="black", size=1, curvature=0.5, ...) {
    geom_curve(x=x, y=y, xend=xend, yend=yend, 
               color=color, size=size, curvature=curvature, ...)
}

# ppng <- function(...) { grDevices::png(..., family=c("Noto Sans"), res=150, type="cairo") }
ppng <- function(...) { grDevices::png(..., family=c("Symbola"), res=150, type="cairo") }

sticker_png <- function(filename) {
    ppng(filename, width=sticker_px, height=sticker_px)
}

make_logo <- function(dir) {
    l_tiles <- list()
    l_tiles[[1]] <- .png_to_grid(.to_png_simple(file.path(dir, "t_front_s1_a.png")))
    l_tiles[[2]] <- .png_to_grid(.to_png_simple(file.path(dir, "t_front_s2_a.png")))
    l_tiles[[3]] <- .png_to_grid(.to_png_simple(file.path(dir, "t_front_s3_a.png")))
    l_tiles[[4]] <- .png_to_grid(.to_png_simple(file.path(dir, "t_front_s4_a.png")))
    l_squares <- lapply(1:4, function(x) { rectGrob(gp=gpar(fill=NA, col="grey")) })
    x_c <- 0.5 + 0.5*cos(seq(0, 2*pi, length.out=100))
    y_c <- 0.5 + 0.5*sin(seq(0, 2*pi, length.out=100))
    x_r <- c(1, 1, 0, 0, 1, 1)
    y_r <- c(0.5, 0, 0, 1, 1, 0.5)
    l_coins <- list()
    l_coins[[1]] <- .png_to_grid(.to_png_simple(file.path(dir, "s_coin_value__n.png"))) 
    l_coins[[2]] <- .png_to_grid(.to_png_simple(file.path(dir, "s_coin_value__a.png"))) 
    l_coins[[3]] <- .png_to_grid(.to_png_simple(file.path(dir, "s_coin_suit_s4_.png"))) 
    l_coins[[4]] <- .png_to_grid(.to_png_simple(file.path(dir, "s_coin_value__3.png"))) 
    l_coins[[5]] <- .png_to_grid(.to_png_simple(file.path(dir, "s_coin_suit_s2_.png"))) 
    l_coins[[6]] <- .png_to_grid(.to_png_simple(file.path(dir, "s_coin_suit_s1_.png"))) 
    l_inverse_circles <- lapply(1:6, function(x) { polygonGrob(x = c(x_c, x_r), y=c(y_c, y_r), gp=gpar(fill="white", col="white")) })
    l_circles <- lapply(1:6, function(x) { circleGrob(gp=gpar(col="grey", fill=NA)) })

    ppng(file.path(dir, "logo.png"), width=4, height=4.67, unit="in")
    # dev.new(width=4, height=4.67, unit="in")
    grid.newpage()
    vp <- viewport(y=unit(2.67, "in"), width=unit(4, "in"), height=unit(4, "in"))
    pushViewport(vp)
    grid.arrange(grobs=l_tiles, nrow=2, newpage=FALSE, padding=0)
    grid.arrange(grobs=l_squares, nrow=2, newpage=FALSE, padding=0)
    upViewport()
    vp <- viewport(y=unit(0.335, "in"), width=unit(4, "in"), height=unit(0.67, "in"))
    pushViewport(vp)
    grid.arrange(grobs=l_coins, nrow=1, newpage=FALSE, padding=0)
    grid.arrange(grobs=l_inverse_circles, nrow=1, newpage=FALSE, padding=0)
    grid.arrange(grobs=l_circles, nrow=1, newpage=FALSE, padding=0)
    upViewport()
    dev.off()

}

.to_png_simple <- function(f) { png <- png::readPNG(f) }

#' @export
make_images <- function(arg) {
    arg$directory <- sprintf("png/%s", arg$suit_family)
    cat(arg$directory,"\n")
    if (dir.exists(arg$directory)) { unlink(arg$directory, recursive=TRUE) }
    dir.create(arg$directory)
    make_tiles(arg)
    make_stickers(arg)
    make_pawns(arg)
    make_logo(arg$directory)
    invisible(NULL)
}

sticker_filename <- function(component, suit_name, rank_name) {
    filename <- sprintf("s_%s_%s_%s.png", component, suit_name, rank_name)
}


tile_filename <- function(component, suit_name, rank_name) {
    filename <- sprintf("t_%s_%s_%s.png", component, suit_name, rank_name)
}
pawn_filename <- function(component, suit_name, rank_name="") {
    filename <- sprintf("p_%s_%s_%s.png", component, suit_name, rank_name)
}

tile_png <- function(filename) {
    tile_px = 675
    ppng(filename, width=tile_px, height=tile_px)
}

add_bleed_lines <- function(arg, component, add_safezone=TRUE) {
    offset <- switch(component, tile = 40 / 675, card=0.05, sticker=0.1)
    bl_alpha <- 0.5
    bl_size <- 2
    gp_bleed <- gpar(col=arg$neutral_col1, lwd=bl_size, alpha=bl_alpha)
    gp_safe <- gpar(col=arg$neutral_col2, lwd=bl_size, alpha=bl_alpha)
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
gl_col <- neutral_col1
gl_size <- 4
hl_col <- neutral_col1
hl_size <- 0.5

ho <- 0.33

add_hexlines <- function(arg, omit_direction=FALSE, hl_col=arg$neutral_col1) {
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

make_pawns <- function(arg) {
    # Pawn
    for (i_s in seq(along=arg$suit_names)) {
        suit_name <- arg$suit_names[i_s]
        suit_color <- arg$suit_colors[i_s]
        suit_symbol <- arg$suit_symbols[i_s]
        if (arg$inverted) {
            bcol <- suit_color
            scol <- arg$background
        } else {
            bcol <- arg$background
            scol <- suit_color
        }

        filename <- file.path(arg$directory, pawn_filename("pawn", suit_name))
        ppng(filename, width=0.75,  height=4.5, unit="in")
        grid.newpage()
        grid.rect(gp = gpar(fill=bcol))
        gp_tr <- gpar(col=scol, fontsize=40)
        grid.text(suit_symbol, y=0.65, rot=180, gp=gp_tr)
        grid.text(suit_symbol, y=0.35, rot=0, gp=gp_tr)
        grid.lines(y=0.5, gp=gpar(col=arg$neutral_col1, lty="dashed"))
        grid.lines(y=0.1, gp=gpar(col=arg$neutral_col1, lty="dashed"))
        grid.lines(y=0.9, gp=gpar(col=arg$neutral_col1, lty="dashed"))
        grid.rect(gp = gpar(col=arg$neutral_col1, fill=NA))
        dev.off()

        filename <- file.path(arg$directory, pawn_filename("belt", suit_name))
        ppng(filename, width=1.5,  height=0.5, unit="in")
        # try(dev.off())
        # dev.new(width=1.5, height=0.5)
        grid.newpage()
        grid.rect(gp = gpar(fill=bcol))
        gp_tr <- gpar(col=scol, fontsize=20)
        grid.text(suit_symbol, gp=gp_tr)
        grid.lines(y=0.9, gp=gpar(col=scol, lwd=8))
        grid.lines(y=0.1, gp=gpar(col=scol, lwd=8))
        grid.rect(gp = gpar(col=arg$neutral_col1, fill=NA))
        dev.off()

        # if (interactive()) { browser() }
    }
}


make_tiles <- function(arg) {
    o_neutral_col <- arg$neutral_col1
    fs_tr <- 110
    fs_ts <- 60
    #### Joker tile
    if (arg$standardish) {
        neutral_col <- o_neutral_col
    } else {
        neutral_col <- arg$neutral_col1
    }
    if (arg$inverted) {
        bcol <- neutral_col
        scol <- arg$background
    } else {
        bcol <- arg$background
        scol <- neutral_col
    }
    filename <- file.path(arg$directory, tile_filename("front", "joker", ""))
    tile_png(filename)
    grid.newpage()
    grid.rect(gp = gpar(fill=bcol))
    if (arg$add_checkers) {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(fill=arg$neutral_col1))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(fill=arg$neutral_col1))
    }
    if (arg$add_bleed_lines) {
        add_bleed_lines(arg, "tile")
    }
    if (arg$standardish) {
        gp_tr <- gpar(col=scol, fontsize=fs_ts)
        grid.text(arg$joker_symbol, x=0.25, y=.75, gp=gp_tr)
        grid.text(arg$joker_symbol, x=0.75, y=.25, gp=gp_tr)

        if (arg$add_checkers) {
            gp_tr <- gpar(col=bcol, fontsize=fs_ts)
            grid.text(arg$joker_symbol, x=0.25, y=.25, gp=gp_tr)
            grid.text(arg$joker_symbol, x=0.75, y=.75, gp=gp_tr)
        } else {
            grid.text(arg$joker_symbol, x=0.25, y=.25, gp=gp_tr)
            grid.text(arg$joker_symbol, x=0.75, y=.75, gp=gp_tr)
        }

        # gp_tr <- gpar(col=scol, fontsize=180)
        # grid.text(arg$joker_symbol, 0.25, 0.75, gp=gp_tr)
    } else {
        #### Do lots more stuff
        if (arg$inverted) 
            add_hexlines(arg, hl_col=arg$background)
        else 
            add_hexlines(arg)
         
        gp_tr <- gpar(col=scol, fontsize=fs_ts)
        grid.text(arg$joker_symbol, x=0.25, y=.75, gp=gp_tr)
        grid.text(arg$joker_symbol, x=0.75, y=.25, gp=gp_tr)

        if (arg$add_checkers) {
            gp_tr <- gpar(col=bcol, fontsize=fs_ts)
            grid.text(arg$joker_symbol, x=0.25, y=.25, gp=gp_tr)
            grid.text(arg$joker_symbol, x=0.75, y=.75, gp=gp_tr)
        } else {
            grid.text(arg$joker_symbol, x=0.25, y=.25, gp=gp_tr)
            grid.text(arg$joker_symbol, x=0.75, y=.75, gp=gp_tr)
        }
        # gp_tr <- gpar(col=scol, fontsize=fs_tr)
        # grid.text(arg$joker_symbol, gp=gp_tr)

        # gp_tr <- gpar(col=scol, fontsize=180)
        # grid.text(arg$joker_symbol, 0.25, 0.75, gp=gp_tr)
    }
    dev.off()

    filename <- file.path(arg$directory, tile_filename("back", "", ""))
    tile_png(filename)
    grid.newpage()
    grid.rect(gp = gpar(fill=arg$background))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "tile")
    if (arg$standardish) {
        gp_gl <- gpar(col=o_neutral_col, lwd=10)
    } else {
        gp_gl <- gpar(col=arg$neutral_col1, lwd=10)
        add_hexlines(arg)
    }
    grid.lines(x=0.5, gp=gp_gl)
    grid.lines(y=0.5, gp=gp_gl)
    dev.off()

    # Tile front
    for (i_s in seq(along=arg$suit_names)) {
        for (i_r in seq(along=arg$rank_names)) {
            suit_name <- arg$suit_names[i_s]
            rank_name <- arg$rank_names[i_r]
            suit_symbol <- arg$suit_symbols[i_s]
            suit_color <- arg$suit_colors[i_s]
            if (rank_name == "a" && arg$use_suit_as_ace)
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

            filename <- file.path(arg$directory, tile_filename("front", suit_name, rank_name))
            tile_png(filename)
            grid.newpage()
            grid.rect(gp = gpar(fill=bcol))
            if (arg$add_bleed_lines)
                add_bleed_lines(arg, "tile")
            if (arg$add_checkers) {
                grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(fill=arg$neutral_col1))
                grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(fill=arg$neutral_col1))
            }
            if (arg$standardish) {
                gp_tr <- gpar(col=scol, fontsize=fs_tr)
                grid.text(rank_symbol, gp=gp_tr)

                gp_ts <- gpar(col=scol, fontsize=fs_ts)
                vp <- viewport(x=0.25, y=0.75, width=0.5, height=0.5)
                pushViewport(vp)
                grid.text(suit_symbol, gp=gp_ts)
                upViewport()
            } else {
                #### Do lots more stuff
                add_hexlines(arg)
                gp_tr <- gpar(col=scol, fontsize=fs_tr)
                grid.text(rank_symbol, gp=gp_tr)

                gp_ts <- gpar(col=scol, fontsize=fs_ts)
                vp <- viewport(x=0.25, y=0.75, width=0.5, height=0.5)
                pushViewport(vp)
                grid.text(suit_symbol, gp=gp_ts)
                upViewport()
            }

            # gl_col <- arg$neutral_col1
            # gl_size <- 4
            # gp_gl <- gpar(col=gl_col, lwd=gl_size)
            # grid.lines(x=0.5, gp=gp_gl)
            # grid.lines(y=0.5, gp=gp_gl)
            dev.off()
        }
    }
}

add_directional_marker <- function(arg, back=TRUE) {
    o_neutral_col <- arg$neutral_col1
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
    grid.lines(x=c(0.75, 1), y=c(0.75, 1), gp=gpar(col=o_neutral_col, lwd=10))
}

make_stickers <- function(arg) {
    o_neutral_col <- arg$neutral_col1

    circle_lwd <- 22
    cr <- 0.45

    for (i_s in seq(along=arg$suit_names)) {
        for (i_r in seq(along=arg$rank_names)) {
            suit_name <- arg$suit_names[i_s]
            rank_name <- arg$rank_names[i_r]
            suit_symbol <- arg$suit_symbols[i_s]
            suit_color <- arg$suit_colors[i_s]
            rank_symbol <- arg$rank_symbols[i_r]
            if (rank_name == "a" && arg$use_suit_as_ace)
                ace_symbol <- suit_symbol
            else
                ace_symbol <- rank_symbol

            if (arg$inverted) {
                bcol <- suit_color
                scol <- arg$background
            } else {
                bcol <- arg$background
                scol <- suit_color
            }

            od_fontsize <- 50 # orthodox die font size
            rd_fontsize <- od_fontsize # reform die font size
            oc_fontsize <- 50 # orthodox coin font size
            rc_fontsize <- oc_fontsize # reform coin font size


            if (arg$standardish) {

                # tile value
                filename <- file.path(arg$directory, sticker_filename("tile_value", suit_name, rank_name))
                sticker_png(filename)
                grid.newpage()
                grid.rect(gp = gpar(fill=bcol))
                if (arg$add_bleed_lines)
                    add_bleed_lines(arg, "sticker")
                symbol <- ifelse(rank_name == "a", ace_symbol, rank_symbol)
                grid.text(symbol, gp = gpar(col=scol, fontsize=od_fontsize) )
                dev.off()
            }


            # die
            if (arg$standardish) {

                filename <- file.path(arg$directory, sticker_filename("die", suit_name, rank_name))
                sticker_png(filename)
                grid.newpage()
                grid.rect(gp = gpar(fill=bcol))
                if (arg$add_bleed_lines)
                    add_bleed_lines(arg, "sticker")
                symbol <- ifelse(rank_name == "a", suit_symbol, rank_symbol)
                grid.text(symbol, gp = gpar(col=scol, fontsize=od_fontsize) )
                dev.off()

            } else {

                #### Add suit
                filename <- file.path(arg$directory, sticker_filename("die", suit_name, rank_name))
                sticker_png(filename)
                grid.newpage()
                grid.rect(gp = gpar(fill=bcol))
                if (arg$add_bleed_lines)
                    add_bleed_lines(arg, "sticker")
                grid.text(rank_symbol, gp = gpar(col=scol, fontsize=rd_fontsize))
                dev.off()

            }


            # chip value
            if (arg$standardish) {
                filename <- file.path(arg$directory, sticker_filename("chip_value", suit_name, rank_name))
                sticker_png(filename)
                grid.newpage()
                grid.rect(gp = gpar(fill=bcol))
                # grid.circle(r=cr, gp=gpar(col=scol, lwd=circle_lwd, fill=bcol))
                if (arg$add_bleed_lines)
                    add_bleed_lines(arg, "sticker")
                grid.text(rank_symbol, rot=-45, gp = gpar(col=scol, fontsize=oc_fontsize) )
                add_directional_marker(arg, back=FALSE)
                dev.off()

            } else {

                filename <- file.path(arg$directory, sticker_filename("chip_value", suit_name, rank_name))
                sticker_png(filename)
                grid.newpage()
                grid.rect(gp = gpar(fill=bcol))
                # grid.circle(r=cr, gp=gpar(col=scol, lwd=circle_lwd, fill=bcol))
                if (arg$add_bleed_lines)
                    add_bleed_lines(arg, "sticker")
                grid.text(rank_symbol, rot=-45, gp = gpar(col=scol, fontsize=rc_fontsize) )
                add_directional_marker(arg, back=FALSE)
                dev.off()

            }
        }
        # tile suite
        if (arg$standardish) {
            filename <- file.path(arg$directory, sticker_filename("tile_suite", suit_name, ""))
            sticker_png(filename)
            grid.newpage()
            grid.rect(gp = gpar(fill=bcol))
            if (arg$add_bleed_lines)
                add_bleed_lines(arg, "sticker")
            grid.text(suit_symbol, gp = gpar(col=scol, fontsize=od_fontsize) )
            dev.off()
        }

        # coin suit
        if (arg$standardish) {
            filename <- file.path(arg$directory, sticker_filename("coin_suit", suit_name, ""))
            sticker_png(filename)
            grid.newpage()
            grid.rect(gp = gpar(fill=bcol))
            if (arg$add_bleed_lines)
                add_bleed_lines(arg, "sticker")
            grid.text(suit_symbol, rot=-45, gp = gpar(col=scol, fontsize=oc_fontsize) )
            add_directional_marker(arg, back=TRUE)
            dev.off()

        } else {
            filename <- file.path(arg$directory, sticker_filename("coin_suit", suit_name, ""))
            sticker_png(filename)
            grid.newpage()
            grid.rect(gp = gpar(fill=bcol))
            if (arg$add_bleed_lines)
                add_bleed_lines(arg, "sticker")
            grid.text(suit_symbol, rot=-45, gp = gpar(col=scol, fontsize=rc_fontsize) )
            add_directional_marker(arg, back=TRUE)
            dev.off()
        }

        # chip suit
        if (arg$standardish) {
            filename <- file.path(arg$directory, sticker_filename("chip_suit", suit_name, ""))
            sticker_png(filename)
            grid.newpage()
            grid.rect(gp = gpar(fill=bcol))
            # grid.circle(r=cr, gp=gpar(col=scol, lwd=circle_lwd, fill=bcol))
            if (arg$add_bleed_lines)
                add_bleed_lines(arg, "sticker")
            grid.text(suit_symbol, rot=-45, gp = gpar(col=scol, fontsize=oc_fontsize) )
            add_directional_marker(arg, back=TRUE)
            dev.off()

        } else {
            filename <- file.path(arg$directory, sticker_filename("chip_suit", suit_name, ""))
            sticker_png(filename)
            grid.newpage()
            grid.rect(gp = gpar(fill=bcol))
            # grid.circle(r=cr, gp=gpar(col=scol, lwd=circle_lwd, fill=bcol))
            if (arg$add_bleed_lines)
                add_bleed_lines(arg, "sticker")
            grid.text(suit_symbol, rot=-45, gp = gpar(col=scol, fontsize=rc_fontsize) )
            add_directional_marker(arg, back=TRUE)
            dev.off()
        }

        # suit dice
        filename <- file.path(arg$directory, sticker_filename("suit_die", suit_name, i_s))
        sticker_png(filename)
        grid.newpage()
        grid.rect(gp = gpar(fill=bcol))
        if (arg$add_bleed_lines)
            add_bleed_lines(arg, "sticker")
        grid.text(suit_symbol, gp = gpar(col=scol, fontsize=od_fontsize) )
        dev.off()

        # pawn saucer suit
        filename <- file.path(arg$directory, sticker_filename("saucer_suit", suit_name, ""))
        sticker_png(filename)
        grid.newpage()
        grid.rect(gp = gpar(fill=bcol))
        if (arg$add_bleed_lines)
            add_bleed_lines(arg, "sticker")
        grid.text(suit_symbol, rot=-45, gp = gpar(col=scol, fontsize=oc_fontsize) )
        add_directional_marker(arg, back=TRUE)
        dev.off()


    }
    # pawn saucer hidden
    filename <- file.path(arg$directory, sticker_filename("saucer_hidden", "", ""))
    sticker_png(filename)
    grid.newpage()
    grid.rect(gp = gpar(fill=arg$background))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    add_directional_marker(arg, back=FALSE)
    dev.off()

    # suit dice
    filename <- file.path(arg$directory, sticker_filename("suit_die", "joker", ""))
    sticker_png(filename)
    grid.newpage()
    grid.rect(gp = gpar(fill=arg$background))
    if (arg$standardish) {
        color <- o_neutral_col
    } else {
        color <- arg$neutral_col1
    }
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    if (arg$inverted) {
        grid.rect(gp = gpar(fill=color))
        grid.text(arg$joker_symbol, gp = gpar(col=arg$background, fontsize=od_fontsize) )
    } else {
        grid.rect(gp = gpar(fill=arg$background))
        grid.text(arg$joker_symbol, gp = gpar(col=color, fontsize=od_fontsize) )
    }
    dev.off()

    # suit/rank ace
    filename <- file.path(arg$directory, sticker_filename("suit_die", "ace", ""))
    sticker_png(filename)
    grid.newpage()
    grid.rect(gp = gpar(fill=arg$background))
    if (arg$standardish)
        symbol <- arg$joker_symbol
    else
        symbol <- arg$rank_symbols[2]
    if (arg$standardish) {
        color <- o_neutral_col
    } else {
        color <- arg$neutral_col1
    }
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    if (arg$inverted) {
        grid.rect(gp = gpar(fill=color))
        grid.text(symbol, gp = gpar(col=arg$background, fontsize=od_fontsize) )
    } else {
        grid.rect(gp = gpar(fill=arg$background))
        grid.text(symbol, gp = gpar(col=color, fontsize=od_fontsize) )
    }
    dev.off()

    # rank dice
    for (i_r in seq(along=arg$rank_symbols)) {
        rank_symbol <- arg$rank_symbols[i_r]
        rank_name <- arg$rank_names[i_r]
        if (rank_name == "a" && arg$use_suit_as_ace)
            rank_symbol <- arg$joker_symbol
        filename <- file.path(arg$directory, sticker_filename("rank_die", "", rank_name))
        sticker_png(filename)
        grid.newpage()
        if (arg$standardish) {
            color <- o_neutral_col
        } else {
            color <- arg$neutral_col1
        }
        if (arg$add_bleed_lines)
            add_bleed_lines(arg, "sticker")
        grid.rect(gp = gpar(fill=arg$background))
        grid.text(rank_symbol, gp = gpar(col=color, fontsize=od_fontsize) )
        dev.off()

        # coin value
        if (arg$standardish) {
            filename <- file.path(arg$directory, sticker_filename("coin_value", "", rank_name))
            if(!file.exists(filename)) {
                sticker_png(filename)
                grid.newpage()
                grid.rect(gp = gpar(fill=arg$background))
                if (arg$add_bleed_lines)
                    add_bleed_lines(arg, "sticker")
                grid.text(rank_symbol, rot=-45, gp = gpar(col=o_neutral_col, fontsize=oc_fontsize) )
                add_directional_marker(arg, back=FALSE)
                dev.off()
            }

        } else {

            filename <- file.path(arg$directory, sticker_filename("coin_value", "", rank_name))
            if(!file.exists(filename)) {
                sticker_png(filename)
                grid.newpage()
                grid.rect(gp = gpar(fill=arg$background))
                if (arg$add_bleed_lines)
                    add_bleed_lines(arg, "sticker")
                grid.text(rank_symbol, rot=-45, gp = gpar(col=arg$neutral_col1, fontsize=rc_fontsize) )
                add_directional_marker(arg, back=FALSE)
                dev.off()
            }
        }
    }

    filename <- file.path(arg$directory, sticker_filename("suit_die", "null", ""))
    sticker_png(filename)
    grid.newpage()
    grid.rect(gp = gpar(fill=arg$background))
    if (arg$add_bleed_lines)
        add_bleed_lines(arg, "sticker")
    dev.off()

    # tile back
    if (arg$standardish) {
        filename <- file.path(arg$directory, sticker_filename("tile_back", "", ""))
        sticker_png(filename)
        grid.newpage()
        grid.rect(gp = gpar(fill=bcol))
        if (arg$add_bleed_lines)
            add_bleed_lines(arg, "sticker")

        grid.lines(x=0.5, gp=gpar(col=o_neutral_col, lwd=8))
        grid.lines(y=0.5, gp=gpar(col=o_neutral_col, lwd=8))
        dev.off()
    }
}
