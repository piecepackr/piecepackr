seg <- function(x, y, xend, yend, color="black", size=50, ...) {
    grid.segments(x0=x, y0=y, x1=xend, y1=yend, gp=gpar(col=color, lwd=size))
}

ppsvg <- function(...) { svg(..., family=c("Symbola")) }

addViewport <- function(...) { 
    suppressWarnings(pushViewport(viewport(..., clip="on")))
    upViewport()
}

grid.inversecircle <- function() {
    x_c <- 0.5 + 0.5*cos(seq(0, 2*pi, length.out=100))
    y_c <- 0.5 + 0.5*sin(seq(0, 2*pi, length.out=100))
    x_r <- c(1, 1, 0, 0, 1, 1)
    y_r <- c(0.5, 0, 0, 1, 1, 0.5)
    grid.polygon(x = c(x_c, x_r), y=c(y_c, y_r), gp=gpar(fill="white", col="white"))
}

get_background_color_helper <- function(component, i_s, arg) {
    suited <- is_suited(component)
    if (suited && !is.null(arg$background_color.suited)) {
        arg$background_color.suited
    } else if (!suited && !is.null(arg$background_color.unsuited)) {
        arg$background_color.unsuited
    } else {
        arg$background_color
    }
}

get_suit_color_helper <- function(component, i_s, arg) {
    arg$suit_colors[i_s]
}

get_background_color <- function(component, i_s, arg) {
    bcol <- get_background_color_helper(component, i_s, arg)
    scol <- get_suit_color_helper(component, i_s, arg)
    if (should_invert(component, arg))
        scol
    else
        bcol
}

get_suit_color <- function(component, i_s, arg) {
    bcol <- get_background_color_helper(component, i_s, arg)
    scol <- get_suit_color_helper(component, i_s, arg)
    if (should_invert(component, arg))
        bcol
    else
        scol
}

get_suit_symbol <- function(component, i_s, arg) {
    suit_symbol <- arg$suit_symbols[i_s]
    suit_symbol
}

get_rank_symbol <- function(component, i_s, i_r, arg) {
    if (i_r == 2 && arg$use_suit_as_ace)
        rank_symbol <- arg$suit_symbols[i_s]
    else
        rank_symbol <- arg$rank_symbols[i_r]
    rank_symbol
}       

get_style <- function(component, arg) {
    style <- arg$style
    style
}

get_component_opt <- function(component, i_s, i_r, arg) {
    style <- get_style(component, arg)
    bcol <- get_background_color(component, i_s, arg)
    scol <- get_suit_color(component, i_s, arg)
    hexline_col <- get_suit_color(component, 5, arg) #### use scol? or add a style?
    checker_col <- arg$suit_colors[5] #### won't show hex lines
    rank_symbol <- get_rank_symbol(component, i_s, i_r, arg)
    suit_symbol <- get_suit_symbol(component, i_s, arg)
    list(style=style, bcol=bcol, scol=scol, hexline_col=hexline_col, 
         checker_col=checker_col, rank_symbol=rank_symbol, suit_symbol=suit_symbol)
}

should_invert <- function(component, arg) {
    suited <- is_suited(component)
    if (suited && !is.null(arg$invert_colors.suited)) {
        arg$invert_colors.suited
    } else if (!suited && !is.null(arg$invert_colors.unsuited)) {
        arg$invert_colors.unsuited
    } else {
        arg$invert_colors
    }
}

is_suited <- function(component) {
    switch(component,
           tile_back = FALSE,
           tile_face = TRUE, #### joker_tile_face?
           coin_back = TRUE,
           coin_face = FALSE,
           die = TRUE,
           rank_die = FALSE,
           suit_die = TRUE,
           suit_rank_die = TRUE, #### null
           saucer_face = TRUE,
           saucer_back = FALSE,
           pawn = TRUE,
           belt = TRUE,
           chip_face = TRUE,
           chip_back = TRUE)
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
        draw_tile_face(ii, 2, arg)
    }
    seekViewport(paste0("coin.1"))
    draw_coin_value(1, arg)
    seekViewport(paste0("coin.2"))
    draw_coin_value(2, arg)
    seekViewport(paste0("coin.3"))
    draw_coin_back(4, arg)
    seekViewport(paste0("coin.4"))
    draw_coin_value(3, arg)
    seekViewport(paste0("coin.5"))
    draw_coin_back(2, arg)
    seekViewport(paste0("coin.6"))
    draw_coin_back(1, arg)

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
    opt <- get_component_opt("pawn", i_s, 1, arg)
    neutral_col <- arg$suit_colors[5]

    grid.rect(gp = gpar(fill=opt$bcol))
    gp_tr <- gpar(col=opt$scol, fontsize=40)
    grid.text(opt$suit_symbol, y=0.65, rot=180, gp=gp_tr)
    grid.text(opt$suit_symbol, y=0.35, rot=0, gp=gp_tr)
    grid.lines(y=0.5, gp=gpar(col=neutral_col, lty="dashed"))
    grid.lines(y=0.1, gp=gpar(col=neutral_col, lty="dashed"))
    grid.lines(y=0.9, gp=gpar(col=neutral_col, lty="dashed"))
    grid.rect(gp = gpar(col=neutral_col, fill=NA))

    invisible(NULL)
}

draw_pawn_belt <- function(i_s, arg) {
    opt <- get_component_opt("belt", i_s, 1, arg)
    neutral_col <- arg$suit_colors[5]

    grid.rect(gp = gpar(fill=opt$bcol))
    gp_tr <- gpar(col=opt$scol, fontsize=20)
    grid.text(opt$suit_symbol, gp=gp_tr)
    grid.lines(y=0.9, gp=gpar(col=opt$scol, lwd=8))
    grid.lines(y=0.1, gp=gpar(col=opt$scol, lwd=8))
    grid.rect(gp = gpar(col=neutral_col, fill=NA))

    invisible(NULL)
}

draw_joker_tile_face <- function(arg, draw_border = TRUE) {
    opt <- get_component_opt("tile_face", 5, 1, arg) #### joker_tile_face?

    #### Add option to use each of four suits instead?
    symbol.1 <- get_suit_symbol("tile_face", 5, arg) 
    symbol.2 <- get_suit_symbol("tile_face", 5, arg)
    symbol.3 <- get_suit_symbol("tile_face", 5, arg)
    symbol.4 <- get_suit_symbol("tile_face", 5, arg)
    grid.rect(gp = gpar(fill=opt$bcol))
    if (arg$add_checkers) {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(fill=opt$checker_col))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(fill=opt$checker_col))
    }
    if (opt$style == "simple_hex") {
        add_hexlines(arg, hl_col=opt$hexline_col)
    }
    gp_tr <- gpar(col=opt$scol, fontsize=fs_ts)
    grid.text(symbol.1, x=0.25, y=.75, gp=gp_tr)
    grid.text(symbol.3, x=0.75, y=.25, gp=gp_tr)

    if (arg$add_checkers) 
        gp_tr <- gpar(col=opt$bcol, fontsize=fs_ts)
    grid.text(symbol.4, x=0.25, y=.25, gp=gp_tr)
    grid.text(symbol.2, x=0.75, y=.75, gp=gp_tr)

    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

fs_tr <- 72 #### font size tile rank
fs_ts <- 32  #### font size tile suit

draw_tile_back <- function(arg, draw_border = TRUE) {
    opt <- get_component_opt("tile_back", 5, 1, arg)
    
    grid.rect(gp = gpar(fill=opt$bcol))
    if (opt$style == "simple_hex") 
        add_hexlines(arg, hl_col=opt$hexline_col)
    gp_gl <- gpar(col=opt$scol, lwd=8, lineend="square")
    grid.lines(x=0.5, gp=gp_gl)
    grid.lines(y=0.5, gp=gp_gl)
    if (draw_border)
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_tile_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("tile_face", i_s, i_r, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    if (arg$add_checkers) {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(fill=opt$checker_col))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(fill=opt$checker_col))
    }
    if (opt$style == "simple_hex")
        add_hexlines(arg, hl_col=opt$hexline_col) 

    gp_tr <- gpar(col=opt$scol, fontsize=fs_tr)
    grid.text(opt$rank_symbol, gp=gp_tr)

    gp_ts <- gpar(col=opt$scol, fontsize=fs_ts)
    vp <- viewport(x=0.25, y=0.75, width=0.5, height=0.5)
    pushViewport(vp)
    grid.text(opt$suit_symbol, gp=gp_ts)
    upViewport()

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

            filename <- file.path(arg$directory, tile_filename("front", i_s, i_r))
            ppsvg(filename)
            grid.newpage()
            draw_tile_face(i_s, i_r, arg)
            dev.off()
        }
    }
}

add_directional_marker <- function(component, i_s, arg) {
    scol <- get_suit_color(component, i_s, arg)
    neutral_col <- arg$suit_colors[5]

    if (arg$directional_marker == "none") {
        dcol <- NA
    } else if (arg$directional_marker == "neutral") {
        dcol <- neutral_col
    } else {
        dcol <- scol
    }
    grid.lines(x=c(0.5, 0.5), y=c(0.90, 0.95), gp=gpar(col=dcol, lwd=10))
}

od_fontsize <- 30 # orthodox die font size
oc_fontsize <- 30 # orthodox coin font size

draw_suit_die_face <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("suit_die", i_s, 1, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    grid.text(opt$suit_symbol, gp = gpar(col=opt$scol, fontsize=od_fontsize) )
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}


draw_pawn_saucer_suit <- function(i_s, arg, draw_border = TRUE) {
    component <- ifelse(i_s < 5, "saucer_face", "saucer_back")
    opt <- get_component_opt(component, i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    grid.text(opt$suit_symbol, gp = gpar(col=opt$scol, fontsize=oc_fontsize) )
    add_directional_marker(component, i_s, arg)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_rankdie <- function(i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("rank_die", 5, i_r, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    grid.text(opt$rank_symbol, gp = gpar(col=opt$scol, fontsize=od_fontsize) )
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_suitdie_null <- function(arg, draw_border = TRUE) {
    grid.rect(gp = gpar(fill=arg$background_color)) #### don't use get_background_color?
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_coin_value <- function(i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("coin_face", 5, i_r, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    grid.text(opt$rank_symbol, gp = gpar(col=opt$scol, fontsize=oc_fontsize) )
    add_directional_marker("coin_face", 5, arg)
    if (draw_border) {
        grid.circle(gp=gpar(col="grey", fill=NA))
    }
    invisible(NULL)
}

draw_die_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("die", i_s, i_r, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    grid.text(opt$rank_symbol, gp = gpar(col=opt$scol, fontsize=od_fontsize))
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_chip_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("chip_face", i_s, i_r, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    # grid.circle(r=cr, gp=gpar(col=opt$scol, lwd=circle_lwd, fill=bcol))
    grid.text(opt$rank_symbol, gp = gpar(col=opt$scol, fontsize=oc_fontsize) )
    add_directional_marker("chip_face", i_s, arg)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_coin_back <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("coin_back", i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    grid.text(opt$suit_symbol, gp = gpar(col=opt$scol, fontsize=oc_fontsize) )
    add_directional_marker("coin_back", i_s, arg)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_chip_back <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("chip_back", i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    # grid.circle(r=cr, gp=gpar(col=opt$scol, lwd=circle_lwd, fill=opt$bcol))
    grid.text(opt$suit_symbol, gp = gpar(col=opt$scol, fontsize=oc_fontsize))
    add_directional_marker("chip_back", i_s, arg)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}
