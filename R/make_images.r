COMPONENTS <- c("tile_back", "tile_face", #### joker_tile_face?
           "coin_back", "coin_face",
           "piecepack_die", "rank_die", "suit_die", "suit_rank_die", #### null
           "saucer_face", "saucer_back",
           "pawn", "pawn_belt",
           "chip_face", "chip_back")

seg <- function(x, y, xend, yend, color="black", size=50, ...) {
    grid.segments(x0=x, y0=y, x1=xend, y1=yend, gp=gpar(col=color, lwd=size))
}

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
    component_str <- paste0("background_color.", component)
    if (!is.null(arg[[component_str]])) {
        arg[[component_str]]
    } else if (suited && !is.null(arg$background_color.suited)) {
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

should_invert <- function(component, arg) {
    suited <- is_suited(component)
    component_str <- paste0("invert_colors.", component)
    if (!is.null(arg[[component_str]])) {
        arg[[component_str]]
    } else if (suited && !is.null(arg$invert_colors.suited)) {
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
           piecepack_die = TRUE,
           rank_die = FALSE,
           suit_die = TRUE,
           suit_rank_die = TRUE, #### null
           saucer_face = TRUE,
           saucer_back = FALSE,
           pawn = TRUE,
           pawn_belt = TRUE,
           chip_face = TRUE,
           chip_back = TRUE)
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

get_rank_symbols <- function(component, arg) {
    component_str <- paste0("rank_symbols.", component)
    if (!is.null(arg[[component_str]]))
        arg[[component_str]]
    else
        arg$rank_symbols
}
get_suit_symbols <- function(component, arg) {
    component_str <- paste0("suit_symbols.", component)
    if (!is.null(arg[[component_str]]))
        arg[[component_str]]
    else
        arg$suit_symbols
}
get_use_suit_as_ace <- function(component, arg) {
    component_str <- paste0("use_suit_as_ace.", component)
    if (!is.null(arg[[component_str]]))
        arg[[paste0("use_suit_as_ace.", component)]]
    else
        arg$use_suit_as_ace
}

get_rank_symbol <- function(component, i_s, i_r, arg) {
    rank_symbols <- get_rank_symbols(component, arg)
    suit_symbols <- get_suit_symbols(component, arg)
    use_suit_as_ace <- get_use_suit_as_ace(component, arg)
    if (i_r == 2 && use_suit_as_ace)
        rank_symbol <- suit_symbols[i_s]
    else
        rank_symbol <- rank_symbols[i_r]
    rank_symbol
}       

get_style <- function(component, arg) {
    style <- arg$style
    style
}

get_suit_fontsize <- function(component, arg) {
    fs <- switch(component,
                 "pawn_belt" = 20,
                 "chip_back" = 30,
                 "coin_back" = 30,
                 "pawn" = 40,
                 "saucer_back" = 30,
                 "saucer_face" = 30,
                 "suit_die" = 30,
                 "tile_face" = 32,
                 30)
    fs
}

get_rank_fontsize <- function(component, arg) {
    fs <- switch(component,
                 "piecepack_die" = 30,
                 "chip_face" = 30,
                 "coin_face" = 30,
                 "rank_die" = 30,
                 "tile_face" = 72,
                 30)
    fs
}

get_component_opt <- function(component, i_s, i_r, arg) {
    style <- get_style(component, arg)
    bcol <- get_background_color(component, i_s, arg)
    scol <- get_suit_color(component, i_s, arg)
    hexline_col <- get_suit_color(component, opts$i_unsuit, arg) #### use scol? or add a style?
    checker_col <- arg$suit_colors[opts$i_unsuit] #### won't show hex lines
    rank_symbol <- get_rank_symbol(component, i_s, i_r, arg)
    rank_fontsize <- get_rank_fontsize(component, arg)
    suit_symbol <- get_suit_symbol(component, i_s, arg)
    suit_fontsize <- get_suit_fontsize(component, arg)
    list(style=style, bcol=bcol, scol=scol, 
         hexline_col=hexline_col, checker_col=checker_col, 
         rank_symbol=rank_symbol, rank_fontsize=rank_fontsize,
         suit_symbol=suit_symbol, suit_fontsize=suit_fontsize)
}


make_preview <- function(arg) {
    svg(file.path(arg$directory, "preview.svg"), 
        family=arg$family, width=4, height=4.67)
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
    for (ii in 1:min(4, arg$n_suits)) {
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
    dir.create(arg$directory, showWarnings=FALSE)
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

draw_pawn <- function(i_s, arg) {
    opt <- get_component_opt("pawn", i_s, 1, arg)
    neutral_col <- arg$suit_colors[5]

    grid.rect(gp = gpar(fill=opt$bcol))
    gp_tr <- gpar(col=opt$scol, fontsize=opt$suit_fontsize)
    grid.text(opt$suit_symbol, y=0.65, rot=180, gp=gp_tr)
    grid.text(opt$suit_symbol, y=0.35, rot=0, gp=gp_tr)
    grid.lines(y=0.5, gp=gpar(col=neutral_col, lty="dashed"))
    grid.lines(y=0.1, gp=gpar(col=neutral_col, lty="dashed"))
    grid.lines(y=0.9, gp=gpar(col=neutral_col, lty="dashed"))
    grid.rect(gp = gpar(col=neutral_col, fill=NA))

    invisible(NULL)
}

draw_pawn_belt <- function(i_s, arg) {
    opt <- get_component_opt("pawn_belt", i_s, 1, arg)
    neutral_col <- arg$suit_colors[5]

    grid.rect(gp = gpar(fill=opt$bcol))
    gp_tr <- gpar(col=opt$scol, fontsize=opt$suit_fontsize)
    grid.text(opt$suit_symbol, gp=gp_tr)
    grid.lines(y=0.9, gp=gpar(col=opt$scol, lwd=8))
    grid.lines(y=0.1, gp=gpar(col=opt$scol, lwd=8))
    grid.rect(gp = gpar(col=neutral_col, fill=NA))

    invisible(NULL)
}

draw_joker_tile_face <- function(arg, draw_border = TRUE) {
    opt <- get_component_opt("tile_face", opts$i_unsuit, 1, arg) #### joker_tile_face?

    #### Add option to use each of four suits instead?
    symbol.1 <- get_suit_symbol("tile_face", opts$i_unsuit, arg) 
    symbol.2 <- get_suit_symbol("tile_face", opts$i_unsuit, arg)
    symbol.3 <- get_suit_symbol("tile_face", opts$i_unsuit, arg)
    symbol.4 <- get_suit_symbol("tile_face", opts$i_unsuit, arg)
    grid.rect(gp = gpar(fill=opt$bcol))
    if (arg$add_checkers) {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(fill=opt$checker_col))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(fill=opt$checker_col))
    }
    if (opt$style == "simple_hex") {
        add_hexlines(arg, hl_col=opt$hexline_col)
    }
    gp_tr <- gpar(col=opt$scol, fontsize=opt$suit_fontsize)
    grid.text(symbol.1, x=0.25, y=.75, gp=gp_tr)
    grid.text(symbol.3, x=0.75, y=.25, gp=gp_tr)

    if (arg$add_checkers) 
        gp_tr <- gpar(col=opt$bcol, fontsize=opt$suit_fontsize)
    grid.text(symbol.4, x=0.25, y=.25, gp=gp_tr)
    grid.text(symbol.2, x=0.75, y=.75, gp=gp_tr)

    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

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

    gp_tr <- gpar(col=opt$scol, fontsize=opt$rank_fontsize)
    grid.text(opt$rank_symbol, gp=gp_tr)

    gp_ts <- gpar(col=opt$scol, fontsize=opt$suit_fontsize)
    vp <- viewport(x=0.25, y=0.75, width=0.5, height=0.5)
    pushViewport(vp)
    grid.text(opt$suit_symbol, gp=gp_ts)
    upViewport()

    if (draw_border)
        grid.rect(gp=gpar(col="grey", fill=NA))

    invisible(NULL)
}

add_directional_marker <- function(component, i_s, arg) {
    scol <- get_suit_color(component, i_s, arg)
    neutral_col <- arg$suit_colors[opts$i_unsuit] ####

    if (arg$directional_marker == "none") {
        dcol <- NA
    } else if (arg$directional_marker == "neutral") {
        dcol <- neutral_col
    } else {
        dcol <- scol
    }
    grid.lines(x=c(0.5, 0.5), y=c(0.90, 0.95), gp=gpar(col=dcol, lwd=10))
}

draw_suit_die_face <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("suit_die", i_s, 1, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize)
    grid.text(opt$suit_symbol, gp=sgp)
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}


draw_pawn_saucer_suit <- function(i_s, arg, draw_border = TRUE) {
    component <- ifelse(i_s < opts$i_unsuit, "saucer_face", "saucer_back")
    opt <- get_component_opt(component, i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize)
    grid.text(opt$suit_symbol, gp=sgp)
    add_directional_marker(component, i_s, arg)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_rankdie <- function(i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("rank_die", opts$i_unsuit, i_r, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    rgp <- gpar(col=opt$scol, fontsize=opt$rank_fontsize)
    grid.text(opt$rank_symbol, gp=rgp)
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_suitdie_null <- function(arg, draw_border = TRUE) {
    #### don't use get_background_color?
    grid.rect(gp = gpar(fill=arg$background_color)) 
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_coin_value <- function(i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("coin_face", opts$i_unsuit, i_r, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    rgp <- gpar(col=opt$scol, fontsize=opt$rank_fontsize)
    grid.text(opt$rank_symbol, gp =  rgp)
    add_directional_marker("coin_face", opts$i_unsuit, arg)
    if (draw_border) {
        grid.circle(gp=gpar(col="grey", fill=NA))
    }
    invisible(NULL)
}

draw_piecepack_die_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("piecepack_die", i_s, i_r, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    rgp <- gpar(col=opt$scol, fontsize=opt$rank_fontsize)
    grid.text(opt$rank_symbol, gp = rgp)
    if (draw_border) 
        grid.rect(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_chip_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("chip_face", i_s, i_r, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    # grid.circle(r=cr, gp=gpar(col=opt$scol, lwd=circle_lwd, fill=bcol))
    rgp <- gpar(col=opt$scol, fontsize=opt$rank_fontsize)
    grid.text(opt$rank_symbol, gp=rgp)
    add_directional_marker("chip_face", i_s, arg)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_coin_back <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("coin_back", i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize)
    grid.text(opt$suit_symbol, gp=sgp)
    add_directional_marker("coin_back", i_s, arg)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}

draw_chip_back <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("chip_back", i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    # grid.circle(r=cr, gp=gpar(col=opt$scol, lwd=circle_lwd, fill=opt$bcol))
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize)
    grid.text(opt$suit_symbol, gp=sgp)
    add_directional_marker("chip_back", i_s, arg)
    if (draw_border) 
        grid.circle(gp=gpar(col="grey", fill=NA))
    invisible(NULL)
}
