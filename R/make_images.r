COMPONENTS <- c("tile_back", "tile_face", 
           "coin_back", "coin_face",
           "piecepack_die", "suit_die", 
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
    suited <- is_suited(component, i_s, arg)
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

get_suit_colors <- function(component, arg) {
    component_str <- paste0("suit_colors.", component)
    if (!is.null(arg[[component_str]])) {
        suit_colors <- arg[[component_str]]
    } else {
        suit_colors <- opts[["suit_colors"]]
    }
    expand_suit_elements(suit_colors, "suit_colors", component, arg) 
}

get_suit_color_helper <- function(component, i_s, arg) {
    suit_colors <- get_suit_colors(component, arg)
    ifelse(i_s <= arg$n_suits, suit_colors[i_s], suit_colors[arg$i_unsuit])
}

should_invert <- function(component, i_s, arg) {
    suited <- is_suited(component, i_s, arg)
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

is_suited <- function(component, i_s, arg) {
    switch(component,
           tile_back = FALSE,
           tile_face = TRUE, 
           coin_back = TRUE,
           coin_face = FALSE,
           piecepack_die = ifelse(i_s <= arg$i_unsuit, TRUE, FALSE),
           suit_die = ifelse(i_s <= arg$i_unsuit, TRUE, FALSE),
           saucer_face = TRUE,
           saucer_back = FALSE,
           pawn = TRUE,
           pawn_belt = TRUE,
           chip_face = TRUE,
           chip_back = TRUE)
}

get_dm_theta <- function(component, arg) {
    component_str <- paste0("dm_theta.", component)
    if (!is.null(arg[[component_str]])) {
        arg[[component_str]]
    } else if (!is.null(arg[["dm_theta"]])) {
        arg[["dm_theta"]]
    } else if (component %in% c("tile_face", "piecepack_die", "suit_die")) {
        135
    } else {
        90
    }
}
get_dm_r <- function(component, arg) {
    component_str <- paste0("dm_r.", component)
    if (!is.null(arg[[component_str]])) {
        arg[[component_str]]
    } else if (!is.null(arg[["dm_r"]])) {
        arg[["dm_r"]]
    } else {
        sqrt(0.25^2 + 0.25^2)
    }
}

get_dm_symbols <- function(component, arg) {
    component_str <- paste0("dm_symbols.", component)
    if (!is.null(arg[[component_str]])) {
        dm_symbols <- arg[[component_str]]
    } else if (!is.null(arg[["dm_symbols"]])) {
        dm_symbols <- arg[["dm_symbols"]]
    } else {
        if (component %in% c("coin_back", "coin_face")) {
            dm_symbols <- rep("●", arg$n_suits + 1)
        } else if (component %in% c("chip_back")) {
            dm_symbols <- rep("⛃", arg$n_suits + 1)
        } else if (component %in% c("saucer_back", "saucer_face")) {
            dm_symbols <- rep("♟", arg$n_suits + 1)
        } else if (component %in% c("suit_die")) {
            dm_symbols <- rep("", arg$n_suits +1)
        } else {
            dm_symbols <- get_suit_symbols(component, arg)
        }
    }
    dm_symbols <- expand_suit_elements(dm_symbols, "suit_symbols", component, arg)
    dm_symbols
}

get_dm_symbol <- function(component, i_s, arg) {
    get_dm_symbols(component, arg)[i_s]
}

get_dm_color <- function(component, i_s, arg) {
    component_str <- paste0("dm_colors.", component)
    if (!is.null(arg[[component_str]])) {
        colors <- expand_suit_elements(arg[[component_str]], "suit_colors", component, arg)[i_s]
    } else if (!is.null(arg[["dm_colors"]])) {
        colors <- expand_suit_elements(arg[["dm_colors"]], "suit_colors", component, arg)[i_s]
    } else {
        colors <- get_suit_color(component, i_s, arg)
    }
}

get_background_color <- function(component, i_s, arg) {
    bcol <- get_background_color_helper(component, i_s, arg)
    scol <- get_suit_color_helper(component, i_s, arg)
    if (should_invert(component, i_s, arg))
        scol
    else
        bcol
}

get_suit_color <- function(component, i_s, arg) {
    bcol <- get_background_color_helper(component, i_s, arg)
    scol <- get_suit_color_helper(component, i_s, arg)
    if (should_invert(component, i_s, arg))
        bcol
    else
        scol
}

get_suit_symbol <- function(component, i_s, arg) {
    get_suit_symbols(component, arg)[i_s]
}

get_rank_symbols <- function(component, arg) {
    component_str <- paste0("rank_symbols.", component)
    if (!is.null(arg[[component_str]]))
        rank_symbols <- arg[[component_str]]
    else
        rank_symbols <- arg$rank_symbols
    expand_rank_elements(rank_symbols, "rank_symbols", component, arg)
}
get_suit_symbols <- function(component, arg, expand=TRUE) {
    component_str <- paste0("suit_symbols.", component)
    if (!is.null(arg[[component_str]]))
        suit_symbols <- arg[[component_str]]
    else
        suit_symbols <- arg$suit_symbols
    if (expand) 
        suit_symbols <- expand_suit_elements(suit_symbols, "suit_symbols", component, arg)
    suit_symbols
}

expand_suit_elements <- function(elements, style, component, arg) {
    if (length(elements) == 1) {
        elements <- rep(elements, arg$n_suits + 1)
    } else if (length(elements) == arg$n_suits) {
        elements <- c(elements, switch(style, scale=1.0, ""))
    }
    if (length(elements) == arg$i_unsuit) {
        elements <- c(elements, switch(style, 
                           scale = 1.0,
                           rank_suit_symbols = elements[arg$i_unsuit],
                           suit_symbols = switch(component, 
                                suit_die = "", piecepack_die = "", elements[arg$i_unsuit]),
                           elements[arg$i_unsuit]))
    }
    elements
}

expand_rank_elements <- function(elements, style, component, arg) {
    if (length(elements) == 1) {
        elements <- rep(elements, arg$n_ranks)
    } 
    if (length(elements) == arg$n_ranks)
        elements <- c(elements, switch(style, rank_symbols = "", elements[arg$n_ranks]))
    elements
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
    suit_symbols <- get_rank_suit_symbols(component, arg)
    use_suit_as_ace <- get_use_suit_as_ace(component, arg)
    if (i_r == 2 && use_suit_as_ace) {
        rank_symbol <- suit_symbols[i_s]
    } else {
        rank_symbol <- rank_symbols[i_r]
    }
    rank_symbol
}       

get_style <- function(component, arg) {
    style <- arg$style
    style
}

get_rank_scales <- function(component, arg) {
    component_str <- paste0("rank_symbols_scale.", component)
    if (!is.null(arg[[component_str]])) {
        scales <- arg[[component_str]]
    } else if (!is.null(arg[["rank_symbols_scale"]])) {
        scales <- arg[["rank_symbols_scale"]]
    } else {
        scales <- 1.0
    }
    expand_rank_elements(scales, "scale", component, arg)
}
get_rank_scale <- function(component, i_r, arg) {
    get_rank_scales(component, arg)[i_r]
}
get_rank_fonts <- function(component, arg) {
    component_str <- paste0("rank_symbols_font.", component)
    if (!is.null(arg[[component_str]])) {
        fonts <- arg[[component_str]]
    } else if (!is.null(arg[["rank_symbols_font"]])) {
        fonts <- arg[["rank_symbols_font"]]
    } else {
        fonts <- arg[["font"]]
    }
    expand_rank_elements(fonts, "font", component, arg)
}
get_rank_font <- function(component, i_s, i_r, arg) {
    rank_font <- get_rank_fonts(component, arg)[i_r]
    suit_font <- get_suit_font(component, i_s,  arg)
    use_suit_as_ace <- get_use_suit_as_ace(component, arg)
    if (i_r == 2 && use_suit_as_ace)
        suit_font
    else
        rank_font
}
get_suit_fonts <- function(component, arg) {
    component_str <- paste0("suit_symbols_font.", component)
    if (!is.null(arg[[component_str]])) {
        fonts <- arg[[component_str]]
    } else if (!is.null(arg[["suit_symbols_font"]])) {
        fonts <- arg[["suit_symbols_font"]]
    } else {
        fonts <- arg[["font"]]
    }
    expand_suit_elements(fonts, "font", component, arg)
}
get_suit_font <- function(component, i_s, arg) {
    get_suit_fonts(component, arg)[i_s]
}
get_dm_fonts <- function(component, arg) {
    component_str <- paste0("dm_symbols_font.", component)
    if (!is.null(arg[[component_str]])) {
        fonts <- arg[[component_str]]
    } else if (!is.null(arg[["dm_symbols_font"]])) {
        fonts <- arg[["dm_symbols_font"]]
    } else if (all(get_dm_symbols(component, arg) == get_suit_symbols(component, arg))){ 
        fonts <- get_suit_fonts(component, arg)
    } else {
        fonts <- arg[["font"]]
    }
    expand_suit_elements(fonts, "font", component, arg)
}
get_dm_font <- function(component, i_s, arg) {
    get_dm_fonts(component, arg)[i_s]
}
get_suit_scales <- function(component, arg) {
    component_str <- paste0("suit_symbols_scale.", component)
    if (!is.null(arg[[component_str]])) {
        scales <- arg[[component_str]]
    } else if (!is.null(arg[["suit_symbols_scale"]])) {
        scales <- arg[["suit_symbols_scale"]]
    } else {
        scales <- 1.0
    }
    expand_suit_elements(scales, "scale", component, arg)
}
get_rank_suit_symbols <- function(component, arg) {
    suit_symbols <- get_suit_symbols(component, arg, expand=FALSE)
    suit_symbols <- expand_suit_elements(suit_symbols, "rank_suit_symbols", component, arg)
    suit_symbols
}
get_suit_scale <- function(component, i_s, arg) {
    get_suit_scales(component, arg)[i_s]
}
get_dm_scales <- function(component, arg) {
    component_str <- paste0("dm_symbols_scale.", component)
    if (!is.null(arg[[component_str]])) {
        scales <- arg[[component_str]]
    } else if (!is.null(arg[["dm_symbols_scale"]])) {
        scales <- arg[["dm_symbols_scale"]]
    } else if (all(get_dm_symbols(component, arg) == get_suit_symbols(component, arg))) { 
        scales <- get_suit_scales(component, arg)
    } else {
        scales <- 1.0
    }
    expand_suit_elements(scales, "scale", component, arg)
}
get_dm_scale <- function(component, i_s, arg) {
    get_dm_scales(component, arg)[i_s]
}

get_suit_fontsize <- function(component, i_s, arg) {
    scale <- get_suit_scale(component, i_s, arg)
    fs <- switch(component,
                 "pawn_belt" = 20,
                 "chip_back" = 24,
                 "coin_back" = 28,
                 "pawn" = 40,
                 "saucer_back" = 32,
                 "saucer_face" = 32,
                 "suit_die" = 28,
                 24)
    scale * fs
}

get_dm_fontsize <- function(component, i_s, arg) {
    scale <- get_dm_scale(component, i_s, arg)
    fs <- switch(component,
                 "tile_face" = 32,
                 10)
    scale * fs
}

get_rank_fontsize <- function(component, i_s, i_r, arg) {
    rank_scale <- get_rank_scale(component, i_r, arg)
    suit_scale <- get_suit_scale(component, i_s,  arg)
    use_suit_as_ace <- get_use_suit_as_ace(component, arg)
    if (i_r == 2 && use_suit_as_ace)
        scale <- suit_scale
    else
        scale <- rank_scale
    fs <- switch(component,
                 "piecepack_die" = 20,
                 "chip_face" = 28,
                 "coin_face" = 28,
                 "tile_face" = 72,
                 20)
    scale * fs
}
get_hexline_color <- function(component, i_s, arg) {
    if(!is.null(arg[["hexline_colors"]]))
        arg[["hexline_colors"]][i_s]
    else
        NA
}

get_component_opt <- function(component, i_s, i_r, arg) {
    style <- get_style(component, arg)
    bcol <- get_background_color(component, i_s, arg)
    scol <- get_suit_color(component, i_s, arg)
    dm_col <- get_dm_color(component, i_s, arg)
    hexline_col <- get_hexline_color(component, i_s, arg) 
    checker_col <- arg$suit_colors[arg$i_unsuit] #### won't show hex lines
    rank_symbol <- get_rank_symbol(component, i_s, i_r, arg)
    rank_fontsize <- get_rank_fontsize(component, i_s, i_r, arg)
    rank_font <- get_rank_font(component, i_s, i_r, arg)
    suit_symbol <- get_suit_symbol(component, i_s, arg)
    suit_fontsize <- get_suit_fontsize(component, i_s, arg)
    suit_font <- get_suit_font(component, i_s, arg)
    dm_symbol <- get_dm_symbol(component, i_s, arg)
    dm_fontsize <- get_dm_fontsize(component, i_s, arg)
    dm_font <- get_dm_font(component, i_s, arg)
    theta <- get_dm_theta(component, arg)
    r <- get_dm_r(component, arg)
    dm_x <- r * cos(pi * theta / 180) + 0.5
    dm_y <- r * sin(pi * theta / 180) + 0.5

    list(style=style, bcol=bcol, scol=scol, 
         hexline_col=hexline_col, checker_col=checker_col, 
         rank_symbol=rank_symbol, rank_fontsize=rank_fontsize, rank_font=rank_font,
         suit_symbol=suit_symbol, suit_fontsize=suit_fontsize, suit_font=suit_font,
         dm_col=dm_col, dm_symbol=dm_symbol,
         dm_fontsize=dm_fontsize, dm_font=dm_font, dm_x=dm_x, dm_y=dm_y)
}


make_preview <- function(arg) {
    pheight <- 2*tile_width+3*die_width
    svg(file.path(arg$directory, "preview.svg"), 
        family=arg$font, width=3*tile_width, height=pheight)
    # dev.new(width=4, height=4.67, unit="in")
    grid.newpage()

    # Build viewports
    pushViewport(viewport(name="main"))
    addViewport(y=inch(pheight-tile_width), width=inch(3 * tile_width), height=inch(2 * tile_width), name="tiles")
    downViewport("tiles")
    addViewport(x=1/3-1/6, y=3/4, width=inch(tile_width), height=inch(tile_width), name="tile.1")
    addViewport(x=3/3-1/6, y=3/4, width=inch(tile_width), height=inch(tile_width), name="tile.2")
    addViewport(x=3/3-1/6, y=1/4, width=inch(tile_width), height=inch(tile_width), name="tile.3")
    addViewport(x=1/3-1/6, y=1/4, width=inch(tile_width), height=inch(tile_width), name="tile.4")
    addViewport(x=2/3-1/6, y=3/4, width=inch(tile_width), height=inch(tile_width), name="tile.5")
    addViewport(x=2/3-1/6, y=1/4, width=inch(tile_width), height=inch(tile_width), name="tile.6")
    seekViewport("main")
    addViewport(x=inch(1.5*coin_width), y=inch(1.5*die_width), width=inch(3*coin_width), height=inch(2*coin_width), name="coins")
    downViewport("coins")
    for (ii in 1:3) {
        addViewport(x=ii/3-1/6, y=3/4, height=1/2, width=1/3, name=paste0("coin.", ii))
        addViewport(x=ii/3-1/6, y=1/4, height=1/2, width=1/3, name=paste0("coin.", 3+ii))
    }
    seekViewport("main")
    addViewport(x=3/3-1/6, y=inch(1.5*die_width), width=inch(4 * die_width), height=inch(3 * die_width), name="suitrankdie")
    addViewport(x=inch(tile_width+2.5*die_width), y=inch(2*die_width), 
                width=inch(2 * saucer_width), height=inch(saucer_width), name="saucers")
    downViewport('saucers')
    addViewport(x=0.25, width=0.5, name="saucer.face")
    addViewport(x=0.75, width=0.5, name="saucer.back")

    # Draw components
    seekViewport("tile.6")
    if (arg$n_suits < 6) {
        draw_tile_back(arg)
    } else {
        draw_tile_face(6, 2, arg)
    }
    seekViewport("tile.5")
    if (arg$n_suits < 5) {
        draw_tile_back(arg)
    } else {
        draw_tile_face(5, 2, arg)
    }
    for (ii in 1:min(4, arg$n_suits)) {
        seekViewport(paste0("tile.", ii))
        draw_tile_face(ii, 2, arg)
    }
    seekViewport(paste0("coin.1"))
    draw_coin_face(1, arg)
    seekViewport(paste0("coin.2"))
    draw_coin_face(2, arg)
    seekViewport(paste0("coin.3"))
    if (opts$n_suits < 5)
        draw_coin_back(4, arg)
    else
        draw_coin_back(5, arg)
    seekViewport(paste0("coin.4"))
    draw_coin_face(3, arg)
    seekViewport(paste0("coin.5"))
    if (opts$n_suits < 6)
        draw_coin_back(2, arg)
    else
        draw_coin_back(6, arg)
    seekViewport(paste0("coin.6"))
    draw_coin_back(3, arg)

    seekViewport("suitrankdie")
    draw_suitrank_die(arg)

    seekViewport("saucer.face")
    draw_pawn_saucer(1, arg)
    seekViewport("saucer.back")
    draw_pawn_saucer(arg$i_unsuit, arg)

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

draw_pawn <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("pawn", i_s, 1, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    gp_tr <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, y=0.65, rot=180, gp=gp_tr)
    grid.text(opt$suit_symbol, y=0.35, rot=0, gp=gp_tr)
    grid.lines(y=0.5, gp=gpar(col=opt$scol, lty="dashed"))
    grid.lines(y=0.1, gp=gpar(col=opt$scol, lty="dashed"))
    grid.lines(y=0.9, gp=gpar(col=opt$scol, lty="dashed"))

    if (draw_border)
        grid.rect(gp = gpar(col=arg$border_color, fill=NA))

    invisible(NULL)
}

draw_pawn_belt <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("pawn_belt", i_s, 1, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    gp_tr <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, gp=gp_tr)
    grid.lines(y=0.9, gp=gpar(col=opt$scol, lwd=8))
    grid.lines(y=0.1, gp=gpar(col=opt$scol, lwd=8))

    if (draw_border)
        grid.rect(gp = gpar(col=arg$border_color, fill=NA))

    invisible(NULL)
}

draw_tile_back <- function(arg, draw_border = TRUE) {
    opt <- get_component_opt("tile_back", opts$i_unsuit, 1, arg)
    
    grid.rect(gp = gpar(fill=opt$bcol))
    add_hexlines(arg, hl_col=opt$hexline_col)
    gp_gl <- gpar(col=opt$scol, lwd=8, lineend="square")
    grid.lines(x=0.5, gp=gp_gl)
    grid.lines(y=0.5, gp=gp_gl)
    if (draw_border)
        grid.rect(gp=gpar(col=arg$border_color, fill=NA))
    invisible(NULL)
}

draw_tile_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("tile_face", i_s, i_r, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    if (arg$add_checkers) {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(fill=opt$checker_col))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(fill=opt$checker_col))
    }
    add_hexlines(arg, hl_col=opt$hexline_col) 

    gp_tr <- gpar(col=opt$scol, fontsize=opt$rank_fontsize, fontfamily=opt$rank_font)
    grid.text(opt$rank_symbol, gp=gp_tr)

    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)

    if (draw_border)
        grid.rect(gp=gpar(col=arg$border_color, fill=NA))

    invisible(NULL)
}

draw_suit_die_face <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("suit_die", i_s, 1, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, gp=sgp)
    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)
    if (draw_border) 
        grid.rect(gp=gpar(col=arg$border_color, fill=NA))
    invisible(NULL)
}


draw_pawn_saucer <- function(i_s, arg, draw_border = TRUE) {
    component <- ifelse(i_s < arg$i_unsuit, "saucer_face", "saucer_back")
    opt <- get_component_opt(component, i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, gp=sgp)
    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)
    if (draw_border) 
        grid.circle(gp=gpar(col=arg$border_color, fill=NA))
    invisible(NULL)
}

draw_coin_face <- function(i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("coin_face", arg$i_unsuit, i_r, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    rgp <- gpar(col=opt$scol, fontsize=opt$rank_fontsize, fontfamily=opt$rank_font)
    grid.text(opt$rank_symbol, gp =  rgp)
    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)
    if (draw_border) {
        grid.circle(gp=gpar(col=arg$border_color, fill=NA))
    }
    invisible(NULL)
}

draw_piecepack_die_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("piecepack_die", i_s, i_r, arg)

    grid.rect(gp = gpar(fill=opt$bcol))
    rgp <- gpar(col=opt$scol, fontsize=opt$rank_fontsize, fontfamily=opt$rank_font)
    grid.text(opt$rank_symbol, gp = rgp)
    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)
    if (draw_border) 
        grid.rect(gp=gpar(col=arg$border_color, fill=NA))
    invisible(NULL)
}

draw_chip_face <- function(i_s, i_r, arg, draw_border = TRUE) {
    opt <- get_component_opt("chip_face", i_s, i_r, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    # grid.circle(r=cr, gp=gpar(col=opt$scol, lwd=circle_lwd, fill=bcol))
    rgp <- gpar(col=opt$scol, fontsize=opt$rank_fontsize, fontfamily=opt$rank_font)
    grid.text(opt$rank_symbol, gp=rgp)
    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)
    if (draw_border) 
        grid.circle(gp=gpar(col=arg$border_color, fill=NA))
    invisible(NULL)
}

draw_coin_back <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("coin_back", i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, gp=sgp)
    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)
    if (draw_border) 
        grid.circle(gp=gpar(col=arg$border_color, fill=NA))
    invisible(NULL)
}

draw_chip_back <- function(i_s, arg, draw_border = TRUE) {
    opt <- get_component_opt("chip_back", i_s, 1, arg)

    grid.circle(gp = gpar(fill=opt$bcol))
    # grid.circle(r=cr, gp=gpar(col=opt$scol, lwd=circle_lwd, fill=opt$bcol))
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, gp=sgp)
    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)
    if (draw_border) 
        grid.circle(gp=gpar(col=arg$border_color, fill=NA))
    invisible(NULL)
}
