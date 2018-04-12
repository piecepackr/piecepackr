COMPONENTS <- c("tile_back", "tile_face", 
           "coin_back", "coin_face",
           "piecepack_die", "suit_die", 
           "pawn", "saucer_face", "saucer_back",
           "chip_face", "chip_back")

seg <- function(x, y, xend, yend, color="black", ...) {
    grid.segments(x0=x, y0=y, x1=xend, y1=yend, gp=gpar(col=color, ...))
}

addViewport <- function(...) { 
    suppressWarnings(pushViewport(viewport(..., clip="on")))
    upViewport()
}

grid.halma <- function(gp) {
    y_cutoff <- 0.55
    y_frac <- 0.5
    theta <- rev(seq(0, 360, length.out=100) - 90)
    r <- 0.5
    x <- 0.5 + to_x(theta, r)
    y <- 1 + y_frac * (to_y(theta, r) - r)
    indices <- which(y >= y_cutoff)
    grid.polygon(x = c(0,0, x[indices],1,1), y=c(0,0.3,y[indices],0.3,0), gp=gp)
}

grid.inversecircle <- function() {
    theta <- seq(0, 2*pi, length.out=100)
    r <- 0.5
    x_c <- 0.5 + to_x(theta, r)
    y_c <- 0.5 + to_y(theta, r)
    x_r <- c(1, 1, 0, 0, 1, 1)
    y_r <- c(0.5, 0, 0, 1, 1, 0.5)
    grid.polygon(x = c(x_c, x_r), y=c(y_c, y_r), gp=gpar(fill="white", col="white"))
}

grid.kite <- function(gp) {
    x <- c(0.5, 0, 0.5, 1, 0.5)
    y <- c(0, 0.25, 1, 0.25, 0)
    grid.polygon(x, y, gp=gp)
}

grid.pp.polygon_fn <- function(n_vertices, theta) { 
    theta <- seq(0, 360, length.out=n_vertices+1) + theta
    r <- 0.5
    x <- to_x(theta, r) + 0.5
    y <- to_y(theta, r) + 0.5
    function(gp) { grid.polygon(x, y, gp=gp) } 
}

splice <- function(x0, x1) {
    vec <- as.numeric()
    for (ii in 1:length(x1)) {
        vec <- append(vec, x0[ii])
        vec <- append(vec, x1[ii])
    }
    append(vec, x0[ii+1])
}

grid.star_fn <- function(theta) {
    theta_outer <- seq(0, 360, length.out=5+1) + theta
    theta_inner <- seq(36, 360-36, length.out=5) + theta
    r_outer <- 0.5
    r_inner <- 0.2
    x_outer <- to_x(theta_outer, r_outer) + 0.5
    x_inner <- to_x(theta_inner, r_inner) + 0.5
    y_outer <- to_y(theta_outer, r_outer) + 0.5
    y_inner <- to_y(theta_inner, r_inner) + 0.5
    x <- splice(x_outer, x_inner)
    y <- splice(y_outer, y_inner)
    function(gp) { grid.polygon(x, y, gp=gp) }
}

get_style_element <- function(style, component, arg, default=NULL, suited=FALSE) {
    component_str <- paste0(style, ".", component)
    suited_component_str <- paste0(style, ".", "suited")
    unsuited_component_str <- paste0(style, ".", "unsuited")
    if (!is.null(arg[[component_str]])) {
        arg[[component_str]]
    } else if (suited && !is.null(arg[[suited_component_str]])) {
        arg[[suited_component_str]]
    } else if (!suited && !is.null(arg[[unsuited_component_str]])) {
        arg[[unsuited_component_str]]
    } else if (!is.null(arg[[style]])) {
        arg[[style]]
    } else {
        default
    }
}

get_background_color_helper <- function(component, i_s, arg) {
    suited <- is_suited(component, i_s, arg)
    get_style_element("background_color", component, arg, suited=suited)
}

get_suit_colors <- function(component, arg) {
    suit_colors <- get_style_element("suit_colors", component, arg)
    expand_suit_elements(suit_colors, "suit_colors", component, arg) 
}

get_shape_theta <- function(component, arg) {
    get_style_element("shape_theta", component, arg, 90)
}

get_shape <- function(component, arg) {
    get_style_element("shape", component, arg,
        switch(component,
               tile_back = "rect",
               tile_face = "rect",
               coin_back = "circle",
               coin_face = "circle",
               piecepack_die = "rect",
               suit_die = "rect",
               saucer_face = "circle",
               saucer_back = "circle",
               pawn = "halma",
               pawn_belt = "rect",
               chip_face = "circle",
               chip_back = "circle")
        )
}

get_grid_shape <- function(shape, theta) {
    switch(shape,
           circle = grid.circle,
           halma = grid.halma,
           kite = grid.kite,
           rect = grid.rect,
           star = grid.star_fn(theta),
           grid.pp.polygon_fn(as.numeric(shape), theta))
}

get_shape_fn <- function(component, arg) {
    shape <- get_shape(component, arg)
    theta <- get_shape_theta(component, arg)
    get_grid_shape(shape, theta)
}

get_suit_color_helper <- function(component, i_s, arg) {
    suit_colors <- get_suit_colors(component, arg)
    ifelse(i_s <= arg$n_suits, suit_colors[i_s], suit_colors[arg$i_unsuit])
}

should_invert <- function(component, i_s, arg) {
    suited <- is_suited(component, i_s, arg)
    get_style_element("invert_colors", component, arg, suited=suited)
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
    get_style_element("dm_theta", component, arg,
        ifelse(component %in% c("tile_face", "piecepack_die", "suit_die"), 135, 90)
    )
}
get_dm_r <- function(component, arg) {
    get_style_element("dm_r", component, arg, sqrt(0.25^2 + 0.25^2))
}

get_dm_symbols <- function(component, arg) {
    dm_symbols <- get_style_element("dm_symbols", component, arg, {
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
    })
    dm_symbols <- expand_suit_elements(dm_symbols, "suit_symbols", component, arg)
    dm_symbols
}

get_dm_symbol <- function(component, i_s, arg) {
    get_dm_symbols(component, arg)[i_s]
}

get_dm_color <- function(component, i_s, arg) {
    colors <- get_style_element("dm_colors", component, arg, get_suit_color(component, i_s, arg))
    colors <- expand_suit_elements(colors, "suit_colors", component, arg)
    colors[i_s]
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
get_checker_color <- function(component, i_s, arg) {
    colors <- get_style_element("checker_colors", component, arg, NA)
    colors <- expand_suit_elements(colors, "checker_colors", component, arg)
    colors[i_s]
}
get_gridline_color <- function(component, i_s, arg) {
    colors <- get_style_element("gridline_colors", component, arg, 
        c(rep(NA, arg$n_suits), get_suit_color(component, arg$i_unsuit, arg)))
    colors <- expand_suit_elements(colors, "gridline_colors", component, arg)
    colors[i_s]
}
get_hexline_color <- function(component, i_s, arg) {
    colors <- get_style_element("hexline_colors", component, arg, NA)
    colors <- expand_suit_elements(colors, "hexline_colors", component, arg)
    colors[i_s]
}

get_suit_symbol <- function(component, i_s, arg) {
    get_suit_symbols(component, arg)[i_s]
}

get_rank_symbols <- function(component, arg) {
    rank_symbols <- get_style_element("rank_symbols", component, arg)
    expand_rank_elements(rank_symbols, "rank_symbols", component, arg)
}
get_suit_symbols <- function(component, arg, expand=TRUE) {
    suit_symbols <- get_style_element("suit_symbols", component, arg)
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
                           gridline_colors = NA,
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
    get_style_element("use_suit_as_ace", component, arg)
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
    get_style_element("style", component, arg)
}

get_rank_scales <- function(component, arg) {
    scales <- get_style_element("rank_symbols_scale", component, arg, 1.0)
    expand_rank_elements(scales, "scale", component, arg)
}
get_rank_scale <- function(component, i_r, arg) {
    get_rank_scales(component, arg)[i_r]
}
get_rank_fonts <- function(component, arg) {
    fonts <- get_style_element("rank_symbols_font", component, arg, arg[["font"]])
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
    fonts <- get_style_element("suit_symbols_font", component, arg, arg[["font"]])
    expand_suit_elements(fonts, "font", component, arg)
}
get_suit_font <- function(component, i_s, arg) {
    get_suit_fonts(component, arg)[i_s]
}
get_dm_fonts <- function(component, arg) {
    component_str <- paste0("dm_symbols_font.", component)
    fonts <- get_style_element("dm_symbols_font", component, arg, {
        if (all(get_dm_symbols(component, arg) == get_suit_symbols(component, arg))){ 
            fonts <- get_suit_fonts(component, arg)
        } else {
            fonts <- arg[["font"]]
        }
    })
    expand_suit_elements(fonts, "font", component, arg)
}
get_dm_font <- function(component, i_s, arg) {
    get_dm_fonts(component, arg)[i_s]
}
get_suit_scales <- function(component, arg) {
    scales <- get_style_element("suit_symbols_scale", component, arg, 1.0)
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
    scales <- get_style_element("dm_symbols_scale", component, arg, {
        if (all(get_dm_symbols(component, arg) == get_suit_symbols(component, arg))) { 
            scales <- get_suit_scales(component, arg)
        } else {
            scales <- 1.0
        }
    })
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

to_x <- function(theta, r) { 
    r * cos(pi * theta / 180) 
}

to_y <- function(theta, r) {
    r * sin(pi * theta / 180)
}

to_r <- function(x, y) {
    sqrt(x^2, y^2)
}

to_theta <- function(x, y) {
    atan2(y, x)
}

get_component_opt <- function(component, i_s, i_r, arg) {
    shape <- get_shape(component, arg)
    shape_fn <- get_shape_fn(component, arg)
    shape_theta <- get_shape_theta(component, arg)
    style <- get_style(component, arg)
    bcol <- get_background_color(component, i_s, arg)
    scol <- get_suit_color(component, i_s, arg)
    dm_col <- get_dm_color(component, i_s, arg)
    border_col <- arg$border_color
    checker_col <- get_checker_color(component, i_s, arg)
    gridline_col <- get_gridline_color(component, i_s, arg)
    hexline_col <- get_hexline_color(component, i_s, arg) 
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
    dm_x <- to_x(theta, r) + 0.5
    dm_y <- to_y(theta, r) + 0.5

    list(style=style, bcol=bcol, scol=scol, 
         border_col=border_col, checker_col=checker_col, 
         gridline_col=gridline_col, hexline_col=hexline_col, 
         rank_symbol=rank_symbol, rank_fontsize=rank_fontsize, rank_font=rank_font,
         suit_symbol=suit_symbol, suit_fontsize=suit_fontsize, suit_font=suit_font,
         dm_col=dm_col, dm_symbol=dm_symbol, 
         shape=shape, shape_fn=shape_fn, shape_theta=shape_theta,
         dm_fontsize=dm_fontsize, dm_font=dm_font, dm_x=dm_x, dm_y=dm_y)
}


make_preview <- function(arg) {
    pheight <- 2*tile_width+3*die_width
    svg(file.path(arg$svg_preview_dir, paste0(arg$deck_filename, ".svg")), 
        family=arg$font, width=3*tile_width, height=pheight)
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

    invisible(dev.off())
}

#' @export
make_previews <- function(arg) {
    dir.create(arg$svg_preview_dir, recursive=TRUE, showWarnings=FALSE)
    make_preview(arg)
}

#' @export
make_images <- function(arg, directory) {
    dir.create(directory, recursive=TRUE, showWarnings=FALSE)
    # make_tiles(arg)
    # make_stickers(arg)
    # make_pawns(arg)
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

add_gridlines <- function(opt) {
    gridline_col <- opt$gridline_col
    shape <- opt$shape
    if (shape == "rect") {
        lwd <- 8
        gp_gl <- gpar(col=gridline_col, lwd=lwd, lineend="square")
        grid.lines(x=0.5, gp=gp_gl)
        grid.lines(y=0.5, gp=gp_gl)
    } else if (shape %in% c("circle", "kite", "halma")) {
        stop(paste("Don't know how to add grid lines to shape", shape))
    } else {
        lwd <- 4
        n_vertices <- as.numeric(opt$shape)
        theta <- opt$shape_theta
        theta <- seq(0, 360, length.out=n_vertices+1) + theta
        theta <- theta[1:(length(theta)-1)]
        nt <- length(theta)
        n <- floor(nt / 2)
        r <- 0.5
        x <- 0.5 + to_x(theta, r)
        y <- 0.5 + to_y(theta, r)
        for (ii in 1:nt) {
            i_next <- ii+n
            if (i_next > nt)
                i_next <- i_next %% nt
            seg(x[ii], y[ii], x[i_next], y[i_next] , gridline_col, lwd=lwd)
        }
    }
}

add_checkers <- function(opt) {
    checker_col <- opt$checker_col
    shape <- opt$shape
    if (shape == "rect") {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(col=NA, fill=checker_col))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(col=NA, fill=checker_col))
    } else if (shape %in% c("circle", "kite", "halma")) {
        stop(paste("Don't know how to add checkers to shape", shape))
    } else {
        n_vertices <- as.numeric(opt$shape)
        theta <- opt$shape_theta
        theta <- seq(0, 360, length.out=n_vertices+1) + theta
        nt <- length(theta) - 1
        r <- 0.5
        x <- 0.5 + to_x(theta, r)
        y <- 0.5 + to_y(theta, r)
        for (ii in 1:nt) {
            if( ii %% 2) {
                xs <- c(0.5, x[ii], x[ii+1])
                ys <- c(0.5, y[ii], y[ii+1])
                grid.polygon(xs, ys, gp=gpar(col=NA, fill=checker_col))
            }
        }
    }
}

add_hexlines <- function(opt, omit_direction=FALSE) {
    hl_col <- opt$hexline_col
    ho <- 0.25
    hl_size <- 4
    if (omit_direction %in% 1:2)  # upper left
        NULL
    else
        seg(0, 1 - ho, ho, 1, hl_col, lwd=hl_size) 
    if (omit_direction %in% 3:4)  # lower left
        NULL
    else
        seg(0, ho, ho, 0, hl_col, lwd=hl_size) 
    if (omit_direction %in% 5:6)  # lower right
        NULL
    else
        seg(1, ho, 1 - ho, 0, hl_col, lwd=hl_size) 
    if (omit_direction %in% 7:8)  # upper right
        NULL
    else
        seg(1, 1 - ho, 1 - ho, 1, hl_col, lwd=hl_size) 
}

add_tile_elements <- function(opt) {
    #### what to do with non-square shapes?
    add_checkers(opt)
    add_hexlines(opt)
    add_gridlines(opt)
}

add_dm_symbol <- function(opt) {
    dmgp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=dmgp)
}

add_rank_symbol <- function(opt) {
    rgp <- gpar(col=opt$scol, fontsize=opt$rank_fontsize, fontfamily=opt$rank_font)
    grid.text(opt$rank_symbol, gp=rgp)
}

add_suit_symbol <- function(opt) {
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, gp=sgp)
}

add_background <- function(opt) {
    opt$shape_fn(gp = gpar(col=NA, fill=opt$bcol))
}

add_border <- function(opt) {
    opt$shape_fn(gp = gpar(col=opt$border_col, fill=NA))
}

draw_pawn_face <- function(i_s, arg) {
    opt <- get_component_opt("pawn", i_s, 1, arg)
    add_background(opt)
    gp_tr <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, y=0.75, gp=gp_tr)
    add_border(opt)
}

draw_pawn <- function(i_s, arg) {
    opt <- get_component_opt("pawn", i_s, 1, arg)

    suppressWarnings({
        addViewport(y=0.30, height=0.4, name="bottom_pawn_face")
        addViewport(y=0.70, height=0.4, name="top_pawn_face", angle=180)
        downViewport("bottom_pawn_face")
        draw_pawn_face(i_s, arg)
        upViewport()
        downViewport("top_pawn_face")
        draw_pawn_face(i_s, arg)
        upViewport()
    })
    grid.lines(y=0.5, gp=gpar(col=opt$border_col, fill=NA, lty="dashed"))
    grid.rect(gp=gpar(col=opt$border_col, fill=NA))
    seg(0.5, 0, 0.5, 0.05, opt$border_col)
    seg(0.5, 1, 0.5, 0.95, opt$border_col)
}

draw_pawn_belt <- function(i_s, arg) {
    opt <- get_component_opt("pawn_belt", i_s, 1, arg)

    add_background(opt)
    add_suit_symbol(opt)
    grid.lines(y=0.9, gp=gpar(col=opt$scol, lwd=8))
    grid.lines(y=0.1, gp=gpar(col=opt$scol, lwd=8))
    add_border(opt)
}

draw_suit_die_face <- function(i_s, arg) {
    opt <- get_component_opt("suit_die", i_s, 1, arg)

    add_background(opt)
    add_suit_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
}

draw_tile_back <- function(arg) {
    opt <- get_component_opt("tile_back", opts$i_unsuit, 1, arg)
    
    add_background(opt)
    add_tile_elements(opt)
    add_border(opt)
}

draw_tile_face <- function(i_s, i_r, arg) {
    opt <- get_component_opt("tile_face", i_s, i_r, arg)

    add_background(opt)
    add_tile_elements(opt)
    add_rank_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
    
}

draw_pawn_saucer <- function(i_s, arg) {
    component <- ifelse(i_s < arg$i_unsuit, "saucer_face", "saucer_back")
    opt <- get_component_opt(component, i_s, 1, arg)

    add_background(opt)
    add_suit_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
}

draw_coin_face <- function(i_r, arg) {
    opt <- get_component_opt("coin_face", arg$i_unsuit, i_r, arg)

    add_background(opt)
    add_rank_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
}

draw_piecepack_die_face <- function(i_s, i_r, arg) {
    opt <- get_component_opt("piecepack_die", i_s, i_r, arg)

    add_background(opt)
    add_rank_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
}

draw_chip_face <- function(i_s, i_r, arg) {
    opt <- get_component_opt("chip_face", i_s, i_r, arg)

    add_background(opt)
    add_rank_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
}

draw_coin_back <- function(i_s, arg) {
    opt <- get_component_opt("coin_back", i_s, 1, arg)

    add_background(opt)
    add_suit_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
}

draw_chip_back <- function(i_s, arg) {
    opt <- get_component_opt("chip_back", i_s, 1, arg)

    add_background(opt)
    add_suit_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
}
