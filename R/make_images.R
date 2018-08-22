#' @importFrom grDevices bmp cairo_pdf cairo_ps dev.off jpeg png svg tiff
#' @import grid

COMPONENTS <- c("tile", "coin", "ppdie", "suitdie", 
           "pawn", "belt", "saucer", "chip")
COMPONENT_AND_SIDES <- c("tile_back", "tile_face", 
           "coin_back", "coin_face",
           "ppdie_face", "suitdie_face", 
           "pawn_face", "pawn_back", 
           "belt_face",  "saucer_face", "saucer_back",
           "chip_face", "chip_back")
COIN_WIDTH <- 3/4
DIE_WIDTH <- 1/2
TILE_WIDTH <- 2
SAUCER_WIDTH <- 7/8
CHIP_WIDTH <- 5/8
PAWN_HEIGHT <- 7/8
PAWN_WIDTH <- 1/2
PAWN_BASE <- 3/8
# PAWN_HEIGHT <- 9/8
# PAWN_WIDTH <- 5/8
BELT_HEIGHT <- 1/2
BELT_WIDTH <- 3 * DIE_WIDTH

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

# grid.inversecircle <- function() {
#     theta <- seq(0, 2*pi, length.out=100)
#     r <- 0.5
#     x_c <- 0.5 + to_x(theta, r)
#     y_c <- 0.5 + to_y(theta, r)
#     x_r <- c(1, 1, 0, 0, 1, 1)
#     y_r <- c(0.5, 0, 0, 1, 1, 0.5)
#     grid.polygon(x = c(x_c, x_r), y=c(y_c, y_r), gp=gpar(fill="white", col="white"))
# }

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

get_component <- function(component_side) {
    split(component_side, "_")[1]
}

get_style_element <- function(style, component_side, cfg, default=NULL, suited=FALSE) {
    component <- get_component(component_side)

    component_side_str <- paste0(style, ".", component_side)
    component_str <- paste0(style, ".", component)
    suited_str <- paste0(style, ".", "suited")
    unsuited_str <- paste0(style, ".", "unsuited")
    if (!is.null(cfg[[component_side_str]])) {
        cfg[[component_side_str]]
    } else if (!is.null(cfg[[component_str]])) {
        cfg[[component_str]]
    } else if (suited && !is.null(cfg[[suited_str]])) {
        cfg[[suited_str]]
    } else if (!suited && !is.null(cfg[[unsuited_str]])) {
        cfg[[unsuited_str]]
    } else if (!is.null(cfg[[style]])) {
        cfg[[style]]
    } else {
        default
    }
}

get_background_color_helper <- function(component_side, i_s, cfg) {
    suited <- is_suited(component_side, i_s, cfg)
    colors <- get_style_element("background_colors", component_side, cfg, suited=suited)
    expand_suit_elements(colors, "background_colors", component_side, cfg)[i_s]
}

get_suit_colors <- function(component_side, cfg) {
    suit_colors <- get_style_element("suit_colors", component_side, cfg)
    expand_suit_elements(suit_colors, "suit_colors", component_side, cfg) 
}

get_shape_theta <- function(component_side, cfg) {
    get_style_element("shape_theta", component_side, cfg, 90)
}

get_shape <- function(component_side, cfg) {
    get_style_element("shape", component_side, cfg,
        switch(component_side,
               tile_back = "rect",
               tile_face = "rect",
               coin_back = "circle",
               coin_face = "circle",
               ppdie_face = "rect",
               suitdie_face = "rect",
               saucer_face = "circle",
               saucer_back = "circle",
               pawn_face = "halma",
               pawn_back = "halma",
               belt_face = "rect",
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

get_shape_fn <- function(component_side, cfg) {
    shape <- get_shape(component_side, cfg)
    theta <- get_shape_theta(component_side, cfg)
    get_grid_shape(shape, theta)
}

get_suit_color_helper <- function(component_side, i_s, cfg) {
    suit_colors <- get_suit_colors(component_side, cfg)
    ifelse(i_s <= cfg$n_suits, suit_colors[i_s], suit_colors[cfg$i_unsuit])
}

should_invert <- function(component_side, i_s, cfg) {
    suited <- is_suited(component_side, i_s, cfg)
    should_inverts <- get_style_element("invert_colors", component_side, cfg, suited=suited)
    expand_suit_elements(should_inverts, "should_inverts", component_side, cfg)[i_s]
}

is_suited <- function(component_side, i_s, cfg) {
    switch(component_side,
           tile_back = FALSE,
           tile_face = TRUE, 
           coin_back = TRUE,
           coin_face = FALSE,
           ppdie_face = ifelse(i_s <= cfg$i_unsuit, TRUE, FALSE),
           suitdie_face = ifelse(i_s <= cfg$i_unsuit, TRUE, FALSE),
           saucer_face = TRUE,
           saucer_back = FALSE,
           pawn_face = TRUE,
           pawn_back = TRUE,
           belt_face = TRUE,
           chip_face = TRUE,
           chip_back = TRUE)
}

get_dm_theta <- function(component_side, cfg) {
    get_style_element("dm_theta", component_side, cfg,
        ifelse(component_side %in% c("tile_face", "ppdie_face", "suitdie_face"), 135, 90)
    )
}
get_dm_r <- function(component_side, cfg) {
    shape <- get_shape(component_side, cfg)
    get_style_element("dm_r", component_side, cfg, 
                      switch(shape,
                             rect = sqrt(0.25^2 + 0.25^2),
                             circle = sqrt(0.25^2 + 0.25^2),
                             halma = 0.25,
                             0.3))
}

get_ps_theta <- function(component_side, cfg) {
    shape <- get_shape(component_side, cfg)
    get_style_element("ps_theta", component_side, cfg,
        ifelse(shape == "halma", -90, 0)
    )
}
get_ps_r <- function(component_side, cfg) {
    shape <- get_shape(component_side, cfg)
    get_style_element("ps_r", component_side, cfg, 
                      switch(shape, halma = 0.25, 0.0))
}

get_dm_symbols <- function(component_side, cfg) {
    dm_symbols <- get_style_element("dm_symbols", component_side, cfg, {
        if (component_side %in% c("coin_back", "coin_face")) {
            dm_symbols <- "\u25cf" # "●"
        } else if (component_side %in% c("chip_back")) {
            dm_symbols <- get_suit_symbols(component_side, cfg)
            # dm_symbols <- "\u26c3" # "⛃"
        } else if (component_side %in% c("saucer_back", "saucer_face")) {
            dm_symbols <- "\u25b2" # "▲"
            # dm_symbols <- "\u265f" # "♟"
        } else if (component_side %in% c("pawn_face")) {
            dm_symbols <- "\u0298\u0298" # "ʘʘ"
            # dm_symbols <- "\u2c7a\u2c7a" # "ⱺⱺ"
            # dm_symbols <- "\U0001f440" # "👀"
            # dm_symbols <- "\U0001f603" # "😃"
        } else if (component_side %in% c("suitdie_face", "pawn_back", 
                                         "belt_face", "tile_back")) {
            dm_symbols <- ""
        } else {
            dm_symbols <- get_suit_symbols(component_side, cfg)
        }
    })
    dm_symbols <- expand_suit_elements(dm_symbols, "suit_symbols", component_side, cfg)
    dm_symbols
}

get_dm_symbol <- function(component_side, i_s, cfg) {
    get_dm_symbols(component_side, cfg)[i_s]
}

get_dm_color <- function(component_side, i_s, cfg) {
    colors <- get_style_element("dm_colors", component_side, cfg, get_suit_color(component_side, i_s, cfg))
    colors <- expand_suit_elements(colors, "suit_colors", component_side, cfg)
    colors[i_s]
}

get_background_color <- function(component_side, i_s, cfg) {
    bcol <- get_background_color_helper(component_side, i_s, cfg)
    scol <- get_suit_color_helper(component_side, i_s, cfg)
    if (should_invert(component_side, i_s, cfg))
        scol
    else
        bcol
}

get_suit_color <- function(component_side, i_s, cfg) {
    bcol <- get_background_color_helper(component_side, i_s, cfg)
    scol <- get_suit_color_helper(component_side, i_s, cfg)
    if (should_invert(component_side, i_s, cfg))
        bcol
    else
        scol
}
get_checker_color <- function(component_side, i_s, cfg) {
    colors <- get_style_element("checker_colors", component_side, cfg, NA)
    colors <- expand_suit_elements(colors, "checker_colors", component_side, cfg)
    colors[i_s]
}
get_gridline_color <- function(component_side, i_s, cfg) {
    colors <- get_style_element("gridline_colors", component_side, cfg, 
        c(rep(NA, cfg$n_suits), get_suit_color(component_side, cfg$i_unsuit, cfg)))
    colors <- expand_suit_elements(colors, "gridline_colors", component_side, cfg)
    colors[i_s]
}
get_hexline_color <- function(component_side, i_s, cfg) {
    colors <- get_style_element("hexline_colors", component_side, cfg, NA)
    colors <- expand_suit_elements(colors, "hexline_colors", component_side, cfg)
    colors[i_s]
}

get_suit_symbol <- function(component_side, i_s, cfg) {
    get_suit_symbols(component_side, cfg)[i_s]
}

get_rank_symbols <- function(component_side, cfg) {
    rank_symbols <- get_style_element("rank_symbols", component_side, cfg)
    expand_rank_elements(rank_symbols, "rank_symbols", component_side, cfg)
}
get_suit_symbols <- function(component_side, cfg, expand=TRUE) {
    suit_symbols <- get_style_element("suit_symbols", component_side, cfg)
    if (expand) 
        suit_symbols <- expand_suit_elements(suit_symbols, "suit_symbols", component_side, cfg)
    suit_symbols
}

expand_suit_elements <- function(elements, style, component_side, cfg) {
    if (length(elements) == 1) {
        elements <- rep(elements, cfg$n_suits + 1)
    } else if (length(elements) == cfg$n_suits) {
        elements <- c(elements, switch(style, scale=1.0, ""))
    }
    if (length(elements) == cfg$i_unsuit) {
        elements <- c(elements, switch(style, 
                           suit_symbols = switch(component_side, 
                                suitdie_face = "", ppdie_face = "", elements[cfg$i_unsuit]),
                           gridline_colors = NA,
                           elements[cfg$i_unsuit]))
    }
    elements
}

expand_rank_elements <- function(elements, style, component_side, cfg) {
    if (length(elements) == 1) {
        elements <- rep(elements, cfg$n_ranks)
    } 
    if (length(elements) == cfg$n_ranks)
        elements <- c(elements, switch(style, rank_symbols = "", elements[cfg$n_ranks]))
    elements
}

get_use_suit_as_ace <- function(component_side, cfg) {
    get_style_element("use_suit_as_ace", component_side, cfg)
}

get_rank_symbol <- function(component_side, i_s, i_r, cfg) {
    rank_symbols <- get_rank_symbols(component_side, cfg)
    suit_symbols <- get_rank_suit_symbols(component_side, cfg)
    use_suit_as_ace <- get_use_suit_as_ace(component_side, cfg)
    if (i_r == 2 && use_suit_as_ace) {
        rank_symbol <- suit_symbols[i_s]
    } else {
        rank_symbol <- rank_symbols[i_r]
    }
    rank_symbol
}       

get_style <- function(component_side, cfg) {
    get_style_element("style", component_side, cfg)
}

get_rank_scales <- function(component_side, cfg) {
    scales <- get_style_element("rank_symbols_scale", component_side, cfg, 1.0)
    expand_rank_elements(scales, "scale", component_side, cfg)
}
get_rank_scale <- function(component_side, i_r, cfg) {
    get_rank_scales(component_side, cfg)[i_r]
}
get_rank_fonts <- function(component_side, cfg) {
    fonts <- get_style_element("rank_symbols_font", component_side, cfg, cfg[["font"]])
    expand_rank_elements(fonts, "font", component_side, cfg)
}
get_rank_font <- function(component_side, i_s, i_r, cfg) {
    rank_font <- get_rank_fonts(component_side, cfg)[i_r]
    suit_font <- get_suit_font(component_side, i_s,  cfg)
    use_suit_as_ace <- get_use_suit_as_ace(component_side, cfg)
    if (i_r == 2 && use_suit_as_ace)
        suit_font
    else
        rank_font
}
get_suit_fonts <- function(component_side, cfg) {
    fonts <- get_style_element("suit_symbols_font", component_side, cfg, cfg[["font"]])
    expand_suit_elements(fonts, "font", component_side, cfg)
}
get_suit_font <- function(component_side, i_s, cfg) {
    get_suit_fonts(component_side, cfg)[i_s]
}
get_dm_fonts <- function(component_side, cfg) {
    fonts <- get_style_element("dm_symbols_font", component_side, cfg, {
        if (all(get_dm_symbols(component_side, cfg) == get_suit_symbols(component_side, cfg))){ 
            fonts <- get_suit_fonts(component_side, cfg)
        } else {
            fonts <- cfg[["font"]]
        }
    })
    expand_suit_elements(fonts, "font", component_side, cfg)
}
get_dm_font <- function(component_side, i_s, cfg) {
    get_dm_fonts(component_side, cfg)[i_s]
}
get_suit_scales <- function(component_side, cfg) {
    scales <- get_style_element("suit_symbols_scale", component_side, cfg, 1.0)
    expand_suit_elements(scales, "scale", component_side, cfg)
}
get_rank_suit_symbols <- function(component_side, cfg) {
    suit_symbols <- get_suit_symbols(component_side, cfg, expand=FALSE)
    suit_symbols <- expand_suit_elements(suit_symbols, "rank_suit_symbols", component_side, cfg)
    suit_symbols
}
get_suit_scale <- function(component_side, i_s, cfg) {
    get_suit_scales(component_side, cfg)[i_s]
}
get_dm_scales <- function(component_side, cfg) {
    scales <- get_style_element("dm_symbols_scale", component_side, cfg, {
        if (all(get_dm_symbols(component_side, cfg) == get_suit_symbols(component_side, cfg))) { 
            scales <- get_suit_scales(component_side, cfg)
        } else {
            scales <- 1.0
        }
    })
    expand_suit_elements(scales, "scale", component_side, cfg)
}
get_dm_scale <- function(component_side, i_s, cfg) {
    get_dm_scales(component_side, cfg)[i_s]
}

get_suit_fontsize <- function(component_side, i_s, cfg) {
    scale <- get_suit_scale(component_side, i_s, cfg)
    fs <- switch(component_side,
                 "belt_face" = 20,
                 "chip_back" = 22,
                 "coin_back" = 28,
                 "pawn_face" = 22,
                 "pawn_back" = 22,
                 "saucer_back" = 32,
                 "saucer_face" = 32,
                 "suitdie_face" = 28,
                 24)
    scale * fs
}

get_dm_fontsize <- function(component_side, i_s, cfg) {
    scale <- get_dm_scale(component_side, i_s, cfg)
    fs <- switch(component_side,
                 "tile_face" = 32,
                 "pawn_face" = 12,
                 "pawn_back" = 12,
                 10)
    scale * fs
}

get_rank_fontsize <- function(component_side, i_s, i_r, cfg) {
    rank_scale <- get_rank_scale(component_side, i_r, cfg)
    suit_scale <- get_suit_scale(component_side, i_s,  cfg)
    use_suit_as_ace <- get_use_suit_as_ace(component_side, cfg)
    if (i_r == 2 && use_suit_as_ace)
        scale <- suit_scale
    else
        scale <- rank_scale
    fs <- switch(component_side,
                 "ppdie_face" = 20,
                 "chip_face" = 22,
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

# to_r <- function(x, y) {
#     sqrt(x^2 + y^2)
# }
# 
# to_theta <- function(x, y) {
#     atan2(y, x)
# }

get_component_opt <- function(component_side, i_s=cfg$i_unsuit, i_r=1, cfg=c2o()) {
    shape <- get_shape(component_side, cfg)
    shape_fn <- get_shape_fn(component_side, cfg)
    shape_theta <- get_shape_theta(component_side, cfg)
    style <- get_style(component_side, cfg)
    bcol <- get_background_color(component_side, i_s, cfg)
    scol <- get_suit_color(component_side, i_s, cfg)
    dm_col <- get_dm_color(component_side, i_s, cfg)
    border_col <- cfg$border_color
    checker_col <- get_checker_color(component_side, i_s, cfg)
    gridline_col <- get_gridline_color(component_side, i_s, cfg)
    hexline_col <- get_hexline_color(component_side, i_s, cfg) 
    rank_symbol <- get_rank_symbol(component_side, i_s, i_r, cfg)
    rank_fontsize <- get_rank_fontsize(component_side, i_s, i_r, cfg)
    rank_font <- get_rank_font(component_side, i_s, i_r, cfg)
    suit_symbol <- get_suit_symbol(component_side, i_s, cfg)
    suit_fontsize <- get_suit_fontsize(component_side, i_s, cfg)
    suit_font <- get_suit_font(component_side, i_s, cfg)
    dm_symbol <- get_dm_symbol(component_side, i_s, cfg)
    dm_fontsize <- get_dm_fontsize(component_side, i_s, cfg)
    dm_font <- get_dm_font(component_side, i_s, cfg)
    dm_theta <- get_dm_theta(component_side, cfg)
    dm_r <- get_dm_r(component_side, cfg)
    dm_x <- to_x(dm_theta, dm_r) + 0.5
    dm_y <- to_y(dm_theta, dm_r) + 0.5
    ps_theta <- get_ps_theta(component_side, cfg)
    ps_r <- get_ps_r(component_side, cfg)
    ps_x <- to_x(ps_theta, ps_r) + 0.5
    ps_y <- to_y(ps_theta, ps_r) + 0.5

    list(style=style, bcol=bcol, scol=scol, 
         border_col=border_col, checker_col=checker_col, 
         gridline_col=gridline_col, hexline_col=hexline_col, 
         rank_symbol=rank_symbol, rank_fontsize=rank_fontsize, rank_font=rank_font,
         suit_symbol=suit_symbol, suit_fontsize=suit_fontsize, suit_font=suit_font,
         dm_col=dm_col, dm_symbol=dm_symbol, 
         shape=shape, shape_fn=shape_fn, shape_theta=shape_theta,
         dm_fontsize=dm_fontsize, dm_font=dm_font,
         dm_x=dm_x, dm_y=dm_y, ps_x=ps_x, ps_y=ps_y)
}

#' Make piecepack deck preview svg
#'
#' Make piecepack deck preview svg
#'
#' @param cfg Piecepack configuration list
#' @export
make_piecepack_preview <- function(cfg=c2o()) {
    dir.create(cfg$svg_preview_dir, recursive=TRUE, showWarnings=FALSE)
    pheight <- 2*TILE_WIDTH+3*DIE_WIDTH
    pwidth <- 3*TILE_WIDTH
    svg(file.path(cfg$svg_preview_dir, paste0(cfg$deck_filename, ".svg")), 
        family=cfg$font, width=3*TILE_WIDTH, height=pheight)
    draw_preview(cfg)
    invisible(dev.off())
}

draw_preview <- function(cfg=c2o()) {
    pheight <- 2*TILE_WIDTH+3*DIE_WIDTH
    pwidth <- 3*TILE_WIDTH
    grid.newpage()

    pushViewport(viewport(name="main", width=inch(pwidth), height=inch(pheight)))
    # tiles
    addViewport(y=inch(pheight-TILE_WIDTH), width=inch(3 * TILE_WIDTH), height=inch(2 * TILE_WIDTH), name="tiles")
    downViewport("tiles")
    x_tiles <- c(1/3, 3/3, 3/3, 1/3, 2/3, 2/3) - 1/6
    y_tiles <- c(3/4, 3/4, 1/4, 1/4, 3/4, 1/4)
    for (i_s in 1:cfg$n_suits) {
        draw_component("tile_face", cfg, i_s=i_s, i_r=2, x=x_tiles[i_s], y=y_tiles[i_s])
    }
    if (cfg$n_suits < 5) {
        draw_component("tile_back", cfg, x=x_tiles[5], y=y_tiles[5])
    } 
    if (cfg$n_suits < 6) {
        draw_component("tile_back", cfg, x=x_tiles[6], y=y_tiles[6])
    } 

    # coins
    seekViewport("main")
    addViewport(x=inch(1.5*COIN_WIDTH), y=inch(1.5*DIE_WIDTH), width=inch(3*COIN_WIDTH), height=inch(2*COIN_WIDTH), name="coins")
    downViewport("coins")
    x_coins <- rep(1:3, 2)/3 - 1/6
    y_coins <- rep(c(3/4, 1/4), each=3)
    draw_component("coin_face", cfg, i_r=1, x=x_coins[1], y=y_coins[1])
    draw_component("coin_face", cfg, i_r=2, x=x_coins[2], y=y_coins[2])
    if (cfg$n_suits < 5)
        draw_component("coin_back", cfg, i_s=4, x=x_coins[3], y=y_coins[3])
    else
        draw_component("coin_back", cfg, i_s=5, x=x_coins[3], y=y_coins[3])
    draw_component("coin_face", cfg, i_r=3, x=x_coins[4], y=y_coins[4])
    if (cfg$n_suits < 6)
        draw_component("coin_back", cfg, i_s=2, x=x_coins[5], y=y_coins[5])
    else
        draw_component("coin_back", cfg, i_s=6, x=x_coins[5], y=y_coins[5])
    draw_component("coin_back", cfg, i_s=3, x=x_coins[6], y=y_coins[6])

    # suitrank die
    seekViewport("main")
    addViewport(x=3/3-1/6, y=inch(1.5*DIE_WIDTH), width=inch(4 * DIE_WIDTH), height=inch(3 * DIE_WIDTH), name="suitrankdie")
    seekViewport("suitrankdie")
    draw_suitrank_die(cfg)

    # saucers
    seekViewport("main")
    addViewport(x=inch(TILE_WIDTH+2.5*DIE_WIDTH), y=inch(2*DIE_WIDTH), 
                width=inch(2 * SAUCER_WIDTH), height=inch(SAUCER_WIDTH), name="saucers")
    downViewport('saucers')
    draw_component("saucer_face", cfg, i_s=1, x=0.25, y=0.5)
    draw_component("saucer_back", cfg, x=0.75, y=0.5)
}


get_pp_width <- function(component) {
    switch(component, 
           belt = BELT_WIDTH,
           chip = CHIP_WIDTH,
           coin = COIN_WIDTH,
           pawn = PAWN_WIDTH,
           ppdie = DIE_WIDTH,
           saucer = SAUCER_WIDTH,
           suitdie = DIE_WIDTH,
           tile = TILE_WIDTH,
           1)
}

get_pp_height <- function(component) {
    switch(component, 
           belt = BELT_HEIGHT,
           chip = CHIP_WIDTH,
           coin = COIN_WIDTH,
           pawn = PAWN_HEIGHT,
           ppdie = DIE_WIDTH,
           saucer = SAUCER_WIDTH,
           suitdie = DIE_WIDTH,
           tile = TILE_WIDTH,
           1)
}

pp_device <- function(filename, component, format, theta) {
    width <- get_pp_width(component)
    height <- get_pp_height(component)
    if (theta %in% c(90, 270)) {
        twidth <- height
        height <- width
        width <- twidth
    }
    res <- 72
    bg <- "transparent"
    dev <- switch(format,
                bmp = bmp(filename, width, height, "in", res=res, bg=bg),
                jpeg = jpeg(filename, width, height, "in", res=res, bg=bg),
                pdf = cairo_pdf(filename, width, height, bg=bg),
                png = png(filename, width, height, "in", res=res, bg=bg),
                ps = cairo_ps(filename, width, height, bg=bg),
                svg = svg(filename, width, height, bg=bg),
                tiff = tiff(filename, width, height, "in", res=res, bg=bg))
    addViewport(angle=theta, name="main")
    downViewport("main")
}

#' Make piecepack images
#'
#' Makes images of individual piecepack components.
#'
#' @param cfg Piecepack configuration list
#' @export
make_piecepack_images <- function(cfg) {
    for (format in cfg$component_formats) {
        directory <- component_directory(cfg, format)
        dir.create(directory, recursive=TRUE, showWarnings=FALSE)
        for (theta in cfg$component_thetas) {
            make_images_helper(cfg, format, theta)
        }
    }
}

component_directory <- function(cfg, format) {
    file.path(cfg[[paste0(format, "_component_dir")]], cfg$deck_filename)
}

component_filename <- function(cfg, component_side, format, theta, 
                               i_s=NULL, i_r=NULL) {
    directory <- component_directory(cfg, format)
    filename <- paste0(component_side, 
                       ifelse(is.null(i_s), "", paste0("_s", i_s)),
                       ifelse(is.null(i_r), "", paste0("_r", i_r)),
                       paste0("_t", theta), paste0(".", format))
    file.path(directory, filename)
}

make_images_helper <- function(cfg, format, theta) {
    suppressWarnings({
        for (component_side in c("saucer_back", "tile_back")) {
            f <- component_filename(cfg, component_side, format, theta)
            pp_device(f, get_component(component_side), format, theta)
            draw_component(component_side, cfg)
            invisible(dev.off())
        }

        for (i_s in 1:cfg$n_suits) {
            for (component_side in c("belt_face", "chip_back", "coin_back", 
                                       "pawn_back", "pawn_face",
                                       "saucer_face", "suitdie_face")) {
                f <- component_filename(cfg, component_side, format, theta, i_s)
                pp_device(f, get_component(component_side), format, theta)
                draw_component(component_side, cfg, i_s)
                invisible(dev.off())
            }

            for (i_r in 1:cfg$n_ranks) {
                for (component_side in c("chip_face", "ppdie_face", "tile_face")) {
                    f <- component_filename(cfg, component_side, format, theta, i_s, i_r)
                    pp_device(f, get_component(component_side), format, theta)
                    draw_component(component_side, cfg, i_s, i_r)
                    invisible(dev.off())
                }
            }
        }
        for (i_r in 1:cfg$n_ranks) {
            component_side <- "coin_face"
            f <- component_filename(cfg, component_side, format, theta, i_r=i_r)
            pp_device(f, get_component(component_side), format, theta)
            draw_component(component_side, cfg, i_r=i_r)
            invisible(dev.off())
        }
    })
}

add_gridlines <- function(opt) {
    gridline_col <- opt$gridline_col
    shape <- opt$shape
    o <- 0.02
    if (shape == "rect") {
        lwd <- 8
        gp_gl <- gpar(col=gridline_col, lwd=lwd, lineend="butt")
        grid.lines(x=0.5, gp=gp_gl)
        grid.lines(y=0.5, gp=gp_gl)
        # seg(0.5, 0+o, 0.5, 1-o, gridline_col, lwd=lwd, lineend="square")
        # seg(0+o, 0.5, 1-o, 0.5, gridline_col, lwd=lwd, lineend="square")
    } else if (shape %in% c("circle", "kite", "halma")) {
        stop(paste("Don't know how to add grid lines to shape", shape))
    } else {
        o <- 0.01
        lwd <- 4
        n_vertices <- as.numeric(opt$shape)
        theta <- opt$shape_theta
        theta <- seq(0, 360, length.out=n_vertices+1) + theta
        theta <- theta[1:(length(theta)-1)]
        nt <- length(theta)
        n <- floor(nt / 2)
        r <- 0.5 - o
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
    grid.text(opt$rank_symbol, x=opt$ps_x, y=opt$ps_y, gp=rgp)
}

add_suit_symbol <- function(opt) {
    sgp <- gpar(col=opt$scol, fontsize=opt$suit_fontsize, fontfamily=opt$suit_font)
    grid.text(opt$suit_symbol, x=opt$ps_x, y=opt$ps_y, gp=sgp)
}

add_background <- function(opt) {
    opt$shape_fn(gp = gpar(col=NA, fill=opt$bcol))
}

add_border <- function(opt) {
    opt$shape_fn(gp = gpar(col=opt$border_col, fill=NA))
}
add_belt_ribbons <- function(opt) {
    addViewport(y=0.1, height=0.2, name="bottom_ribbon")
    downViewport("bottom_ribbon")
    grid.rect(gp=gpar(col=NA, fill=opt$scol))
    upViewport()
    addViewport(y=0.9, height=0.2, name="top_ribbon")
    downViewport("top_ribbon")
    grid.rect(gp=gpar(col=NA, fill=opt$scol))
    upViewport()
}

draw_component_helper <- function(component_side, i_s, i_r, cfg) {
    opt <- get_component_opt(component_side, i_s, i_r, cfg)

    add_background(opt)
    if (component_side %in% c("tile_back", "tile_face"))
        add_tile_elements(opt)
    if (component_side == "belt_face") 
        add_belt_ribbons(opt)
    if (component_side %in% c("belt_face", "chip_back", "coin_back", "pawn_face", "pawn_back", 
                              "saucer_back", "saucer_face", "suitdie_face")) 
        add_suit_symbol(opt) 
    if (component_side %in% c("chip_face", "coin_face", "ppdie_face", "tile_face")) 
        add_rank_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
}

draw_pawn <- function(i_s, cfg) {
    suppressWarnings({
        denominator <- PAWN_HEIGHT + PAWN_BASE
        y <- (PAWN_HEIGHT/2 + PAWN_BASE) / denominator
        height = PAWN_HEIGHT / denominator
        addViewport(y=0.25, height=0.5, name="pawn_front")
        downViewport("pawn_front")
        addViewport(y=y, height=height, name="pawn_face")
        downViewport("pawn_face")
        draw_component("pawn_face", cfg, i_s)
        upViewport()
        upViewport()
        addViewport(y=0.75, height=0.5, name="pawn_rear", angle=180)
        downViewport("pawn_rear")
        addViewport(y=y, height=height, name="pawn_back")
        downViewport("pawn_back")
        draw_component("pawn_back", cfg, i_s)
        upViewport()
        upViewport()
    })
    grid.lines(y=0.5, gp=gpar(col=cfg$border_col, fill=NA, lty="dashed"))
    grid.rect(gp=gpar(col=cfg$border_col, fill=NA))
    ll <- 0.07
    seg(0.5, 0, 0.5, ll, cfg$border_col)
    seg(0.5, 1, 0.5, 1-ll, cfg$border_col)
}

#' Draw piecepack component
#' 
#' Draws a piecepack component onto the graphics device
#' 
#' @param component_side A string with component and side separated by a underscore e.g. "coin_face"
#' @param cfg Piecepack configuration list
#' @param i_s Number of suit
#' @param i_r Number of rank
#' @param x Number between 0 and 1 of where to place component
#' @param y Number between 0 and 1 of where to place component
#' @param width Width of component
#' @param height Height of component
#' @param svg If \code{TRUE} instead of drawing directly into graphics device
#'            export to svg, re-import svg, and then draw it to graphics device.  
#'            This is useful if drawing really big or small and don't want
#'            to play with re-configuring fontsizes.
#' @param ... Extra arguments to pass to \code{grid::viewport} like \code{angle}.
#' @export
draw_component <- function(component_side, cfg=c2o(), i_s=cfg$i_unsuit, i_r=1, x=0.5, y=0.5, 
                           width=NULL, height=NULL, svg=FALSE, ...) {
    component <- get_component(component_side)
    if (is.null(width))
        width=inch(get_pp_width(component))
    if (is.null(height))
        height=inch(get_pp_height(component))
    if (svg) {
        svg_file <- tempfile(fileext=".svg")
        on.exit(unlink(svg_file))
        pp_width=get_pp_width(component)
        pp_height=get_pp_height(component)

        svg(svg_file, width=pp_width, height=pp_height)
        draw_component(component_side, cfg, i_s, i_r)
        invisible(dev.off())

        pushViewport(viewport(x=x, y=y, width=width, height=height, ...))
        grid.draw(pictureGrob(readPicture(svg_file, warn=FALSE)))
        upViewport()
    } else {
        pushViewport(viewport(x=x, y=y, width=width, height=height, ...))
        draw_component_helper(component_side, i_s, i_r, cfg)
        upViewport()
    }
}
