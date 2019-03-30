#' @importFrom grDevices bmp cairo_pdf cairo_ps dev.off jpeg png svg tiff
#' @import grid

COMPONENTS <- c("tile", "coin", "die", "suitdie", 
           "pawn", "belt", "saucer", "chip", "pyramid")
COMPONENT_AND_SIDES <- c("tile_back", "tile_face", 
           "coin_back", "coin_face",
           "die_face", "suitdie_face", 
           "pawn_face", "pawn_back", 
           "belt_face",  "saucer_face", "saucer_back",
           "chip_face", "chip_back", 
           "pyramid_face", "pyramid_back", "pyramid_left", "pyramid_right")
#### "pawn_layout", "die_layout", "suitdie_layout", "suitrankdie_layout", "pyramid_layout", "pyramid_top""
COIN_WIDTH <- 3/4
DIE_WIDTH <- 1/2
TILE_WIDTH <- 2
SAUCER_WIDTH <- 7/8
CHIP_WIDTH <- 5/8
PAWN_HEIGHT <- 7/8
PAWN_WIDTH <- 1/2
PAWN_BASE <- 3/8
PAWN_LAYOUT_HEIGHT <- 2 * PAWN_HEIGHT + 2 * PAWN_BASE
DIE_LAYOUT_WIDTH <- 4 * DIE_WIDTH
DIE_LAYOUT_HEIGHT <- 3 * DIE_WIDTH
BELT_HEIGHT <- 1/2
BELT_WIDTH <- 4 * DIE_WIDTH
PYRAMID_WIDTHS <- 2:8 * 1/8
PYRAMID_HEIGHTS <- 1.538842 * PYRAMID_WIDTHS
PYRAMID_DIAGONALS <- sqrt(PYRAMID_HEIGHTS^2 + (0.5*PYRAMID_WIDTHS)^2)
PYRAMID_LAYOUT_WIDTHS <- PYRAMID_HEIGHTS
PYRAMID_LAYOUT_HEIGHTS <- 2*PYRAMID_DIAGONALS
W <- 3/16
S <- 1
MATCHSTICK_WIDTHS <- c(2*W, rep(W, 5))
MATCHSTICK_HEIGHTS <- c(2*W, S-W, sqrt(2)*S-W, 2*S-W, sqrt(5*S^2)-W, 2*sqrt(2)*S-W)

#' Convert delimiter separated string to vector
#'
#' Converts delimiter separated string to a vector.
#'
#' @param x String to convert
#' @param sep Delimiter (defaults to ",")
#' @param float If `TRUE` cast to numeric
#' @param color if `TRUE` convert empty strings to `"transparent"`
#' @export
cleave <- function(x, sep=",", float=FALSE, color=FALSE) {
    vec <- stringr::str_split(x, sep)
    if (length(vec)) vec <- vec[[1]]
    if (float) {
        as.numeric(vec)
    } else if (color) {
        gsub("^$", "transparent", vec)
    } else {
        vec
    }
} 
col_cleave <- function(x, sep=",") { cleave(x, color=TRUE) }
numeric_cleave <- function(x, sep=",") { cleave(x, sep, float=TRUE) }

seg <- function(x, y, xend, yend, color="black", ...) {
    grid.segments(x0=x, y0=y, x1=xend, y1=yend, gp=gpar(col=color, ...))
}

####
addViewport <- function(...) { 
    pushViewport(viewport(...))
    upViewport()
}

grid.halma <- function(gp=gpar()) {
    y_cutoff <- 0.55
    y_frac <- 0.5
    theta <- rev(seq(0, 360, length.out=100) - 90)
    r <- 0.5
    x <- 0.5 + to_x(theta, r)
    y <- 1 + y_frac * (to_y(theta, r) - r)
    indices <- which(y >= y_cutoff)
    grid.polygon(x = c(0,0, x[indices],1,1), y=c(0,0.3,y[indices],0.3,0), gp=gp)
}

grid.pyramid <- function(gp=gpar()) {
    grid.polygon(x = c(0, 0.5, 1), y = c(0, 1, 0), gp=gp)
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

grid.kite <- function(gp=gpar()) {
    x <- c(0.5, 0, 0.5, 1, 0.5)
    y <- c(0, 0.25, 1, 0.25, 0)
    grid.polygon(x, y, gp=gp)
}

grid.pp.polygon_fn <- function(n_vertices, theta) { 
    theta <- seq(0, 360, length.out=n_vertices+1) + theta
    r <- 0.5
    x <- to_x(theta, r) + 0.5
    y <- to_y(theta, r) + 0.5
    function(gp=gpar()) { grid.polygon(x, y, gp=gp) } 
}

splice <- function(x0, x1) {
    vec <- as.numeric()
    for (ii in 1:length(x1)) {
        vec <- append(vec, x0[ii])
        vec <- append(vec, x1[ii])
    }
    append(vec, x0[ii+1])
}

grid.concave_fn <- function(n_vertices, theta, r=0.2) {
    theta_outer <- seq(0, 360, length.out=n_vertices+1) + theta
    n_degrees <- 360 / n_vertices / 2
    theta_inner <- seq(n_degrees, 360-n_degrees, length.out=n_vertices) + theta
    r_outer <- 0.5
    r_inner <- r
    x_outer <- to_x(theta_outer, r_outer) + 0.5
    x_inner <- to_x(theta_inner, r_inner) + 0.5
    y_outer <- to_y(theta_outer, r_outer) + 0.5
    y_inner <- to_y(theta_inner, r_inner) + 0.5
    x <- splice(x_outer, x_inner)
    y <- splice(y_outer, y_inner)
    function(gp=gpar()) { grid.polygon(x, y, gp=gp) }
}

get_component <- function(component_side) {
    cleave(component_side, "_")[1]
}

# component_side="pawn_face"
# i_s=0
# i_r=0
# style="font"
# 
# if(is.na(component_side)) {
#     component <- NULL
# } else {
#     component <- get_component(component_side)
# }
# suited <- is_suited(component_side, i_s, cfg)
# if (is.null(suited)) suited <- FALSE
# component_affixes <- c(paste0(".", component_side), paste0(".", component), "")
# component_score <- c(2, 1, 0)
# rank_affixes <- c(paste0(".r", i_r), "")
# rank_score <- c(1, 0)
# suit_affixes <- c(paste0(".s", i_s), ifelse(suited, ".suited", ".unsuited"), "")
# suit_score <- c(2, 1, 0)
# dfs <- expand.grid(component_affixes, rank_affixes, suit_affixes, stringsAsFactors=FALSE)
# names(dfs) <- c("component", "rank", "suit")
# dfs$order <- 1:nrow(dfs)
# dfs <- left_join(dfs, data.frame(component=component_affixes, component_score, stringsAsFactors=FALSE), by="component")
# dfs <- left_join(dfs, data.frame(rank=rank_affixes, rank_score, stringsAsFactors=FALSE), by="rank")
# dfs <- left_join(dfs, data.frame(suit=suit_affixes, suit_score, stringsAsFactors=FALSE), by="suit")
# dfs <- mutate(dfs, score = 1e3 * component_score + 1e2 * rank_score + 1e1 * suit_score,
#               affix = paste0(suit, rank, component))
# dfs <- arrange(dfs, desc(score))
# dfs$order

style_ordering <- c(1, 7, 13, 4, 10, 16, 2, 8, 14, 5, 11, 17, 3, 9, 15, 6, 12, 18)

get_style_element <- function(style, component_side=NA, cfg=list(), default=NULL, i_s=0, i_r=0) {

    if(is.na(component_side)) {
        component <- NULL
    } else {
        component <- get_component(component_side)
    }
    suited <- is_suited(component_side, i_s, i_r, cfg)
    if (is.null(suited)) suited <- FALSE

    component_affixes <- c(paste0(".", component_side), paste0(".", component), "")
    rank_affixes <- c(paste0(".r", i_r), "")
    suit_affixes <- c(paste0(".s", i_s), ifelse(suited, ".suited", ".unsuited"), "")
    dfs <- expand.grid(component_affixes, rank_affixes, suit_affixes, stringsAsFactors=FALSE)
    names(dfs) <- c("component", "rank", "suit")
    affixes <- paste0(dfs$suit, dfs$rank, dfs$component)[style_ordering]

    for (affix in affixes) {
        string <- paste0(style, affix)
        value <- cfg[[string]]
        if (!is.null(value)) return (value)
    }
    return (default)
}

get_background_color_helper <- function(component_side, i_s, i_r, cfg) {
    colors <- col_cleave(get_style_element("background_colors", component_side, cfg, "white", i_s, i_r))
    expand_suit_elements(colors, "background_colors", component_side, cfg)[i_s]
}

get_suit_colors <- function(component_side=NA, i_s, i_r, cfg=list(), expand=TRUE) {
    suit_colors <- col_cleave(get_style_element("suit_colors", component_side, cfg, 
                            "#D55E00,#000000,#009E73,#56B4E9,#E69F00"))
    if (expand)
        suit_colors <- expand_suit_elements(suit_colors, "suit_colors", component_side, cfg) 
    suit_colors
}

get_border_colors <- function(component_side=NA, i_s=0, i_r=0, cfg=list(), expand=TRUE) {
    border_colors <- col_cleave(get_style_element("border_colors", component_side, cfg, "grey", i_s, i_r))
    if (expand)
        border_colors <- expand_suit_elements(border_colors, "border_colors", component_side, cfg)
    border_colors
}

get_border_color <- function(component_side, i_s, i_r, cfg) {
    get_border_colors(component_side, i_s, i_r, cfg)[i_s]
}

get_shape_theta <- function(component_side, i_s, i_r, cfg) {
    theta <- numeric_cleave(get_style_element("shape_theta", component_side, cfg, 90, i_s, i_r))
    theta <- expand_suit_elements(theta, "shape_theta", component_side, cfg)
    theta[i_s] 
}

get_shape_r <- function(component_side, i_s, i_r, cfg) {
    r <- numeric_cleave(get_style_element("shape_r", component_side, cfg, 0.2, i_s, i_r))
    r <- expand_suit_elements(r, "shape_r", component_side, cfg)
    r[i_s] 
}

get_shape <- function(component_side, i_s, i_r, cfg) {
    default <- switch(component_side,
               tile_back = "rect",
               tile_face = "rect",
               coin_back = "circle",
               coin_face = "circle",
               die_face = "rect",
               matchstick_back = "rect",
               matchstick_face = "rect",
               pawn_face = "halma",
               pawn_back = "halma",
               saucer_face = "circle",
               saucer_back = "circle",
               pyramid_face = "pyramid",
               pyramid_left = "pyramid",
               pyramid_right = "pyramid",
               pyramid_back = "pyramid",
               belt_face = "rect",
               suitdie_face = "rect",
               chip_face = "circle",
               chip_back = "circle",
               stop(paste("Don't know correct shape for", component_side)))
    get_style_element("shape", component_side, cfg, default, i_s, i_r)
}

get_n_vertices <- function(shape) {
    as.numeric(gsub("convex|concave", "", shape))
}

get_grid_shape <- function(shape, theta=0, r=0.2) {
    if (shape == "circle") {
        grid.circle
    } else if (shape == "rect") {
        grid.rect
    } else if (shape == "kite") {
        grid.kite
    } else if (shape == "halma") {
        grid.halma
    } else if (shape == "pyramid") {
        grid.pyramid
    } else if (grepl("^concave", shape)) {
        grid.concave_fn(get_n_vertices(shape), theta, r) 
    } else if (grepl("^convex", shape)) {
        grid.pp.polygon_fn(get_n_vertices(shape), theta)
    } else {
        stop(paste("Don't know how to draw shape", shape))
    }
}

get_shape_fn <- function(component_side, i_s, i_r, cfg) {
    shape <- get_shape(component_side, i_s, i_r, cfg)
    if(grepl("^con", shape)) 
        theta <- get_shape_theta(component_side, i_s, i_r, cfg)
    else
        theta <- 0
    if(grepl("^concave", shape))
        r <- get_shape_r(component_side, i_s, i_r, cfg)
    else
        r <- 0.2
    get_grid_shape(shape, theta, r)
}

get_suit_color_helper <- function(component_side, i_s, i_r, cfg=list()) {
    suit_colors <- get_suit_colors(component_side, i_s, i_r, cfg)
    ifelse(i_s <= get_n_suits(cfg), suit_colors[i_s], suit_colors[get_i_unsuit(cfg)])
}

should_invert <- function(component_side, i_s, i_r, cfg) {
    should_inverts <- get_style_element("invert_colors", component_side, cfg, FALSE, i_s, i_r)
    expand_suit_elements(should_inverts, "should_inverts", component_side, cfg)[i_s]
}

is_suited <- function(component_side, i_s, i_r, cfg) {
    switch(component_side,
           tile_back = FALSE,
           tile_face = TRUE, 
           coin_back = TRUE,
           coin_face = FALSE,
           die_face = ifelse(i_s <= get_i_unsuit(cfg), TRUE, FALSE),
           suitdie_face = ifelse(i_s <= get_i_unsuit(cfg), TRUE, FALSE),
           saucer_face = TRUE,
           saucer_back = FALSE,
           pawn_face = TRUE,
           pawn_back = TRUE,
           belt_face = TRUE,
           chip_face = TRUE,
           chip_back = TRUE)
}

get_dm_theta <- function(component_side, i_s, i_r, cfg) {
    default <- ifelse(component_side %in% c("tile_face", "die_face", "suitdie_face"), 135, 90)
    default <- ifelse(component_side == "matchstick_face" && i_r == 1, 135, default)
    theta <- numeric_cleave(get_style_element("dm_theta", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(theta, "dm_theta", component_side, cfg)[i_s]
}
get_dm_r <- function(component_side, i_s, i_r, cfg) {
    shape <- get_shape(component_side, i_s, i_r, cfg)
    r_corner <- sqrt(0.25^2 + 0.25^2)
    default <- switch(shape,
                     rect = switch(component_side, 
                                   matchstick_face = ifelse(i_r > 1, 0.4, r_corner),
                                   r_corner),
                     circle = r_corner,
                     halma = 0.25,
                     pyramid = 0.1,
                     0.3)
    r <- numeric_cleave(get_style_element("dm_r", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(r, "dm_r", component_side, cfg)[i_s]
}

get_ps_theta <- function(component_side, i_s, i_r, cfg) {
    shape <- get_shape(component_side, i_s, i_r, cfg)
    default <- switch(shape, halma=-90, pyramid=-90, 0)
    theta <- numeric_cleave(get_style_element("ps_theta", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(theta, "ps_theta", component_side, cfg)[i_s]
}
get_ps_r <- function(component_side, i_s, i_r, cfg) {
    shape <- get_shape(component_side, i_s, i_r, cfg)
    default <- switch(shape, halma=0.25, pyramid=0.25, 0.0)
    r <- numeric_cleave(get_style_element("ps_r", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(r, "ps_r", component_side, cfg)[i_s]
}

get_n_suits <- function(cfg=list()) {
    if (is.null(cfg[["n_suits"]])) {
        length(get_suit_symbols(cfg=cfg, expand=FALSE)) - 1
    } else {
        cfg[["n_suits"]]
    }
}

get_n_ranks <- function(cfg=list()) {
    if (is.null(cfg[["n_ranks"]])) {
        length(get_rank_symbols(cfg=cfg, expand=FALSE))
    } else {
        cfg[["n_ranks"]]
    }
}

get_i_unsuit <- function(cfg=list()) { get_n_suits(cfg) + 1 }

get_dm_symbols <- function(component_side, i_s=0, i_r=0, cfg=list()) {
    default <- {
        if (component_side %in% c("coin_back", "coin_face")) {
            dm_symbols <- "\u25cf" # "●"
        } else if (component_side %in% c("chip_back")) {
            dm_symbols <- get_suit_symbols(component_side, i_s, i_r, cfg)
            # dm_symbols <- "\u26c3" # "⛃"
        } else if (component_side %in% c("saucer_back", "saucer_face")) {
            dm_symbols <- "\u25b2" # "▲"
            # dm_symbols <- "\u265f" # "♟"
        } else if (component_side %in% c("pawn_face", "pyramid_face")) {
            dm_symbols <- "\u0298\u0298" # "ʘʘ"
            # dm_symbols <- "\u2c7a\u2c7a" # "ⱺⱺ"
            # dm_symbols <- "\U0001f440" # "👀"
            # dm_symbols <- "\U0001f603" # "😃"
        } else if (component_side %in% c("suitdie_face", "pawn_back", 
                                         "belt_face", "tile_back", "matchstick_back",
                                         "pyramid_left", "pyramid_right", "pyramid_back")) {
            dm_symbols <- ""
        } else {
            dm_symbols <- get_suit_symbols(component_side, i_s, i_r, cfg)
        }
    }
    default <- paste(default, collapse=",")
    dm_symbols <- cleave(get_style_element("dm_symbols", component_side, cfg, default, i_s, i_r))
    dm_symbols <- expand_suit_elements(dm_symbols, "suit_symbols", component_side, cfg)
    dm_symbols
}

get_dm_symbol <- function(component_side, i_s, i_r, cfg) {
    get_dm_symbols(component_side, i_s, i_r, cfg)[i_s]
}

get_dm_color <- function(component_side, i_s, i_r, cfg) {
    default <- get_suit_color(component_side, i_s, i_r, cfg)
    colors <- get_style_element("dm_colors", component_side, cfg, default, i_s, i_r)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "suit_colors", component_side, cfg)
    colors[i_s]
}

get_background_color <- function(component_side, i_s, i_r, cfg) {
    bcol <- get_background_color_helper(component_side, i_s, i_r, cfg)
    scol <- get_suit_color_helper(component_side, i_s, i_r, cfg)
    if (should_invert(component_side, i_s, i_r, cfg))
        scol
    else
        bcol
}

get_suit_color <- function(component_side, i_s, i_r, cfg) {
    bcol <- get_background_color_helper(component_side, i_s, i_r, cfg)
    scol <- get_suit_color_helper(component_side, i_s, i_r, cfg)
    if (should_invert(component_side, i_s, i_r, cfg))
        bcol
    else
        scol
}
get_checker_color <- function(component_side, i_s, i_r, cfg) {
    colors <- col_cleave(get_style_element("checker_colors", component_side, cfg, "transparent", i_s, i_r))
    colors <- expand_suit_elements(colors, "checker_colors", component_side, cfg)
    colors[i_s]
}
get_gridline_color <- function(component_side, i_s, i_r, cfg) {
    if (component_side == "tile_back") {
        default <- c(rep("transparent", get_n_suits(cfg)), 
                     get_suit_color(component_side, get_i_unsuit(cfg), i_r, cfg))
    } else {
        default <- "transparent"
    }
    default <- paste(default, collapse=",")
    colors <- get_style_element("gridline_colors", component_side, cfg, default, i_s, i_r)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "gridline_colors", component_side, cfg)
    colors[i_s]
}
get_hexline_color <- function(component_side, i_s, i_r, cfg) {
    colors <- get_style_element("hexline_colors", component_side, cfg, "transparent", i_s, i_r)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "hexline_colors", component_side, cfg)
    colors[i_s]
}
get_ribbon_color <- function(component_side, i_s, i_r, cfg) {
    default <- {
        if (component_side == "belt_face") {
            get_suit_color(component_side, i_s, i_r, cfg)
        } else {
            "transparent"
        }
    }
    colors <- get_style_element("ribbon_colors", component_side, cfg, default, i_s, i_r)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "suit_colors", component_side, cfg)
    colors[i_s]
}

get_suit_symbol <- function(component_side, i_s, i_r, cfg) {
    get_suit_symbols(component_side, i_s, i_r, cfg)[i_s]
}

get_rank_symbols <- function(component_side=NA, i_s=0, i_r=0, cfg=list(), expand=TRUE) {
    default <- "n,a,2,3,4,5"
    rank_symbols <- cleave(get_style_element("rank_symbols", component_side, cfg, default, i_s, i_r))
    if (expand)
        rank_symbols <- expand_rank_elements(rank_symbols, "rank_symbols", component_side, cfg)
    rank_symbols
}
get_suit_symbols <- function(component_side=NA, i_s=0, i_r=0, cfg=list(), expand=TRUE) {
    default <- "\u2665,\u2660,\u2663,\u2666,\u263c" # "♥,♠,♣,♦,☼"
        # "\u2665,\u2660,\u2663,\u2666,\u2302" # "♥,♠,♣,♦,⌂"
        # "\u2665,\u2660,\u2663,\u2666,\u2605" # "♥,♠,♣,♦,★"
    suit_symbols <- cleave(get_style_element("suit_symbols", component_side, cfg, default, i_s, i_r))
    if (expand) 
        suit_symbols <- expand_suit_elements(suit_symbols, "suit_symbols", component_side, cfg)
    suit_symbols
}

expand_suit_elements <- function(elements, style, component_side, cfg) {
    if (length(elements) < get_n_suits(cfg)) {
        elements <- rep(elements, length.out=get_n_suits(cfg) + 1)
    } else if (length(elements) == get_n_suits(cfg)) {
        elements <- c(elements, switch(style, scale=1.0, ""))
    }
    if (length(elements) == get_i_unsuit(cfg)) {
        elements <- c(elements, switch(style, 
                           suit_symbols = switch(ifelse(is.na(component_side), "NA", component_side), 
                                suitdie_face = "", die_face = "", elements[get_i_unsuit(cfg)]),
                           gridline_colors = NA,
                           elements[get_i_unsuit(cfg)]))
    }
    elements
}

expand_rank_elements <- function(elements, style, component_side, cfg) {
    if (length(elements) < get_n_ranks(cfg)) {
        elements <- rep(elements, length.out=get_n_ranks(cfg))
    } 
    if (length(elements) == get_n_ranks(cfg))
        elements <- c(elements, switch(style, rank_symbols = "", elements[get_n_ranks(cfg)]))
    elements
}

get_use_suit_as_ace <- function(component_side=NA, i_s=0, i_r=0, cfg=list()) {
    get_style_element("use_suit_as_ace", component_side, cfg, FALSE, i_s, i_r)
}

get_rank_symbol <- function(component_side, i_s, i_r, cfg) {
    rank_symbols <- get_rank_symbols(component_side, i_s, i_r, cfg)
    suit_symbols <- get_rank_suit_symbols(component_side, i_s, i_r, cfg)
    use_suit_as_ace <- get_use_suit_as_ace(component_side, i_s, i_r, cfg)
    if (i_r == 2 && use_suit_as_ace) {
        rank_symbol <- suit_symbols[i_s]
    } else {
        rank_symbol <- rank_symbols[i_r]
    }
    rank_symbol
}       

get_rank_scales <- function(component_side, i_s, i_r, cfg) {
    scales <- numeric_cleave(get_style_element("rank_symbols_scale", component_side, cfg, 1.0, i_s, i_r))
    expand_rank_elements(scales, "scale", component_side, cfg)
}
get_rank_scale <- function(component_side, i_s, i_r, cfg) {
    rank_scale <- get_rank_scales(component_side, i_s, i_r, cfg)[i_r]
    suit_scale <- get_suit_scale(component_side, i_s, i_r, cfg)
    use_suit_as_ace <- get_use_suit_as_ace(component_side, i_s, i_r, cfg)
    if (i_r == 2 && use_suit_as_ace)
        scale <- suit_scale
    else
        scale <- rank_scale
    scale
}
get_rank_fonts <- function(component_side, i_s, i_r, cfg) {
    fonts <- cleave(get_style_element("rank_symbols_font", component_side, cfg, get_font(cfg), i_s, i_r))
    expand_rank_elements(fonts, "font", component_side, cfg)
}
get_rank_font <- function(component_side, i_s, i_r, cfg) {
    rank_font <- get_rank_fonts(component_side, i_s, i_r, cfg)[i_r]
    suit_font <- get_suit_font(component_side, i_s,  i_r, cfg)
    use_suit_as_ace <- get_use_suit_as_ace(component_side, i_s, i_r, cfg)
    if (i_r == 2 && use_suit_as_ace)
        suit_font
    else
        rank_font
}
get_suit_fonts <- function(component_side, i_s, i_r, cfg) {
    fonts <- cleave(get_style_element("suit_symbols_font", component_side, cfg, get_font(cfg), i_s, i_r))
    expand_suit_elements(fonts, "font", component_side, cfg)
}
get_suit_font <- function(component_side, i_s, i_r, cfg) {
    get_suit_fonts(component_side, i_s, i_r, cfg)[i_s]
}
get_dm_fonts <- function(component_side, i_s, i_r, cfg) {
    default <- {
        boolean <- identical(get_dm_symbols(component_side, i_s, i_r, cfg),
                     get_suit_symbols(component_side, i_s, i_r, cfg))
        if (boolean) { 
            fonts <- get_suit_fonts(component_side, i_s, i_r, cfg)
        } else {
            fonts <- get_font(cfg)
        }
    }
    default <- paste(default, collapse=",")
    fonts <- cleave(get_style_element("dm_symbols_font", component_side, cfg, default))
    expand_suit_elements(fonts, "font", component_side, cfg)
}
get_dm_font <- function(component_side, i_s, i_r, cfg) {
    get_dm_fonts(component_side, i_s, i_r, cfg)[i_s]
}
get_suit_scales <- function(component_side, i_s, i_r, cfg) {
    scales <- numeric_cleave(get_style_element("suit_symbols_scale", component_side, cfg, 1.0, i_s, i_r))
    expand_suit_elements(scales, "scale", component_side, cfg)
}
get_rank_suit_symbols <- function(component_side, i_s, i_r, cfg) {
    suit_symbols <- get_suit_symbols(component_side, i_s, i_r, cfg, expand=FALSE)
    suit_symbols <- expand_suit_elements(suit_symbols, "rank_suit_symbols", component_side, cfg)
    suit_symbols
}
get_suit_scale <- function(component_side, i_s, i_r, cfg) {
    get_suit_scales(component_side, i_s, i_r, cfg)[i_s]
}
get_dm_scales <- function(component_side, i_s, i_r, cfg) {
    default <- {
        boolean <- identical(get_dm_symbols(component_side, i_s, i_r, cfg),
                             get_suit_symbols(component_side, i_s, i_r, cfg))
        if (boolean) { 
            scales <- get_suit_scales(component_side, i_s, i_r, cfg)
        } else {
            scales <- 1.0
        }
    }
    default <- paste(default, collapse=",")
    scales <- numeric_cleave(get_style_element("dm_symbols_scale", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(scales, "scale", component_side, cfg)
}
get_dm_scale <- function(component_side, i_s, i_r, cfg) {
    get_dm_scales(component_side, i_s, i_r, cfg)[i_s]
}

get_suit_fontsize <- function(component_side, i_s, i_r, cfg) {
    default <- switch(component_side,
                 "belt_face" = 22,
                 "chip_back" = 28,
                 "coin_back" = 34,
                 "pawn_face" = 28,
                 "pawn_back" = 28,
                 "saucer_back" = 42,
                 "saucer_face" = 42,
                 "suitdie_face" = 32,
                 "pyramid_face" = 60 * (i_r+1) / 8,
                 "pyramid_back" = 60 * (i_r+1) / 8,
                 24)
    get_style_element("suit_symbols_fontsize", component_side, cfg, default, i_s, i_r)
}

get_dm_fontsize <- function(component_side, i_s, i_r, cfg) {
    default <- switch(component_side,
                 "tile_face" = 40,
                 "pawn_face" = 12,
                 "pawn_back" = 12,
                 "pyramid_face" = 12 * (i_r+1) / 8, 
                 12)
    get_style_element("dm_fontsize", component_side, cfg, default, i_s, i_r)
}

get_rank_fontsize <- function(component_side, i_s, i_r, cfg) {
    default <- switch(component_side,
                 "die_face" = 20,
                 "chip_face" = 22,
                 "coin_face" = 28,
                 "tile_face" = 72,
                 "pyramid_left"  = 60 * (i_r+1) / 8,
                 "pyramid_right" = 60 * (i_r+1) / 8,
                 20)
    get_style_element("rank_symbols_fontsize", component_side, cfg, default, i_s, i_r)
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

get_ps_element <- function(component_side, suit_element, rank_element) {
    if (component_side %in% c("chip_face", "coin_face", "die_face", "tile_face", "pyramid_left", "pyramid_right", "matchstick_face")) {
        rank_element
    } else if (component_side %in% c("tile_back", "matchstick_back")) {
        NULL
    } else {
        suit_element
    }
}
get_ps_font <- function(component_side=NA, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    rank_font <- get_rank_font(component_side, i_s, i_r, cfg)
    suit_font <- get_suit_font(component_side, i_s, i_r, cfg)
    default <- get_ps_element(component_side, suit_font, rank_font)
    get_style_element("ps_font", component_side, cfg, default, i_s, i_r)
}
get_ps_fontsize <- function(component_side, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    rank_fontsize <- get_rank_fontsize(component_side, i_s, i_r, cfg)
    suit_fontsize <- get_suit_fontsize(component_side, i_s, i_r, cfg)
    default <- get_ps_element(component_side, suit_fontsize, rank_fontsize)
    get_style_element("ps_fontsize", component_side, cfg, default, i_s, i_r)
}
get_ps_scale <- function(component_side, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    rank_scale <- get_rank_scale(component_side, i_s, i_r, cfg)
    suit_scale <- get_suit_scale(component_side, i_s, i_r, cfg)
    default <- get_ps_element(component_side, suit_scale, rank_scale)
    get_style_element("ps_scale", component_side, cfg, default, i_s, i_r)
}
get_ps_symbol <- function(component_side, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    rank_symbol <- get_rank_symbol(component_side, i_s, i_r, cfg)
    suit_symbol <- get_suit_symbol(component_side, i_s, i_r, cfg)
    default <- get_ps_element(component_side, suit_symbol, rank_symbol)
    get_style_element("ps_symbol", component_side, cfg, default, i_s, i_r)
}
get_ps_color <- function(component_side, i_s, i_r, cfg) {
    default <- get_suit_color(component_side, i_s, i_r, cfg)
    get_style_element("ps_color", component_side, cfg, default, i_s, i_r)
}

get_component_opt <- function(component_side, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    # Shape
    shape <- get_shape(component_side, i_s, i_r, cfg)
    shape_fn <- get_shape_fn(component_side, i_s, i_r, cfg)
    shape_theta <- get_shape_theta(component_side, i_s, i_r, cfg)

    # Additional colors
    background_col <- get_background_color(component_side, i_s, i_r, cfg)
    border_col <- get_border_color(component_side, i_s, i_r, cfg)
    checker_col <- get_checker_color(component_side, i_s, i_r, cfg)
    gridline_col <- get_gridline_color(component_side, i_s, i_r, cfg)
    hexline_col <- get_hexline_color(component_side, i_s, i_r, cfg) 
    ribbon_col <- get_ribbon_color(component_side, i_s, i_r, cfg)

    # Overall scaling factor
    scale <- get_scale(cfg)

    # Directional mark symbol
    dm_col <- get_dm_color(component_side, i_s, i_r, cfg)
    dm_font <- get_dm_font(component_side, i_s, i_r, cfg)
    dm_scale <- get_dm_scale(component_side, i_s, i_r, cfg)
    dm_fontsize <- scale * dm_scale * get_dm_fontsize(component_side, i_s, i_r, cfg)
    dm_symbol <- get_dm_symbol(component_side, i_s, i_r, cfg)
    dm_theta <- get_dm_theta(component_side, i_s, i_r, cfg)
    dm_r <- get_dm_r(component_side, i_s, i_r, cfg)
    dm_x <- to_x(dm_theta, dm_r) + 0.5
    dm_y <- to_y(dm_theta, dm_r) + 0.5

    # Primary symbol
    ps_col <- get_ps_color(component_side, i_s, i_r, cfg)
    ps_font <- get_ps_font(component_side, i_s, i_r, cfg)
    ps_scale <- get_ps_scale(component_side, i_s, i_r, cfg)
    if (is.null(ps_scale))
        ps_fontsize <- NULL
    else
        ps_fontsize <- scale * ps_scale * get_ps_fontsize(component_side, i_s, i_r, cfg)
    ps_symbol <- get_ps_symbol(component_side, i_s, i_r, cfg)
    ps_theta <- get_ps_theta(component_side, i_s, i_r, cfg)
    ps_r <- get_ps_r(component_side, i_s, i_r, cfg)
    ps_x <- to_x(ps_theta, ps_r) + 0.5
    ps_y <- to_y(ps_theta, ps_r) + 0.5

    list(shape=shape, shape_fn=shape_fn, shape_theta=shape_theta,
         background_col=background_col, border_col=border_col, gridline_col=gridline_col,
         checker_col=checker_col, hexline_col=hexline_col, ribbon_col=ribbon_col, 
         dm_col=dm_col, dm_symbol=dm_symbol, dm_fontsize=dm_fontsize, dm_font=dm_font,
         dm_x=dm_x, dm_y=dm_y, 
         ps_col=ps_col, ps_symbol=ps_symbol, ps_fontsize=ps_fontsize, ps_font=ps_font,
         ps_x=ps_x, ps_y=ps_y)
}

#### Move function out, add support for stdout, option to not create directory?
#' Make piecepack deck preview svg
#'
#' Make piecepack deck preview svg
#'
#' @param cfg Piecepack configuration list
#' @param output_filename Filename of PnP output
#' @export
make_preview <- function(cfg=list(), output_filename="svg/previews/piecepack_deck.svg") {

    unlink(output_filename)
    directory <- dirname(output_filename)
    dir.create(directory, recursive=TRUE, showWarnings=FALSE)

    pheight <- 3*TILE_WIDTH
    pwidth <- 3*TILE_WIDTH
    pp_device(output_filename, width=pwidth, height=pheight)
    draw_preview(cfg)
    invisible(dev.off())
}

#' Draw piecepack deck preview 
#'
#' Draw piecepack deck preview
#'
#' @param cfg Piecepack configuration list
#' @export
draw_preview <- function(cfg=list()) {
    pheight <- 3*TILE_WIDTH
    pwidth <- 3*TILE_WIDTH
    pushViewport(viewport(name="main", width=inch(pwidth), height=inch(pheight)))

    df <- tibble::tribble(
                ~component_side, ~i_s, ~i_r, ~x, ~y,
                "tile_face", 1, 2, 1, 5,
                "tile_face", 2, 2, 5, 5,
                "tile_face", 3, 2, 5, 3,
                "tile_face", 4, 2, 1, 3,
                "tile_face", 5, 2, 3, 5,
                "tile_face", 6, 2, 3, 3,
                "coin_face", NA,  1, 1*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH + 0.5*COIN_WIDTH,
                "coin_face", NA,  2, 2*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH + 0.5*COIN_WIDTH,
                "coin_back",  4, NA, 3*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH + 0.5*COIN_WIDTH,
                "coin_face", NA,  3, 1*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH - 0.5*COIN_WIDTH,
                "coin_back",  2, NA, 2*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH - 0.5*COIN_WIDTH,
                "coin_back",  3, NA, 3*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH - 0.5*COIN_WIDTH,
                "saucer_face",  1, NA, TILE_WIDTH + 1.5*DIE_WIDTH, 0.5 * TILE_WIDTH + 0.5 * SAUCER_WIDTH,
                "saucer_back", NA, NA, TILE_WIDTH + 1.5*DIE_WIDTH, 0.5 * TILE_WIDTH - 0.5 * SAUCER_WIDTH,
                "pawn_face", 2, NA, TILE_WIDTH + 3*DIE_WIDTH, 0.5*TILE_WIDTH + 0.5*PAWN_HEIGHT,
                "pawn_back", 2, NA, TILE_WIDTH + 3*DIE_WIDTH, 0.5*TILE_WIDTH - 0.5*PAWN_HEIGHT,
                "suitrankdie_layoutRF", NA, NA, pwidth - 2*DIE_WIDTH, 0.5*TILE_WIDTH 
          )
    df$angle <- 0

    if (get_n_suits(cfg) < 5) {
        df[5, "component_side"] <- "tile_back"
        df[5, "i_s"] <- get_i_unsuit(cfg)
        df[5, "i_r"] <- NA
    }
    if (get_n_suits(cfg) < 6) {
        df[6, "component_side"] <- "tile_back"
        df[6, "i_s"] <- get_i_unsuit(cfg)
        df[6, "i_r"] <- NA
    }
    if (get_n_suits(cfg) > 4) df[11, "i_s"] <- 5
    if (get_n_suits(cfg) > 5) df[12, "i_s"] <- 6
    df[16, "angle"] <- 180
    draw_components(df, cfg=cfg, units="inches")
}

get_pp_width <- function(component_side, i_r) {
    switch(component_side,
           die_layoutLF = DIE_LAYOUT_WIDTH,
           die_layoutRF = DIE_LAYOUT_WIDTH,
           suitdie_layoutLF = DIE_LAYOUT_WIDTH,
           suitdie_layoutRF = DIE_LAYOUT_WIDTH,
           suitrankdie_layoutLF = DIE_LAYOUT_WIDTH,
           suitrankdie_layoutRF = DIE_LAYOUT_WIDTH,
           pawn_layout = PAWN_WIDTH,
           pyramid_layout = PYRAMID_LAYOUT_WIDTHS[i_r],
           pyramid_top = PYRAMID_WIDTHS[i_r],
           { 
        component <- get_component(component_side)
        switch(component, 
           belt = BELT_WIDTH,
           chip = CHIP_WIDTH,
           coin = COIN_WIDTH,
           die = DIE_WIDTH,
           matchstick = MATCHSTICK_WIDTHS[i_r],
           pawn = PAWN_WIDTH,
           pyramid = PYRAMID_WIDTHS[i_r],
           saucer = SAUCER_WIDTH,
           suitdie = DIE_WIDTH,
           tile = TILE_WIDTH,
           stop(paste("Don't know width of component", component)))
    })
}

get_pp_height <- function(component_side, i_r) {
    switch(component_side,
           die_layoutLF = DIE_LAYOUT_HEIGHT,
           die_layoutRF = DIE_LAYOUT_HEIGHT,
           suitdie_layoutLF = DIE_LAYOUT_HEIGHT,
           suitdie_layoutRF = DIE_LAYOUT_HEIGHT,
           suitrankdie_layoutLF = DIE_LAYOUT_HEIGHT,
           suitrankdie_layoutRF = DIE_LAYOUT_HEIGHT,
           pawn_layout = PAWN_LAYOUT_HEIGHT,
           pyramid_top = PYRAMID_WIDTHS[i_r],
           pyramid_layout = PYRAMID_LAYOUT_HEIGHTS[i_r],
           {
        component <- get_component(component_side)
        switch(component, 
               belt = BELT_HEIGHT,
               chip = CHIP_WIDTH,
               coin = COIN_WIDTH,
               die = DIE_WIDTH,
               matchstick = MATCHSTICK_HEIGHTS[i_r],
               pawn = PAWN_HEIGHT,
               pyramid = PYRAMID_HEIGHTS[i_r],
               saucer = SAUCER_WIDTH,
               suitdie = DIE_WIDTH,
               tile = TILE_WIDTH,
               stop(paste("Don't know height of component", component)))
    })
}

pp_device <- function(filename, component_side=NULL, theta=0, i_r = 1,
                      width=NULL, height=NULL, res=72) {
    format <- tools::file_ext(filename)
    if (is.null(width)) width <- get_pp_width(component_side, i_r)
    if (is.null(height)) height <- get_pp_height(component_side, i_r)
    if (theta %in% c(90, 270)) {
        twidth <- height
        height <- width
        width <- twidth
    }
    bg <- "transparent"
    dev <- switch(format,
                bmp = bmp(filename, width, height, "in", res=res, bg=bg),
                jpeg = jpeg(filename, width, height, "in", res=res, bg=bg),
                pdf = cairo_pdf(filename, width, height, bg=bg),
                png = png(filename, width, height, "in", res=res, bg=bg),
                ps = cairo_ps(filename, width, height, bg=bg),
                svg = svg(filename, width, height, bg=bg),
                tiff = tiff(filename, width, height, "in", res=res, bg=bg))
    pushViewport(viewport(angle=theta, name="main"))
}

component_filename <- function(directory, cfg, component_side, format, theta, 
                               i_s=NULL, i_r=NULL) {
    filename <- paste0(component_side, 
                       ifelse(is.null(i_s), "", paste0("_s", i_s)),
                       ifelse(is.null(i_r), "", paste0("_r", i_r)),
                       paste0("_t", theta), paste0(".", format))
    file.path(directory, filename)
}

#' Make piecepack images
#'
#' Makes images of individual piecepack components.
#'
#' @param cfg Piecepack configuration list
#' @param directory Directory where to place images
#' @param format Format
#' @param thetas Angle to rotate images
#' @export
make_images <- function(cfg=list(), directory=tempdir(), format="svg", thetas=0) {
    for (theta in thetas) make_images_helper(directory, cfg, format, theta)
}
make_images_helper <- function(directory, cfg, format, theta) {
    suppressWarnings({
        for (component_side in c("saucer_back", "tile_back")) {
            f <- component_filename(directory, cfg, component_side, format, theta)
            pp_device(f, component_side, theta)
            draw_component(component_side, cfg)
            invisible(dev.off())
        }

        for (i_s in 1:get_n_suits(cfg)) {
            for (component_side in c("belt_face", "chip_back", "coin_back", 
                                       "pawn_back", "pawn_face",
                                       "saucer_face", "suitdie_face")) {
                f <- component_filename(directory, cfg, component_side, format, theta, i_s)
                pp_device(f, component_side, theta)
                draw_component(component_side, cfg, i_s)
                invisible(dev.off())
            }

            for (i_r in 1:get_n_ranks(cfg)) {
                for (component_side in c("chip_face", "die_face", "tile_face")) {
                    f <- component_filename(directory, cfg, component_side, format, theta, i_s, i_r)
                    pp_device(f, component_side, theta)
                    draw_component(component_side, cfg, i_s, i_r)
                    invisible(dev.off())
                }
            }
        }
        for (i_r in 1:get_n_ranks(cfg)) {
            component_side <- "coin_face"
            f <- component_filename(directory, cfg, component_side, format, theta, i_r=i_r)
            pp_device(f, component_side, theta)
            draw_component(component_side, cfg, i_r=i_r)
            invisible(dev.off())
        }
    })
}

add_gridlines <- function(opt) {
    gridline_col <- opt$gridline_col
    if (is_color_invisible(gridline_col)) return (invisible(NULL))
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
        n_vertices <- get_n_vertices(shape)
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
    if (is_color_invisible(checker_col)) return (invisible(NULL))
    shape <- opt$shape
    if (shape == "rect") {
        grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(col=NA, fill=checker_col))
        grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(col=NA, fill=checker_col))
    } else if (shape %in% c("circle", "kite", "halma")) {
        stop(paste("Don't know how to add checkers to shape", shape))
    } else {
        n_vertices <- get_n_vertices(shape)
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

is_color_invisible <- function(color) {
    if (is.na(color))
        return (TRUE)
    if (color == "transparent")
        return (TRUE)
    return (FALSE)
}

add_hexlines <- function(opt, omit_direction=FALSE) {
    hl_col <- opt$hexline_col
    if(is_color_invisible(hl_col)) return (invisible(NULL))
    shape <- opt$shape
    if (shape != "rect") {
        stop(paste("Don't know how to add hexlines to shape", shape))
    }
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

add_dm_symbol <- function(opt) {
    gp <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, fontfamily=opt$dm_font)
    grid.text(opt$dm_symbol, x=opt$dm_x, y=opt$dm_y, gp=gp)
}
add_primary_symbol <- function(opt) {
    gp <- gpar(col=opt$ps_col, fontsize=opt$ps_fontsize, fontfamily=opt$ps_font)
    grid.text(opt$ps_symbol, x=opt$ps_x, y=opt$ps_y, gp=gp)
}

add_background <- function(opt) {
    opt$shape_fn(gp = gpar(col=NA, fill=opt$background_col))
}

add_border <- function(opt) {
    opt$shape_fn(gp = gpar(col=opt$border_col, fill=NA))
}
add_ribbons <- function(opt) {
    ribbon_col <- opt$ribbon_col
    if (is_color_invisible(ribbon_col)) return (invisible(NULL))
    shape <- opt$shape
    if (shape != "rect")
        stop(paste("Don't know how to add ribbons to shape", shape))
    addViewport(y=0.1, height=0.2, name="bottom_ribbon")
    downViewport("bottom_ribbon")
    grid.rect(gp=gpar(col=NA, fill=ribbon_col))
    upViewport()
    addViewport(y=0.9, height=0.2, name="top_ribbon")
    downViewport("top_ribbon")
    grid.rect(gp=gpar(col=NA, fill=ribbon_col))
    upViewport()
}

draw_component_helper <- function(component_side, i_s, i_r, cfg) {
    default <- switch(component_side,
                      die_layoutLF = draw_die_layoutLF,
                      die_layoutRF = draw_die_layoutRF,
                      suitdie_layoutLF = draw_suitdie_layoutLF,
                      suitdie_layoutRF = draw_suitdie_layoutRF,
                      suitrankdie_layoutLF = draw_suitrankdie_layoutLF,
                      suitrankdie_layoutRF = draw_suitrankdie_layoutRF,
                      pawn_layout = draw_pawn_layout,
                      pyramid_layout = draw_pyramid_layout,
                      pyramid_top = draw_pyramid_top,
                      draw_component_basic)
    draw_fn <- get_style_element("draw_component_fn", component_side, cfg, default, i_s, i_r)
    if (is.character(draw_fn))
        draw_fn <- match.fun(draw_fn)
    draw_fn(component_side, i_s, i_r, cfg)
    invisible(NULL)
}

opt_cache_key <- function(component_side, i_s, i_r) {
    paste(component_side, i_s, i_r, sep=".")
}

#' Create piecepack configuration list with a cache of component opts
#'
#' Adds a \code{cache} and \code{signature} attribute and a \code{pp_cfg} class
#' to a list of configuration options.  
#' The cache stores pre-computed component opt lists.
#' Once done this significantly speeds up the drawing of piecepack components
#' with that configuration list.  However if you later change the configuration list you
#' should run this again to re-compute the cache otherwise \code{draw_component} may 
#' not work as intended.
#' @param cfg List of configuration options
#' @examples
#'  \donttest{
#'    cfg <- list()
#'    system.time(replicate(500, draw_component("tile_face", cfg, 4, 4)))
#'    system.time(cfg <- pp_cfg(cfg))
#'    system.time(replicate(500, draw_component("tile_face", cfg, 4, 4)))
#'  }
#'   
#' @exportClass pp_cfg
#' @export
pp_cfg <- function(cfg=list()) {
    signature <- paste(unlist(cfg), collapse='')
    if (!is.null(attr(cfg, "signature"))) {
        if (attr(cfg, "signature") == signature) return(cfg)
    }
    attr(cfg, "signature") <- signature
    attr(cfg, "cache") <- list()
    class(cfg) <- "pp_cfg"

    n_ranks <- get_n_ranks(cfg)
    n_suits <- get_n_suits(cfg)
    i_unsuit <- n_suits + 1
    key <- opt_cache_key("tile_back", i_unsuit, 0)
    attr(cfg, "cache")[[key]] <- get_component_opt("tile_back", i_unsuit, 0, cfg)
    key <- opt_cache_key("saucer_back", i_unsuit, 0)
    attr(cfg, "cache")[[key]] <- get_component_opt("saucer_back", i_unsuit, 0, cfg)
    key <- opt_cache_key("tile_face", i_unsuit, n_ranks+1)
    attr(cfg, "cache")[[key]] <- get_component_opt("tile_face", i_unsuit, n_ranks+1, cfg) #### joker tile
    for (i_r in 1:n_ranks) {
        key <- opt_cache_key("coin_face", i_unsuit, i_r)
        attr(cfg, "cache")[[key]] <- get_component_opt("coin_face", i_unsuit, i_r, cfg)
    }
    for (i_s in 1:n_suits) {
        for (cs in c("coin_back", "chip_back", "saucer_face", "belt_face", 
                     "pawn_face", "pawn_back")) {
        key <- opt_cache_key(cs, i_s, 0)
        attr(cfg, "cache")[[key]] <- get_component_opt(cs, i_s, 0, cfg)
        }
    }
    for (i_s in 1:(i_unsuit+1)) {
        for (i_r in 1:n_ranks) {
            for (cs in c("chip_face", "die_face", "tile_face")) {
                key <- opt_cache_key(cs, i_s, i_r)
                attr(cfg, "cache")[[key]] <- get_component_opt(cs, i_s, i_r, cfg)
            }
        }
        key <- opt_cache_key("suitdie_face", i_s, 0)
        attr(cfg, "cache")[[key]] <- get_component_opt("suitdie_face", i_s, 0, cfg)
    }
    cfg
}


#' @export
print.pp_cfg <- function(x, ...) {
    for(name in names(x)) {
        cat(paste0("$", name, " : ", x[[name]]), "\n")
    }
}

draw_component_basic <- function(component_side, i_s, i_r, cfg) {
    key <- opt_cache_key(component_side, i_s, i_r)
    if(is.null(attr(cfg, "cache")[[key]])) {
        # cat("missing", key, "\n")
        opt <- get_component_opt(component_side, i_s, i_r, cfg)
    } else {
        opt <- attr(cfg, "cache")[[key]]
    }
    add_background(opt)
    add_checkers(opt)
    add_hexlines(opt)
    add_gridlines(opt)
    add_ribbons(opt)
    add_primary_symbol(opt)
    add_dm_symbol(opt)
    add_border(opt)
    invisible(NULL)
}

draw_pyramid_top <- function(component_side, i_s, i_r, cfg) {
    cfg$scale <- 0.3 * get_scale(cfg)
    draw_component("pyramid_face",  cfg, i_s, i_r, y=0.75, width=1.0, height=0.5, angle=180)
    draw_component("pyramid_back",  cfg, i_s, i_r, y=0.25, width=1.0, height=0.5, angle=0)
    draw_component("pyramid_left",  cfg, i_s, i_r, x=0.25, width=1.0, height=0.5, angle=-90)
    draw_component("pyramid_right", cfg, i_s, i_r, x=0.75, width=1.0, height=0.5, angle=90)
}

draw_pawn_layout <- function(component_side, i_s, i_r, cfg) {
    suppressWarnings({
        denominator <- PAWN_HEIGHT + PAWN_BASE
        y <- (PAWN_HEIGHT/2 + PAWN_BASE) / denominator
        height = PAWN_HEIGHT / denominator
        pushViewport(viewport(y=0.25, height=0.5))
        pushViewport(viewport(y=y, height=height))
        draw_component("pawn_face", cfg, i_s)
        popViewport()
        popViewport()
        pushViewport(viewport(y=0.75, height=0.5, name="pawn_rear", angle=180))
        pushViewport(viewport(y=y, height=height, name="pawn_back"))
        draw_component("pawn_back", cfg, i_s)
        popViewport()
        popViewport()
    })
    border_col <- get_border_color("pawn_face", i_s, 0, cfg)
    grid.lines(y=0.5, gp=gpar(col=border_col, fill=NA, lty="dashed"))
    grid.rect(gp=gpar(col=border_col, fill=NA))
    ll <- 0.07
    seg(0.5, 0, 0.5, ll, border_col)
    seg(0.5, 1, 0.5, 1-ll, border_col)
}

draw_pyramid_layout <- function(component_side, i_s, i_r, cfg) {
    suppressWarnings({
        thetas <- c(72, 36, 0, -36, -72)
        r <- 0.5
        x <- to_x(thetas, r)
        y <- 0.5 + 0.5*to_y(thetas, r)
        components <- c("pyramid_face", "pyramid_right", "pyramid_back", "pyramid_left", "pyramid_face")
        angles <- c(90+72, 90+36, 90, 90-36, 90-72)
        for(ii in 1:5) {
            pushViewport(viewport(width=inch(PYRAMID_WIDTHS[i_r]), height=inch(PYRAMID_HEIGHTS[i_r]), angle=angles[ii], x=x[ii], y=y[ii]))
            draw_component(components[ii], cfg, i_s, i_r)
            popViewport()
        }
    })
}

#' Draw piecepack components
#' 
#' \code{draw_component} Draws a single piecepack component onto the graphics device.  
#' \code{draw_components} draws several piecepack components specified in a data frame  
#'    applying \code{draw_component_wrapper} to each row.
#' 
#' @rdname draw_component
#' @param component_side A string with component and side separated by a underscore e.g. "coin_face"
#' @param cfg Piecepack configuration list
#' @param i_s Number of suit
#' @param i_r Number of rank
#' @param x Where to place component on x axis of viewport
#' @param y Where to place component on y axis of viewport
#' @param width Width of component
#' @param height Height of component
#' @param svg If \code{TRUE} instead of drawing directly into graphics device
#'            export to svg, re-import svg, and then draw it to graphics device.  
#'            This is useful if drawing really big or small and don't want
#'            to play with re-configuring fontsizes.
#' @param ... With \code{draw_component} extra arguments to pass to \code{grid::viewport} like \code{angle}, with \code{draw_components} extra arguments to pass to \code{draw_component_wrapper}, with \code{draw_component_wrapper} ignored.
#' @export
draw_component <- function(component_side, cfg=list(), i_s=get_i_unsuit(cfg), i_r=0, x=0.5, y=0.5, 
                           width=NULL, height=NULL, svg=FALSE, ...) {
    if (is.null(width))
        width=inch(get_pp_width(component_side, i_r))
    if (is.null(height))
        height=inch(get_pp_height(component_side, i_r))
    if (svg) {
        svg_file <- tempfile(fileext=".svg")
        on.exit(unlink(svg_file))
        pp_width=get_pp_width(component_side, i_r)
        pp_height=get_pp_height(component_side, i_r)

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

#' @rdname draw_component
#' @param df A data frame specifying arguments to ``draw_component_wrapper`` 
#' @export
draw_components <- function(df, ...) {
    ll <- purrr::pmap_dfr(df, draw_component_wrapper, ...)
    invisible(NULL)
}

#' @rdname draw_component
#' @param units String specifying the units for the corresponding numeric values
#' @param angle Angle to draw component at
#' @param cfg_name String of list name storing configuration
#' @param cfg_list List of configuration lists
draw_component_wrapper <- function(..., component_side="tile_back", x=0.5, y=0.5, i_s=NA, i_r=NA, width=NA, height=NA, svg=FALSE, units="npc", angle=0, cfg=NULL, cfg_name=NA, cfg_list=NULL) {
    x <- unit(x, units)
    y <- unit(y, units)

    if (is.null(cfg)) {
        if (is.na(cfg_name)) {
            cfg <- list()
        } else if (is.list(cfg_list)) {
            cfg <- cfg_list[[cfg_name]]
        } else {
            cfg <- dynGet(cfg_name)
        }
    }
    if (is.na(i_r)) i_r <- 0
    if (is.na(i_s)) i_s <- get_i_unsuit(cfg)
    if (is.na(width))
        width <- NULL
    else
        width <- unit(width, units)
    if (is.na(height))
        height <- NULL
    else
        height <- unit(height, units)
    draw_component(component_side, cfg, i_s, i_r, x, y, width, height, svg, angle=angle)
    list(purrr_value=NA)
}

