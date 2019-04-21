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

make_get_style_fn <- function(style, default) {
    function(cfg=list()) get_style_element(style, cfg=cfg, default=default)
}
get_fontfamily <- make_get_style_fn("fontfamily", "sans")
get_fontface <- make_get_style_fn("fontface", "plain")
get_scale <- make_get_style_fn("scale", 1.0)

get_background_color_helper <- function(component_side, i_s, i_r, cfg) {
    colors <- col_cleave(get_style_element("background_color", component_side, cfg, "white", i_s, i_r))
    expand_suit_elements(colors, "background_colors", component_side, cfg)[i_s]
}

get_border_colors <- function(component_side=NA, i_s=0, i_r=0, cfg=list(), expand=TRUE) {
    border_colors <- col_cleave(get_style_element("border_color", component_side, cfg, "grey", i_s, i_r))
    if (expand)
        border_colors <- expand_suit_elements(border_colors, "border_colors", component_side, cfg)
    border_colors
}

get_border_color <- function(component_side, i_s, i_r, cfg) {
    get_border_colors(component_side, i_s, i_r, cfg)[i_s]
}

get_shape_t <- function(component_side, i_s, i_r, cfg) {
    t <- numeric_cleave(get_style_element("shape_t", component_side, cfg, 90, i_s, i_r))
    t <- expand_suit_elements(t, "shape_t", component_side, cfg)
    t[i_s] 
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
               stop(paste("Don't know correct shape for", component_side)))
    get_style_element("shape", component_side, cfg, default, i_s, i_r)
}

get_n_vertices <- function(shape) {
    as.numeric(gsub("convex|concave", "", shape))
}

get_grid_shape <- function(shape, t=0, r=0.2) {
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
        grid.concave_fn(get_n_vertices(shape), t, r) 
    } else if (grepl("^convex", shape)) {
        grid.pp.convex_fn(get_n_vertices(shape), t)
    } else {
        stop(paste("Don't know how to draw shape", shape)) 
    }
}

get_suit_color_helper <- function(component_side, i_s, i_r, cfg=list()) {
    suit_colors <- col_cleave(get_style_element("suit_color", component_side, cfg, 
                            "#D55E00,#000000,#009E73,#56B4E9,#E69F00"))
    suit_colors <- expand_suit_elements(suit_colors, "suit_colors", component_side, cfg) 
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
           belt_face = TRUE)
}

get_dm_t <- function(component_side, i_s, i_r, cfg) {
    default <- ifelse(component_side %in% c("tile_face", "die_face", "suitdie_face"), 135, 90)
    default <- ifelse(component_side == "matchstick_face" && i_r == 1, 135, default)
    t <- numeric_cleave(get_style_element("dm_t", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(t, "dm_t", component_side, cfg)[i_s]
}
get_dm_r <- function(component_side, i_s, i_r, cfg) {
    shape <- get_shape(component_side, i_s, i_r, cfg)
    r_corner <- sqrt(0.25^2 + 0.25^2)
    default <- switch(shape,
                     rect = switch(component_side, 
                                   matchstick_face = ifelse(i_r > 1, 0.4, r_corner),
                                   r_corner),
                     circle = switch(component_side,
                                     saucer_face = 0.3,
                                     saucer_back = 0.3,
                                     r_corner),
                     halma = 0.25,
                     pyramid = 0.1,
                     0.3)
    r <- numeric_cleave(get_style_element("dm_r", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(r, "dm_r", component_side, cfg)[i_s]
}

get_ps_t <- function(component_side, i_s, i_r, cfg) {
    shape <- get_shape(component_side, i_s, i_r, cfg)
    default <- switch(shape, halma=-90, pyramid=-90, 0)
    t <- numeric_cleave(get_style_element("ps_t", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(t, "ps_t", component_side, cfg)[i_s]
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
    dm_symbols <- cleave(get_style_element("dm_text", component_side, cfg, default, i_s, i_r))
    dm_symbols <- expand_suit_elements(dm_symbols, "suit_symbols", component_side, cfg)
    dm_symbols
}

get_dm_text <- function(component_side, i_s, i_r, cfg) {
    get_dm_symbols(component_side, i_s, i_r, cfg)[i_s]
}

get_dm_color <- function(component_side, i_s, i_r, cfg) {
    default <- get_suit_color(component_side, i_s, i_r, cfg)
    colors <- get_style_element("dm_color", component_side, cfg, default, i_s, i_r)
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
get_gridline_color <- function(component_side, i_s, i_r, cfg) {
    if (component_side == "tile_back") {
        default <- c(rep("transparent", get_n_suits(cfg)), 
                     get_suit_color(component_side, get_i_unsuit(cfg), i_r, cfg))
    } else {
        default <- "transparent"
    }
    default <- paste(default, collapse=",")
    colors <- get_style_element("gridline_color", component_side, cfg, default, i_s, i_r)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "gridline_colors", component_side, cfg)
    colors[i_s]
}
get_mat_color <- function(component_side, i_s, i_r, cfg) {
    default <- {
        if (component_side %in% c("belt_face", "saucer_face", "saucer_back")) {
            get_suit_color(component_side, i_s, i_r, cfg)
        } else {
            "transparent"
        }
    }
    colors <- get_style_element("mat_color", component_side, cfg, default, i_s, i_r)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "suit_colors", component_side, cfg)
    colors[i_s]
}
get_mat_width <- function(component_side, i_s, i_r, cfg) {
    default <- {
        if (component_side == "belt_face") {
            "0.2,0"
        } else if (component_side %in% c("saucer_face", "saucer_back")) {
            "0.1428571"  # 1/7 which mimics 5/8" pawn in 7/8" saucer
        } else {
            "0"
        }
    }
    widths <- get_style_element("mat_width", component_side, cfg, default, i_s, i_r)
    widths <- numeric_cleave(widths)
}

get_suit_symbol <- function(component_side, i_s, i_r, cfg) {
    get_suit_symbols(component_side, i_s, i_r, cfg)[i_s]
}

get_rank_symbols <- function(component_side=NA, i_s=0, i_r=0, cfg=list(), expand=TRUE) {
    default <- "n,a,2,3,4,5"
    rank_symbols <- cleave(get_style_element("rank_text", component_side, cfg, default, i_s, i_r))
    if (expand)
        rank_symbols <- expand_rank_elements(rank_symbols, "rank_symbols", component_side, cfg)
    rank_symbols
}
get_suit_symbols <- function(component_side=NA, i_s=0, i_r=0, cfg=list(), expand=TRUE) {
    default <- "\u2665,\u2660,\u2663,\u2666,\u263c" # "♥,♠,♣,♦,☼"
        # "\u2665,\u2660,\u2663,\u2666,\u2302" # "♥,♠,♣,♦,⌂"
        # "\u2665,\u2660,\u2663,\u2666,\u2605" # "♥,♠,♣,♦,★"
    suit_symbols <- cleave(get_style_element("suit_text", component_side, cfg, default, i_s, i_r))
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

use_suit_as_ace <- function(component_side=NA, i_s=0, i_r=0, cfg=list()) {
    use_suit_as_ace <- get_style_element("use_suit_as_ace", component_side, cfg, FALSE, i_s, i_r)
    ifelse(use_suit_as_ace && i_r == 2, TRUE, FALSE)
}

get_rank_symbol <- function(component_side, i_s, i_r, cfg) {
    if (use_suit_as_ace(component_side, i_s, i_r, cfg)) {
        suit_symbols <- get_suit_symbols(component_side, i_s, i_r, cfg, expand=FALSE)
        expand_suit_elements(suit_symbols, "rank_suit_symbols", component_side, cfg)[i_s]
    } else {
        get_rank_symbols(component_side, i_s, i_r, cfg)[i_r]
    }
} 

get_rank_scale <- function(component_side, i_s, i_r, cfg) {
    if (use_suit_as_ace(component_side, i_s, i_r, cfg)) {
        get_suit_scale(component_side, i_s, i_r, cfg)
    } else {
        scales <- numeric_cleave(get_style_element("rank_scale", component_side, cfg, 1.0, i_s, i_r))
        expand_rank_elements(scales, "scale", component_side, cfg)[i_r]
    }
}
get_rank_fontfamily <- function(component_side, i_s, i_r, cfg) {
    if (use_suit_as_ace(component_side, i_s, i_r, cfg)) {
        get_suit_fontfamily(component_side, i_s,  i_r, cfg)
    } else {
        fontfamilies <- cleave(get_style_element("rank_fontfamily", component_side, cfg, get_fontfamily(cfg), i_s, i_r))
        expand_rank_elements(fontfamilies, "font", component_side, cfg)[i_r]
    }
}
get_rank_fontface <- function(component_side, i_s, i_r, cfg) {
    if (use_suit_as_ace(component_side, i_s, i_r, cfg)) {
        get_suit_fontface(component_side, i_s,  i_r, cfg)
    } else {
        fontfaces <- cleave(get_style_element("rank_fontface", component_side, cfg, get_fontface(cfg), i_s, i_r))
        expand_rank_elements(fontfaces, "fontface", component_side, cfg)[i_r]
    }
}
get_suit_fonts <- function(component_side, i_s, i_r, cfg) {
    fonts <- cleave(get_style_element("suit_fontfamily", component_side, cfg, get_fontfamily(cfg), i_s, i_r))
    expand_suit_elements(fonts, "font", component_side, cfg)
}
get_suit_fontfamily <- function(component_side, i_s, i_r, cfg) {
    get_suit_fonts(component_side, i_s, i_r, cfg)[i_s]
}
get_suit_fontface <- function(component_side, i_s, i_r, cfg) {
    fontfaces <- cleave(get_style_element("suit_fontface", component_side, cfg, get_fontface(cfg), i_s, i_r))
    expand_suit_elements(fontfaces, "fontface", component_side, cfg)[i_s]
}
get_suit_scales <- function(component_side, i_s, i_r, cfg) {
    scales <- numeric_cleave(get_style_element("suit_scale", component_side, cfg, 1.0, i_s, i_r))
    expand_suit_elements(scales, "scale", component_side, cfg)
}
get_suit_scale <- function(component_side, i_s, i_r, cfg) {
    get_suit_scales(component_side, i_s, i_r, cfg)[i_s]
}
are_suits_dm <- function(component_side, i_s, i_r, cfg) {
    identical(get_dm_symbols(component_side, i_s, i_r, cfg),
              get_suit_symbols(component_side, i_s, i_r, cfg))
}
get_dm_fontface <- function(component_side, i_s, i_r, cfg) {
    if (are_suits_dm(component_side, i_s, i_r, cfg)) { 
        default <- get_suit_fontface(component_side, i_s, i_r, cfg)
    } else {
        default <- get_fontface(cfg)
    }
    fonts <- cleave(get_style_element("dm_fontface", component_side, cfg, default))
    expand_suit_elements(fonts, "fontface", component_side, cfg)[i_s]
}
get_dm_fontfamily <- function(component_side, i_s, i_r, cfg) {
    if (are_suits_dm(component_side, i_s, i_r, cfg)) { 
        default <- get_suit_fontfamily(component_side, i_s, i_r, cfg)
    } else {
        default <- get_fontfamily(cfg)
    }
    fonts <- cleave(get_style_element("dm_fontfamily", component_side, cfg, default))
    expand_suit_elements(fonts, "font", component_side, cfg)[i_s]
}
get_dm_scale <- function(component_side, i_s, i_r, cfg) {
    if (are_suits_dm(component_side, i_s, i_r, cfg)) { 
        default <- get_suit_scale(component_side, i_s, i_r, cfg)
    } else {
        default <- 1.0
    }
    scales <- numeric_cleave(get_style_element("dm_scale", component_side, cfg, default, i_s, i_r))
    expand_suit_elements(scales, "scale", component_side, cfg)[i_s]
}

get_suit_fontsize <- function(component_side, i_s, i_r, cfg) {
    default <- switch(component_side,
                 "belt_face" = 22,
                 "coin_back" = 34,
                 "pawn_face" = 28,
                 "pawn_back" = 28,
                 "saucer_back" = 28,
                 "saucer_face" = 28,
                 "suitdie_face" = 32,
                 "pyramid_face" = 60 * (i_r+1) / 8,
                 "pyramid_back" = 60 * (i_r+1) / 8,
                 24)
    get_style_element("suit_fontsize", component_side, cfg, default, i_s, i_r)
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
                 "coin_face" = 28,
                 "tile_face" = 72,
                 "pyramid_left"  = 60 * (i_r+1) / 8,
                 "pyramid_right" = 60 * (i_r+1) / 8,
                 20)
    get_style_element("rank_fontsize", component_side, cfg, default, i_s, i_r)
}


get_ps_element <- function(component_side, suit_element, rank_element, neither_element=NA) {
    if (component_side %in% c("coin_face", "die_face", "tile_face", "pyramid_left", "pyramid_right", "matchstick_face")) {
        rank_element
    } else if (component_side %in% c("tile_back", "matchstick_back")) {
        neither_element
    } else {
        suit_element
    }
}
get_ps_fontface <- function(component_side=NA, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    rank_fontface <- get_rank_fontface(component_side, i_s, i_r, cfg)
    suit_fontface <- get_suit_fontface(component_side, i_s, i_r, cfg)
    default <- get_ps_element(component_side, suit_fontface, rank_fontface, "plain")
    get_style_element("ps_fontface", component_side, cfg, default, i_s, i_r)
}
get_ps_fontfamily <- function(component_side=NA, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    rank_font <- get_rank_fontfamily(component_side, i_s, i_r, cfg)
    suit_font <- get_suit_fontfamily(component_side, i_s, i_r, cfg)
    default <- get_ps_element(component_side, suit_font, rank_font)
    get_style_element("ps_fontfamily", component_side, cfg, default, i_s, i_r)
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
get_ps_text <- function(component_side, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    rank_symbol <- get_rank_symbol(component_side, i_s, i_r, cfg)
    suit_symbol <- get_suit_symbol(component_side, i_s, i_r, cfg)
    default <- get_ps_element(component_side, suit_symbol, rank_symbol, NULL)
    get_style_element("ps_text", component_side, cfg, default, i_s, i_r)
}
get_ps_color <- function(component_side, i_s, i_r, cfg) {
    default <- get_suit_color(component_side, i_s, i_r, cfg)
    get_style_element("ps_color", component_side, cfg, default, i_s, i_r)
}

get_component_opt <- function(component_side, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    key <- opt_cache_key(component_side, i_s, i_r)
    if(!is.null(attr(cfg, "cache")[[key]])) {
        return(attr(cfg, "cache")[[key]])
    }

    # Shape
    shape <- get_shape(component_side, i_s, i_r, cfg)
    shape_r <- get_shape_r(component_side, i_s, i_r, cfg)
    shape_t <- get_shape_t(component_side, i_s, i_r, cfg)

    # Additional colors
    background_col <- get_background_color(component_side, i_s, i_r, cfg)
    border_col <- get_border_color(component_side, i_s, i_r, cfg)
    gridline_col <- get_gridline_color(component_side, i_s, i_r, cfg)
    mat_col <- get_mat_color(component_side, i_s, i_r, cfg)
    mat_width <- get_mat_width(component_side, i_s, i_r, cfg)

    # Overall scaling factor
    scale <- get_scale(cfg)

    # Directional mark symbol
    dm_col <- get_dm_color(component_side, i_s, i_r, cfg)
    dm_scale <- get_dm_scale(component_side, i_s, i_r, cfg)
    dm_fontfamily <- get_dm_fontfamily(component_side, i_s, i_r, cfg)
    dm_fontface <- get_dm_fontface(component_side, i_s, i_r, cfg)
    dm_fontsize <- scale * dm_scale * get_dm_fontsize(component_side, i_s, i_r, cfg)
    dm_text <- get_dm_text(component_side, i_s, i_r, cfg)
    dm_t <- get_dm_t(component_side, i_s, i_r, cfg)
    dm_r <- get_dm_r(component_side, i_s, i_r, cfg)
    dm_x <- to_x(dm_t, dm_r) + 0.5
    dm_y <- to_y(dm_t, dm_r) + 0.5

    # Primary symbol
    ps_col <- get_ps_color(component_side, i_s, i_r, cfg)
    ps_scale <- get_ps_scale(component_side, i_s, i_r, cfg)
    ps_fontfamily <- get_ps_fontfamily(component_side, i_s, i_r, cfg)
    ps_fontface <- get_ps_fontface(component_side, i_s, i_r, cfg)
    ps_fontsize <- scale * ps_scale * get_ps_fontsize(component_side, i_s, i_r, cfg)
    ps_text <- get_ps_text(component_side, i_s, i_r, cfg)
    ps_t <- get_ps_t(component_side, i_s, i_r, cfg)
    ps_r <- get_ps_r(component_side, i_s, i_r, cfg)
    ps_x <- to_x(ps_t, ps_r) + 0.5
    ps_y <- to_y(ps_t, ps_r) + 0.5

    list(shape=shape, shape_r=shape_r, shape_t=shape_t, 
         background_col=background_col, border_col=border_col, 
         gridline_col=gridline_col,  mat_col=mat_col, mat_width=mat_width,
         dm_col=dm_col, dm_text=dm_text, 
         dm_fontsize=dm_fontsize, 
         dm_fontfamily=dm_fontfamily, dm_fontface=dm_fontface,
         dm_x=dm_x, dm_y=dm_y, 
         ps_col=ps_col, ps_text=ps_text, 
         ps_fontsize=ps_fontsize, 
         ps_fontfamily=ps_fontfamily, ps_fontface=ps_fontface,
         ps_x=ps_x, ps_y=ps_y)
}
