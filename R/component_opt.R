get_piece <- function(piece_side) {
    cleave(piece_side, "_")[1]
}

# nolint start
# piece_side="pawn_face"
# suit=0
# rank=0
# style="font"
# 
# if(is.na(piece_side)) {
#     piece <- NULL
# } else {
#     piece <- get_piece(piece_side)
# }
# suited <- is_suited(piece_side, suit, cfg)
# if (is.null(suited)) suited <- FALSE
# piece_affixes <- c(paste0(".", piece_side), paste0(".", piece), "")
# piece_score <- c(2, 1, 0)
# rank_affixes <- c(paste0(".r", rank), "")
# rank_score <- c(1, 0)
# suit_affixes <- c(paste0(".s", suit), ifelse(suited, ".suited", ".unsuited"), "")
# suit_score <- c(2, 1, 0)
# dfs <- expand.grid(piece_affixes, rank_affixes, suit_affixes, stringsAsFactors=FALSE)
# names(dfs) <- c("piece", "rank", "suit")
# dfs$order <- 1:nrow(dfs)
# dfs <- left_join(dfs, data.frame(piece=piece_affixes, piece_score, stringsAsFactors=FALSE), by="piece")
# dfs <- left_join(dfs, data.frame(rank=rank_affixes, rank_score, stringsAsFactors=FALSE), by="rank")
# dfs <- left_join(dfs, data.frame(suit=suit_affixes, suit_score, stringsAsFactors=FALSE), by="suit")
# dfs <- mutate(dfs, score = 1e3 * piece_score + 1e2 * rank_score + 1e1 * suit_score,
#               affix = paste0(suit, rank, piece))
# dfs <- arrange(dfs, desc(score))
# dfs$order
# nolint end

style_ordering <- c(1, 7, 13, 4, 10, 16, 2, 8, 14, 5, 11, 17, 3, 9, 15, 6, 12, 18)

get_style_element <- function(style, piece_side=NA, cfg=list(), default=NULL, suit=0, rank=0) {

    if (is.na(piece_side)) {
        piece <- NULL
    } else {
        piece <- get_piece(piece_side)
    }
    suited <- is_suited(piece_side, suit, rank, cfg)
    if (is.null(suited)) suited <- FALSE

    piece_affixes <- c(paste0(".", piece_side), paste0(".", piece), "")
    rank_affixes <- c(paste0(".r", rank), "")
    suit_affixes <- c(paste0(".s", suit), ifelse(suited, ".suited", ".unsuited"), "")
    dfs <- expand.grid(piece_affixes, rank_affixes, suit_affixes, stringsAsFactors=FALSE)
    names(dfs) <- c("piece", "rank", "suit")
    affixes <- paste0(dfs$suit, dfs$rank, dfs$piece)[style_ordering]

    for (affix in affixes) {
        string <- paste0(style, affix)
        value <- cfg[[string]]
        if (!is.null(value)) return(value)
    }
    return(default)
}

is_legit_cfg_style <- function(cfg_style) {
    # style
    # style.suit
    # style.rank
    # style.piece
    # style.suit.rank
    # style.suit.piece
    # style.rank.piece
    # style.suit.rank.piece
    ss <- cleave(cfg_style, sep="\\.")
    if (!is_legit_style(ss[1])) return(FALSE)
    if (length(ss) == 2) {
        return(is_legit_suit(ss[2]) || is_legit_rank(ss[2]) || is_legit_piece(ss[2]))
    }
    if (length(ss) == 3) {
        return((is_legit_suit(ss[2]) && (is_legit_rank(ss[3]) || is_legit_piece(ss[3]))) ||
                  (is_legit_rank(ss[2]) && is_legit_piece(ss[3])))
    }
    if (length(ss) == 4) {
        return(is_legit_suit(ss[2]) && is_legit_rank(ss[3]) && is_legit_piece(ss[4]))
    }
    if (length(ss) > 4) return(FALSE)
    TRUE
}

styles <- c(paste(c("ps", "dm"),
                  rep(c("text", "fontface", "fontfamily", "fontsize", "cex", "t", "r", "color"), each=2),
                  sep="_"),
            "shape", "shape_t", "shape_r", "background_color",
            "invert_colors", "border_color", "border_lex",
            "gridline_color", "gridline_lex", "edge_color", "annotation_color",
            "mat_color", "mat_width", "suit_color",
            paste(c("rank", "suit"),
                  rep(c("text", "fontface", "fontfamily", "cex"), each=2),
                  sep="_"),
            "use_suit_as_ace", "fontfamily", "fontface", "cex", "n_ranks", "n_suits",
            "coin_arrangement", "die_arrangement",
            "width", "height", "depth", "grob_fn", "shadow_fn",
            "title", "description", "credit", "copyright")
is_legit_style <- function(style) {
    style %in% styles
}
is_legit_rank <- function(rank) {
    grepl("r[[:digit:]]+", rank)
}
is_legit_suit <- function(suit) {
    (suit %in% c("suited", "unsuited")) || grepl("s[[:digit:]]", suit)
}
pieces <- c(paste0(rep(c("tile", "coin", "pawn", "saucer", "matchstick", "pyramid"), 3),
                   rep(c("", "_face", "_back"), each=6)),
            "die", "belt", "die_face", "belt_face", "pyramid_left", "pyramid_right")
is_legit_piece <- function(piece) {
    piece %in% pieces
}
warn_cfg <- function(cfg) {
    for (nn in names(cfg)) {
        if (!is_legit_cfg_style(nn)) {
            warning(paste(nn, "is not a recognized configuration"))
        }
    }
}

make_get_style_fn <- function(style, default) {
    function(cfg=list()) get_style_element(style, cfg=cfg, default=default)
}
get_fontfamily <- make_get_style_fn("fontfamily", "sans")
get_fontface <- make_get_style_fn("fontface", "plain")
get_cex <- make_get_style_fn("cex", 1.0)

get_background_color_helper <- function(piece_side, suit, rank, cfg) {
    colors <- col_cleave(get_style_element("background_color", piece_side, cfg, "white", suit, rank))
    expand_suit_elements(colors, "background_colors", piece_side, cfg)[suit]
}

get_border_colors <- function(piece_side=NA, suit=0, rank=0, cfg=list(), expand=TRUE) {
    border_colors <- col_cleave(get_style_element("border_color", piece_side, cfg, "grey", suit, rank))
    if (expand)
        border_colors <- expand_suit_elements(border_colors, "border_colors", piece_side, cfg)
    border_colors
}

get_border_color <- function(piece_side, suit, rank, cfg) {
    get_border_colors(piece_side, suit, rank, cfg)[suit]
}

get_border_lex <- function(piece_side, suit, rank, cfg) {
    lex <- get_style_element("border_lex", piece_side, cfg, 1, suit, rank)
    lex
}

get_shape_t <- function(piece_side, suit, rank, cfg) {
    t <- numeric_cleave(get_style_element("shape_t", piece_side, cfg, 90, suit, rank))
    t <- expand_suit_elements(t, "shape_t", piece_side, cfg)
    t[suit]
}

get_shape_r <- function(piece_side, suit, rank, cfg) {
    r <- numeric_cleave(get_style_element("shape_r", piece_side, cfg, 0.2, suit, rank))
    r <- expand_suit_elements(r, "shape_r", piece_side, cfg)
    r[suit]
}

get_shape <- function(piece_side, suit, rank, cfg) {
    default <- switch(piece_side,
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
               pyramid_top = "rect",
               belt_face = "rect",
               suitdie_face = "rect",
               stop(paste("Don't know correct shape for", piece_side)))
    get_style_element("shape", piece_side, cfg, default, suit, rank)
}

get_n_vertices <- function(shape) {
    as.numeric(gsub("convex|concave", "", shape))
}

get_suit_color_helper <- function(piece_side, suit, rank, cfg=list()) {
    suit_colors <- col_cleave(get_style_element("suit_color", piece_side, cfg,
                            "#D55E00,#000000,#009E73,#56B4E9,#E69F00", suit, rank))
    suit_colors <- expand_suit_elements(suit_colors, "suit_colors", piece_side, cfg)
    ifelse(suit <= get_n_suits(cfg), suit_colors[suit], suit_colors[get_i_unsuit(cfg)])
}

should_invert <- function(piece_side, suit, rank, cfg) {
    should_inverts <- get_style_element("invert_colors", piece_side, cfg, FALSE, suit, rank)
    expand_suit_elements(should_inverts, "should_inverts", piece_side, cfg)[suit]
}

is_suited <- function(piece_side, suit, rank, cfg) {
    if (is.na(piece_side))
        FALSE
    else
        has_suit(piece_side) && (suit <= get_i_unsuit(cfg))
}

get_dm_t <- function(piece_side, suit, rank, cfg) {
    default <- ifelse(piece_side %in% c("tile_face", "die_face", "suitdie_face"), 135, 90)
    default <- ifelse(piece_side == "matchstick_face" && rank == 1, 135, default)
    t <- numeric_cleave(get_style_element("dm_t", piece_side, cfg, default, suit, rank))
    expand_suit_elements(t, "dm_t", piece_side, cfg)[suit]
}
get_dm_r <- function(piece_side, suit, rank, cfg) {
    shape <- get_shape(piece_side, suit, rank, cfg)
    r_corner <- sqrt(0.25^2 + 0.25^2)
    default <- switch(shape,
                     rect = switch(piece_side,
                                   matchstick_face = ifelse(rank > 1, 0.4, r_corner),
                                   r_corner),
                     circle = switch(piece_side,
                                     saucer_face = 0.3,
                                     saucer_back = 0.3,
                                     r_corner),
                     halma = 0.25,
                     pyramid = 0.1,
                     0.3)
    r <- numeric_cleave(get_style_element("dm_r", piece_side, cfg, default, suit, rank))
    expand_suit_elements(r, "dm_r", piece_side, cfg)[suit]
}

get_ps_t <- function(piece_side, suit, rank, cfg) {
    shape <- get_shape(piece_side, suit, rank, cfg)
    default <- switch(shape, halma=-90, pyramid=-90, 0)
    t <- numeric_cleave(get_style_element("ps_t", piece_side, cfg, default, suit, rank))
    expand_suit_elements(t, "ps_t", piece_side, cfg)[suit]
}
get_ps_r <- function(piece_side, suit, rank, cfg) {
    shape <- get_shape(piece_side, suit, rank, cfg)
    default <- switch(shape, halma=0.25, pyramid=0.25, 0.0)
    r <- numeric_cleave(get_style_element("ps_r", piece_side, cfg, default, suit, rank))
    expand_suit_elements(r, "ps_r", piece_side, cfg)[suit]
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

get_i_unsuit <- function(cfg=list()) get_n_suits(cfg) + 1

get_die_arrangement <- function(cfg=list()) {
    if (is.null(cfg[["die_arrangement"]])) {
        "counter_down"
    } else {
        cfg[["die_arrangement"]]
    }
}
get_coin_arrangement <- function(cfg=list()) {
    if (is.null(cfg[["coin_arrangement"]])) {
        180
    } else {
        as.numeric(cfg[["coin_arrangement"]])
    }
}

get_dm_symbols <- function(piece_side, suit=0, rank=0, cfg=list()) {
    default <- {
        if (piece_side %in% c("coin_back", "coin_face")) {
            dm_symbols <- "\u25cf"
        } else if (piece_side %in% c("saucer_back", "saucer_face")) {
            dm_symbols <- "\u25b2"
        } else if (piece_side %in% c("pawn_face", "pyramid_face")) {
            dm_symbols <- "\u0298\u0298"
        } else if (piece_side %in% c("suitdie_face", "pawn_back",
                                         "belt_face", "tile_back", "matchstick_back",
                                         "pyramid_left", "pyramid_right", "pyramid_back")) {
            dm_symbols <- ""
        } else {
            dm_symbols <- get_suit_symbols(piece_side, suit, rank, cfg)
        }
    }
    default <- paste(default, collapse=",")
    dm_symbols <- cleave(get_style_element("dm_text", piece_side, cfg, default, suit, rank))
    dm_symbols <- expand_suit_elements(dm_symbols, "suit_symbols", piece_side, cfg)
    dm_symbols
}

get_dm_text <- function(piece_side, suit, rank, cfg) {
    get_dm_symbols(piece_side, suit, rank, cfg)[suit]
}

get_dm_color <- function(piece_side, suit, rank, cfg) {
    default <- get_suit_color(piece_side, suit, rank, cfg)
    colors <- get_style_element("dm_color", piece_side, cfg, default, suit, rank)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "suit_colors", piece_side, cfg)
    colors[suit]
}

get_background_color <- function(piece_side, suit, rank, cfg) {
    bcol <- get_background_color_helper(piece_side, suit, rank, cfg)
    scol <- get_suit_color_helper(piece_side, suit, rank, cfg)
    if (should_invert(piece_side, suit, rank, cfg))
        scol
    else
        bcol
}

get_suit_color <- function(piece_side, suit, rank, cfg) {
    bcol <- get_background_color_helper(piece_side, suit, rank, cfg)
    scol <- get_suit_color_helper(piece_side, suit, rank, cfg)
    if (should_invert(piece_side, suit, rank, cfg))
        bcol
    else
        scol
}
get_gridline_color <- function(piece_side, suit, rank, cfg) {
    if (piece_side == "tile_back") {
        default <- c(rep("transparent", get_n_suits(cfg)),
                     get_suit_color(piece_side, get_i_unsuit(cfg), rank, cfg))
    } else {
        default <- "transparent"
    }
    default <- paste(default, collapse=",")
    colors <- get_style_element("gridline_color", piece_side, cfg, default, suit, rank)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "gridline_colors", piece_side, cfg)
    colors[suit]
}

get_gridline_lex <- function(piece_side, suit, rank, cfg) {
    lex <- get_style_element("gridline_lex", piece_side, cfg, 1, suit, rank)
    lex
}

get_mat_color <- function(piece_side, suit, rank, cfg) {
    default <- {
        if (piece_side %in% c("belt_face", "saucer_face", "saucer_back")) {
            get_suit_color(piece_side, suit, rank, cfg)
        } else {
            "transparent"
        }
    }
    colors <- get_style_element("mat_color", piece_side, cfg, default, suit, rank)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "suit_colors", piece_side, cfg)
    colors[suit]
}
get_mat_width <- function(piece_side, suit, rank, cfg) {
    default <- {
        if (piece_side == "belt_face") {
            "0.2,0"
        } else if (piece_side %in% c("saucer_face", "saucer_back")) {
            "0.1428571"  # 1/7 which mimics 5/8" pawn in 7/8" saucer
        } else {
            "0"
        }
    }
    widths <- get_style_element("mat_width", piece_side, cfg, default, suit, rank)
    widths <- numeric_cleave(widths)
}

get_edge_color <- function(piece_side, suit, rank, cfg) {
    neutral_col <- get_background_color_helper("tile_back", suit=get_i_unsuit(cfg), rank=0, cfg)
    suit_col <- get_suit_color_helper("pawn_face", suit=suit, rank=0, cfg)

    piece <- get_piece(piece_side)
    default <- switch(piece,
                    pawn = suit_col,
                    die = suit_col,
                    matchstick = suit_col,
                    pyramid = suit_col,
                    neutral_col)
    colors <- get_style_element("edge_color", piece_side, cfg, default, suit, rank)
    colors <- col_cleave(colors)
    colors <- expand_suit_elements(colors, "suit_colors", piece_side, cfg)
    colors[suit]
}

get_suit_symbol <- function(piece_side, suit, rank, cfg) {
    get_suit_symbols(piece_side, suit, rank, cfg)[suit]
}

get_rank_symbols <- function(piece_side=NA, suit=0, rank=0, cfg=list(), expand=TRUE) {
    default <- "n,a,2,3,4,5"
    rank_symbols <- cleave(get_style_element("rank_text", piece_side, cfg, default, suit, rank))
    if (expand)
        rank_symbols <- expand_rank_elements(rank_symbols, "rank_symbols", piece_side, cfg)
    rank_symbols
}
get_suit_symbols <- function(piece_side=NA, suit=0, rank=0, cfg=list(), expand=TRUE) {
    default <- "\u2665,\u2660,\u2663,\u2666,\u263c"
    suit_symbols <- cleave(get_style_element("suit_text", piece_side, cfg, default, suit, rank))
    if (expand)
        suit_symbols <- expand_suit_elements(suit_symbols, "suit_symbols", piece_side, cfg)
    suit_symbols
}

expand_suit_elements <- function(elements, style, piece_side, cfg) {
    if (length(elements) < get_n_suits(cfg)) {
        elements <- rep(elements, length.out=get_n_suits(cfg) + 1)
    } else if (length(elements) == get_n_suits(cfg)) {
        elements <- c(elements, switch(style, cex=1.0, suit_colors="grey", ""))
    }
    if (length(elements) == get_i_unsuit(cfg)) {
        elements <- c(elements, switch(style,
                           suit_symbols = switch(ifelse(is.na(piece_side), "NA", piece_side),
                                suitdie_face = "", die_face = "", elements[get_i_unsuit(cfg)]),
                           gridline_colors = NA,
                           elements[get_i_unsuit(cfg)]))
    }
    elements
}

expand_rank_elements <- function(elements, style, piece_side, cfg) {
    if (length(elements) < get_n_ranks(cfg)) {
        elements <- rep(elements, length.out=get_n_ranks(cfg))
    }
    if (length(elements) == get_n_ranks(cfg))
        elements <- c(elements, switch(style, rank_symbols = "", elements[get_n_ranks(cfg)]))
    elements
}

use_suit_as_ace <- function(piece_side=NA, suit=0, rank=0, cfg=list()) {
    use_suit_as_ace <- get_style_element("use_suit_as_ace", piece_side, cfg, FALSE, suit, rank)
    ifelse(use_suit_as_ace && rank == 2, TRUE, FALSE)
}

get_rank_symbol <- function(piece_side, suit, rank, cfg) {
    if (use_suit_as_ace(piece_side, suit, rank, cfg)) {
        suit_symbols <- get_suit_symbols(piece_side, suit, rank, cfg, expand=FALSE)
        expand_suit_elements(suit_symbols, "rank_suit_symbols", piece_side, cfg)[suit]
    } else {
        get_rank_symbols(piece_side, suit, rank, cfg)[rank]
    }
}

get_rank_cex <- function(piece_side, suit, rank, cfg) {
    if (use_suit_as_ace(piece_side, suit, rank, cfg)) {
        get_suit_cex(piece_side, suit, rank, cfg)
    } else {
        cex <- numeric_cleave(get_style_element("rank_cex", piece_side, cfg, 1.0, suit, rank))
        expand_rank_elements(cex, "cex", piece_side, cfg)[rank]
    }
}
get_rank_fontfamily <- function(piece_side, suit, rank, cfg) {
    if (use_suit_as_ace(piece_side, suit, rank, cfg)) {
        get_suit_fontfamily(piece_side, suit,  rank, cfg)
    } else {
        fontfamilies <- cleave(get_style_element("rank_fontfamily", piece_side, cfg, get_fontfamily(cfg), suit, rank))
        expand_rank_elements(fontfamilies, "font", piece_side, cfg)[rank]
    }
}
get_rank_fontface <- function(piece_side, suit, rank, cfg) {
    if (use_suit_as_ace(piece_side, suit, rank, cfg)) {
        get_suit_fontface(piece_side, suit,  rank, cfg)
    } else {
        fontfaces <- cleave(get_style_element("rank_fontface", piece_side, cfg, get_fontface(cfg), suit, rank))
        expand_rank_elements(fontfaces, "fontface", piece_side, cfg)[rank]
    }
}
get_suit_fonts <- function(piece_side, suit, rank, cfg) {
    fonts <- cleave(get_style_element("suit_fontfamily", piece_side, cfg, get_fontfamily(cfg), suit, rank))
    expand_suit_elements(fonts, "font", piece_side, cfg)
}
get_suit_fontfamily <- function(piece_side, suit, rank, cfg) {
    get_suit_fonts(piece_side, suit, rank, cfg)[suit]
}
get_suit_fontface <- function(piece_side, suit, rank, cfg) {
    fontfaces <- cleave(get_style_element("suit_fontface", piece_side, cfg, get_fontface(cfg), suit, rank))
    expand_suit_elements(fontfaces, "fontface", piece_side, cfg)[suit]
}
get_suit_cex <- function(piece_side, suit, rank, cfg) {
    cex <- numeric_cleave(get_style_element("suit_cex", piece_side, cfg, 1.0, suit, rank))
    expand_suit_elements(cex, "cex", piece_side, cfg)[suit]
}
are_suits_dm <- function(piece_side, suit, rank, cfg) {
    identical(get_dm_symbols(piece_side, suit, rank, cfg),
              get_suit_symbols(piece_side, suit, rank, cfg))
}
get_dm_fontface <- function(piece_side, suit, rank, cfg) {
    if (are_suits_dm(piece_side, suit, rank, cfg)) {
        default <- get_suit_fontface(piece_side, suit, rank, cfg)
    } else {
        default <- get_fontface(cfg)
    }
    fonts <- cleave(get_style_element("dm_fontface", piece_side, cfg, default))
    expand_suit_elements(fonts, "fontface", piece_side, cfg)[suit]
}
get_dm_fontfamily <- function(piece_side, suit, rank, cfg) {
    if (are_suits_dm(piece_side, suit, rank, cfg)) {
        default <- get_suit_fontfamily(piece_side, suit, rank, cfg)
    } else {
        default <- get_fontfamily(cfg)
    }
    fonts <- cleave(get_style_element("dm_fontfamily", piece_side, cfg, default))
    expand_suit_elements(fonts, "font", piece_side, cfg)[suit]
}
get_dm_cex <- function(piece_side, suit, rank, cfg) {
    if (are_suits_dm(piece_side, suit, rank, cfg)) {
        default <- get_suit_cex(piece_side, suit, rank, cfg)
    } else {
        default <- 1.0
    }
    cex <- numeric_cleave(get_style_element("dm_cex", piece_side, cfg, default, suit, rank))
    expand_suit_elements(cex, "cex", piece_side, cfg)[suit]
}

get_suit_fontsize <- function(piece_side, suit, rank, cfg) {
    default <- switch(piece_side,
                 "belt_face" = 22,
                 "coin_back" = 34,
                 "pawn_face" = 28,
                 "pawn_back" = 28,
                 "saucer_back" = 28,
                 "saucer_face" = 28,
                 "suitdie_face" = 32,
                 "pyramid_face" = 60 * (rank+1) / 8,
                 "pyramid_back" = 60 * (rank+1) / 8,
                 24)
    get_style_element("suit_fontsize", piece_side, cfg, default, suit, rank)
}

get_dm_fontsize <- function(piece_side, suit, rank, cfg) {
    default <- switch(piece_side,
                 "tile_face" = 40,
                 "pawn_face" = 12,
                 "pawn_back" = 12,
                 "pyramid_face" = 12 * (rank+1) / 8,
                 12)
    get_style_element("dm_fontsize", piece_side, cfg, default, suit, rank)
}

get_rank_fontsize <- function(piece_side, suit, rank, cfg) {
    default <- switch(piece_side,
                 "die_face" = 20,
                 "coin_face" = 28,
                 "tile_face" = 72,
                 "pyramid_left"  = 60 * (rank+1) / 8,
                 "pyramid_right" = 60 * (rank+1) / 8,
                 20)
    get_style_element("rank_fontsize", piece_side, cfg, default, suit, rank)
}


get_ps_element <- function(piece_side, suit_element, rank_element, neither_element=NA) {
    if (piece_side %in% c("coin_face", "die_face", "tile_face", "pyramid_left", "pyramid_right", "matchstick_face")) {
        rank_element
    } else if (piece_side %in% c("tile_back", "matchstick_back")) {
        neither_element
    } else {
        suit_element
    }
}
get_ps_fontface <- function(piece_side=NA, suit=get_i_unsuit(cfg), rank=1, cfg=list()) {
    rank_fontface <- get_rank_fontface(piece_side, suit, rank, cfg)
    suit_fontface <- get_suit_fontface(piece_side, suit, rank, cfg)
    default <- get_ps_element(piece_side, suit_fontface, rank_fontface, "plain")
    get_style_element("ps_fontface", piece_side, cfg, default, suit, rank)
}
get_ps_fontfamily <- function(piece_side=NA, suit=get_i_unsuit(cfg), rank=1, cfg=list()) {
    rank_font <- get_rank_fontfamily(piece_side, suit, rank, cfg)
    suit_font <- get_suit_fontfamily(piece_side, suit, rank, cfg)
    default <- get_ps_element(piece_side, suit_font, rank_font)
    get_style_element("ps_fontfamily", piece_side, cfg, default, suit, rank)
}
get_ps_fontsize <- function(piece_side, suit=get_i_unsuit(cfg), rank=1, cfg=list()) {
    rank_fontsize <- get_rank_fontsize(piece_side, suit, rank, cfg)
    suit_fontsize <- get_suit_fontsize(piece_side, suit, rank, cfg)
    default <- get_ps_element(piece_side, suit_fontsize, rank_fontsize)
    get_style_element("ps_fontsize", piece_side, cfg, default, suit, rank)
}
get_ps_cex <- function(piece_side, suit=get_i_unsuit(cfg), rank=1, cfg=list()) {
    rank_cex <- get_rank_cex(piece_side, suit, rank, cfg)
    suit_cex <- get_suit_cex(piece_side, suit, rank, cfg)
    default <- get_ps_element(piece_side, suit_cex, rank_cex)
    get_style_element("ps_cex", piece_side, cfg, default, suit, rank)
}
get_ps_text <- function(piece_side, suit=get_i_unsuit(cfg), rank=1, cfg=list()) {
    rank_symbol <- get_rank_symbol(piece_side, suit, rank, cfg)
    suit_symbol <- get_suit_symbol(piece_side, suit, rank, cfg)
    default <- get_ps_element(piece_side, suit_symbol, rank_symbol, NULL)
    get_style_element("ps_text", piece_side, cfg, default, suit, rank)
}
get_ps_color <- function(piece_side, suit, rank, cfg) {
    default <- get_suit_color(piece_side, suit, rank, cfg)
    get_style_element("ps_color", piece_side, cfg, default, suit, rank)
}

get_piece_opt_helper <- function(piece_side, suit, rank, cfg) {
    # Shape
    shape <- get_shape(piece_side, suit, rank, cfg)
    shape_r <- get_shape_r(piece_side, suit, rank, cfg)
    shape_t <- get_shape_t(piece_side, suit, rank, cfg)

    # Additional colors
    background_color <- get_background_color(piece_side, suit, rank, cfg)
    border_color <- get_border_color(piece_side, suit, rank, cfg)
    border_lex <- get_border_lex(piece_side, suit, rank, cfg)
    gridline_color <- get_gridline_color(piece_side, suit, rank, cfg)
    gridline_lex <- get_gridline_lex(piece_side, suit, rank, cfg)
    mat_color <- get_mat_color(piece_side, suit, rank, cfg)
    mat_width <- get_mat_width(piece_side, suit, rank, cfg)
    edge_color <- get_edge_color(piece_side, suit, rank, cfg)

    # Overall scaling factor
    cex <- get_cex(cfg)

    # Directional mark symbol
    dm_color <- get_dm_color(piece_side, suit, rank, cfg)
    dm_cex <- get_dm_cex(piece_side, suit, rank, cfg)
    dm_fontfamily <- get_dm_fontfamily(piece_side, suit, rank, cfg)
    dm_fontface <- get_dm_fontface(piece_side, suit, rank, cfg)
    dm_fontsize <- cex * dm_cex * get_dm_fontsize(piece_side, suit, rank, cfg)
    dm_text <- get_dm_text(piece_side, suit, rank, cfg)
    dm_t <- get_dm_t(piece_side, suit, rank, cfg)
    dm_r <- get_dm_r(piece_side, suit, rank, cfg)
    dm_x <- to_x(dm_t, dm_r) + 0.5
    dm_y <- to_y(dm_t, dm_r) + 0.5

    # Primary symbol
    ps_color <- get_ps_color(piece_side, suit, rank, cfg)
    ps_cex <- get_ps_cex(piece_side, suit, rank, cfg)
    ps_fontfamily <- get_ps_fontfamily(piece_side, suit, rank, cfg)
    ps_fontface <- get_ps_fontface(piece_side, suit, rank, cfg)
    ps_fontsize <- cex * ps_cex * get_ps_fontsize(piece_side, suit, rank, cfg)
    ps_text <- get_ps_text(piece_side, suit, rank, cfg)
    ps_t <- get_ps_t(piece_side, suit, rank, cfg)
    ps_r <- get_ps_r(piece_side, suit, rank, cfg)
    ps_x <- to_x(ps_t, ps_r) + 0.5
    ps_y <- to_y(ps_t, ps_r) + 0.5

    list(shape=shape, shape_r=shape_r, shape_t=shape_t,
         background_color=background_color,
         border_color=border_color, border_lex=border_lex, edge_color=edge_color,
         gridline_color=gridline_color, gridline_lex=gridline_lex,
         mat_color=mat_color, mat_width=mat_width,
         dm_color=dm_color, dm_text=dm_text,
         dm_fontsize=dm_fontsize,
         dm_fontfamily=dm_fontfamily, dm_fontface=dm_fontface,
         dm_x=dm_x, dm_y=dm_y,
         ps_color=ps_color, ps_text=ps_text,
         ps_fontsize=ps_fontsize,
         ps_fontfamily=ps_fontfamily, ps_fontface=ps_fontface,
         ps_x=ps_x, ps_y=ps_y)
}
