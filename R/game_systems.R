#' Standard game systems
#'
#' \code{game_systems} returns a list of \code{pp_cfg} objects
#' representing several game systems and pieces.
#' \code{to_subpack} and \code{to_hexpack} will attempt to generate matching (piecepack stackpack)
#'      subpack and (piecepack) hexpack \code{pp_cfg} R6 objects respectively given a piecepack configuration.
#'
#' Contains the following game systems:\describe{
#' \item{checkers1, checkers2}{Checkers and checkered boards in six color schemes.
#'       Checkers are represented a piecepackr \dQuote{bit}.  The \dQuote{board} \dQuote{face} is a checkered board
#'       and the \dQuote{back} is a lined board.
#'       Color is controlled by suit and number of rows/columns by rank.
#'       \code{checkers1} has one inch squares and \code{checkers2} has two inch squares.}
#' \item{dice}{Traditional six-sided pipped dice in six color schemes (color controlled by their suit).}
#' \item{dominoes, dominoes_black, dominoes_blue, dominoes_green, dominoes_red, dominoes_white, dominoes_yellow}{
#'      Traditional pipped dominoes in six color schemes (\code{dominoes} and \code{dominoes_white} are the same).
#'      In each color scheme the number of pips on the \dQuote{top} of the domino is
#'      controlled by their \dQuote{rank} and on the \dQuote{bottom} by their \dQuote{suit}.}
#' \item{dual_piecepacks_expansion}{A companion piecepack with a special suit scheme.
#'               See \url{https://trevorldavis.com/piecepackr/dual-piecepacks-pnp.html}.}
#' \item{hexpack}{A hexagonal extrapolation of the piecepack designed by Nathan Morse and Daniel Wilcox.
#'                See \url{https://boardgamegeek.com/boardgameexpansion/35424/hexpack}.}
#' \item{meeples}{Standard 16mm x 16mm x 10mm \dQuote{meeples} in six colors represented by a \dQuote{bit}.}
#' \item{piecepack}{A public domain game system invented by James "Kyle" Droscha.
#'   See \url{http://www.ludism.org/ppwiki}.
#'   Configuration also contains the following piecepack accessories:\describe{
#'     \item{piecepack dice cards}{An accessory proposed by John Braley.
#'                                 See \url{http://www.ludism.org/ppwiki/PiecepackDiceCards}.}
#'     \item{piecepack matchsticks}{A public domain accessory developed by Dan Burkey.
#'                                 See \url{http://www.ludism.org/ppwiki/PiecepackMatchsticks}.}
#'     \item{piecepack pyramids}{A public domain accessory developed by Tim Schutz.
#'                              See \url{http://www.ludism.org/ppwiki/PiecepackPyramids}.}
#'     \item{piecepack saucers}{A public domain accessory developed by Karol M. Boyle at Mesomorph Games.
#'              See \url{https://web.archive.org/web/20190719155827/http://www.piecepack.org/Accessories.html}.}
#'   }}
#' \item{playing_cards, playing_cards_colored, playing_cards_tarot}{
#'       Poker-sized \code{card} components for various playing card decks:\describe{
#'        \item{playing_cards}{A traditional deck of playing cards with 4 suits
#'            and 13 ranks (A, 2-10, J, Q, K) plus a 14th "Joker" rank.}
#'        \item{playing_cards_colored}{Like \code{playing_cards} but with five colored suits:
#'            red hearts, black spades, green clubs, blue diamonds, and yellow stars.}
#'        \item{playing_cards_tarot}{A (French Bourgeois) deck of tarot playing cards:
#'            first four suits are hearts, spades, clubs, and diamonds with
#'            14 ranks (ace through jack, knight, queen, king) plus a
#'            fifth "suit" of 22 trump cards (1-21 plus an "excuse").}}}
#' \item{playing_cards_expansion}{A piecepack with the standard ``French'' playing card suits.
#'                                See \url{http://www.ludism.org/ppwiki/PlayingCardsExpansion}.}
#' \item{subpack}{A mini piecepack.  Designed to be used with the \code{piecepack} to make piecepack
#'               ``stackpack'' diagrams.  See \url{http://www.ludism.org/ppwiki/StackPack}.}
#' }
#' @param style If \code{NULL} (the default) uses suit glyphs from the default \dQuote{sans} font.
#'        If \code{"dejavu"} it will use suit glyphs from the "DejaVu Sans" font (must be installed on the system).
#' @param cfg List of configuration options
#' @examples
#'        cfgs <- game_systems()
#'        names(cfgs)
#'
#'     if (require("grid")) {
#'        # standard dice
#'        grid.newpage()
#'        grid.piece("die_face", x=1:6, default.units="in", rank=1:6, suit=1:6,
#'                   op_scale=0.5, cfg=cfgs$dice)
#'
#'        # dominoes
#'        grid.newpage()
#'        colors <- c("black", "red", "green", "blue", "yellow", "white")
#'        cfg <- paste0("dominoes_", rep(colors, 2))
#'        grid.piece("tile_face", x=rep(4:1, 3), y=rep(2*3:1, each=4), suit=1:12, rank=1:12+1,
#'                   cfg=cfg, default.units="in", envir=cfgs, op_scale=0.5)
#'
#'        # various piecepack expansions
#'        grid.newpage()
#'        df_tiles <- data.frame(piece_side="tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1),
#'                               suit=NA, angle=NA, z=NA, stringsAsFactors=FALSE)
#'        df_coins <- data.frame(piece_side="coin_back", x=rep(4:1, 4), y=rep(4:1, each=4),
#'                               suit=c(1,2,1,2,2,1,2,1,4,3,4,3,3,4,3,4),
#'                               angle=rep(c(180,0), each=8), z=1/4+1/16, stringsAsFactors=FALSE)
#'        df <- rbind(df_tiles, df_coins)
#'        pmap_piece(df, cfg = cfgs$piecepack, op_scale=0.5, default.units="in")
#'
#'        grid.newpage()
#'        df_coins <- data.frame(piece_side="coin_back", x=rep(4:1, 4), y=rep(4:1, each=4),
#'                               suit=c(1,4,1,4,4,1,4,1,2,3,2,3,3,2,3,2),
#'                               angle=rep(c(180,0), each=8), z=1/4+1/16, stringsAsFactors=FALSE)
#'        df <- rbind(df_tiles, df_coins)
#'        pmap_piece(df, cfg = cfgs$playing_cards_expansion, op_scale=0.5, default.units="in")
#'
#'        grid.newpage()
#'        pmap_piece(df, cfg = cfgs$dual_piecepacks_expansion, op_scale=0.5, default.units="in")
#'     }
#' @seealso \code{\link{pp_cfg}} for information about the \code{pp_cfg} objects returned by \code{game_systems}.
#' @export
game_systems <- function(style=NULL) {
    styles <- c("dejavu", "dejavu3d", "sans", "sans3d")
    if (!is.null(style) && is.na(match(style, styles))) {
        stop(paste("Don't have a customized configuration for style", style))
    }
    if (is.null(style)) style <- "sans"
    is_3d <- grepl("3d$", style)
    if (grepl("^sans", style)) {
        piecepack_suits <- list(suit_text="\u263c,\u25d8,\u0238,\u03ee,\u2202")
        pce_suit_text <- "\u2665,\u2660,\u2663,\u2666,\u2202"
        pc_suit_text <- list(suit_text="\u2665,\u2660,\u2663,\u2666,*",
                             suit_cex.s5=1.3)
    } else if (grepl("^dejavu", style)) {
        piecepack_suits <- list(suit_text="\u2742,\u25d0,\u265b,\u269c,\u0ed1",
                                suit_cex.s2=0.9, dm_cex.coin=0.5, fontfamily="DejaVu Sans")
        pce_suit_text <- "\u2665,\u2660,\u2663,\u2666,\u0ed1"
        pc_suit_text <- list(suit_text="\u2665,\u2660,\u2663,\u2666,\u2605")
    }
    if (is_3d) {
        style_3d <- list(suit_color.s4 = "#0072B2",
                         invert_colors.pawn = TRUE,
                         invert_colors.die = TRUE,
                         background_color="burlywood",
                         border_color="transparent",
                         mat_color.tile_back = "burlywood",
                         edge_color.tile = "black", edge_color.coin = "black")
        color_list <- list(suit_color = cb_suit_colors_pure,
                           border_color = "transparent", border_lex = 0,
                           edge_color.board = "black")
    } else {
        style_3d <- NULL
        color_list <- list(suit_color = cb_suit_colors_impure,
                           border_color = "black", border_lex = 4,
                           edge_color.board = "white")
    }
    piecepack_base <- list(border_color="black", border_lex=4, depth.coin=0.25,
                           invert_colors.matchstick = TRUE, ps_cex.r2.matchstick = 0.7,
                           dm_r.r1.matchstick = 0, dm_cex.r1.matchstick = 1.5, suit_color.s2.matchstick = "grey30",
                           mat_color.tile_back="white", mat_width.tile_back=0.05, suit_color.unsuited="black",
                           invert_colors.bit = TRUE,
                           rank_text=",a,2,3,4,5", use_suit_as_ace=TRUE)
    piecepack <- c(style_3d, piecepack_suits, piecepack_base)

    playing_cards_expansion <- piecepack
    playing_cards_expansion$suit_text <- pce_suit_text
    playing_cards_expansion$suit_color <- "#D55E00,#000000,#000000,#D55E00,#000000"

    hexpack <- c(piecepack, list(shape.tile="convex6", border_lex=3,
                                 shape_t.tile="60",  dm_t.tile_face=-90,
                                 width.tile=4/sqrt(3), height.tile=4/sqrt(3),
                                 shape.coin="convex3"))

    dpe_base <- c(invert_colors.suited=TRUE,
                  mat_color.tile_face="white", mat_width.tile_face=0.05,
                  border_color.s2.die="grey40", border_color.s2.pawn="grey40")

    dual_piecepacks_expansion <- c(piecepack, dpe_base)
    dual_piecepacks_expansion$suit_text <- pce_suit_text

    dice_list <- list(n_suits = 6, n_ranks = 6,
                        rank_text = "1,2,3,4,5,6",
                        width.die = 16 / 25.4, # 16 mm dice most common
                        background_color = "white,white,white,white,black,black",
                        invert_colors = TRUE,
                        die_arrangement = "opposites_sum_to_5",
                        grob_fn.card = cardGrobFn(type = "circle"),
                        grob_fn.die = pippedGrobFn(0, FALSE))
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE

    playing_cards_list <- list(n_ranks = 14,
                                 rank_text = "A,2,3,4,5,6,7,8,9,10,J,Q,K,\n\n\n\nJ\nO\nK\nE\nR",
                                 grob_fn.card = cardGrobFn(),
                                 grob_fn.r11.card = cardGrobFn(-11),
                                 grob_fn.r12.card = cardGrobFn(-12),
                                 grob_fn.r13.card = cardGrobFn(-13),
                                 grob_fn.r14.card = cardGrobFn(-14),
                                 suit_text.r14 = "",
                                 border_color = "black", border_lex = 4)
    playing_cards_list$n_suits <- 4
    playing_cards_list$suit_color <- "#D55E00,#000000,#000000,#D55E00,#E59F00"

    playing_cards <- pp_cfg(c(playing_cards_list, pc_suit_text))
    playing_cards$has_piecepack <- FALSE
    playing_cards$has_cards <- TRUE

    playing_cards_colored <- c(playing_cards_list, pc_suit_text)
    playing_cards_colored$n_suits <- 5
    playing_cards_colored$suit_color <- cb_suit_colors_pure

    playing_cards_colored <- pp_cfg(playing_cards_colored)
    playing_cards_colored$has_piecepack <- FALSE
    playing_cards_colored$has_cards <- TRUE

    playing_cards_tarot <- playing_cards_list
    playing_cards_tarot$rank_text <- "A,2,3,4,5,6,7,8,9,10,J,C,Q,K"
    fool_text <- ifelse(is.null(style), "*", "\u2605")
    playing_cards_tarot$rank_text.s5 <- paste(c(1:21, fool_text), collapse = ",")
    playing_cards_tarot$n_suits <- 5
    playing_cards_tarot$n_ranks <- 22

    tarot_suit_text <- "\u2665,\u2660,\u2663,\u2666,"
    playing_cards_tarot$suit_text <- tarot_suit_text
    playing_cards_tarot$suit_text.r14 <- tarot_suit_text
    playing_cards_tarot$suit_color <- "#D55E00,#000000,#000000,#D55E00,#000000"
    for (i in 15:22) {
        playing_cards_tarot[[paste0("grob_fn.r", i, ".card")]] <- cardGrobFn(-i)
    }
    playing_cards_tarot <- pp_cfg(playing_cards_tarot)
    playing_cards_tarot$has_piecepack <- FALSE
    playing_cards_tarot$has_cards <- TRUE

    meeples_list <- list(shape.bit = "meeple", n_suits = 6,
                         width.bit = 16 / 25.4, height.bit = 16 / 25.4, depth.bit = 10 / 25.4,
                         ps_text.bit = "", dm_text.bit = "",
                         invert_colors.bit = TRUE, background_color = "white")
    meeples <- pp_cfg(c(meeples_list, color_list))
    meeples$has_piecepack <- FALSE
    meeples$has_bits <- TRUE

    list(checkers1 = checkers(1, color_list),
         checkers2 = checkers(2, color_list),
         dice = dice,
         dominoes = dominoes(color_list$suit_color[6], "black", color_list$border_color),
         dominoes_black = dominoes(color_list$suit_color[2], "white", color_list$border_color),
         dominoes_blue = dominoes(color_list$suit_color[4], "white", color_list$border_color),
         dominoes_green = dominoes(color_list$suit_color[3], "white", color_list$border_color),
         dominoes_red = dominoes(color_list$suit_color[1], "white", color_list$border_color),
         dominoes_white = dominoes(color_list$suit_color[6], "black", color_list$border_color),
         dominoes_yellow = dominoes(color_list$suit_color[5], "black", color_list$border_color),
         dual_piecepacks_expansion=pp_cfg(dual_piecepacks_expansion),
         hexpack=to_hexpack(piecepack),
         meeples=meeples,
         piecepack=pp_cfg(piecepack),
         playing_cards = playing_cards,
         playing_cards_colored = playing_cards_colored,
         playing_cards_tarot = playing_cards_tarot,
         playing_cards_expansion=pp_cfg(playing_cards_expansion),
         subpack=to_subpack(piecepack))
}

cb_suit_colors_impure <- c("#D55E00", "grey30", "#009E73", "#56B4E9", "#E69F00", "#FFFFFF")
cb_suit_colors_pure <- c("#D55E00", "#000000", "#009E73", "#56B4E9", "#E69F00", "#FFFFFF")

dominoes <- function(background_color = "white", suit_color = "black", border_color = "black", mat_width = 0) {
    border_lex <- ifelse(border_color == "black", 4, 0)
    dominoes <- pp_cfg(list(n_suits = 13, n_ranks = 13,
                            width.tile = 1,
                            height.tile = 2,
                            depth.tile = 0.25, # 3/8 professional, 1/2 jumbo
                            width.die = 16 / 25.4,
                            suit_color = suit_color, background_color = background_color,
                            mat_width = mat_width, mat_color = suit_color,
                            border_color = border_color, border_lex = border_lex,
                            die_arrangement = "opposites_sum_to_5",
                            grob_fn.die = pippedGrobFn(0, FALSE),
                            grob_fn.card = cardGrobFn(-1, type = "circle"),
                            gridline_color.tile_back = "transparent",
                            gridline_color.tile_face = suit_color,
                            gridline_lex.tile_face = 6,
                            grob_fn.tile_face = dominoGrobFn(-1, FALSE)
                            ))
    dominoes$has_piecepack <- FALSE
    dominoes$has_dice <- TRUE
    dominoes$has_tiles <- TRUE
    dominoes
}

checkers <- function(cell_width = 1, color_list) {
    checkers <- list(n_suits = 6, n_ranks = 12,
                     width.board = 8 * cell_width,
                     height.board = 8 * cell_width,
                     width.bit = 0.75 * cell_width, invert_colors.bit = TRUE,
                     ps_text.bit = "", dm_text.bit = "",
                     grob_fn.r1.board_face = checkeredBoardGrobFn(8, 8),
                     grob_fn.r1.board_back = linedBoardGrobFn(8, 8),
                     gridline_color.board_face = cb_suit_colors_impure,
                     gridline_color.board_back = cb_suit_colors_pure,
                     gridline_lex.board = 4,
                     suit_color = cb_suit_colors_impure,
                     background_color = "white",
                     gridline_color.s6.board_face = "grey80",
                     gridline_color.s6.board_back = "grey80")
    for (i in seq(2, 12)) {
        checkers[[paste0("width.r", i, ".board")]] <- i * cell_width
        checkers[[paste0("height.r", i, ".board")]] <- i * cell_width
        checkers[[paste0("grob_fn.r", i, ".board_face")]] <- checkeredBoardGrobFn(i, i)
        checkers[[paste0("grob_fn.r", i, ".board_back")]] <- linedBoardGrobFn(i, i)
    }
    checkers <- pp_cfg(c(checkers, color_list))
    checkers$has_piecepack <- FALSE
    checkers$has_boards <- TRUE
    checkers$has_bits <- TRUE
    checkers
}

#' @rdname game_systems
#' @export
to_hexpack <- function(cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    hexpack <- as.list(cfg)
    hexpack$shape.tile <- "convex6"
    hexpack$border_lex <- 3
    hexpack$shape_t.tile <- 60
    hexpack$dm_t.tile_face <- -90
    hexpack$width.tile <- 4/sqrt(3)
    hexpack$height.tile <- 4/sqrt(3)
    hexpack$shape.coin <- "convex3"
    pp_cfg(hexpack)
}

#' @rdname game_systems
#' @export
to_subpack <- function(cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    subpack <- as.list(cfg)
    subpack$width.tile <- (3/8)*cfg$get_width("tile")
    subpack$width.coin <- 0.4*cfg$get_width("coin")
    subpack$width.die <- 0.6*cfg$get_width("die")
    subpack$width.pawn <- 0.4*cfg$get_width("pawn")
    subpack$height.pawn <- 0.4*cfg$get_height("pawn")
    subpack$width.saucer <- 0.4*cfg$get_width("saucer")
    subpack$cex <- 0.4
    pp_cfg(subpack)
}
