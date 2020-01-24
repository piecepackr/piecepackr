#' Standard game systems
#'
#' \code{game_systems} returns a list of \code{pp_cfg} objects
#' representing several game systems.
#' \code{to_subpack} and \code{to_hexpack} will attempt to generate matching (piecepack stackpack)
#'      subpack and (piecepack) hexpack \code{pp_cfg} R6 objects respectively given a piecepack configuration.
#'
#' Contains the following game systems:\describe{
#' \item{dice}{Traditional six-sided pipped dice in six color schemes (controlled by their suit).}
#' \item{dominoes_black, dominoes_blue, dominoes_green, dominoes_red, dominoes_white, dominoes_yellow}{
#'      Traditional pipped dominoes in six color schemes.
#'      In each scheme the number of pips on the \dQuote{top} of the domino is
#'      controlled by their \dQuote{rank} and on the \dQuote{bottom} by their \dQuote{suit}.}
#' \item{dual_piecepacks_expansion}{A companion piecepack with a special suit scheme.
#'               See \url{https://trevorldavis.com/piecepackr/dual-piecepacks-pnp.html}.}
#' \item{hexpack}{A hexagonal extrapolation of the piecepack designed by Nathan Morse and Daniel Wilcox.
#'                See \url{https://boardgamegeek.com/boardgameexpansion/35424/hexpack}.}
#' \item{piecepack}{A public domain game system invented by James "Kyle" Droscha.
#'   See \url{http://www.ludism.org/ppwiki}.
#'   Configuration also contains the following piecepack accessories:\describe{
#'     \item{piecepack matchsticks}{A public domain accessory developed by Dan Burkey.
#'                                 See \url{http://www.ludism.org/ppwiki/PiecepackMatchsticks}.}
#'     \item{piecepack pyramids}{A public domain accessory developed by Tim Schutz.
#'                              See \url{http://www.ludism.org/ppwiki/PiecepackPyramids}.}
#'     \item{piecepack saucers}{A public domain accessory developed by Karol M. Boyle at Mesomorph Games.
#'                              See \url{http://www.piecepack.org/Accessories.html}.}
#'   }}
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
#'        df_tiles <- data.frame(piece_side="tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1),
#'                               suit=NA, angle=NA, z=NA, stringsAsFactors=FALSE)
#'        df_coins <- data.frame(piece_side="coin_back", x=rep(4:1, 4), y=rep(4:1, each=4),
#'                               suit=c(1,4,1,4,4,1,4,1,2,3,2,3,3,2,3,2),
#'                               angle=rep(c(180,0), each=8), z=1/4+1/16, stringsAsFactors=FALSE)
#'        df <- rbind(df_tiles, df_coins)
#'        df$cfg <- "playing_cards_expansion"
#'
#'        pmap_piece(df, envir=cfgs, op_scale=0.5, default.units="in")
#' @seealso \code{\link{pp_cfg}} for information about the \code{pp_cfg} objects returned by \code{game_systems}.
#' @export
game_systems <- function(style=NULL) {
    if (is.null(style)) {
        piecepack_suits <- list(suit_text="\u263c,\u25d8,\u0238,\u03ee,\u2202")
        pce_suit_text <- "\u2665,\u2660,\u2663,\u2666,\u2202"
    } else if (style == "dejavu") {
        piecepack_suits <- list(suit_text="\u2742,\u25d0,\u265b,\u269c,\u0ed1",
                                suit_cex.s2=0.9, dm_cex.coin=0.5, fontfamily="DejaVu Sans")
        pce_suit_text <- "\u2665,\u2660,\u2663,\u2666,\u0ed1"
    } else if (style == "dejavu3d") {
        piecepack_suits <- list(suit_text="\u2742,\u25d0,\u265b,\u269c,\u0ed1",
                                background_color="burlywood", border_color="transparent",
                                mat_color.tile_back = "burlywood", width.tile = 1.95,
                                edge_color.tile = "black", edge_color.coin = "black",
                                suit_cex.s2=0.9, dm_cex.coin=0.5, fontfamily="DejaVu Sans")
        pce_suit_text <- "\u2665,\u2660,\u2663,\u2666,\u0ed1"
    } else {
        stop(paste("Don't have a customized configuration for style", style))
    }
    piecepack_base <- list(border_color="black", border_lex=4, depth.coin=0.25,
                          mat_color.tile_back="white", mat_width.tile_back=0.05, suit_color.unsuited="black",
                          rank_text=",a,2,3,4,5", use_suit_as_ace=TRUE)
    piecepack <- c(piecepack_suits, piecepack_base)

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

    dice <- pp_cfg(list(n_suits = 6, n_ranks = 6,
                        width.die = 16 / 25.4, # 16 mm dice most common
                        suit_color = "#D55E00,#000000,#009E73,#56B4E9,#E69F00,#FFFFFF",
                        background_color = "white,white,white,white,black,black",
                        invert_colors = TRUE,
                        border_color = "black", border_color.s2 = "grey30", border_lex = 4,
                        grob_fn.die = pippedGrobFn(0, FALSE)
                        ))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE

    list(dice = dice,
         dominoes_black = dominoes("black", "white", "grey30"),
         dominoes_blue = dominoes("#56B4E9", "white", "black"),
         dominoes_green = dominoes("#009E73", "white", "black"),
         dominoes_red = dominoes("#D55E00", "white", "black"),
         dominoes_white = dominoes("white", "black", "black"),
         dominoes_yellow = dominoes("#E69F00", "black", "black"),
         dual_piecepacks_expansion=pp_cfg(dual_piecepacks_expansion),
         hexpack=to_hexpack(piecepack),
         piecepack=pp_cfg(piecepack),
         playing_cards_expansion=pp_cfg(playing_cards_expansion),
         subpack=to_subpack(piecepack))
}

dominoes <- function(background_color = "white", suit_color = "black", border_color = "black") {
    dominoes <- pp_cfg(list(n_suits = 13, n_ranks = 13,
                            width.tile = 1, height.tile = 2,
                            width.die = 16 / 25.4,
                            suit_color = suit_color, background_color = background_color,
                            border_color = border_color, border_lex = 4,
                            grob_fn.die = pippedGrobFn(0, FALSE),
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
