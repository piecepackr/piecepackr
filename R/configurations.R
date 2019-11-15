#' Standard game systems
#'
#' \code{game_systems} returns a list of \code{pp_cfg} objects
#' of some game systems: the piecepack (plus a playing cards expansion and a subpack).
#' @param style If \code{NULL} (the default) uses suit glyphs from the default \dQuote{sans} font.  
#'        If \code{dejavu} uses suit glyphs from the "DejaVu Sans" font (must be installed on the system).
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
#' @seealso [pp_cfg()] for information about \code{pp_cfg} objects.
#' @export
game_systems <- function(style=NULL) {
    if (is.null(style)) {
        piecepack <- list(suit_text="\u263c,\u25d8,\u0238,\u03ee,\u2202",  
                          border_color="black", border_lex=4, depth.coin=0.25,
                          mat_color.tile_back="white", mat_width.tile_back=0.05, suit_color.unsuited="black",
                          rank_text=",a,2,3,4,5", use_suit_as_ace=TRUE)
        playing_cards_expansion <- piecepack
        playing_cards_expansion$suit_text <- "\u2665,\u2660,\u2663,\u2666,\u2202"
    } else if (style == "dejavu") {
        piecepack <- list(suit_text="\u2742,\u25d0,\u265b,\u269c,\u0ed1", fontfamily="DejaVu Sans", 
                          border_color="black", border_lex=4, dm_cex.coin=0.5, depth.coin=0.25, 
                          mat_color.tile_back="white", mat_width.tile_back=0.05, suit_color.unsuited="black",
                          rank_text=",a,2,3,4,5", use_suit_as_ace=TRUE)
        playing_cards_expansion <- piecepack
        playing_cards_expansion$suit_text <- "\u2665,\u2660,\u2663,\u2666,\u0ed1"
    } else {
        stop(paste("Don't have a customized configuration for style", style))
    }
    playing_cards_expansion$suit_color <- "#D55E00,#000000,#000000,#D55E00,#000000"
    subpack <- c(piecepack, list(width.tile=0.75, width.coin=0.3, width.die=0.3, 
                                 width.pawn=0.4 * 0.5, height.pawn=0.4 * 0.875,
                                 cex=0.4))

    list(piecepack=pp_cfg(piecepack), playing_cards_expansion=pp_cfg(playing_cards_expansion),
         subpack=pp_cfg(subpack))
}

