#' Standard game systems
#'
#' \code{game_systems} returns a list of \code{pp_cfg} objects
#' representing several game systems.
#'
#' Contains the following game systems:\itemize{
#' \item{icehouse pieces}
#' \item{piecepack plus several piecepack accessories/expansions:\itemize{
#'   \item{piecepack matchsticks}
#'   \item{piecepack pyramids}
#'   \item{piecepack saucers}
#'   \item{hexpack}
#'   \item{playing cards expansion}
#'   \item{dual piecepacks expansion}
#'   \item{(stackpack) subpack aka mini piecepack}
#' }}}
#' @param style If \code{NULL} (the default) uses suit glyphs from the default \dQuote{sans} font.
#'        If \code{"dejavu"} it will use suit glyphs from the "DejaVu Sans" font (must be installed on the system).
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

    icehouse_pieces <- list(n_ranks=4, n_suits=6,
                        width.r1.pyramid=11/32, width.r2.pyramid=9/16,
                        width.r3.pyramid=25/32, width.r4.pyramid=1,
                        height.r1.pyramid=5/8, height.r2.pyramid=1,
                        height.r3.pyramid=1.375, height.r4.pyramid=1.75,
                        rank_text=",\u25cf,\u25cf\u25cf,\u25cf\u25cf\u25cf",
                        suit_color="#D55E00,#808080,#009E73,#56B4E9,#E69F00,#808080",
                        border_color.pyramid="#D55E00,#808080,#009E73,#56B4E9,#E69F00,#808080",
                        background_color.pyramid="#D55E0080,#000000,#009E7380,#56B4E980,#E69F0080,#FFFFFF",
                        border_lex.pyramid=4, grob_fn.pyramid=icehousePyramidGrob)

    list(dual_piecepacks_expansion=pp_cfg(dual_piecepacks_expansion),
         hexpack=to_hexpack(piecepack),
         icehouse_pieces=pp_cfg(icehouse_pieces),
         piecepack=pp_cfg(piecepack),
         playing_cards_expansion=pp_cfg(playing_cards_expansion),
         subpack=to_subpack(piecepack))
}

#' @rdname pp_cfg
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

#' @rdname pp_cfg
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

icehousePyramidGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)

    shape_fn <- get_shape_grob_fn(opt$shape, opt$shape_t, opt$shape_r)

    # Background
    background_grob <- shape_fn(gp=gpar(col=NA, fill=opt$background_color))

    # Circles
    gp_c <- gpar(fill=opt$ps_color, col=opt$ps_color)
    c1_grob <- circleGrob(x=unit(0.5, "npc"), y=unit(0.48, "cm"), r=unit(0.12, "cm"), gp=gp_c)
    c2_grob <- circleGrob(x=unit(0.5, "npc")-unit(0.38, "cm"), y=unit(0.48, "cm"), r=unit(0.12, "cm"), gp=gp_c)
    c3_grob <- circleGrob(x=unit(0.5, "npc")-unit(0.76, "cm"), y=unit(0.48, "cm"), r=unit(0.12, "cm"), gp=gp_c)
    c_grob <- switch(rank, nullGrob(), c1_grob, gList(c1_grob, c2_grob), gList(c1_grob, c2_grob, c3_grob))

    # Border
    border_grob <- shape_fn(gp=gpar(col=opt$border_color, fill=NA, lex=opt$border_lex))
    gl <- gList(background_grob, c_grob, border_grob)

    gTree(children=gl, name=piece_side)
}
