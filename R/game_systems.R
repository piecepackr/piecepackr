#' Standard game systems
#'
#' \code{game_systems} returns a list of \code{pp_cfg} objects
#' representing several game systems and pieces.
#' \code{to_subpack} and \code{to_hexpack} will attempt to generate matching (piecepack stackpack)
#'      subpack and (piecepack) hexpack \code{pp_cfg} R6 objects respectively given a piecepack configuration.
#'
#' Contains the following game systems:\describe{
#' \item{alquerque}{Boards and pieces in six color schemes for Alquerque}
#' \item{checkers1, checkers2}{Checkers and checkered boards in six color schemes.
#'       Checkers are represented by a piecepackr \dQuote{bit}.
#'       The \dQuote{board} \dQuote{face} is a checkered board and the \dQuote{back} is a lined board.
#'       Color is controlled by suit and number of rows/columns by rank.
#'       \code{checkers1} has one inch squares and \code{checkers2} has two inch squares.}
#' \item{chess1, chess2}{Chess pieces, boards, and dice in six color schemes.
#'       Chess pieces are represented by a \dQuote{bit} (face).
#'       The \dQuote{board} \dQuote{face} is a checkered board and the \dQuote{back} is a lined board.
#'       Color is controlled by suit and number of rows/columns by rank.
#'       \code{chess1} has one inch squares and \code{chess2} has two inch squares.
#'       Currently uses print-and-play style discs instead of 3D Staunton chess pieces.}
#' \item{dice}{Traditional six-sided pipped dice in six color schemes (color controlled by their suit).}
#' \item{dice_d4, dice_numeral, dice_d8, dice_d10, dice_d10_percentile, dice_d12, dice_d20}{
#'       Polyhedral dice most commonly used to play wargames, roleplaying games, and trading card games:\describe{
#'   \item{dice_d4}{Four-sided dice in six color schemes (color controlled by their suit).
#'                  Tetrahedrons with the rank as a numeral at the top point.}
#'   \item{dice_numeral}{Six-sided dice with numerals instead of pips in six color schemes (color controlled by their suit).}
#'   \item{dice_d8}{Eight-sided dice in six color schemes (color controlled by their suit).
#'                  Octahedrons with the rank as a numeral at the top face.}
#'   \item{dice_d10}{Ten-sided dice in six color schemes (color controlled by their suit).
#'                  Pentagonal trapezohedrons with the rank as a numeral at the top face.
#'                  The rank of ten is represented by a zero.}
#'   \item{dice_d10_percentile}{Ten-sided dice in six color schemes (color controlled by their suit).
#'                  Pentagonal trapezohedrons with the rank as a numeral followed by a zero at the top face.
#'                  The rank of ten is represented by a zero.}
#'   \item{dice_d12}{Twelve-sided dice in six color schemes (color controlled by their suit).
#'                  Dodecahedrons with the rank as a numeral at the top face.}
#'   \item{dice_d20}{Twenty-sided dice in six color schemes (color controlled by their suit).
#'                   Icosahedrons with the rank as a numeral at the top face.}}}
#' \item{dice_fudge}{\dQuote{Fudge} dice in six color schemes (color controlled by their suit).
#'                   \dQuote{Fudge} dice have three ranks "+", " ", and "-" repeated twice.}
#' \item{dominoes, dominoes_black, dominoes_blue, dominoes_green, dominoes_red, dominoes_white, dominoes_yellow}{
#'      Traditional pipped dominoes in six color schemes (\code{dominoes} and \code{dominoes_white} are the same).
#'      In each color scheme the number of pips on the \dQuote{top} of the domino is
#'      controlled by their \dQuote{rank} and on the \dQuote{bottom} by their \dQuote{suit}.
#'      Supports up to double-18 sets.}
#' \item{dominoes_chinese, dominoes_chinese_black}{`dominoes_chinese` has Asian-style six-sided pipped dice with
#'       white background and black and red pips.
#'       The \dQuote{tile}'s are Chinese dominoes (1" x 2.5") whose number of pips are controlled
#'       by both their \dQuote{rank} and their \dQuote{suit}. `dominoes_chinese_black` are like `dominoes_chinese` but the
#'       dice and dominoes have a black background and white and red pips.}
#' \item{go}{Go stones and lined boards in six color schemes.
#'           Go stones are represented by a \dQuote{bit} and the board is a \dQuote{board}.
#'           Color is controlled by suit and number of rows/columns by rank.}
#' \item{meeples}{Standard 16mm x 16mm x 10mm \dQuote{meeples} in six colors represented by a \dQuote{bit}.}
#' \item{morris}{Various morris aka mills aka merels games in six colors.
#'               Color is controlled by suit and \dQuote{size} of morris board
#'               is controlled by rank e.g. \dQuote{Six men's morris} corresponds to a rank of 6 and
#'               \dQuote{Nine men's morris} corresponds to a rank of 9.
#'               Game pieces are represented by stones.}
#' \item{piecepack, dual_piecepacks_expansion, playing_cards_expansion, hexpack, subpack, piecepack_inverted}{
#'   The piecepack is a public domain game system invented by James "Kyle" Droscha.
#    There are also several public domain accessories and expansions:
#'   See \url{https://www.ludism.org/ppwiki} for more info about the piecepack and its accessories/expansions.
#'   \describe{
#'   \item{piecepack}{A standard piecepack.  The configuration also contains the following piecepack accessories:\describe{
#'     \item{piecepack dice cards}{An accessory proposed by John Braley.
#'                                 See \url{https://www.ludism.org/ppwiki/PiecepackDiceCards}.}
#'     \item{piecepack matchsticks}{A public domain accessory developed by Dan Burkey.
#'                                 See \url{https://www.ludism.org/ppwiki/PiecepackMatchsticks}.}
#'     \item{piecepack pyramids}{A public domain accessory developed by Tim Schutz.
#'                              See \url{https://www.ludism.org/ppwiki/PiecepackPyramids}.}
#'     \item{piecepack saucers}{A public domain accessory developed by Karol M. Boyle at Mesomorph Games.
#'              See \url{https://web.archive.org/web/20190719155827/http://www.piecepack.org/Accessories.html}.}}}
#' \item{piecepack_inverted}{The standard piecepack with its color scheme inverted.
#'           Intended to aid in highlighting special pieces in diagrams.}
#' \item{dual_piecepacks_expansion}{A companion piecepack with a special suit scheme.
#'               See \url{https://trevorldavis.com/piecepackr/dual-piecepacks-pnp.html}.}
#' \item{playing_cards_expansion}{A piecepack with the standard \dQuote{French} playing card suits.
#'                                See \url{https://www.ludism.org/ppwiki/PlayingCardsExpansion}.}
#' \item{hexpack}{A hexagonal extrapolation of the piecepack designed by Nathan Morse and Daniel Wilcox.
#'                See \url{https://boardgamegeek.com/boardgameexpansion/35424/hexpack}.}
#' \item{subpack}{A mini piecepack.  Designed to be used with the \code{piecepack} to make piecepack
#'               \dQuote{stackpack} diagrams.  See \url{https://www.ludism.org/ppwiki/StackPack}.}
#'}}
#' \item{playing_cards, playing_cards_colored, playing_cards_tarot}{
#'       Poker-sized \code{card} components for various playing card decks:\describe{
#'        \item{playing_cards}{A traditional deck of playing cards with 4 suits
#'            and 13 ranks (A, 2-10, J, Q, K) plus a 14th \dQuote{Joker} rank.}
#'        \item{playing_cards_colored}{Like \code{playing_cards} but with five colored suits:
#'            red hearts, black spades, green clubs, blue diamonds, and yellow stars.}
#'        \item{playing_cards_tarot}{A (French Bourgeois) deck of tarot playing cards:
#'            first four suits are hearts, spades, clubs, and diamonds with
#'            14 ranks (ace through jack, knight, queen, king) plus a 15th \dQuote{Joker} rank
#'            and a fifth "suit" of 22 trump cards (1-21 plus an \dQuote{excuse}).}}}
#' \item{reversi}{Boards and pieces for Reversi.
#'    "board_face" provides lined boards with colored backgrounds.
#'    "board_back" provides checkered boards.
#'    "bit_face" / "bit_back" provides circular game tokens with differently colored sides:
#'    red paired with green, black paired with white, and blue paired with yellow.
#'}
#' }
#' @param style If \code{NULL} (the default) uses suit glyphs from the default \dQuote{sans} font.
#'              If \code{"dejavu"} it will use suit glyphs from the "DejaVu Sans" font
#'              (must be installed on the system).
#' @param round If \code{TRUE} the \dQuote{shape} of \dQuote{tiles} and \dQuote{cards}
#'              will be \dQuote{roundrect} instead of \dQuote{rect} (the default).
#' @param pawn If \code{"token"} (default) the piecepack pawn will be a two-sided token in a \dQuote{halma} outline,
#'             if \code{"peg-doll"} the piecepack pawn will be a \dQuote{peg doll} style pawn, and
#'             if \code{"joystick"} the piecepack pawn will be a \dQuote{joystick} style pawn.
#'             Note for the latter two pawn styles only \code{pawn_top} will work with \code{grid.piece}.
#' @examples
#' cfgs <- game_systems(pawn = "joystick")
#' names(cfgs)
#'
#' \donttest{# May take more than 5 seconds on CRAN servers
#' # standard dice, meeples, and joystick pawns
#' if (requireNamespace("grid", quietly = TRUE) && piecepackr:::device_supports_unicode()) {
#'    opt <- options(piecepackr.at.inform = FALSE)
#'    grid::grid.newpage()
#'    dice <-  c("d4", "numeral", "d8", "d10", "d12", "d20")
#'    cfg <- paste0("dice_", dice)
#'    grid.piece("die_face", suit = c(1:6, 1), rank = 1:6,
#'               cfg = cfg, envir = cfgs, x = 1:6, y = 1,
#'               default.units = "in", op_scale = 0.5)
#'    grid.piece("die_face", rank=1:6, suit=1:6,
#'               x=1:6, y=2, default.units="in",
#'               op_scale=0.5, cfg=cfgs$dice)
#'    grid.piece("bit_face", suit=1:6,
#'               x=1:6, y=3, default.units="in",
#'               op_scale=0.5, cfg=cfgs$meeple)
#'    grid.piece("pawn_top", suit=1:6,
#'               x=1:6, y=4, default.units="in",
#'               op_scale=0.5, cfg=cfgs$piecepack)
#'    options(opt)
#' }
#' }
#' # dominoes
#' if (requireNamespace("grid", quietly = TRUE)) {
#'    grid::grid.newpage()
#'    colors <- c("black", "red", "green", "blue", "yellow", "white")
#'    cfg <- paste0("dominoes_", rep(colors, 2))
#'    grid.piece("tile_face",  suit=1:12, rank=1:12+1,
#'               cfg=cfg, envir=cfgs,
#'               x=rep(6:1, 2), y=rep(2*2:1, each=6),
#'               default.units="in", op_scale=0.5)
#' }
#' # piecepack "playing card expansion"
#' if (requireNamespace("grid", quietly = TRUE) && piecepackr:::device_supports_unicode()) {
#'    grid::grid.newpage()
#'    df_tiles <- data.frame(piece_side="tile_back",
#'                           x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1),
#'                           suit=NA, angle=NA, z=1/8,
#'                           stringsAsFactors=FALSE)
#'    df_coins <- data.frame(piece_side="coin_back",
#'                           x=rep(4:1, 4), y=rep(4:1, each=4),
#'                           suit=c(1,4,1,4,4,1,4,1,2,3,2,3,3,2,3,2),
#'                           angle=rep(c(180,0), each=8), z=1/4+1/16,
#'                           stringsAsFactors=FALSE)
#'    df <- rbind(df_tiles, df_coins)
#'    pmap_piece(df, cfg = cfgs$playing_cards_expansion, op_scale=0.5,
#'               default.units="in")
#' }
#' @seealso \code{\link{pp_cfg}} for information about the \code{pp_cfg} objects returned by \code{game_systems}.
#' @export
game_systems <- function(style = NULL, round = FALSE, pawn = "token") {
    style <- game_systems_style(style)

    is_3d <- grepl("3d$", style)
    rect_shape <- ifelse(round, "roundrect", "rect")
    color_list <- color_list_fn(is_3d)

    cards <- playing_cards(style, rect_shape)
    packs <- piecepack(style, color_list, rect_shape, pawn)

    list(alquerque = alquerque(1, color_list),
         checkers1 = checkers(1, color_list),
         checkers2 = checkers(2, color_list),
         chess1 = chess(style, 1, color_list),
         chess2 = chess(style, 2, color_list),
         dice = dice(color_list, rect_shape),
         dice_d4 = dice_d4(style, color_list),
         dice_d8 = dice_d8(style, color_list),
         dice_d10 = dice_d10(style, color_list),
         dice_d10_percentile = dice_d10_percentile(style, color_list),
         dice_d12 = dice_d12(style, color_list),
         dice_d20 = dice_d20(style, color_list),
         dice_fudge = dice_fudge(color_list, rect_shape),
         dice_numeral = dice_numeral(style, color_list, rect_shape),
         dominoes = dominoes(color_list$suit_color[6], "black", color_list$border_color, rect_shape),
         dominoes_black = dominoes(color_list$suit_color[2], "white", color_list$border_color, rect_shape),
         dominoes_blue = dominoes(color_list$suit_color[4], "white", color_list$border_color, rect_shape),
         dominoes_green = dominoes(color_list$suit_color[3], "white", color_list$border_color, rect_shape),
         dominoes_red = dominoes(color_list$suit_color[1], "white", color_list$border_color, rect_shape),
         dominoes_white = dominoes(color_list$suit_color[6], "black", color_list$border_color, rect_shape),
         dominoes_yellow = dominoes(color_list$suit_color[5], "black", color_list$border_color, rect_shape),
         dominoes_chinese = dominoes_chinese(color_list, rect_shape),
         dominoes_chinese_black = dominoes_chinese(color_list, rect_shape, invert = TRUE),
         dual_piecepacks_expansion = packs$dpe,
         go = go(1, color_list),
         hexpack = packs$hexpack,
         meeples = meeples(color_list),
         morris = morris(1, color_list),
         piecepack = packs$base,
         piecepack_inverted = packs$inverted,
         playing_cards = cards$base,
         playing_cards_colored = cards$color,
         playing_cards_tarot = cards$tarot,
         playing_cards_expansion = packs$pce,
         reversi = reversi(1, color_list),
         subpack = packs$subpack)
}

color_list_fn <- function(is_3d = FALSE) {
    if (is_3d) {
        list(background_color="burlywood",
             suit_color = cb_suit_colors_pure,
             border_color = "transparent", border_lex = 0,
             edge_color.board = "black")
    } else {
        list(background_color="white",
             suit_color = cb_suit_colors_impure,
             border_color = "black", border_lex = 4,
             edge_color.board = "white")
    }
}

game_systems_style <- function(style = "sans") {
    styles <- c("dejavu", "dejavu3d", "sans", "sans3d")
    if (!is.null(style) && is.na(match(style, styles))) {
        abort(paste("Don't have a customized configuration for style", style))
    }

    if (is.null(style))
        style <- "sans"

    if (grepl("dejavu", style) && !suppressWarnings(has_font("Dejavu Sans")))
        inform(sprintf("'style = \"%s\"' but 'has_font(\"Dejavu Sans\")' is 'FALSE'", style))

    style
}

# Okabe-Ito palette i.e. `palette.colors(n = 8, palette = "Okabe-Ito", names=TRUE)` plus white
#                         vermillion black    bluishgreen  skyblue    orange     white
cb_suit_colors_pure <- c("#D55E00", "#000000", "#009E73", "#56B4E9", "#E69F00", "#FFFFFF")
# Swap sky blue for blue and orange for yellow
# cb_suit_colors_pure <- c("#D55E00", "#000000", "#009E73", "#0072B2", "#F0E442", "#FFFFFF")
# Swap sky blue for blue (but keep orange)
# cb_suit_colors_pure <- c("#D55E00", "#000000", "#009E73", "#0072B2", "#E69F00", "#FFFFFF")
cb_suit_colors_impure <- cb_suit_colors_pure
cb_suit_colors_impure[2L] <- "grey30"

dice <- function(color_list = color_list_fn(), rect_shape = "rect") {
    dice_list <- list(n_suits = 6, n_ranks = 6,
                      rank_text = "1,2,3,4,5,6",
                      width.die = 16 / 25.4, # 16 mm dice most common
                      background_color = "white,white,white,white,black,black",
                      invert_colors = TRUE,
                      die_arrangement = "opposites_sum_to_5",
                      shape.card = rect_shape,
                      grob_fn.card = cardGrobFn(grob_type = "circle"),
                      grob_fn.die = pippedGrobFn(0, "die"))
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

dominoes_chinese <- function(color_list = color_list_fn(), rect_shape = "rect", invert = FALSE) {
    dice_list <- list(n_suits = 6, n_ranks = 6,
                      rank_text = "1,2,3,4,5,6",
                      width.die = 16 / 25.4, # 16 mm dice most common
                      background_color = ifelse(invert, color_list$suit_color[2], "white"),
                      suit_color = ifelse(invert, "white", "black"),
                      die_arrangement = "1,2,3,6>,5,4",
                      grob_fn.die = pippedGrobFn(0, "die_chinese"),
                      width.tile = 1,
                      height.tile = 2.5,
                      depth.tile = 0.5,
                      gridline_color.tile_back = "transparent",
                      gridline_color.tile_face = "transparent",
                      gridline_lex.tile_face = 6,
                      shape.tile = rect_shape,
                      grob_fn.tile_face = dominoGrobFn(0, "domino_chinese"),
                      shape.card = rect_shape,
                      grob_fn.card = cardGrobFn(grob_type = "circle")
    )
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

dice_fudge <- function(color_list = color_list_fn(), rect_shape = "rect") {
    dice_list <- list(n_suits = 6, n_ranks = 6,
                      rank_text = "-, ,+,+, ,-",
                      dm_text.die = "",
                      ps_cex.die = 2,
                      grob_fn.die = fudgeGrob,
                      width.die = 16 / 25.4, # 16 mm dice most common
                      background_color = "white,white,white,white,black,black",
                      invert_colors = TRUE,
                      die_arrangement = "opposites_sum_to_5",
                      shape.card = rect_shape)
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

dice_d4 <- function(style = "sans", color_list = color_list_fn()) {
    dice_list <- list(n_suits = 6, n_ranks = 4,
                      rank_text.die = 1:4,
                      dm_text.die = "",
                      ps_cex.die = 1,
                      fontfamily = ifelse(grepl("^dejavu", style), "DejaVu Sans", "sans"),
                      grob_fn.die = d4Grob,
                      op_grob_fn.die = d4TopGrob,
                      obj_fn.die = save_d4_obj,
                      width.die =  (21 / 25.4) / 0.8660254, # if 21 mm vertex to vertex
                      height.die =  (21 / 25.4) / 0.8660254, # if 21 mm vertex to vertex
                      depth.die = (sqrt(6) / 3) * (21 / 25.4),
                      background_color = "white,white,white,white,black,black",
                      shape.die = "convex3",
                      shape_t.die = 90,
                      invert_colors = TRUE)
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

dice_d8 <- function(style = "sans", color_list = color_list_fn()) {
    dice_list <- list(n_suits = 6, n_ranks = 8,
                      rank_text.die = 1:8,
                      ps_cex.die = 1.15,
                      ps_r.die = 0.03,
                      ps_t.die = 90,
                      dm_text.die = "",
                      dm_text.r6.die = "\u2012",
                      dm_r.die = ifelse(grepl("^dejavu", style), 0.14, 0.13),
                      dm_t.die = 270,
                      dm_cex.die = 1.5,
                      fontfamily = ifelse(grepl("^dejavu", style), "DejaVu Sans", "sans"),
                      op_grob_fn.die = d8TopGrob,
                      obj_fn.die = save_d8_obj,
                      width.die =  (18 / 25.4) / 0.8660254, # if 18 mm vertex to vertex
                      height.die =  (18 / 25.4) / 0.8660254, # if 18 mm vertex to vertex
                      depth.die = (sqrt(6) / 3) * (18 / 25.4), # inradius = sqrt(6) * a / 6
                      background_color = "white,white,white,white,black,black",
                      shape.die = "convex3",
                      shape_t.die = 90,
                      invert_colors = TRUE)
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

# We'll use alpha-90-beta-90 angle kites
# See `utils-d10.R` for more notes on calculations
dice_d10 <- function(style = "sans", color_list = color_list_fn()) {
    dice_list <- list(n_suits = 6, n_ranks = 10,
                      rank_text.die = c(1:9, 0),
                      ps_cex.die = 1.15,
                      ps_r.die = -0.08,
                      ps_t.die = 90,
                      dm_text.die = "",
                      dm_text.r6.die = "\u2012",
                      dm_text.r9.die = "\u2012",
                      dm_r.die = ifelse(grepl("^dejavu", style), 0.29, 0.28),
                      dm_t.die = 270,
                      dm_cex.die = 1.5,
                      fontfamily = ifelse(grepl("^dejavu", style), "DejaVu Sans", "sans"),
                      op_grob_fn.die = d10TopGrob,
                      obj_fn.die = save_d10_obj,
                      width.die =  0.4913446110983896164548, # if kite height 5/8"
                      height.die =  5 / 8,
                      depth.die =  0.5621585749837085810299,
                      background_color = "white,white,white,white,black,black",
                      shape.die = "kite",
                      shape_r.die = 0.5 - 0.1909830056250526320039,
                      invert_colors = TRUE)
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

dice_d10_percentile <- function(style = "sans", color_list = color_list_fn()) {
    dice_list <- list(n_suits = 6, n_ranks = 10,
                      rank_text.die = c(1:9, 0),
                      dm_text.die = "0",
                      dm_cex.die = 1.15,
                      dm_r.die = -0.05,
                      dm_t.die = 180,
                      ps_cex.die = 1.00,
                      ps_r.die = 0.20,
                      ps_t.die = 180,
                      fontfamily = ifelse(grepl("^dejavu", style), "DejaVu Sans", "sans"),
                      op_grob_fn.die = d10TopGrob,
                      obj_fn.die = save_d10_obj,
                      width.die =  5 / 8,
                      height.die =  0.4913446110983896164548,
                      depth.die =  0.5621585749837085810299,
                      background_color = "white,white,white,white,black,black",
                      shape.die = "kite",
                      shape_r.die = 0.5 - 0.1909830056250526320039,
                      shape_t.die = 0,
                      invert_colors = TRUE)
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

dice_d12 <- function(style = "sans", color_list = color_list_fn()) {
    dice_list <- list(n_suits = 6, n_ranks = 12,
                      rank_text.die = 1:12,
                      ps_cex.die = 1.15,
                      ps_r.die = 0.01,
                      ps_t.die = 90,
                      dm_text.die = "",
                      dm_text.r6.die = "\u2012",
                      dm_text.r9.die = "\u2012",
                      dm_r.die = ifelse(grepl("^dejavu", style), 0.25, 0.22),
                      dm_t.die = 270,
                      dm_cex.die = 1.5,
                      fontfamily = ifelse(grepl("^dejavu", style), "DejaVu Sans", "sans"),
                      op_grob_fn.die = d12TopGrob,
                      obj_fn.die = save_d12_obj,
                      width.die =  (5 / 16) / 0.5877853, # if 5/16" vertex to vertex
                      height.die =  (5 / 16) / 0.5877853, # if 5/16" vertex to vertex
                      depth.die = 2 * 1.113516364 * (5 / 16), # inradius = 1.113516364 * a
                      background_color = "white,white,white,white,black,black",
                      shape.die = "convex5",
                      shape_t.die = 90,
                      invert_colors = TRUE)
    for (i in 10:12) {
        dice_list[[paste0("ps_cex.r", i, ".die")]] <- 1.00
        # dice_list[[paste0("ps_r.r", i, ".die")]] <- 0.02
    }
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

dice_d20 <- function(style = "sans", color_list = color_list_fn()) {
    dice_list <- list(n_suits = 6, n_ranks = 20,
                      rank_text.die = 1:20,
                      ps_cex.die = 0.65,
                      ps_r.die =  -0.02,
                      ps_t.die = 90,
                      fontfamily = ifelse(grepl("^dejavu", style), "DejaVu Sans", "sans"),
                      dm_text.die = "",
                      dm_text.r6.die = "\u2012",
                      dm_text.r9.die = "\u2012",
                      dm_r.die = ifelse(grepl("^dejavu", style), 0.12, 0.13),
                      dm_t.die = 270,
                      dm_cex.die = 1.0,
                      op_grob_fn.die = d20TopGrob,
                      obj_fn.die = save_d20_obj,
                      width.die =  0.5 / 0.8660254, # if 1/2" vertex to vertex
                      height.die =  0.5 / 0.8660254, # if 1/2" vertex to vertex
                      depth.die = 2 * 0.5 * (3 * sqrt(3) + sqrt(15)) / 12,
                      background_color = "white,white,white,white,black,black",
                      shape.die = "convex3",
                      shape_t.die = 90,
                      invert_colors = TRUE)
    for (i in 1:9) {
        dice_list[[paste0("ps_cex.r", i, ".die")]] <- 0.72
        dice_list[[paste0("ps_r.r", i, ".die")]] <- 0.02
    }
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

dice_numeral <- function(style = "sans", color_list = color_list_fn(), rect_shape = "rect") {
    dice_list <- list(n_suits = 6, n_ranks = 6,
                      fontfamily = ifelse(grepl("^dejavu", style), "DejaVu Sans", "sans"),
                      rank_text = 1:6,
                      ps_cex.die = 1.8,
                      dm_text.die = "",
                      dm_text.r6.die = "\u2012",
                      dm_r.die = 0.315,
                      dm_t.die = 270,
                      dm_cex.die = 2.4,
                      width.die = 16 / 25.4, # 16 mm dice most common
                      background_color = "white,white,white,white,black,black",
                      invert_colors = TRUE,
                      die_arrangement = "1<,2>,3>,6v,5,4",
                      shape.card = rect_shape)
    dice <- pp_cfg(c(dice_list, color_list))
    dice$has_piecepack <- FALSE
    dice$has_dice <- TRUE
    dice
}

meeples <- function(color_list = color_list_fn()) {
    meeples_list <- list(shape.bit = "meeple", n_suits = 6,
                         width.bit = 16 / 25.4, height.bit = 16 / 25.4, depth.bit = 10 / 25.4,
                         ps_text.bit = "", dm_text.bit = "",
                         invert_colors.bit = TRUE, background_color = "white")
    meeples <- pp_cfg(c(meeples_list, color_list))
    meeples$has_piecepack <- FALSE
    meeples$has_bits <- TRUE
    meeples
}

alquerque <- function(cell_width = 1, color_list = color_list_fn()) {
    alquerque <- list(n_suits = 6, n_ranks = 1,
                      width.board = 5 * cell_width,
                      height.board = 5 * cell_width,
                      grob_fn.board_face = alquerqueBoardGrobFn(),
                      gridline_color.board_face = cb_suit_colors_pure,
                      gridline_color.board_back = "transparent",
                      gridline_lex.board = 4,
                      suit_color = cb_suit_colors_impure,
                      background_color = color_list$background_color,
                      gridline_color.s6.board_face = "grey80")
    alquerque <- pp_cfg(c(alquerque, go_stone(cell_width / 2), color_list))
    alquerque$has_piecepack <- FALSE
    alquerque$has_boards <- TRUE
    alquerque$has_bits <- TRUE
    alquerque
}

morris <- function(cell_width = 1, color_list = color_list_fn()) {
    morris <- list(n_suits = 6, n_ranks = 15,
                   width.board = 7 * cell_width,
                   height.board = 7 * cell_width,
                   grob_fn.r1.board_face = morrisBoardGrobFn(12),
                   gridline_color.board_face = cb_suit_colors_pure,
                   gridline_color.board_back = "transparent",
                   gridline_lex.board = 4,
                   suit_color = cb_suit_colors_impure,
                   background_color = color_list$background_color,
                   gridline_color.s6.board_face = "grey80")
    for (i in seq(2, 15)) {
        if (i < 5) {
            morris[[paste0("width.r", i, ".board")]] <- 3 * cell_width
            morris[[paste0("height.r", i, ".board")]] <- 3 * cell_width
        } else if (i < 8) {
            morris[[paste0("width.r", i, ".board")]] <- 5 * cell_width
            morris[[paste0("height.r", i, ".board")]] <- 5 * cell_width
        } else {
            morris[[paste0("width.r", i, ".board")]] <- 7 * cell_width
            morris[[paste0("height.r", i, ".board")]] <- 7 * cell_width
        }
        morris[[paste0("grob_fn.r", i, ".board_face")]] <- morrisBoardGrobFn(i)
    }
    morris <- pp_cfg(c(morris, go_stone(cell_width), color_list))
    morris$has_piecepack <- FALSE
    morris$has_boards <- TRUE
    morris$has_bits <- TRUE
    morris
}

go_stone <- function(cell_width) {
    list(width.bit = 0.8700787 * cell_width,
         depth.bit = 0.3937008 * cell_width,
         invert_colors.bit = TRUE,
         op_grob_fn.bit = basicEllipsoid,
         obj_fn.bit = function(...) save_ellipsoid_obj(..., subdivide=4),
         ps_text.bit = "", dm_text.bit = ""
    )
}

dominoes <- function(background_color = "white", suit_color = "black", border_color = "black",
                     rect_shape = "rect", mat_width = 0) {
    border_lex <- ifelse(border_color == "black", 4, 0)
    dominoes <- pp_cfg(list(n_suits = 18 + 1, n_ranks = 18 + 1,
                            width.tile = 1,
                            height.tile = 2,
                            depth.tile = 0.25, # 3/8 professional, 1/2 jumbo
                            width.die = 16 / 25.4,
                            suit_color = suit_color, background_color = background_color,
                            mat_width = mat_width, mat_color = suit_color,
                            border_color = border_color, border_lex = border_lex,
                            die_arrangement = "opposites_sum_to_5",
                            grob_fn.die = pippedGrobFn(0, "die"),
                            grob_fn.card = cardGrobFn(-1, "card", grob_type = "circle"),
                            gridline_color.tile_back = "transparent",
                            gridline_color.tile_face = suit_color,
                            gridline_lex.tile_face = 6,
                            shape.tile = rect_shape, shape.card = rect_shape,
                            grob_fn.tile_face = dominoGrobFn(-1)
                            ))
    dominoes$has_piecepack <- FALSE
    dominoes$has_dice <- TRUE
    dominoes$has_tiles <- TRUE
    dominoes
}

checker_piece <- function(cell_width) {
    list(width.bit = 0.75 * cell_width, invert_colors.bit = TRUE,
         ps_text.bit = "", dm_text.bit = "")

}

checkers <- function(cell_width = 1, color_list = color_list_fn()) {
    checkers <- list(n_suits = 6, n_ranks = 12,
                     width.board = 8 * cell_width,
                     height.board = 8 * cell_width,
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
    checkers <- pp_cfg(c(checkers, checker_piece(cell_width), color_list))
    checkers$has_piecepack <- FALSE
    checkers$has_boards <- TRUE
    checkers$has_bits <- TRUE
    checkers
}

chess <- function(style = "sans", cell_width = 1, color_list = color_list_fn()) {
    if (grepl("^sans", style)) {
        rank_cex_die <- 1.3
        black_chess_ranks <- c("p", "n", "b", "r", "q", "k")
        white_chess_ranks <- c("P", "N", "B", "R", "Q", "K")
    } else if (grepl("^dejavu", style)) {
        rank_cex_die <- 1.5
        black_chess_ranks <- c("\u265f", "\u265e", "\u265d", "\u265c", "\u265b", "\u265a")
        white_chess_ranks <- c("\u2659", "\u2658", "\u2657", "\u2656", "\u2655", "\u2654")
    }
    chess <- list(n_suits = 6, n_ranks = 12,
                  width.board = 8 * cell_width,
                  height.board = 8 * cell_width,
                  width.bit = 0.75 * cell_width,
                  ps_text.bit_back = "", dm_text.bit = "",
                  grob_fn.r1.board_face = checkeredBoardGrobFn(8, 8),
                  grob_fn.r1.board_back = linedBoardGrobFn(8, 8),
                  gridline_color.board_face = cb_suit_colors_impure,
                  gridline_color.board_back = cb_suit_colors_pure,
                  gridline_lex.board = 4,
                  suit_text = "",
                  rank_cex.bit = 1.4 * cell_width,
                  rank_cex.die = rank_cex_die,
                  rank_text = black_chess_ranks,
                  rank_text.s6 = white_chess_ranks,
                  suit_color = cb_suit_colors_pure,
                  suit_color.s6 = "black",
                  background_color = "white",
                  edge_color.bit = color_list$edge_color.board,
                  gridline_color.s6.board_face = "grey80",
                  gridline_color.s6.board_back = "grey80")
    for (i in seq(2, 12)) {
        chess[[paste0("width.r", i, ".board")]] <- i * cell_width
        chess[[paste0("height.r", i, ".board")]] <- i * cell_width
        chess[[paste0("grob_fn.r", i, ".board_face")]] <- checkeredBoardGrobFn(i, i)
        chess[[paste0("grob_fn.r", i, ".board_back")]] <- linedBoardGrobFn(i, i)
    }
    chess <- pp_cfg(c(chess, color_list))
    chess$has_piecepack <- FALSE
    chess$has_boards <- TRUE
    chess$has_bits <- TRUE
    chess$has_dice <- TRUE
    chess
}

go <- function(cell_width = 1, color_list = color_list_fn()) {
    go <- list(n_suits = 6, n_ranks = 19,
               width.board = (18 + 1) * cell_width,
               height.board = (18 + 1) * cell_width,
               grob_fn.board_back = basicPieceGrob,
               grob_fn.r1.board_face = linedBoardGrobFn(18, 18, 0.5),
               gridline_color.board_face = cb_suit_colors_pure,
               gridline_color.board_back = "transparent",
               gridline_lex.board = 4,
               suit_color = cb_suit_colors_impure,
               background_color = color_list$background_color,
               gridline_color.s6.board_face = "grey80")
    for (i in seq(2, 18)) {
        go[[paste0("width.r", i, ".board")]] <- i * cell_width
        go[[paste0("height.r", i, ".board")]] <- i * cell_width
        go[[paste0("grob_fn.r", i, ".board_face")]] <- linedBoardGrobFn(i - 1, i - 1, 0.5)
    }
    go <- pp_cfg(c(go, go_stone(cell_width), color_list))
    go$has_piecepack <- FALSE
    go$has_boards <- TRUE
    go$has_bits <- TRUE
    go
}

piecepack <- function(style = "sans", color_list = color_list_fn(), rect_shape = "rect", pawn = "token") {
    if (grepl("^sans", style)) {
        piecepack_suits <- list(suit_text="\u263c,\u25d8,\u0238,\u03ee,\u2202")
        pce_suit_text <- "\u2665,\u2660,\u2663,\u2666,\u2202"
    } else if (grepl("^dejavu", style)) {
        piecepack_suits <- list(suit_text="\u2742,\u25d0,\u265b,\u269c,\u0ed1",
                                suit_cex.s2=0.9, dm_cex.coin=0.5, fontfamily="DejaVu Sans")
        pce_suit_text <- "\u2665,\u2660,\u2663,\u2666,\u0ed1"
    }
    is_3d <- grepl("3d$", style)
    if (is_3d) {
        style_3d <- list(suit_color.s4 = "#0072B2",
                         invert_colors.pawn = TRUE,
                         invert_colors.die = TRUE,
                         background_color.die = "white",
                         border_color="transparent",
                         mat_color.tile_back = "burlywood",
                         edge_color.tile = "black", edge_color.coin = "black",
                         border_color.pyramid="black", border_lex.pyramid=1, background_color.pyramid="white")
    } else {
        style_3d <- NULL
    }
    piecepack_base <- list(depth.coin=0.25,
                           invert_colors.matchstick = TRUE,
                           ps_cex.r2.matchstick = 0.7,
                           dm_r.r1.matchstick = 0, dm_cex.r1.matchstick = 1.5,
                           suit_color.s2.matchstick = "grey30",
                           suit_color.s2.bit = "grey30",
                           mat_color.tile_back="white", mat_width.tile_back=0.05,
                           suit_color.unsuited="black",
                           invert_colors.bit = TRUE,
                           rank_text=",a,2,3,4,5",
                           use_suit_as_ace=TRUE,
                           rank_text.pyramid=LETTERS[1:6],
                           suit_text.saucer_back="", suit_cex.saucer_face = 0.9,
                           suit_cex.pyramid_face=0.5, ps_r.pyramid_face=-0.08,
                           dm_cex.pyramid_face=4.0, dm_text.pyramid_face="|", dm_r.pyramid_face=-0.22,
                           ps_cex.pyramid_left=0.7, ps_r.pyramid_left=-0.00,
                           ps_cex.pyramid_right=0.7, ps_r.pyramid_right=-0.00,
                           use_suit_as_ace.pyramid=FALSE,
                           shape.tile = rect_shape, shape.card = rect_shape)
    shapes <- shapes_cfg(color_list)
    pawn <- switch(pawn,
                   "peg-doll" = peg_doll_pawn(shapes),
                   "joystick" = joystick_pawn(shapes),
                   NULL)
    piecepack <- c(pawn, style_3d, piecepack_suits, color_list, piecepack_base)

    playing_cards_expansion <- piecepack
    playing_cards_expansion$suit_text <- pce_suit_text
    playing_cards_expansion$suit_color <- "#D55E00,#000000,#000000,#D55E00,#000000"
    playing_cards_expansion$suit_color.s3.matchstick <- "grey30"
    playing_cards_expansion$suit_color.s3.bit <- "grey30"


    hexpack <- c(piecepack, list(shape.tile="convex6", border_lex=3,
                                 shape_t.tile="60",  dm_t.tile_face=-90,
                                 width.tile=4/sqrt(3), height.tile=4/sqrt(3),
                                 shape.coin="convex3"))

    dpe_base <- list(invert_colors.suited=TRUE,
                     border_color.s2.die="black", border_color.s2.pawn="black",
                     suit_color.s2.board_face = "black")

    dual_piecepacks_expansion <- c(piecepack, dpe_base)
    dual_piecepacks_expansion$suit_text <- pce_suit_text

    pi_base <- list(invert_colors = TRUE,
                    invert_colors.matchstick = FALSE,
                    suit_color.s2.matchstick = "black",
                    suit_color.s2.board_face = "black",
                    suit_color.card_back = "grey30",
                    suit_color.coin_face = "grey30",
                    edge_color.coin = "white",
                    edge_color.tile = "white")
    piecepack_inverted <- c(pi_base, piecepack)

    piecepack <- c(list(suit_color = cb_suit_colors_pure),
                   piecepack)

    list(base = pp_cfg(piecepack),
         dpe = pp_cfg(dual_piecepacks_expansion),
         hexpack = to_hexpack(piecepack),
         inverted = pp_cfg(piecepack_inverted),
         pce = pp_cfg(playing_cards_expansion),
         subpack = to_subpack(piecepack))
}

playing_cards <- function(style = "sans", rect_shape = "rect") {
    if (grepl("^sans", style)) {
        face_labels <- c("", "\u050a", "\u046a", "\u0238")
        fool_text <- "*"
        pc_suit_text <- list(suit_text="\u2665,\u2660,\u2663,\u2666,*",
                             suit_cex.s5=1.3)
    } else if (grepl("^dejavu", style)) {
        face_labels <- c("", "\u265e", "\u265b", "\u265a")
        fool_text <- "\u2605"
        pc_suit_text <- list(suit_text="\u2665,\u2660,\u2663,\u2666,\u2605")
    }

    playing_cards_list <- list(n_ranks = 14,
                               rank_text = "A,2,3,4,5,6,7,8,9,10,J,Q,K,\n\n\n\nJ\nO\nK\nE\nR",
                               grob_fn.card = cardGrobFn(),
                               grob_fn.r11.card = faceCardGrobFn(face_labels[1]),
                               grob_fn.r12.card = faceCardGrobFn(face_labels[3]),
                               grob_fn.r13.card = faceCardGrobFn(face_labels[4]),
                               grob_fn.r14.card = jokerCardGrobFn(TRUE),
                               shape.card = rect_shape,
                               border_color = "black", border_lex = 4)
    playing_cards_list$n_suits <- 4
    playing_cards_list$suit_color <- "#D55E00,#000000,#000000,#D55E00,#E59F00"

    playing_cards <- c(playing_cards_list, pc_suit_text)
    playing_cards$grob_fn.s3.r14.card <- jokerCardGrobFn(FALSE)
    playing_cards$grob_fn.s4.r14.card <- jokerCardGrobFn(FALSE)
    playing_cards <- pp_cfg(playing_cards)
    playing_cards$has_piecepack <- FALSE
    playing_cards$has_cards <- TRUE

    playing_cards_colored <- c(playing_cards_list, pc_suit_text)
    playing_cards_colored$n_suits <- 5
    playing_cards_colored$suit_color <- cb_suit_colors_pure

    playing_cards_colored <- pp_cfg(playing_cards_colored)
    playing_cards_colored$has_piecepack <- FALSE
    playing_cards_colored$has_cards <- TRUE

    playing_cards_tarot <- playing_cards_list
    playing_cards_tarot$rank_text <- "A,2,3,4,5,6,7,8,9,10,J,C,Q,K,\n\n\n\nJ\nO\nK\nE\nR"
    playing_cards_tarot$grob_fn.r12.card <- faceCardGrobFn(face_labels[2], "low")
    playing_cards_tarot$grob_fn.r13.card <- faceCardGrobFn(face_labels[3])
    playing_cards_tarot$grob_fn.r14.card <- faceCardGrobFn(face_labels[4])
    playing_cards_tarot$grob_fn.r15.card <- jokerCardGrobFn(TRUE)
    playing_cards_tarot$grob_fn.s3.r15.card <- jokerCardGrobFn(FALSE)
    playing_cards_tarot$grob_fn.s4.r15.card <- jokerCardGrobFn(FALSE)
    playing_cards_tarot$rank_text.s5 <- c(1:21, fool_text)
    playing_cards_tarot$n_suits <- 5
    playing_cards_tarot$n_ranks <- 22

    tarot_suit_text <- "\u2665,\u2660,\u2663,\u2666,"
    playing_cards_tarot$suit_text <- tarot_suit_text
    playing_cards_tarot$suit_text.r14 <- tarot_suit_text
    playing_cards_tarot$suit_color <- "#D55E00,#000000,#000000,#D55E00,#000000"
    playing_cards_tarot$grob_fn.s5.card <- cardGrobFn(0)
    playing_cards_tarot <- pp_cfg(playing_cards_tarot)
    playing_cards_tarot$has_piecepack <- FALSE
    playing_cards_tarot$has_cards <- TRUE

    list(base = playing_cards, color = playing_cards_colored, tarot = playing_cards_tarot)
}

reversi <- function(cell_width = 1, color_list = color_list_fn()) {
    reversi <- list(n_suits = 6, n_ranks = 12,
                     width.board = 8 * cell_width,
                     height.board = 8 * cell_width,
                     grob_fn.r1.board_face = linedBoardGrobFn(8, 8),
                     grob_fn.r1.board_back = checkeredBoardGrobFn(8, 8),
                     background_color.board_face = cb_suit_colors_impure,
                     gridline_color.board_face = "black",
                     gridline_lex.board = 4,
                     suit_color = cb_suit_colors_impure,
                     background_color = "white",
                     gridline_color.s6.board_back = "grey80")
    for (i in seq(2, 12)) {
        reversi[[paste0("width.r", i, ".board")]] <- i * cell_width
        reversi[[paste0("height.r", i, ".board")]] <- i * cell_width
        reversi[[paste0("grob_fn.r", i, ".board_face")]] <- linedBoardGrobFn(i, i)
        reversi[[paste0("grob_fn.r", i, ".board_back")]] <- checkeredBoardGrobFn(i, i)
    }
    reversi <- pp_cfg(c(reversi, reversi_piece(cell_width, color_list), color_list))
    reversi$has_piecepack <- FALSE
    reversi$has_boards <- TRUE
    reversi$has_bits <- TRUE
    reversi
}

shapes_cfg <- function(color_list = color_list_fn()) {
    shapes <- list(n_suits = 6, n_ranks = 4,
                   invert_colors = TRUE,
                   ps_text = "", dm_text = "",
                   background_color = "white",
                   width = 1, height = 1, depth = 1,
                   shape.r1.bit = "circle",
                   shape.r2.bit = "circle",
                   op_grob_fn.r2.bit = basicEllipsoid,
                   obj_fn.r2.bit = function(...) save_ellipsoid_obj(..., subdivide=4),
                   shape.r3.bit = "pyramid",
                   shape.r4.bit = "rect")
    pp_cfg(c(shapes, color_list))
}

reversi_piece <- function(cell_width = 1, color_list = color_list_fn()) {

    shapes_top <- shapes_cfg(color_list)
    color_list$suit_color <- color_list$suit_color[c(3, 6, 1, 5, 4, 2)]
    shapes_bot <- shapes_cfg(color_list)
    envir <- list(shapes_top = shapes_top, shapes_bot = shapes_bot)

    df_reversi <- tibble(piece_side = "bit_back", rank = 1,
                         width = 1, height = 1, depth = c(0.5, 0.5),
                         x = 0.5, y = 0.5, z = c(0.25, 0.75),
                         cfg = c("shapes_bot", "shapes_top"))

    reversi_piece <- CompositePiece$new(df_reversi, envir=envir, ref_side="face")

    list(width.bit= 0.75 * cell_width,
         height.bit = 0.70 * cell_width,
         depth.bit = 0.25 * cell_width,
         grob_fn.bit = reversi_piece$grob_fn,
         obj_fn.bit = reversi_piece$obj_fn,
         op_grob_fn.bit = reversi_piece$op_grob_fn)
}

peg_doll_pawn <- function(shapes) {

    pegdoll_depth <- c(0.55, 1.0 * 0.75 / 1.5)
    df_pegdoll <- tibble(piece_side = "bit_back", rank = c(1, 2),
                          width =  1, height = 1, depth = pegdoll_depth,
                          x = 0.5, y = 0.5, z = c(0.5 * pegdoll_depth[1], 1 - 0.5 * pegdoll_depth[2]),
                          cfg = "shapes")

    pegdoll <- CompositePiece$new(df_pegdoll, envir=list(shapes=shapes))

    list(width.pawn=0.75, depth.pawn=0.75, height.pawn=1.5,
         edge_color.pawn=cb_suit_colors_pure,
         edge_color.s4.pawn="#0072B2",
         background_color.belt_face="white",
         mat_color.belt_face="transparent",
         suit_cex.belt_face=1.5,
         obj_fn.pawn=save_peg_doll_obj,
         grob_fn.pawn = pegdoll$grob_fn,
         op_grob_fn.pawn = pegdoll$op_grob_fn)
}

joystick_pawn <- function(shapes) {

    jd <- c(0.15, 0.6, 0.5 * 5 / 8)
    df_joystick <- tibble(piece_side = "bit_back", rank = c(1, 1, 2),
                          width =  c(1, 0.3, 0.5),
                          height = c(1, 0.3, 0.5),
                          depth = jd,
                          x = rep(0.5, 3), y = rep(0.5, 3),
                          z = c(0.5 * jd[1], jd[1] + 0.5 * jd[2], 1 - 0.5 * jd[3]),
                          cfg = "shapes")

    joystick <- CompositePiece$new(df_joystick, envir=list(shapes=shapes))

    list(grob_fn.pawn = joystick$grob_fn,
         obj_fn.pawn = joystick$obj_fn,
         op_grob_fn.pawn = joystick$op_grob_fn,
         width.pawn=5/8, height.pawn=1.0, depth.pawn=5/8)
}

#' @rdname game_systems
#' @inheritParams pp_cfg
#' @export
to_hexpack <- function(cfg = getOption("piecepackr.cfg", pp_cfg())) {
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
#' @inheritParams pp_cfg
#' @export
to_subpack <- function(cfg = getOption("piecepackr.cfg", pp_cfg())) {
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
