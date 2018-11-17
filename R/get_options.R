FORMATS <- c("bmp", "jpeg", "pdf", "png", "ps", "svg", "tiff")

#' @importFrom optparse OptionParser add_option parse_args
configuration_options <- function(parser, pp_n_suits=NULL, pp_n_ranks=NULL) {

    # Symbols
    parser <- add_option(parser, "--rank_symbols", default=NULL, 
                         help=sprintf('(default "%s")', paste(get_rank_symbols(expand=FALSE), collapse=",")))
    parser <- add_option(parser, "--suit_symbols", default=NULL, 
                         help=sprintf('(default "%s")', paste(get_suit_symbols(expand=FALSE), collapse=","))) 
    parser <- add_option(parser, "--dm_symbols", default=NULL, 
                         help='(default is to try to pick a reasonable directional mark symbol based on the component)')
    parser <- add_option(parser, "--header_font", default=NULL, 
                         help='(default is to use the value of "font" option)')
    parser <- add_option(parser, "--rank_symbols_font", default=NULL, 
                         help='(default is to use the value of "font" option)')
    parser <- add_option(parser, "--suit_symbols_font", default=NULL, 
                         help='(default is to use the value of "font" option)')
    parser <- add_option(parser, "--dm_symbols_font", default=NULL, 
                         help='(default is to use the value of "font" option)')
    parser <- add_option(parser, "--rank_symbols_scale", default=NULL, 
                         help='(default is not to adjust scale)')
    parser <- add_option(parser, "--suit_symbols_scale", default=NULL, 
                         help='(default is not to adjust scale)')
    parser <- add_option(parser, "--dm_symbols_scale", default=NULL, 
                         help='(default is not to adjust scale)')

    # Style
    parser <- add_option(parser, "--shape", default=NULL,
                         help=paste('either "rect", "circle", "halma", "kite", "star",',
                                   "or a number representing number of sides of a regular polygon",
                                   "(default is to choose reasonable shape based on component)'"))
    parser <- add_option(parser, "--shape_theta", default=NULL, type="double",
                         help=paste('If shape is a number or "star" then angle of first vertex',
                                   '(in degrees) of polygon', '(default is "90")'))

    parser <- add_option(parser, "--dm_theta", default=NULL, type="double", 
                         help=paste('Angle (in degrees) of polar coordinates of direction mark',
                                   '(default 135 for tile faces and die faces and 90 for everything else)'))
    parser <- add_option(parser, "--dm_r", default=NULL, type="double", 
                         help=paste('Radius from center (relative units)',
                                  'of polar coordinates of direction mark',
                                  '(default "sqrt(.25^2+.25^2)" if its shape is "rect" or "circle" otherwise "0.3")'))
    parser <- add_option(parser, "--ps_theta", default=NULL, type="double", 
                         help=paste('Angle (in degrees) of polar coordinates of primary symbol',
                                   '(default 135 for tile faces and die faces and 90 for everything else)'))
    parser <- add_option(parser, "--ps_r", default=NULL, type="double", 
                         help=paste('Radius from center (relative units)',
                                  'of polar coordinates of primary symbol',
                                  '(default "sqrt(.25^2+.25^2)" if its shape is "rect" or "circle" otherwise "0.3")'))

    # Font
    parser <- add_option(parser, "--font", default=NULL,
                         help=sprintf('Default font family (default "%s")', get_font()))

    # Color scheme
    parser <- add_option(parser, "--suit_colors", default=NULL, 
                         help=sprintf('(default "%s")', paste(get_suit_colors(expand=FALSE), collapse=",")))
    parser <- add_option(parser, "--dm_colors", default=NULL, 
                         help='(default the value of the "suit_colors" option)')
    parser <- add_option(parser, "--background_colors", default=NULL, 
                         help='(default "white")') 
    parser <- add_option(parser, "--border_colors", default=NULL, 
                         help='(default "grey")')
    parser <- add_option(parser, "--checker_colors", default=NULL,
                         help='(default is not to draw any checkers on tiles)')
    parser <- add_option(parser, "--gridline_colors", default=NULL,
                         help='(default is to draw gridlines on tile back using the "unsuit" suit color)')
    parser <- add_option(parser, "--hexline_colors", default=NULL,
                         help='(default is not to draw any hexlines on tiles)')
    parser <- add_option(parser, "--ribbon_colors", default=NULL,
                         help='(default is to draw ribbons on pawn belts)')

    # Booleans
    parser <- add_option(parser, "--invert_colors", action="store_true", default=NULL, 
                         help='(default "FALSE")')
    parser <- add_option(parser, "--uninvert_colors", action="store_false", dest="invert_colors",
                         default=NULL, help='Opposite of "invert_colors" option')
    parser <- add_option(parser, "--use_suit_as_ace", action="store_true", default=NULL, 
                         help=sprintf('(default "%s")', get_use_suit_as_ace()))
    parser <- add_option(parser, "--use_ace_as_ace", dest="use_suit_as_ace", 
                         action="store_false", default=NULL, 
                         help='Opposite of "use_suit_as_ace" option')

    # Misc
    parser <- add_option(parser, "--n_ranks", default=NULL, type="double", 
                         help='(default is inferred from "rank_symbols" option)')
    parser <- add_option(parser, "--n_suits", default=NULL, type="double", 
                         help='(default is inferred from "suit_symbols" option)')
    parser <- add_option(parser, "--pp_die_arrangement", 
                         help=sprintf('Either "counter" or "opposites_sum_to_5" (default "%s")', get_pp_die_arrangement()))


    # Cascading variants
    if (is.null(pp_n_ranks)) {
        pp_n_ranks <- as.numeric(Sys.getenv("PP_N_RANKS"))
        pp_n_ranks <- ifelse(is.na(pp_n_ranks), 0, pp_n_ranks)
    }
    if (is.null(pp_n_suits)) {
        pp_n_suits <- as.numeric(Sys.getenv("PP_N_SUITS"))
        pp_n_suits <- ifelse(is.na(pp_n_suits), 0, pp_n_suits)
    }
    if (as.logical(pp_n_ranks)) 
        rank_affixes <- c(paste0(".r", 1:pp_n_ranks), "")
    else
        rank_affixes <- ""
    if (as.logical(pp_n_suits))
        suit_affixes <- c(paste0(".s", 1:pp_n_suits), ".suited", ".unsuited", "")
    else
        suit_affixes <- c(".suited", ".unsuited", "")
    component_affixes <- c(paste0(".", c(COMPONENT_AND_SIDES, COMPONENTS)), "")
    dfs <- expand.grid(component_affixes, rank_affixes, suit_affixes, stringsAsFactors=FALSE)
    names(dfs) <- c("component", "rank", "suit")
    affixes <- paste0(dfs$suit, dfs$rank, dfs$component)
    styles <- c("rank_symbols", "rank_symbols_font", "rank_symbols_scale",
                "suit_symbols", "suit_symbols_font", "suit_symbols_scale",
                "dm_symbols", "dm_symbols_font", "dm_symbols_scale",
                "dm_theta", "dm_r", "ps_theta", "ps_r",
                "dm_colors", "suit_colors", "background_colors",
                "hexline_colors", "checker_colors", "gridline_colors", "ribbon_colors",
                "border_colors", 
                "shape", "shape_theta")
    for (affix in affixes) {
        if (affix == "") next

        # Non-booleans
        for (style in styles) {
            opt_str <- paste0("--", style, affix)
            parser <- add_option(parser, opt_str, default=NULL)
        }

        # Booleans
        opt_str <- paste0("--use_suit_as_ace", affix)
        parser <- add_option(parser, opt_str, default=NULL, action="store_true")
        opt_str <- paste0("--use_ace_as_ace", affix)
        dest_str <- paste0("use_suit_as_ace", affix)
        parser <- add_option(parser, opt_str, dest=dest_str, action="store_false", default=NULL)
        opt_str <- paste0("--invert_colors", affix)
        parser <- add_option(parser, opt_str, default=NULL, action="store_true")
        opt_str <- paste0("--uninvert_colors", affix)
        dest_str <- paste0("invert_colors", affix)
        parser <- add_option(parser, opt_str, dest=dest_str, default=NULL, action="store_false")
    }
    parser
}


#' Read piecepack configuration JSON
#'
#' Read in and process piecepack configuration JSON.
#' 
#' @param cfg_file Filename of json configuration.  Default is to read in from standard input.
#' @export
read_configuration <- function(cfg_file="stdin") {
    con <- file(cfg_file)
    opts <- jsonlite::fromJSON(readLines(con, encoding="UTF-8"))
    close(con)
    opts
}

swirl <- "\uaa5c" # ꩜
star <- "\u2605" # ★
french_suits <- "\u2665,\u2660,\u2663,\u2666," # ♥,♠,♣,♦,
french_suits_white <- "\u2661,\u2664,\u2667,\u2662," # ♡,♤,♧,♢,
chess_ranks <- "\u265f,\u265e,\u265d,\u265c,\u265b,\u265a" # ♟,♞,♝,♜,♛,♚
# chess_ranks = " --rank_symbols=P,N,B,R,Q,K --rank_symbols_font='Chess Utrecht' --rank_symbols_scale=1.3"
# chess_ranks = " --rank_symbols=♘,♙,♗,♖,♕,♔"
ca_suits = "\u2665,\u2660,\u2663,\u2666,\u265a,\u2693," # ♥,♦,♣,♠,♚,⚓,
swiss_suits = "\ue005,\ue004,\ue000,\ue003,\ue00a" # ,,,, 
# swiss_suits = ' --suit_symbols=🌹,⛊,🌰,🔔,★ --suit_symbols_scale=0.7,1.0,0.8,0.8,1'

chess_ranks = "\u265f,\u265e,\u265d,\u265c,\u265b,\u265a" # ♟,♞,♝,♜,♛,♚

CFG <- list(swirl_s5_noto = list(suit_symbols.s5 = swirl,
                                suit_symbols_font.s5 = "Noto Sans Cham",
                                suit_symbols_scale.s5 = 0.9),
            swirl_s5 = list(suit_symbols.s5 = swirl), 
            chess_ranks = list(rank_symbols = chess_ranks),
            ca_suits_noto = list(suit_symbols = ca_suits, 
                                suit_symbols.s7 = star,
                               suit_symbols_scale=1,1,1,1,1,0.8,1,
                               suit_symbols_font="Noto Sans Symbols"),
            french_suits = list(suit_symbols = french_suits),
            french_suits_noto = list(suit_symbols = french_suits, suit_symbols_font="Noto Sans Symbols"),
            french_suits_white = list(suit_symbols = french_suits_white), 
            french_suits_white_noto = list(suit_symbols = french_suits_white, suit_symbols_font="Noto Sans Symbols"),
            swiss_suits_quivira = list(suit_symbols = swiss_suits, suit_symbols_font="Quivira")) 

#' Load piecepack configurations
#'
#' Load in piecepack configurations
#'
#' @param cfg Configuration list to augment
#' @param cfg_files Filenames of json configurations.
#' @param internal_cfgs Strings of internal piecepackr configurations.
#' @export
load_configurations <- function(cfg=list(), cfg_files=character(0), internal_cfgs=character(0)) {
    for (cfg_file in cfg_files) {
        cfg <- c(cfg, read_configuration(cfg_file))
    }
    for (internal_cfg in internal_cfgs) {
        cfg <- c(cfg, CFG[[internal_cfg]])
    }
    cfg
}
