split <- function(x, sep=",") { strsplit(x, sep)[[1]] } 

#' @export
get_options <- function(args=commandArgs(TRUE)) {
    parser <- OptionParser("Program to make piecepack images")
    parser <- add_option(parser, "--set_label", default="collection")
    parser <- add_option(parser, "--set_name", default="set")
    parser <- add_option(parser, "--deck_label", default="deck")
    parser <- add_option(parser, "--suit_colors", default="darkred,black,darkgreen,darkblue,grey")
    # parser <- add_option(parser, "--background", default="seashell3")
    parser <- add_option(parser, "--background", default="white")
    parser <- add_option(parser, "--directional_marker_style", default="neutral", help="'neutral' use the neutral color, 'matching' use suit color on suit sides and neutral on neutral sides, 'none' suppress direction mark (warning: can result in a non-conforming piecepack)")
    parser <- add_option(parser, "--suit_symbols", default="♠,♥,♣,♦,★")
    parser <- add_option(parser, "--rank_symbols", default="NA2345")
    parser <- add_option(parser, "--chip_rank_symbols", default=NULL, help="Rank symbols for 'chips', defaults to what ``rank_symbols`` is set to. 'A,B,C,D,E,F' is good if building piecepack pyramids.")
    parser <- add_option(parser, "--style", default="basic", help="'basic' or 'simple_hex'")
    parser <- add_option(parser, "--use_suit_as_ace", action="store_true", default=FALSE)
    parser <- add_option(parser, "--inverted", action="store_true", default=FALSE)
    opts <- parse_args(parser, args)

    opts$parallel <- FALSE
    opts$fast <- TRUE
    opts$add_checkers <- FALSE
    opts$add_bleed_lines <- FALSE
    # opts$background <- "seashell3"
    # opts$background <- "tan"
    # opts$background <- "white"
    opts$rank_names <- c("n", "a", "2", "3", "4", "5")

    opts$copyright <- "© 2016-2018 Trevor L Davis. Some Rights Reserved."
    opts$license1 <- "This work is licensed under a CC BY-SA 4.0 license:"
    opts$license2 <- "https://creativecommons.org/licenses/by-sa/4.0/"
    opts$suit_names <- c("s1", "s2", "s3", "s4")

    opts$rank_symbols <- split(opts$rank_symbols)
    opts$suit_colors <- split(opts$suit_colors)
    opts$suit_symbols <- split(opts$suit_symbols)
    if (is.null(opts$chip_rank_symbols)) 
        opts$chip_rank_symbols <- opts$rank_symbols
    else
        opts$chip_rank_symbols <- split(opts$chip_rank_symbols)

    # opts$suit_colors <- c("dimgrey", "hotpink2", "darkolivegreen3", "lightblue2")
    # opts$suit_colors <- c("white", "orange2", "yellow", "purple")
    # opts$suit_colors <- rep(c("black", "darkred"), 2)
    # opts$suit_colors <- rep(c("dimgrey", "hotpink2"), 2)
    # opts$suit_colors <- rep(c("white", "orange2"), 2)
    # opts$suit_colors <- rep(c("black"), 4)
    # opts$suit_colors <- rep(c("dimgrey"), 4)
    # opts$suit_colors <- rep(c("white"), 4)
    # opts$suit_colors <- c("black", "darkred", "darkgreen", "darkblue")
    # opts$suit_colors <- c("dimgrey", "hotpink2", "darkolivegreen3", "lightblue2")
    # opts$suit_colors <- c("white", "orange2", "yellow", "purple")
    # opts$suit_colors <- rep(c("black", "darkred"), 2)
    # opts$suit_colors <- rep(c("dimgrey", "hotpink2"), 2)
    # opts$suit_colors <- rep(c("white", "orange2"), 2)
    # opts$suit_colors <- rep(c("black"), 4)
    # opts$suit_colors <- rep(c("dimgrey"), 4)
    # opts$suit_colors <- rep(c("white"), 4)

    opts
}
