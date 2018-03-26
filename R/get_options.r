split <- function(x, sep=",") { strsplit(x, sep)[[1]] } 

configuration_options <- function(args=commandArgs(TRUE)) {
    default_str <- "(default %default)"
    parser <- OptionParser("Program to make piecepack images")
    parser <- add_option(parser, "--deck_title", default=NULL, 
                         help='(default "Deck title")')
    parser <- add_option(parser, "--deck_filename", default=NULL, 
                         help='(default "deck_filename")')

    # Symbols
    parser <- add_option(parser, "--suit_symbols", default=NULL, 
                         help='(default "♠,♥,♣,♦,★")')
    parser <- add_option(parser, "--rank_symbols", default=NULL, 
                         help='(default "N,A,2,3,4,5")')

    # Style
    parser <- add_option(parser, "--directional_marker", default=NULL, 
                         help='(default "neutral")')
    parser <- add_option(parser, "--use_suit_as_ace", action="store_true", default=NULL, 
                         help='(default "FALSE")')
    parser <- add_option(parser, "--use_ace_as_ace", dest="use_suit_as_ace", 
                         action="store_false", default=NULL, help='Opposite of use_suit_as_ace (default "TRUE")')
    parser <- add_option(parser, "--style", default=NULL, 
                         help='"basic" or "simple_hex" (default "basic")')

    # Font
    parser <- add_option(parser, "--family", default="sans",
                         help='Default font family (default "sans")')

    # Color scheme
    parser <- add_option(parser, "--suit_colors", default=NULL, 
                         help='(default "darkred,black,darkgreen,darkblue,grey")')
    parser <- add_option(parser, "--background_color", default=NULL, 
                         help='(default "white")')
    parser <- add_option(parser, "--invert_colors", action="store_true", default=FALSE, 
                         help='(default "FALSE")')
    parser <- add_option(parser, "--uninvert_colors", action="store_false", dest="invert_colors",
                         default=FALSE, help='Opposite of invert_colors (default "FALSE")')
    parser <- add_option(parser, c("-f", "--file"), default=NULL, 
                         help="Filename to write configuration to (default outputs to standard output)")

    # Suited/unsuited variants
    parser <- add_option(parser, "--background_color.suited", default=NULL, 
                         help='(default is value of "background_color" option)')
    parser <- add_option(parser, "--background_color.unsuited", default=NULL, 
                         help='(default is value of "background_color" option)')

    parser <- add_option(parser, "--invert_colors.suited", action="store_true", default=NULL, 
                         help='(default is value of "invert_colors" option)')
    parser <- add_option(parser, "--invert_colors.unsuited", action="store_true", default=NULL, 
                         help='(default is value of "invert_colors" option)')
    parser <- add_option(parser, "--uninvert_colors.suited", action="store_false", default=NULL, 
                         dest="invert_colors.suited", help='(default is value of "invert_colors" option)')
    parser <- add_option(parser, "--uninvert_colors.unsuited", action="store_false", default=NULL, 
                         dest="invert_colors.unsuited", help='(default is value of "invert_colors" option)')

    # Components variants
    for (component in COMPONENTS) {
        # Symbols
        opt_str <- paste0("--rank_symbols.", component)
        parser <- add_option(parser, opt_str, default=NULL, 
                             help='(default is value of "rank_symbols" option)')
        opt_str <- paste0("--suit_symbols.", component)
        parser <- add_option(parser, opt_str, default=NULL, 
                             help='(default is value of "rank_symbols" option)')

        # Style
        opt_str <- paste0("--use_suit_as_ace.", component)
        parser <- add_option(parser, opt_str, default=NULL, action="store_true",
                             help='(default is value of "use_suit_as_ace" option)')
        opt_str <- paste0("--use_ace_as_ace.", component)
        dest_str <- paste0("use_suit_as_ace.", component)
        parser <- add_option(parser, opt_str, dest=dest_str, 
                             action="store_false", default=NULL, 
                             help=paste('Opposite of', opt_str))

        # Color
        opt_str <- paste0("--background_color.", component)
        suited <- is_suited(component)
        parser <- add_option(parser, opt_str, default=NULL, help=paste('default is value of',
                                   ifelse(suited, "background_color.suited", "background_color.unsuited")))
        opt_str <- paste0("--invert_colors.", component)
        suited <- is_suited(component)
        parser <- add_option(parser, opt_str, default=NULL, action="store_true", 
                             help=paste('default is value of', 
                                   ifelse(suited, "invert_colors.suited", "invert_colors.unsuited")))
        opt_str <- paste0("--uninvert_colors.", component)
        suited <- is_suited(component)
        parser <- add_option(parser, opt_str, default=NULL, action="store_false", 
                             help=paste('default is value of', 
                                   ifelse(suited, "invert_colors.suited", "invert_colors.unsuited")))
    }

    opts <- parse_args(parser, args)
    opts
}


#' @export
make_style <- function(args=commandArgs(TRUE)) {
    opts <- configuration_options(args)
    filename <- opts$file
    opts$file <- NULL
    opts$help <- NULL

    if (is.null(filename)) {
        writeLines(jsonlite::toJSON(opts, pretty=TRUE), stdout())
    } else {
        writeLines(jsonlite::toJSON(opts, pretty=TRUE), filename)
    }
}

#' @export
get_arrangement_opts <- function(args=commandArgs(TRUE)) {
    parser <- OptionParser("Program to arrange piecepacks")
    parser <- add_option(parser, "--collection_filename", default=NULL,
                         help='(default "collection_filename")')
    parser <- add_option(parser, "--collection_title", default=NULL,
                         help='(default "collection_title")')
    parser <- add_option(parser, "--decks", help='(all of them)')

    opts <- parse_args(parser, args)

    if (is.null(opts$set_filename))
        opts$set_filename <- "collection_filename"
    if (is.null(opts$set_title))
        opts$set_title <- "collection_title"
    if (is.null(opts$decks))
        opts$decks <- gsub(".pdf$", "", list.files("pdf"))
    else
        opts$decks <- split(opts$decks)

    opts <- add_copyright(opts)
    opts
}

add_copyright <- function(opts) {
    opts$program <- "Generated by the Configurable Piecepack PDF Maker."
    opts$copyright <- "© 2016-2018 Trevor L Davis. Some Rights Reserved."
    opts$license1 <- "This work is licensed under a CC BY-SA 4.0 license:"
    opts$license2 <- "https://creativecommons.org/licenses/by-sa/4.0/"

    opts
}

process_configuration <- function(opts) {
    opts$parallel <- FALSE
    opts$fast <- TRUE
    opts$add_checkers <- FALSE
    opts$add_bleed_lines <- FALSE

    if (is.null(opts$deck_title))
        opts$deck_title <- "deck_title"
    if (is.null(opts$deck_filename))
        opts$deck_filename <- "deck_filename"

    if (is.null(opts[["suit_symbols"]]))
        opts$suit_symbols <- "♥,♠,♣,♦,★"
    if (is.null(opts[["rank_symbols"]]))
        opts$rank_symbols <- "N,A,2,3,4,5"
    if (is.null(opts[["directional_marker"]]))
        opts$directional_marker <- "neutral"
    if (is.null(opts[["use_suit_as_ace"]]))
        opts$use_suit_as_ace <- FALSE
    if (is.null(opts[["style"]]))
        opts$style <- "basic"
    if (is.null(opts[["suit_colors"]]))
        opts$suit_colors <- "darkred,black,darkgreen,darkblue,grey"
    if (is.null(opts[["background_color"]]))
        opts$background_color <- "white"
    if (is.null(opts[["invert_colors"]]))
        opts$invert_colors <- FALSE

    opts <- add_copyright(opts)

    opts$suit_colors <- split(opts$suit_colors)

    opts$rank_symbols <- split(opts$rank_symbols)
    opts$suit_symbols <- split(opts$suit_symbols)
    for(component in COMPONENTS) {
        rank_symbol_str <- paste0("rank_symbols.", component)
        if (is.null(opts[[rank_symbol_str]]))
            opts[[rank_symbol_str]] <- opts$rank_symbols
        else
            opts[[rank_symbol_str]] <- split(opts[[rank_symbol_str]])

        suit_symbols_str <- paste0("suit_symbols.", component)
        if (is.null(opts[[suit_symbols_str]]))
            opts[[suit_symbols_str]] <- opts$suit_symbols
        else
            opts[[suit_symbols_str]] <- split(opts[[suit_symbols_str]])
    }

    opts$n_suits <- length(opts$suit_symbols) - 1
    opts$i_unsuit <- opts$n_suits + 1

    opts$directory <- file.path("svg", opts$deck_filename)

    opts
}

#' @export
read_style <- function(args=commandArgs(TRUE)) {
    parser <- OptionParser()
    parser <- add_option(parser, c('-f', '--file'), default=NULL, help="Source of piecepack style information (default stdin input)")
    opts <- parse_args(parser, args)

    if (is.null(opts$file)) 
        con <- file("stdin")
    else 
        con <- file(opts$file)
    
    opts <- jsonlite::fromJSON(readLines(con))
    close(con)

    opts <- process_configuration(opts)
    opts
}
