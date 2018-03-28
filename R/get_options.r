split <- function(x, sep=",") { stringr::str_split(x, sep)[[1]] } 

configuration_options <- function(args=commandArgs(TRUE)) {
    default_str <- "(default %default)"
    parser <- OptionParser("Program to make piecepack configurations for exec/make_piecepack")
    parser <- add_option(parser, "--deck_title", default=NULL, 
                         help='(default "Deck title")')
    parser <- add_option(parser, "--deck_filename", default=NULL, 
                         help='(default "deck_filename")')

    # Symbols
    parser <- add_option(parser, "--rank_symbols", default=NULL, 
                         help='(default "N,A,2,3,4,5")')
    parser <- add_option(parser, "--suit_symbols", default=NULL, 
                         help='(default "♠,♥,♣,♦,★")')
    parser <- add_option(parser, "--directional_mark_symbols", default=NULL, 
                         help='(default the value of the "suit_symbols" option)')
    parser <- add_option(parser, "--rank_symbols_font", default=NULL, 
                         help='(default is to use the value of "font" option)')
    parser <- add_option(parser, "--suit_symbols_font", default=NULL, 
                         help='(default is to use the value of "font" option)')
    parser <- add_option(parser, "--directional_mark_symbols_font", default=NULL, 
                         help='(default is to use the value of "font" option)')
    parser <- add_option(parser, "--rank_symbols_scale", default=NULL, 
                         help='(default is not to adjust scale)')
    parser <- add_option(parser, "--suit_symbols_scale", default=NULL, 
                         help='(default is not to adjust scale)')
    parser <- add_option(parser, "--directional_mark_symbols_scale", default=NULL, 
                         help='(default is not to adjust scale)')

    # Style
    parser <- add_option(parser, "--use_suit_as_ace", action="store_true", default=NULL, 
                         help='(default "FALSE")')
    parser <- add_option(parser, "--use_ace_as_ace", dest="use_suit_as_ace", 
                         action="store_false", default=NULL, help='Opposite of use_suit_as_ace (default "TRUE")')
    # parser <- add_option(parser, "--style", default=NULL, 
    #                      help='"basic" or "simple_hex" (default "basic")')
    parser <- add_option(parser, "--hexline_colors", default=NULL,
                         help='(default is not to draw any hexlines)')
    parser <- add_option(parser, "--directional_mark_theta", default=NULL, type="double", 
                         help='(default 135 for tile faces and die faces and 90 for everything else)')

    # Font
    parser <- add_option(parser, "--font", default=NULL,
                         help='Default font family (default "sans")')

    # Color scheme
    parser <- add_option(parser, "--suit_colors", default=NULL, 
                         help='(default "darkred,black,darkgreen,darkblue,grey")')
    parser <- add_option(parser, "--directional_mark_colors", default=NULL, 
                         help='(default the value of the "suit_colors" option)')
    parser <- add_option(parser, "--background_color", default=NULL, 
                         help='(default "white")')
    parser <- add_option(parser, "--invert_colors", action="store_true", default=NULL, 
                         help='(default "FALSE")')
    parser <- add_option(parser, "--uninvert_colors", action="store_false", dest="invert_colors",
                         default=NULL, help='Opposite of invert_colors (default "FALSE")')
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
        for (style in c("rank_symbols", "rank_symbols_font", "rank_symbols_scale",
                        "suit_symbols", "suit_symbols_font", "suit_symbols_scale",
                        "directional_mark_symbols", "directional_mark_symbols_font", 
                        "directional_mark_symbols_scale", "directional_mark_colors")) {
            opt_str <- paste0("--", style, ".", component)
            parser <- add_option(parser, opt_str, default=NULL, 
                             help=paste0('(default is value of "', style, '" option)'))

        }
        # Style
        opt_str <- paste0("--use_suit_as_ace.", component)
        parser <- add_option(parser, opt_str, default=NULL, action="store_true",
                             help='(default is value of "use_suit_as_ace" option)')
        opt_str <- paste0("--use_ace_as_ace.", component)
        dest_str <- paste0("use_suit_as_ace.", component)
        parser <- add_option(parser, opt_str, dest=dest_str, 
                             action="store_false", default=NULL, 
                             help=paste('Opposite of', opt_str))
        opt_str <- paste0("--directional_mark_theta.", component)
        parser <- add_option(parser, opt_str, default=NULL, type="double",
                             help='(default is value of "directional_mark_theta" option)')

        # Color
        opt_str <- paste0("--background_color.", component)
        parser <- add_option(parser, opt_str, default=NULL, 
                             help='default is either value of "background_color.suited" or "background_color.unsuited"')
        opt_str <- paste0("--invert_colors.", component)
        parser <- add_option(parser, opt_str, default=NULL, action="store_true", 
                             help='default is either value of "invert_colors.suited" or "invert_colors.unsuited"')
        opt_str <- paste0("--uninvert_colors.", component)
        parser <- add_option(parser, opt_str, default=NULL, action="store_false", 
                             help='default is either value of "invert_colors.suited" or "invert_colors.unsuited"')

    }

    # Misc
    parser <- add_option(parser, "--n_suits", default=NULL, type="double", 
                         help='(default is inferred from "suit_symbols" option)')

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

    if (is.null(opts[["font"]]))
        opts$font <- "sans"

    if (is.null(opts[["rank_symbols"]]))
        opts$rank_symbols <- "N,A,2,3,4,5"
    if (is.null(opts[["suit_symbols"]]))
        opts$suit_symbols <- "♥,♠,♣,♦,★"
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

    opts$suit_symbols <- split(opts$suit_symbols)
    opts$suit_colors <- split(opts$suit_colors)
    opts$rank_symbols <- split(opts$rank_symbols)
    if (is.null(opts$n_suits))
        opts$n_suits <- length(opts$suit_symbols) - 1
    opts$i_unsuit <- opts$n_suits + 1


    if (!is.null(opts[["hexline_colors"]]))
        opts$hexline_colors = split(opts[["hexline_colors"]])
    if (!is.null(opts[["directional_mark_symbols"]]))
        opts$directional_mark_symbols <- split(opts[["directional_mark_symbols"]])
    if (!is.null(opts[["rank_symbols_font"]]))
        opts[["rank_symbols_font"]] <- split(opts[["rank_symbols_font"]])
    if (!is.null(opts[["suit_symbols_font"]]))
        opts[["suit_symbols_font"]] <- split(opts[["suit_symbols_font"]])
    if (!is.null(opts[["directional_mark_symbols_font"]]))
        opts[["directional_mark_symbols_font"]] <- split(opts[["directional_mark_symbols_font"]])
    if (!is.null(opts[["rank_symbols_scale"]]))
        opts[["rank_symbols_scale"]] <- as.numeric(split(opts[["rank_symbols_scale"]]))
    if (!is.null(opts[["suit_symbols_scale"]]))
        opts[["suit_symbols_scale"]] <- as.numeric(split(opts[["suit_symbols_scale"]]))
    if (!is.null(opts[["directional_mark_symbols_scale"]]))
        opts[["directional_mark_symbols_scale"]] <- as.numeric(split(opts[["directional_mark_symbols_scale"]]))
    for(component in COMPONENTS) {
        for (style in c("rank_symbols", "suit_symbols", "directional_mark_symbols",
                        "rank_symbols_font", "suit_symbols_font", "directional_mark_symbols_font",
                        "directional_mark_colors")) {
            component_str <- paste0(style, ".", component)
            if (!is.null(opts[[component_str]]))
                opts[[component_str]] <- split(opts[[component_str]])
        }
        for (style in c("rank_symbols_scale", "suit_symbols_scale", "directional_mark_symbols_scale")) {
            component_str <- paste0(style, ".", component)
            if (!is.null(opts[[component_str]]))
                opts[[component_str]] <- as.numeric(split(opts[[component_str]]))

        }
    }


    opts$directory <- file.path("svg", opts$deck_filename)

    opts
}

#' @export
read_style <- function(args=commandArgs(TRUE)) {
    parser <- OptionParser("Program to make piecepack deck pdf")
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
