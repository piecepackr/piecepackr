split <- function(x, sep=",") { stringr::str_split(x, sep)[[1]] } 
col_split <- function(x, sep=",") { gsub("^$", "transparent", split(x, sep)) }

configuration_options <- function(args=commandArgs(TRUE)) {
    default_str <- "(default %default)"
    parser <- OptionParser("Program to make piecepack configurations for exec/make_piecepack")
    parser <- add_option(parser, "--deck_title", default=NULL, 
                         help='(default "")')
    parser <- add_option(parser, "--deck_filename", default=NULL, 
                         help='Filename prefix (default "piecepack_deck")')
    parser <- add_option(parser, "--pdf_deck_dir", default=NULL,
                         help='Directory to put pdfs in (default "pdf/decks")')
    parser <- add_option(parser, "--svg_preview_dir", default=NULL,
                         help='Directory to put pdfs in (default "svg/previews")')

    # Symbols
    parser <- add_option(parser, "--rank_symbols", default=NULL, 
                         help='(default "N,A,2,3,4,5")')
    parser <- add_option(parser, "--suit_symbols", default=NULL, 
                         help='(default "♠,♥,♣,♦,★")')
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
    parser <- add_option(parser, "--use_suit_as_ace", action="store_true", default=NULL, 
                         help='(default "FALSE")')
    parser <- add_option(parser, "--use_ace_as_ace", dest="use_suit_as_ace", 
                         action="store_false", default=NULL, 
                         help='Opposite of "use_suit_as_ace" option')
    # parser <- add_option(parser, "--style", default=NULL, 
    #                      help='"basic" or "simple_hex" (default "basic")')
    parser <- add_option(parser, "--shape", default=NULL,
                         help=paste('either "rect", "circle", "halma", "kite", "star",',
                                   "or a number representing number of sides of a regular polygon",
                                   "(default is to choose reasonable shape based on component)'"))
    parser <- add_option(parser, "--shape_theta", default=NULL, type="double",
                         help=paste('If shape is a number or "star" then angle of first vertex',
                                   '(in degrees) of polygon', '(default is "90")'))

    parser <- add_option(parser, "--checker_colors", default=NULL,
                         help='(default is not to draw any checkers on tiles)')
    parser <- add_option(parser, "--gridline_colors", default=NULL,
                         help='(default is to draw gridlines on tile back using the "unsuit" suit color)')
    parser <- add_option(parser, "--hexline_colors", default=NULL,
                         help='(default is not to draw any hexlines on tiles)')
    parser <- add_option(parser, "--dm_theta", default=NULL, type="double", 
                         help=paste('Angle (in degrees) of polar coordinates of direction mark',
                                   '(default 135 for tile faces and die faces and 90 for everything else)'))
    parser <- add_option(parser, "--dm_r", default=NULL, type="double", 
                         help=paste('Radius from center (relative units)',
                                  'of polar coordinates of direction mark',
                                  '(default "sqrt(.25^2+.25^2)" if its shape is "rect" or "circle" otherwise "0.3")'))

    # Font
    parser <- add_option(parser, "--font", default=NULL,
                         help='Default font family (default "sans")')

    # Color scheme
    parser <- add_option(parser, "--suit_colors", default=NULL, 
                         help='(default "darkred,black,darkgreen,darkblue,grey")')
    parser <- add_option(parser, "--dm_colors", default=NULL, 
                         help='(default the value of the "suit_colors" option)')
    parser <- add_option(parser, "--background_colors", default=NULL, 
                         help='(default "transparent")')
    parser <- add_option(parser, "--border_color", default=NULL, 
                         help='(default "grey")')
    parser <- add_option(parser, "--invert_colors", action="store_true", default=NULL, 
                         help='(default "FALSE")')
    parser <- add_option(parser, "--uninvert_colors", action="store_false", dest="invert_colors",
                         default=NULL, help='Opposite of "invert_colors" option')
    parser <- add_option(parser, c("-f", "--file"), default=NULL, 
                         help="Filename to write configuration to (default outputs to standard output)")

    # Suited/unsuited variants
    parser <- add_option(parser, "--background_colors.suited", default=NULL, 
                         help='(default is value of "background_colors" option)')
    parser <- add_option(parser, "--background_colors.unsuited", default=NULL, 
                         help='(default is value of "background_colors" option)')

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
                        "suit_colors", "suit_symbols", "suit_symbols_font", "suit_symbols_scale",
                        "dm_symbols", "dm_symbols_font", "shape",
                        "dm_symbols_scale", "dm_colors")) {
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
                             help=paste0('Opposite of "', dest_str, '" option'))
        for (style in c("dm_theta", "dm_r", "shape_theta")) {
            opt_str <- paste0("--", style, ".", component)
            parser <- add_option(parser, opt_str, default=NULL, type="double",
                                 help=paste0('(default is value of "', style, '" option)'))
        }

        # Color
        opt_str <- paste0("--background_colors.", component)
        parser <- add_option(parser, opt_str, default=NULL, 
                             help='default is either value of "background_colors.suited" or "background_colors.unsuited"')
        opt_str <- paste0("--invert_colors.", component)
        parser <- add_option(parser, opt_str, default=NULL, action="store_true", 
                             help='default is either value of "invert_colors.suited" or "invert_colors.unsuited"')
        opt_str <- paste0("--uninvert_colors.", component)
        dest_str <- paste0("invert_colors.", component)
        parser <- add_option(parser, opt_str, dest=dest_str,
                             default=NULL, action="store_false", 
                             help=paste0('Opposite of "', dest_str, '" option'))
    }

    # Misc
    parser <- add_option(parser, "--n_ranks", default=NULL, type="double", 
                         help='(default is inferred from "rank_symbols" option)')
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
    parser <- add_option(parser, "--author", default="",
                         help='Pdf author (default "")')
    parser <- add_option(parser, "--filename", default="piecepack_collection",
                         help='Filename prefix (default "%default")')
    parser <- add_option(parser, "--keywords", default="piecepack",
                         help='Pdf keywords (default "%default")')
    parser <- add_option(parser, "--subject", default="", 
                         help='Pdf subject (default "%default")')
    parser <- add_option(parser, "--title", default="Piecepack collection", 
                         help='Pdf title (default "%default")')
    parser <- add_option(parser, "--decks", 
                         help='Comma separated list of decks to collect (default collects all of them)')
    parser <- add_option(parser, "--pdf_deck_dir", default="pdf/decks",
                         help='(default "%default")')
    parser <- add_option(parser, "--pdf_collection_dir", default="pdf/collections",
                         help='(default "%default")')
    parser <- add_option(parser, "--pdf_preview_dir", default="pdf/previews",
                         help='(default "%default")')
    parser <- add_option(parser, "--svg_preview_dir", default="svg/previews",
                         help='(default "svg/previews")')
    parser <- add_option(parser, "--paper", default="letter",
                         help='Pdf paper size, either "letter" or "A4" (default "%default")')
    parser <- add_option(parser, "--font", default="sans",
                         help='Default font family (default "%default")')

    opts <- parse_args(parser, args)

    if (is.null(opts$decks))
        opts$decks <- gsub(".pdf$", "", list.files(opts$pdf_deck_dir))
    else
        opts$decks <- split(opts$decks)

    opts <- add_copyright(opts)
    opts$header_font <- opts$font
    opts
}

add_copyright <- function(opts) {
    # opts$program <- "Generated by the Configurable Piecepack PDF Maker."
    opts$program <- "Generated by the piecepack R package: github.com/trevorld/piecepack"
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
        opts$deck_title <- ""

    if (is.null(opts$deck_filename))
        opts$deck_filename <- "piecepack_deck"
    if (is.null(opts$pdf_deck_dir))
        opts$pdf_deck_dir <- "pdf/decks"
    if (is.null(opts$svg_preview_dir))
        opts$svg_preview_dir <- "svg/previews"

    if (is.null(opts[["font"]]))
        opts$font <- "sans"
    if (is.null(opts[["header_font"]]))
        opts$header_font <- opts$font

    if (is.null(opts[["rank_symbols"]]))
        opts$rank_symbols <- "N,A,2,3,4,5"
    if (is.null(opts[["suit_symbols"]]))
        opts$suit_symbols <- "♥,♠,♣,♦,★"
    if (is.null(opts[["suit_colors"]]))
        opts$suit_colors <- "darkred,black,darkgreen,darkblue,grey"
    if (is.null(opts[["use_suit_as_ace"]]))
        opts$use_suit_as_ace <- FALSE
    if (is.null(opts[["style"]]))
        opts$style <- "basic"
    if (is.null(opts[["background_colors"]]))
        opts$background_colors <- "transparent"
    if (is.null(opts[["border_color"]]))
        opts$border_color <- "grey"
    if (is.null(opts[["invert_colors"]]))
        opts$invert_colors <- FALSE

    opts <- add_copyright(opts)

    opts$suit_symbols <- split(opts$suit_symbols)
    opts$suit_colors <- col_split(opts$suit_colors)
    opts$rank_symbols <- split(opts$rank_symbols)
    if (is.null(opts$n_ranks))
        opts$n_ranks <- length(opts$rank_symbols)
    if (is.null(opts$n_suits))
        opts$n_suits <- length(opts$suit_symbols) - 1
    opts$i_unsuit <- opts$n_suits + 1


    if (!is.null(opts[["background_colors"]]))
        opts$background_colors = col_split(opts[["background_colors"]])
    if (!is.null(opts[["checker_colors"]]))
        opts$checker_colors = col_split(opts[["checker_colors"]])
    if (!is.null(opts[["gridline_colors"]]))
        opts$gridline_colors = col_split(opts[["gridline_colors"]])
    if (!is.null(opts[["hexline_colors"]]))
        opts$hexline_colors = col_split(opts[["hexline_colors"]])
    if (!is.null(opts[["dm_symbols"]]))
        opts$dm_symbols <- split(opts[["dm_symbols"]])
    if (!is.null(opts[["rank_symbols_font"]]))
        opts[["rank_symbols_font"]] <- split(opts[["rank_symbols_font"]])
    if (!is.null(opts[["suit_symbols_font"]]))
        opts[["suit_symbols_font"]] <- split(opts[["suit_symbols_font"]])
    if (!is.null(opts[["dm_symbols_font"]]))
        opts[["dm_symbols_font"]] <- split(opts[["dm_symbols_font"]])
    if (!is.null(opts[["rank_symbols_scale"]]))
        opts[["rank_symbols_scale"]] <- as.numeric(split(opts[["rank_symbols_scale"]]))
    if (!is.null(opts[["suit_symbols_scale"]]))
        opts[["suit_symbols_scale"]] <- as.numeric(split(opts[["suit_symbols_scale"]]))
    if (!is.null(opts[["dm_symbols_scale"]]))
        opts[["dm_symbols_scale"]] <- as.numeric(split(opts[["dm_symbols_scale"]]))
    for(suited in c("suited", "unsuited")) {
        component_str <- paste0("background_colors", ".", suited)
        if (!is.null(opts[[component_str]]))
            opts[[component_str]] <- col_split(opts[[component_str]])
    }
    for(component in COMPONENTS) {
        for (style in c("rank_symbols", "suit_symbols", "dm_symbols",
                        "rank_symbols_font", "suit_symbols_font", "dm_symbols_font")) {
            component_str <- paste0(style, ".", component)
            if (!is.null(opts[[component_str]]))
                opts[[component_str]] <- split(opts[[component_str]])
        }
        for (style in c( "background_colors", "suit_colors", "dm_colors")) {
            component_str <- paste0(style, ".", component)
            if (!is.null(opts[[component_str]]))
                opts[[component_str]] <- col_split(opts[[component_str]])
        }
        for (style in c("rank_symbols_scale", "suit_symbols_scale", "dm_symbols_scale")) {
            component_str <- paste0(style, ".", component)
            if (!is.null(opts[[component_str]]))
                opts[[component_str]] <- as.numeric(split(opts[[component_str]]))

        }
    }

    opts
}

#' @export
read_style <- function(args=commandArgs(TRUE)) {
    parser <- OptionParser("Program to make piecepack deck pdf")
    parser <- add_option(parser, c('-f', '--file'), default=NULL, help="Source of piecepack style information (default stdin input)")
    parser <- add_option(parser, "--paper", default="letter",
                         help='Default paper size, either "letter" or "A4" (default "letter")')
    args <- parse_args(parser, args)

    if (is.null(args$file)) 
        con <- file("stdin")
    else 
        con <- file(args$file)
    
    opts <- jsonlite::fromJSON(readLines(con))
    close(con)

    opts$paper <- args$paper

    opts <- process_configuration(opts)
    opts
}

#' @export
c2o <- function(args=NULL) {
    process_configuration(configuration_options(args))
}
