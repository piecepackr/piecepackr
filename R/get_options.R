FORMATS <- c("bmp", "jpeg", "pdf", "png", "ps", "svg", "tiff")

#' @importFrom optparse OptionParser add_option parse_args
configuration_options <- function(args=commandArgs(TRUE)) {
    parser <- OptionParser(paste("Program to make piecepack configurations.",
                               "Set environmental variables PP_N_RANKS and/or PP_N_SUITS to configure individual ranks/suits"))
    parser <- add_option(parser, "--deck_title", default=NULL, 
                         help=sprintf('(default "%s")', get_deck_title()))
    parser <- add_option(parser, "--deck_filename", default=NULL, 
                         help=sprintf('Filename prefix (default "%s")', get_deck_filename()))

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
    parser <- add_option(parser, c("-f", "--file"), default=NULL, 
                         help="Filename to write configuration to (default outputs to standard output)")
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

    # Directories
    parser <- add_option(parser, "--pdf_deck_dir", default=NULL,
                         help=sprintf('Directory to put pdfs in (default "%s")', get_pdf_deck_dir()))

    parser <- add_option(parser, "--svg_preview_dir", default=NULL,
                         help=sprintf('Directory to put pdfs in (default "%s")', get_svg_preview_dir()))

    for (format in FORMATS) {
        option <- paste0("--", format, "_component_dir")
        parser <- add_option(parser, option, default=NULL,
                         help=sprintf('Directory to put %s component images in (default "%s/components")', format, format))

    }

    # Components
    parser <- add_option(parser, "--component_formats", default=NULL,
                         help='Default image formats for ``make_images`` (default "pdf,png,svg")')
    parser <- add_option(parser, "--component_thetas", default=NULL,
                         help='Default rotations for ``make_images`` (default "0,90,180,270")')

    # Cascading variants
    n_ranks <- as.numeric(Sys.getenv("PP_N_RANKS"))
    n_ranks <- ifelse(is.na(n_ranks), 0, n_ranks)
    n_suits <- as.numeric(Sys.getenv("PP_N_SUITS"))
    n_suits <- ifelse(is.na(n_suits), 0, n_suits)
    if (length(n_ranks)) 
        rank_affixes <- c(paste0(".r", 1:n_ranks), "")
    else
        rank_affixes <- ""
    if (length(n_suits))
        suit_affixes <- c(paste0(".s", 1:n_suits), ".suited", ".unsuited", "")
    else
        suit_affixes <- ""
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
    opts <- parse_args(parser, args)
    opts
}

#' Generate piecepack configuration JSON
#'
#' Processes command-line options and writes to JSON.
#' 
#' @param args Character vector of command-line arguments.
#' @export
configure_piecepack <- function(args=commandArgs(TRUE)) {
    opts <- configuration_options(args)
    filename <- opts$file
    opts$file <- NULL
    opts$help <- NULL
    .to_json(opts, filename)
}

.to_json <- function(opts, filename=NULL) {
    if (is.null(filename)) {
        writeLines(jsonlite::toJSON(opts, pretty=TRUE), stdout())
    } else {
        writeLines(jsonlite::toJSON(opts, pretty=TRUE), filename, useBytes=TRUE)
    }
}

#' Get collect piecepacks options
#'
#' Get collect piecepacks options.
#' 
#' @param args Character vector of command-line arguments.
#' @export
get_arrangement_opts <- function(args=commandArgs(TRUE)) { ####
    parser <- OptionParser("Program to collect piecepacks")
    parser <- add_option(parser, "--author", default=get_author(),
                         help='Pdf author (default "")')
    parser <- add_option(parser, "--filename", default="piecepack_collection", ####
                         help='Filename prefix (default "%default")')
    parser <- add_option(parser, "--keywords", default=get_keywords(),
                         help='Pdf keywords (default "%default")')
    parser <- add_option(parser, "--subject", default=get_subject(), 
                         help='Pdf subject (default "%default")')
    parser <- add_option(parser, "--title", default=get_title(), 
                         help='Pdf title (default "%default")')
    parser <- add_option(parser, "--decks", 
                         help='Comma separated list of decks to collect (default collects all of them)')
    parser <- add_option(parser, "--pdf_deck_dir", default=get_pdf_deck_dir(),
                         help='(default "%default")')
    parser <- add_option(parser, "--pdf_collection_dir", default=get_pdf_collection_dir(),
                         help='(default "%default")')
    parser <- add_option(parser, "--pdf_preview_dir", default=get_pdf_preview_dir(),
                         help='(default "%default")')
    parser <- add_option(parser, "--svg_preview_dir", default=get_svg_preview_dir(),
                         help='(default "svg/previews")')
    parser <- add_option(parser, "--paper", default=get_paper(),
                         help='Pdf paper size, either "letter" or "A4" (default "%default")')
    parser <- add_option(parser, "--font", default=get_font(),
                         help='Default font family (default "%default")')

    parse_args(parser, args)
}


process_configuration <- function(opts) {
    ####
    for(format in FORMATS) {
        option_str <- paste0(format, "_component_dir")
        if (is.null(opts[[option_str]]))
            opts[[option_str]] <- file.path(format, "components")
    }
    if (is.null(opts[["component_formats"]]))
        opts$component_formats <- "pdf,png,svg"
    opts$component_formats <- split(opts$component_formats)
    if (is.null(opts[["component_thetas"]]))
        opts$component_thetas <- "0,90,180,270"
    opts$component_thetas <- numeric_split(opts$component_thetas)

    opts
}

#' Read piecepack configuration JSON
#'
#' Read in and process piecepack configuration JSON.
#' 
#' @param args Character vector of command-line arguments.
#' @param description Description to pass to \code{OptionParser}.
#' @export
read_configuration <- function(args=commandArgs(TRUE), description="") {
    parser <- OptionParser(description)
    parser <- add_option(parser, c('-f', '--file'), default=NULL, help="Source of piecepack style information (default stdin input)")
    parser <- add_option(parser, "--paper", default="letter",
                         help='Default paper size, either "letter" or "A4" (default "letter")')
    args <- parse_args(parser, args)

    if (is.null(args$file)) 
        con <- file("stdin")
    else 
        con <- file(args$file)
    
    opts <- jsonlite::fromJSON(readLines(con, encoding="UTF-8"))
    close(con)

    opts$paper <- args$paper

    opts <- process_configuration(opts)
    opts
}

#' Read piecepack configuration list from command-line arguments
#' 
#' Read piecepack configuration list from command-line arguments
#' 
#' @param args Character vector of command-line arguments.
#' @export
c2o <- function(args=NULL) {
    process_configuration(configuration_options(args))
}
