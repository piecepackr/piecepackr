COMPONENT_AND_SIDES <- c("tile_back", "tile_face", "coin_back", "coin_face",
           "die_face", "suitdie_face",
           "pawn_face", "pawn_back", "belt_face",  "saucer_face", "saucer_back",
           "pyramid_face", "pyramid_back", "pyramid_left", "pyramid_right", "pyramid_top",
           "matchstick_face", "matchstick_back")

piece_device <- function(filename, piece_side=NULL, cfg=list(), angle=0, suit=1, rank=1,
                         width=NULL, height=NULL, res=72) {
    cfg <- as_pp_cfg(cfg)
    width <- width %||% cfg$get_width(piece_side, suit, rank)
    height <- height %||% cfg$get_height(piece_side, suit, rank)
    if (angle %in% c(90, 270)) {
        twidth <- height
        height <- width
        width <- twidth
    }
    pp_device(filename, width, height, res, bg = "transparent")
    pushViewport(viewport(angle=angle, name="main"))
}

pp_device <- function(filename, width, height, res=72, bg="transparent") {
    args <- list(filename = filename, width = width, height = height,
                 units = "in", res = res, bg = bg)
    dev_fn <- pp_device_fn(filename)
    args <- args[names(args) %in% names(formals(dev_fn))]
    do.call(dev_fn, args)
}

pp_device_fn <- function(filename) {
    format <- tools::file_ext(tolower(filename))
    switch(format,
           bmp = grDevices::bmp,
           jpeg = grDevices::jpeg,
           jpg = grDevices::jpeg,
           pdf = grDevices::cairo_pdf,
           png = grDevices::png,
           ps = grDevices::cairo_ps,
           svg = grDevices::svg,
           svgz = grDevices::svg,
           tiff = grDevices::tiff)
}

piece_filename <- function(directory, piece_side, format, angle,
                               suit=NULL, rank=NULL) {
    filename <- paste0(piece_side,
                       ifelse(is.null(suit), "", paste0("_s", suit)),
                       ifelse(is.null(rank), "", paste0("_r", rank)),
                       paste0("_t", angle), paste0(".", format))
    file.path(directory, filename)
}

#' Save piecepack images
#'
#' Saves images of all individual piecepack pieces.
#'
#' @param cfg Piecepack configuration list
#' @param directory Directory where to place images
#' @param format Character vector of formats to save images in
#' @param angle Numeric vector of angles to rotate images (in degrees)
#' @examples
#'   \donttest{# May take more than 5 seconds on CRAN server
#'   if (all(capabilities(c("cairo", "png")))) {
#'       cfg <- pp_cfg(list(suit_color="darkred,black,darkgreen,darkblue,grey"))
#'       save_piece_images(cfg, directory=tempdir(), format="svg", angle=0)
#'       save_piece_images(cfg, directory=tempdir(), format="png", angle=90)
#'   }
#'   }
#' @export
save_piece_images <- function(cfg = getOption("piecepackr.cfg", pp_cfg()),
                              directory=tempdir(), format="svg", angle=0) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    opt <- options(piecepackr.op_scale = 0)
    on.exit(options(opt))

    stopifnot(dir.exists(directory))

    for (f in format) {
        for (a in angle) make_images_helper(directory, cfg, f, a)
    }
}

pp_dev_off <- function(f, format) {
    v <- grDevices::dev.off()
    if (format == "svgz") {
        rcon <- file(f, "r")
        svg <- readLines(rcon)
        close(rcon)
        wcon <- gzfile(f, "w")
        writeLines(svg, wcon)
        close(wcon)
    }
    invisible(v)
}

#### Check if configuration has that piece?
#### Extend to boards, bits, and cards?
#### What to do if don't want other side of two sided pieces?
#### Allow list of which pieces to save?
make_images_helper <- function(directory, cfg, format, angle) {
    lacks_rank_lacks_suit <- intersect(COMPONENT_AND_SIDES,
                                       intersect(cfg$lacks_rank, cfg$lacks_suit))
    lacks_rank_has_suit <- intersect(setdiff(COMPONENT_AND_SIDES, cfg$lacks_suit),
                                     (cfg$lacks_rank))
    has_rank_lacks_suit <- intersect(setdiff(COMPONENT_AND_SIDES, cfg$lacks_rank),
                                     (cfg$lacks_suit))
    has_rank_has_suit <- setdiff(COMPONENT_AND_SIDES,
                                 union(cfg$lacks_rank, cfg$lacks_suit))

    suppressWarnings({
        for (cs in lacks_rank_lacks_suit) {
            f <- piece_filename(directory, cs, format, angle)
            piece_device(f, cs, cfg, angle)
            grid.piece(cs, NA, NA, cfg)
            pp_dev_off(f, format)
        }
        for (cs in lacks_rank_has_suit) {
            for (suit in 1:cfg$n_suits) {
                f <- piece_filename(directory, cs, format, angle, suit)
                piece_device(f, cs, cfg, angle)
                grid.piece(cs, suit, NA, cfg)
                pp_dev_off(f, format)
            }
        }
        for (cs in has_rank_lacks_suit) {
            for (rank in 1:cfg$n_ranks) {
                f <- piece_filename(directory, cs, format, angle, rank=rank)
                piece_device(f, cs, cfg, angle)
                grid.piece(cs, NA, rank, cfg)
                pp_dev_off(f, format)
            }
        }
        for (cs in has_rank_has_suit) {
            for (suit in 1:cfg$n_suits) {
                for (rank in 1:cfg$n_ranks) {
                    f <- piece_filename(directory, cs, format, angle, suit, rank)
                    piece_device(f, cs, cfg, angle, rank=rank)
                    grid.piece(cs, suit, rank, cfg)
                    pp_dev_off(f, format)
                }
            }
        }
    })
}
