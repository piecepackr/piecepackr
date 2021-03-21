#' @import grid

COMPONENT_AND_SIDES <- c("tile_back", "tile_face", "coin_back", "coin_face",
           "die_face", "suitdie_face",
           "pawn_face", "pawn_back", "belt_face",  "saucer_face", "saucer_back",
           "pyramid_face", "pyramid_back", "pyramid_left", "pyramid_right", "pyramid_top",
           "matchstick_face", "matchstick_back")

pp_device <- function(filename, piece_side=NULL, cfg=list(), angle=0, suit=1, rank=1,
                      width=NULL, height=NULL, res=72) {
    cfg <- as_pp_cfg(cfg)
    format <- tools::file_ext(filename)
    if (is.null(width)) width <- cfg$get_width(piece_side, suit, rank)
    if (is.null(height)) height <- cfg$get_height(piece_side, suit, rank)
    if (angle %in% c(90, 270)) {
        twidth <- height
        height <- width
        width <- twidth
    }
    bg <- "transparent"
    switch(format,
           bmp = grDevices::bmp(filename, width, height, "in", res=res, bg=bg),
           jpeg = grDevices::jpeg(filename, width, height, "in", res=res, bg=bg),
           jpg = grDevices::jpeg(filename, width, height, "in", res=res, bg=bg),
           pdf = grDevices::cairo_pdf(filename, width, height, bg=bg),
           png = grDevices::png(filename, width, height, "in", res=res, bg=bg),
           ps = grDevices::cairo_ps(filename, width, height, bg=bg),
           svg = grDevices::svg(filename, width, height, bg=bg),
           svgz = grDevices::svg(filename, width, height, bg=bg),
           tiff = grDevices::tiff(filename, width, height, "in", res=res, bg=bg))
    pushViewport(viewport(angle=angle, name="main"))
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
#'   \donttest{
#'     if (all(capabilities(c("cairo", "png")))) {
#'         cfg <- pp_cfg(list(suit_color="darkred,black,darkgreen,darkblue,grey"))
#'         save_piece_images(cfg, directory=tempdir(), format="svg", angle=0)
#'         save_piece_images(cfg, directory=tempdir(), format="png", angle=90)
#'     }
#'   }
#'
#' @export
save_piece_images <- function(cfg=pp_cfg(), directory=tempdir(), format="svg", angle=0) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))

    if (!dir.exists(directory)) stop(paste("directory", directory, "does not exist"))
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
    suppressWarnings({
        for (cs in COMPONENT_AND_SIDES) {
            if (!has_suit(cs) && !has_rank(cs)) {
                f <- piece_filename(directory, cs, format, angle)
                pp_device(f, cs, cfg, angle)
                grid.piece(cs, NA, NA, cfg)
                pp_dev_off(f, format)
            }
            if (has_suit(cs) && !has_rank(cs)) {
                for (suit in 1:cfg$n_suits) {
                    f <- piece_filename(directory, cs, format, angle, suit)
                    pp_device(f, cs, cfg, angle)
                    grid.piece(cs, suit, NA, cfg)
                    pp_dev_off(f, format)
                }
            }
            if (!has_suit(cs) && has_rank(cs)) {
                for (rank in 1:cfg$n_ranks) {
                    f <- piece_filename(directory, cs, format, angle, rank=rank)
                    pp_device(f, cs, cfg, angle)
                    grid.piece(cs, NA, rank, cfg)
                    pp_dev_off(f, format)
                }
            }
            if (has_suit(cs) && has_rank(cs)) {
                for (suit in 1:cfg$n_suits) {
                    for (rank in 1:cfg$n_ranks) {
                        f <- piece_filename(directory, cs, format, angle, suit, rank)
                        pp_device(f, cs, cfg, angle, rank=rank)
                        grid.piece(cs, suit, rank, cfg)
                        pp_dev_off(f, format)
                    }
                }
            }
        }
    })
}
