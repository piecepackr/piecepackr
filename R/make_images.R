#' @importFrom grDevices bmp cairo_pdf cairo_ps dev.off jpeg png svg tiff
#' @import grid

COMPONENT_AND_SIDES <- c("tile_back", "tile_face", "coin_back", "coin_face",
           "die_face", "suitdie_face", 
           "pawn_face", "pawn_back", "belt_face",  "saucer_face", "saucer_back",
           "pyramid_face", "pyramid_back", "pyramid_left", "pyramid_right", "pyramid_top",
           "matchstick_face", "matchstick_back")

has_suit <- function(cs) {
    !(cs %in% c("tile_back", "saucer_back", "coin_face"))
}
has_rank <- function(cs) {
    !(cs %in% c("tile_back", "coin_back", "suitdie_face",
                "pawn_face", "pawn_back", "belt_face", 
                "saucer_face", "saucer_back"))
}

COIN_WIDTH <- 3/4
DIE_WIDTH <- 1/2
TILE_WIDTH <- 2
# SAUCER_WIDTH <- 7/8
SAUCER_WIDTH <- 3/4 # better for diagrams of hex games played with coin+pawn
PAWN_HEIGHT <- 7/8
PAWN_WIDTH <- 1/2
DIE_LAYOUT_WIDTH <- 4 * DIE_WIDTH
DIE_LAYOUT_HEIGHT <- 3 * DIE_WIDTH
BELT_HEIGHT <- 1/2
BELT_WIDTH <- 0.75 * pi # so can wrap around 3/4" diameter pawns
PYRAMID_WIDTHS <- 2:8 * 1/8
PYRAMID_HEIGHTS <- 1.538842 * PYRAMID_WIDTHS
PYRAMID_DIAGONALS <- sqrt(PYRAMID_HEIGHTS^2 + (0.5*PYRAMID_WIDTHS)^2)
PYRAMID_LAYOUT_WIDTHS <- PYRAMID_HEIGHTS
PYRAMID_LAYOUT_HEIGHTS <- 2*PYRAMID_DIAGONALS
W <- 3/16
S <- 1
MATCHSTICK_WIDTHS <- c(2*W, rep(W, 5))
MATCHSTICK_HEIGHTS <- c(2*W, S-W, sqrt(2)*S-W, 2*S-W, sqrt(5*S^2)-W, 2*sqrt(2)*S-W)



pp_device <- function(filename, piece_side=NULL, cfg=list(), angle=0, rank = 1,
                      width=NULL, height=NULL, res=72) {
    cfg <- as_pp_cfg(cfg)
    format <- tools::file_ext(filename)
    if (is.null(width)) width <- cfg$get_width(piece_side, rank)
    if (is.null(height)) height <- cfg$get_height(piece_side, rank)
    if (angle %in% c(90, 270)) {
        twidth <- height
        height <- width
        width <- twidth
    }
    bg <- "transparent"
    dev <- switch(format,
                bmp = bmp(filename, width, height, "in", res=res, bg=bg),
                jpeg = jpeg(filename, width, height, "in", res=res, bg=bg),
                pdf = cairo_pdf(filename, width, height, bg=bg),
                png = png(filename, width, height, "in", res=res, bg=bg),
                ps = cairo_ps(filename, width, height, bg=bg),
                svg = svg(filename, width, height, bg=bg),
                tiff = tiff(filename, width, height, "in", res=res, bg=bg))
    pushViewport(viewport(angle=angle, name="main"))
}

piece_filename <- function(directory, cfg, piece_side, format, angle, 
                               suit=NULL, rank=NULL) {
    filename <- paste0(piece_side, 
                       ifelse(is.null(suit), "", paste0("_s", suit)),
                       ifelse(is.null(rank), "", paste0("_r", rank)),
                       paste0("_t", angle), paste0(".", format))
    file.path(directory, filename)
}

#' Make piecepack images
#'
#' Makes images of individual piecepack pieces.
#'
#' @param cfg Piecepack configuration list
#' @param directory Directory where to place images
#' @param format Format
#' @param angles Angle to rotate images (in degrees)
#' @export
make_images <- function(cfg=list(), directory=tempdir(), format="svg", angles=0) {
    for (angle in angles) make_images_helper(directory, cfg, format, angle)
}
make_images_helper <- function(directory, cfg, format, angle) {
    suppressWarnings({
        for (cs in COMPONENT_AND_SIDES) {
            if(!has_suit(cs) && !has_rank(cs)) {
                f <- piece_filename(directory, cfg, cs, format, angle)
                pp_device(f, cs, cfg, angle)
                grid.piece(cs, NA, NA, cfg)
                invisible(dev.off())
            }
            if(has_suit(cs) && !has_rank(cs)) {
                for (suit in 1:get_n_suits(cfg)) {
                    f <- piece_filename(directory, cfg, cs, format, angle, suit)
                    pp_device(f, cs, cfg, angle)
                    grid.piece(cs, suit, NA, cfg)
                    invisible(dev.off())
                }
            }
            if(!has_suit(cs) && has_rank(cs)) {
                for (rank in 1:get_n_ranks(cfg)) {
                    f <- piece_filename(directory, cfg, cs, format, angle, rank=rank)
                    pp_device(f, cs, cfg, angle)
                    grid.piece(cs, NA, rank, cfg)
                    invisible(dev.off())
                }
            }
            if(has_suit(cs) && has_rank(cs)) {
                for (suit in 1:get_n_suits(cfg)) {
                    for (rank in 1:get_n_ranks(cfg)) {
                        f <- piece_filename(directory, cfg, cs, format, angle, suit, rank)
                        pp_device(f, cs, cfg, angle, rank)
                        grid.piece(cs, suit, rank, cfg)
                        invisible(dev.off())
                    }
                }
            }
        }
    })
}
