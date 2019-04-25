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
PAWN_BASE <- 3/8
PAWN_LAYOUT_HEIGHT <- 2 * PAWN_HEIGHT + 2 * PAWN_BASE
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


#' Draw piecepack deck preview 
#'
#' Draw piecepack deck preview
#'
#' @param cfg Piecepack configuration list
#' @export
draw_preview <- function(cfg=list()) {
    cfg <- as_pp_cfg(cfg)
    t_width <- cfg$get_pp_width("tile_face")
    c_width <- cfg$get_pp_width("coin_face")
    d_width <- cfg$get_pp_width("die_face")
    s_width <-  cfg$get_pp_width("saucer_face")
    p_height <- cfg$get_pp_height("pawn_face")
    preview_height <- 3*t_width
    preview_width <- 3*t_width
    vp <- viewport(name="preview", width=inch(preview_width), height=inch(preview_height))
    gl <- gList()

    cs_tiles <- rep("tile_face", 6)
    i_s_tiles <- 1:6
    i_r_tiles <- rep(2, 6)
    if (get_n_suits(cfg) < 5) {
        cs_tiles[5] <- "tile_back"
        i_s_tiles[5] <- NA
        i_r_tiles[5] <- NA
    }
    if (get_n_suits(cfg) < 6) {
        cs_tiles[6] <- "tile_back"
        i_s_tiles[6] <- NA
        i_r_tiles[6] <- NA
    }
    x_tiles <- c(1, 5, 5, 1, 3, 3)
    y_tiles <- c(5, 5, 3, 3, 5, 3)
    gl[["tiles"]] <- pieceGrob(cs_tiles, i_s_tiles, i_r_tiles, cfg, x_tiles, y_tiles, 
                   default.units="in", name="tiles")

    cs_coins <- rep(c("coin_face", "coin_back"), each=3)
    i_s_coins <- c(rep(NA, 3), 4:2)
    i_r_coins <- c(1:3, rep(NA, 3))
    x_coins <- rep(1:3, 2)*c_width - 0.5*c_width
    y_coins <- 0.5*t_width + rep(c(0.5, -0.5), each=3)*c_width
    if (get_n_suits(cfg) > 4) i_s_coins[5] <- 5
    if (get_n_suits(cfg) > 5) i_s_coins[6] <- 6

    gl[["coins"]] <- pieceGrob(cs_coins, i_s_coins, i_r_coins, cfg, x_coins, y_coins, 
                   default.units="in", name="coins")

    gl[["saucers"]] <- pieceGrob(c("saucer_face", "saucer_back"), c(1, NA), NA, cfg,
                   t_width+1.5*d_width, 0.5*t_width + c(0.5, -0.5)*s_width,
                   default.units="in", name="saucers")

    gl[["pawns"]] <- pieceGrob(c("pawn_face", "pawn_back"), 2, NA, cfg,
                   t_width+3*d_width, 0.5*t_width + c(0.5, -0.5)*p_height, c(0, 180),
                   default.units="in", name="pawns")

    gl[["suitrankdie"]] <- pieceGrob("suitrankdie_layoutRF", NA, NA, cfg,
                   preview_width-2*d_width, 0.5*t_width,
                   default.units="in", name="suitrankdie")

    grob <- gTree(children=gl, vp=vp)
    grid.draw(grob)
}

pp_device <- function(filename, piece_side=NULL, cfg=list(), angle=0, i_r = 1,
                      width=NULL, height=NULL, res=72) {
    cfg <- as_pp_cfg(cfg)
    format <- tools::file_ext(filename)
    if (is.null(width)) width <- cfg$get_pp_width(piece_side, i_r)
    if (is.null(height)) height <- cfg$get_pp_height(piece_side, i_r)
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
                               i_s=NULL, i_r=NULL) {
    filename <- paste0(piece_side, 
                       ifelse(is.null(i_s), "", paste0("_s", i_s)),
                       ifelse(is.null(i_r), "", paste0("_r", i_r)),
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
                for (i_s in 1:get_n_suits(cfg)) {
                    f <- piece_filename(directory, cfg, cs, format, angle, i_s)
                    pp_device(f, cs, cfg, angle)
                    grid.piece(cs, i_s, NA, cfg)
                    invisible(dev.off())
                }
            }
            if(!has_suit(cs) && has_rank(cs)) {
                for (i_r in 1:get_n_ranks(cfg)) {
                    f <- piece_filename(directory, cfg, cs, format, angle, i_r=i_r)
                    pp_device(f, cs, cfg, angle)
                    grid.piece(cs, NA, i_r, cfg)
                    invisible(dev.off())
                }
            }
            if(has_suit(cs) && has_rank(cs)) {
                for (i_s in 1:get_n_suits(cfg)) {
                    for (i_r in 1:get_n_ranks(cfg)) {
                        f <- piece_filename(directory, cfg, cs, format, angle, i_s, i_r)
                        pp_device(f, cs, cfg, angle, i_r)
                        grid.piece(cs, i_s, i_r, cfg)
                        invisible(dev.off())
                    }
                }
            }
        }
    })
}
