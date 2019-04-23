#' @importFrom grDevices bmp cairo_pdf cairo_ps dev.off jpeg png svg tiff
#' @import grid

COMPONENTS <- c("tile", "coin", 
                "die", "suitdie", 
                "pawn", "belt", "saucer", 
                "pyramid", "matchstick")
COMPONENT_AND_SIDES <- c("tile_back", "tile_face", "coin_back", "coin_face",
           "die_face", "suitdie_face", 
           "pawn_face", "pawn_back", "belt_face",  "saucer_face", "saucer_back",
           "pyramid_face", "pyramid_back", "pyramid_left", "pyramid_right", 
           "matchstick_face", "matchstick_back")
COMPONENT_AND_SIDES_UNSUITED_UNRANKED <- c("tile_back", "saucer_back")
COMPONENT_AND_SIDES_UNSUITED_RANKED <- c("coin_face")
COMPONENT_AND_SIDES_SUITED_UNRANKED <- c("coin_back", "suitdie_face",
                                         "pawn_face", "pawn_back", "saucer_face", "belt_face")
COMPONENT_AND_SIDES_SUITED_RANKED <- c("die_face", "tile_face", 
           "pyramid_face", "pyramid_back", "pyramid_left", "pyramid_right", 
           "matchstick_face", "matchstick_back")
#### "pawn_layout", "die_layout", "suitdie_layout", "suitrankdie_layout", "pyramid_layout", "pyramid_top""
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
    pheight <- 3*TILE_WIDTH
    pwidth <- 3*TILE_WIDTH
    pushViewport(viewport(name="main", width=inch(pwidth), height=inch(pheight)))

    df <- tibble::tribble(
                ~component_side, ~i_s, ~i_r, ~x, ~y,
                "tile_face", 1, 2, 1, 5,
                "tile_face", 2, 2, 5, 5,
                "tile_face", 3, 2, 5, 3,
                "tile_face", 4, 2, 1, 3,
                "tile_face", 5, 2, 3, 5,
                "tile_face", 6, 2, 3, 3,
                "coin_face", NA,  1, 1*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH + 0.5*COIN_WIDTH,
                "coin_face", NA,  2, 2*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH + 0.5*COIN_WIDTH,
                "coin_back",  4, NA, 3*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH + 0.5*COIN_WIDTH,
                "coin_face", NA,  3, 1*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH - 0.5*COIN_WIDTH,
                "coin_back",  2, NA, 2*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH - 0.5*COIN_WIDTH,
                "coin_back",  3, NA, 3*COIN_WIDTH - 0.5*COIN_WIDTH, 0.5*TILE_WIDTH - 0.5*COIN_WIDTH,
                "saucer_face",  1, NA, TILE_WIDTH + 1.5*DIE_WIDTH, 0.5 * TILE_WIDTH + 0.5 * SAUCER_WIDTH,
                "saucer_back", NA, NA, TILE_WIDTH + 1.5*DIE_WIDTH, 0.5 * TILE_WIDTH - 0.5 * SAUCER_WIDTH,
                "pawn_face", 2, NA, TILE_WIDTH + 3*DIE_WIDTH, 0.5*TILE_WIDTH + 0.5*PAWN_HEIGHT,
                "pawn_back", 2, NA, TILE_WIDTH + 3*DIE_WIDTH, 0.5*TILE_WIDTH - 0.5*PAWN_HEIGHT,
                "suitrankdie_layoutRF", NA, NA, pwidth - 2*DIE_WIDTH, 0.5*TILE_WIDTH 
          )
    df$angle <- 0

    if (get_n_suits(cfg) < 5) {
        df[5, "component_side"] <- "tile_back"
        df[5, "i_s"] <- get_i_unsuit(cfg)
        df[5, "i_r"] <- NA
    }
    if (get_n_suits(cfg) < 6) {
        df[6, "component_side"] <- "tile_back"
        df[6, "i_s"] <- get_i_unsuit(cfg)
        df[6, "i_r"] <- NA
    }
    if (get_n_suits(cfg) > 4) df[11, "i_s"] <- 5
    if (get_n_suits(cfg) > 5) df[12, "i_s"] <- 6
    df[16, "angle"] <- 180
    draw_components(df, cfg=cfg, units="inches")
    popViewport()
}

get_pp_width <- function(component_side, cfg=list(), i_r=1) {
    if (grepl("die_layout", component_side)) {
        die_width <- get_style_element("width", "die_face", cfg, DIE_WIDTH)
        return (4 * die_width)
    }
    if (grepl("pyramid_layout", component_side)) {
        pyramid_height <- get_style_element("height", "pyramid_face", cfg, PYRAMID_HEIGHTS[i_r], i_r=i_r)
        return (pyramid_height)
    }
    component <- get_component(component_side)
    default <- switch(component, 
           belt = BELT_WIDTH,
           coin = COIN_WIDTH,
           die = DIE_WIDTH,
           matchstick = MATCHSTICK_WIDTHS[i_r],
           pawn = PAWN_WIDTH,
           pyramid = PYRAMID_WIDTHS[i_r],
           saucer = SAUCER_WIDTH,
           suitdie = DIE_WIDTH,
           tile = TILE_WIDTH,
           stop(paste("Don't know width of component", component)))
    get_style_element("width", component_side, cfg, default, i_r=i_r)
}

get_pp_height <- function(component_side, cfg=list(), i_r=1) {
    if (grepl("die_layout", component_side)) {
        die_height <- get_style_element("height", "die_face", cfg, DIE_WIDTH)
        return (3 * die_height)
    }
    if (grepl("pyramid_layout", component_side)) {
        pyramid_width <- get_style_element("width", "pyramid_face", cfg, PYRAMID_WIDTHS[i_r], i_r=i_r)
        pyramid_height <- get_style_element("height", "pyramid_face", cfg, PYRAMID_HEIGHTS[i_r], i_r=i_r)
        pyramid_diagonal <- sqrt(pyramid_height^2 + (0.5*pyramid_width)^2)
        return (2 * pyramid_diagonal)
    }
    if (grepl("pawn_layout", component_side)) {
        pawn_height <- get_style_element("height", "pawn_face", cfg, PAWN_HEIGHT)
        return((2.5 / (7/8)) * pawn_height)
    }
    width <- get_pp_width(component_side, cfg, i_r)
    component <- get_component(component_side)
    if (component == "matchstick") {
        W <- ifelse(i_r == 1, 0.5*width, width)
        S <- 0.5 * get_pp_width("tile_face", cfg)
        default <- (c(2*W, S-W, sqrt(2)*S-W, 2*S-W, sqrt(5*S^2)-W, 2*sqrt(2)*S-W)[i_r])
    }
    default <- switch(component, 
               belt = BELT_HEIGHT,
               coin = width,
               die = width,
               matchstick = default,
               pawn = PAWN_HEIGHT,
               pyramid = 1.538842 * width,
               saucer = width,
               suitdie = width,
               tile = width,
               stop(paste("Don't know height of component", component))) #nocov
    get_style_element("height", component_side, cfg, default, i_r=i_r)
}

pp_device <- function(filename, component_side=NULL, cfg=list(), angle=0, i_r = 1,
                      width=NULL, height=NULL, res=72) {
    format <- tools::file_ext(filename)
    if (is.null(width)) width <- get_pp_width(component_side, cfg, i_r)
    if (is.null(height)) height <- get_pp_height(component_side, cfg, i_r)
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

component_filename <- function(directory, cfg, component_side, format, angle, 
                               i_s=NULL, i_r=NULL) {
    filename <- paste0(component_side, 
                       ifelse(is.null(i_s), "", paste0("_s", i_s)),
                       ifelse(is.null(i_r), "", paste0("_r", i_r)),
                       paste0("_t", angle), paste0(".", format))
    file.path(directory, filename)
}

#' Make piecepack images
#'
#' Makes images of individual piecepack components.
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
        for (cs in COMPONENT_AND_SIDES_UNSUITED_UNRANKED) {
            f <- component_filename(directory, cfg, cs, format, angle)
            pp_device(f, cs, cfg, angle)
            draw_component(cs, cfg)
            invisible(dev.off())
        }

        for (i_s in 1:get_n_suits(cfg)) {
            for (cs in COMPONENT_AND_SIDES_SUITED_UNRANKED) {
                f <- component_filename(directory, cfg, cs, format, angle, i_s)
                pp_device(f, cs, cfg, angle)
                draw_component(cs, cfg, i_s)
                invisible(dev.off())
            }

            for (i_r in 1:get_n_ranks(cfg)) {
                for (cs in c(COMPONENT_AND_SIDES_SUITED_RANKED, "pyramid_top")) {
                    f <- component_filename(directory, cfg, cs, format, angle, i_s, i_r)
                    pp_device(f, cs, cfg, angle, i_r)
                    draw_component(cs, cfg, i_s, i_r)
                    invisible(dev.off())
                }
            }
        }
        for (i_r in 1:get_n_ranks(cfg)) {
            for (cs in COMPONENT_AND_SIDES_UNSUITED_RANKED) {
                f <- component_filename(directory, cfg, cs, format, angle, i_r=i_r)
                pp_device(f, cs, cfg, angle)
                draw_component(cs, cfg, i_r=i_r)
                invisible(dev.off())
            }
        }
    })
}

draw_component_helper <- function(component_side, i_s, i_r, cfg) {
    default <- switch(component_side,
                      die_layoutLF = draw_die_layoutLF,
                      die_layoutRF = draw_die_layoutRF,
                      suitdie_layoutLF = draw_suitdie_layoutLF,
                      suitdie_layoutRF = draw_suitdie_layoutRF,
                      suitrankdie_layoutLF = draw_suitrankdie_layoutLF,
                      suitrankdie_layoutRF = draw_suitrankdie_layoutRF,
                      pawn_layout = draw_pawn_layout,
                      pyramid_layout = draw_pyramid_layout,
                      pyramid_top = draw_pyramid_top,
                      basic_draw_fn)
    draw_fn <- get_style_element("draw_fn", component_side, cfg, default, i_s, i_r)
    if (is.character(draw_fn))
        draw_fn <- match.fun(draw_fn)
    draw_fn(component_side, i_s, i_r, cfg)
    invisible(NULL)
}

draw_pyramid_top <- function(component_side, i_s, i_r, cfg) {
    cfg$scale <- 0.3 * get_scale(cfg)
    draw_component("pyramid_face",  cfg, i_s, i_r, y=0.75, width=1.0, height=0.5, angle=180)
    draw_component("pyramid_back",  cfg, i_s, i_r, y=0.25, width=1.0, height=0.5, angle=0)
    draw_component("pyramid_left",  cfg, i_s, i_r, x=0.25, width=1.0, height=0.5, angle=-90)
    draw_component("pyramid_right", cfg, i_s, i_r, x=0.75, width=1.0, height=0.5, angle=90)
}
