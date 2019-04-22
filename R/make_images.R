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

get_pp_width <- function(component_side, i_r) {
    switch(component_side,
           die_layoutLF = DIE_LAYOUT_WIDTH,
           die_layoutRF = DIE_LAYOUT_WIDTH,
           suitdie_layoutLF = DIE_LAYOUT_WIDTH,
           suitdie_layoutRF = DIE_LAYOUT_WIDTH,
           suitrankdie_layoutLF = DIE_LAYOUT_WIDTH,
           suitrankdie_layoutRF = DIE_LAYOUT_WIDTH,
           pawn_layout = PAWN_WIDTH,
           pyramid_layout = PYRAMID_LAYOUT_WIDTHS[i_r],
           pyramid_top = PYRAMID_WIDTHS[i_r],
           { 
        component <- get_component(component_side)
        switch(component, 
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
    })
}

get_pp_height <- function(component_side, i_r) {
    switch(component_side,
           die_layoutLF = DIE_LAYOUT_HEIGHT,
           die_layoutRF = DIE_LAYOUT_HEIGHT,
           suitdie_layoutLF = DIE_LAYOUT_HEIGHT,
           suitdie_layoutRF = DIE_LAYOUT_HEIGHT,
           suitrankdie_layoutLF = DIE_LAYOUT_HEIGHT,
           suitrankdie_layoutRF = DIE_LAYOUT_HEIGHT,
           pawn_layout = PAWN_LAYOUT_HEIGHT,
           pyramid_top = PYRAMID_WIDTHS[i_r],
           pyramid_layout = PYRAMID_LAYOUT_HEIGHTS[i_r],
           {
        component <- get_component(component_side)
        switch(component, 
               belt = BELT_HEIGHT,
               coin = COIN_WIDTH,
               die = DIE_WIDTH,
               matchstick = MATCHSTICK_HEIGHTS[i_r],
               pawn = PAWN_HEIGHT,
               pyramid = PYRAMID_HEIGHTS[i_r],
               saucer = SAUCER_WIDTH,
               suitdie = DIE_WIDTH,
               tile = TILE_WIDTH,
               stop(paste("Don't know height of component", component)))
    })
}

pp_device <- function(filename, component_side=NULL, angle=0, i_r = 1,
                      width=NULL, height=NULL, res=72) {
    format <- tools::file_ext(filename)
    if (is.null(width)) width <- get_pp_width(component_side, i_r)
    if (is.null(height)) height <- get_pp_height(component_side, i_r)
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
            pp_device(f, cs, angle)
            draw_component(cs, cfg)
            invisible(dev.off())
        }

        for (i_s in 1:get_n_suits(cfg)) {
            for (cs in COMPONENT_AND_SIDES_SUITED_UNRANKED) {
                f <- component_filename(directory, cfg, cs, format, angle, i_s)
                pp_device(f, cs, angle)
                draw_component(cs, cfg, i_s)
                invisible(dev.off())
            }

            for (i_r in 1:get_n_ranks(cfg)) {
                for (cs in c(COMPONENT_AND_SIDES_SUITED_RANKED, "pyramid_top")) {
                    f <- component_filename(directory, cfg, cs, format, angle, i_s, i_r)
                    pp_device(f, cs, angle, i_r)
                    draw_component(cs, cfg, i_s, i_r)
                    invisible(dev.off())
                }
            }
        }
        for (i_r in 1:get_n_ranks(cfg)) {
            for (cs in COMPONENT_AND_SIDES_UNSUITED_RANKED) {
                f <- component_filename(directory, cfg, cs, format, angle, i_r=i_r)
                pp_device(f, cs, angle)
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
                      draw_component_basic)
    draw_fn <- get_style_element("draw_component_fn", component_side, cfg, default, i_s, i_r)
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

#' Draw piecepack components
#' 
#' \code{draw_component} Draws a single piecepack component onto the graphics device.  
#' \code{draw_components} draws several piecepack components specified in a data frame  
#'    applying \code{draw_component_wrapper} to each row.
#' 
#' @rdname draw_component
#' @param component_side A string with component and side separated by a underscore e.g. "coin_face"
#' @param cfg Piecepack configuration list
#' @param i_s Number of suit
#' @param i_r Number of rank
#' @param x Where to place component on x axis of viewport
#' @param y Where to place component on y axis of viewport
#' @param width Width of component
#' @param height Height of component
#' @param svg If \code{TRUE} instead of drawing directly into graphics device
#'            export to svg, re-import svg, and then draw it to graphics device.  
#'            This is useful if drawing really big or small and don't want
#'            to play with re-configuring fontsizes.
#' @param ... With \code{draw_component} extra arguments to pass to \code{grid::viewport} like \code{angle}, with \code{draw_components} extra arguments to pass to \code{draw_component_wrapper}, with \code{draw_component_wrapper} ignored.
#' @export
draw_component <- function(component_side, cfg=list(), i_s=get_i_unsuit(cfg), i_r=0, x=0.5, y=0.5, 
                           width=NULL, height=NULL, svg=FALSE, ...) {

    if (component_side %in% c(COMPONENT_AND_SIDES_UNSUITED_UNRANKED, COMPONENT_AND_SIDES_UNSUITED_RANKED)) 
        i_s <- get_i_unsuit(cfg)
    if (component_side %in% c(COMPONENT_AND_SIDES_UNSUITED_UNRANKED, COMPONENT_AND_SIDES_SUITED_UNRANKED))
        i_r <- 0

    if (is.null(width))
        width=inch(get_pp_width(component_side, i_r))
    if (is.null(height))
        height=inch(get_pp_height(component_side, i_r))
    if (svg) {
        svg_file <- tempfile(fileext=".svg")
        on.exit(unlink(svg_file))
        pp_width=get_pp_width(component_side, i_r)
        pp_height=get_pp_height(component_side, i_r)

        svg(svg_file, width=pp_width, height=pp_height)
        draw_component(component_side, cfg, i_s, i_r)
        invisible(dev.off())

        pushViewport(viewport(x=x, y=y, width=width, height=height, ...))
        grid.draw(pictureGrob(readPicture(svg_file, warn=FALSE)))
        upViewport()
    } else {
        pushViewport(viewport(x=x, y=y, width=width, height=height, ...))
        draw_component_helper(component_side, i_s, i_r, cfg)
        upViewport()
    }
    invisible(NULL)
}

#' @rdname draw_component
#' @param df A data frame specifying arguments to ``draw_component_wrapper`` 
#' @export
draw_components <- function(df, ...) {
    ll <- purrr::pmap(df, draw_component_wrapper, ...)
    invisible(NULL)
}

#' @rdname draw_component
#' @param units String specifying the units for the corresponding numeric values
#' @param angle Angle to draw component at
#' @param cfg_name String of list name storing configuration
#' @param cfg_list List of configuration lists
draw_component_wrapper <- function(..., component_side="tile_back", x=0.5, y=0.5, i_s=NA, i_r=NA, width=NA, height=NA, svg=FALSE, units="npc", angle=NA, cfg=NULL, cfg_name=NA, cfg_list=NULL) {
    x <- unit(x, units)
    y <- unit(y, units)

    if (is.null(cfg)) {
        if (is.na(cfg_name)) {
            cfg <- list()
        } else if (is.list(cfg_list)) {
            cfg <- cfg_list[[cfg_name]]
        } else {
            cfg <- dynGet(cfg_name)
        }
    }
    if (is.na(i_r)) i_r <- 0
    if (is.na(i_s)) i_s <- get_i_unsuit(cfg)
    if (is.na(angle)) angle <- 0
    if (is.na(width))
        width <- NULL
    else
        width <- unit(width, units)
    if (is.na(height))
        height <- NULL
    else
        height <- unit(height, units)
    draw_component(component_side, cfg, i_s, i_r, x, y, width, height, svg, angle=angle)
}

