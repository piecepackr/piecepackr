#' Draw piecepack components
#' 
#' \code{draw_component} Draws a single piecepack component onto the graphics device.  
#' \code{draw_components} draws several piecepack components specified in a data frame  
#'    applying \code{draw_component_wrapper} to each row.
#' By default \code{draw_component} sets up a viewport and then uses \code{basic_draw_fn} to 
#'   draw the component.
#' \code{basic_draw_fn} uses \code{get_component_opt} to parse the configuration list.
#' 
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
#' @name draw_component
NULL

#' @rdname draw_component
#' @export
draw_component <- function(component_side, cfg=list(), i_s=get_i_unsuit(cfg), i_r=0, x=0.5, y=0.5, 
                           width=NULL, height=NULL, svg=FALSE, ...) {

    if (component_side %in% c(COMPONENT_AND_SIDES_UNSUITED_UNRANKED, COMPONENT_AND_SIDES_UNSUITED_RANKED)) 
        i_s <- get_i_unsuit(cfg)
    if (component_side %in% c(COMPONENT_AND_SIDES_UNSUITED_UNRANKED, COMPONENT_AND_SIDES_SUITED_UNRANKED))
        i_r <- 0

    if (is.null(width))
        width=inch(get_pp_width(component_side, cfg, i_r))
    if (is.null(height))
        height=inch(get_pp_height(component_side, cfg, i_r))
    if (svg) {
        svg_file <- tempfile(fileext=".svg")
        on.exit(unlink(svg_file))
        pp_width=get_pp_width(component_side, cfg, i_r)
        pp_height=get_pp_height(component_side, cfg, i_r)

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
#' @param cfg_list Named list (or environment) of configuration lists
draw_component_wrapper <- function(..., component_side="tile_back", x=0.5, y=0.5, i_s=NA, i_r=NA, width=NA, height=NA, svg=FALSE, units="npc", angle=NA, cfg=NULL, cfg_name=NA, cfg_list=NULL) {
    x <- unit(x, units)
    y <- unit(y, units)

    if (is.null(cfg)) {
        if (is.na(cfg_name)) {
            cfg <- list()
        } else if (!is.null(cfg_list)) {
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

#' @rdname draw_component
#' @name draw_component
#' @export
basic_draw_fn <- function(component_side, i_s, i_r, cfg) {
    opt <- get_component_opt(component_side, i_s, i_r, cfg)

    shape_fn <- get_grid_shape(opt$shape, opt$shape_t, opt$shape_r)

    # Background
    shape_fn(gp=gpar(col=NA, fill=opt$background_col))

    # Gridlines, Mat
    add_gridlines(opt$gridline_col, opt$shape, opt$shape_t)
    add_mat(opt$mat_col, opt$shape, opt$shape_t, opt$mat_width)

    # Primary symbol
    gp_ps <- gpar(col=opt$ps_col, fontsize=opt$ps_fontsize, 
                  fontfamily=opt$ps_fontfamily, fontface=opt$ps_fontface)
    grid.text(opt$ps_text, x=opt$ps_x, y=opt$ps_y, gp=gp_ps)

    # Directional mark
    gp_dm <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, 
                  fontfamily=opt$dm_fontfamily, fontface=opt$ps_fontface)
    grid.text(opt$dm_text, x=opt$dm_x, y=opt$dm_y, gp=gp_dm)

    # Border 
    shape_fn(gp=gpar(col=opt$border_col, fill=NA))

    invisible(NULL)
}

#' @rdname draw_component
#' @export
get_component_opt <- function(component_side, i_s=get_i_unsuit(cfg), i_r=1, cfg=list()) {
    key <- opt_cache_key(component_side, i_s, i_r)
    if(!is.null(attr(cfg, "cache")[[key]])) {
        return(attr(cfg, "cache")[[key]])
    }

    # Shape
    shape <- get_shape(component_side, i_s, i_r, cfg)
    shape_r <- get_shape_r(component_side, i_s, i_r, cfg)
    shape_t <- get_shape_t(component_side, i_s, i_r, cfg)

    # Additional colors
    background_col <- get_background_color(component_side, i_s, i_r, cfg)
    border_col <- get_border_color(component_side, i_s, i_r, cfg)
    gridline_col <- get_gridline_color(component_side, i_s, i_r, cfg)
    mat_col <- get_mat_color(component_side, i_s, i_r, cfg)
    mat_width <- get_mat_width(component_side, i_s, i_r, cfg)

    # Overall scaling factor
    scale <- get_scale(cfg)

    # Directional mark symbol
    dm_col <- get_dm_color(component_side, i_s, i_r, cfg)
    dm_scale <- get_dm_scale(component_side, i_s, i_r, cfg)
    dm_fontfamily <- get_dm_fontfamily(component_side, i_s, i_r, cfg)
    dm_fontface <- get_dm_fontface(component_side, i_s, i_r, cfg)
    dm_fontsize <- scale * dm_scale * get_dm_fontsize(component_side, i_s, i_r, cfg)
    dm_text <- get_dm_text(component_side, i_s, i_r, cfg)
    dm_t <- get_dm_t(component_side, i_s, i_r, cfg)
    dm_r <- get_dm_r(component_side, i_s, i_r, cfg)
    dm_x <- to_x(dm_t, dm_r) + 0.5
    dm_y <- to_y(dm_t, dm_r) + 0.5

    # Primary symbol
    ps_col <- get_ps_color(component_side, i_s, i_r, cfg)
    ps_scale <- get_ps_scale(component_side, i_s, i_r, cfg)
    ps_fontfamily <- get_ps_fontfamily(component_side, i_s, i_r, cfg)
    ps_fontface <- get_ps_fontface(component_side, i_s, i_r, cfg)
    ps_fontsize <- scale * ps_scale * get_ps_fontsize(component_side, i_s, i_r, cfg)
    ps_text <- get_ps_text(component_side, i_s, i_r, cfg)
    ps_t <- get_ps_t(component_side, i_s, i_r, cfg)
    ps_r <- get_ps_r(component_side, i_s, i_r, cfg)
    ps_x <- to_x(ps_t, ps_r) + 0.5
    ps_y <- to_y(ps_t, ps_r) + 0.5

    list(shape=shape, shape_r=shape_r, shape_t=shape_t, 
         background_col=background_col, border_col=border_col, 
         gridline_col=gridline_col,  mat_col=mat_col, mat_width=mat_width,
         dm_col=dm_col, dm_text=dm_text, 
         dm_fontsize=dm_fontsize, 
         dm_fontfamily=dm_fontfamily, dm_fontface=dm_fontface,
         dm_x=dm_x, dm_y=dm_y, 
         ps_col=ps_col, ps_text=ps_text, 
         ps_fontsize=ps_fontsize, 
         ps_fontfamily=ps_fontfamily, ps_fontface=ps_fontface,
         ps_x=ps_x, ps_y=ps_y)
}
