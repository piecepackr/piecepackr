opt_cache_key <- function(piece_side, i_s, i_r, type) {
    paste(piece_side, i_s, i_r, type, sep=".")
}

#' @import R6
Config <- R6Class("pp_cfg",
    public = list(
        i_unsuit = NULL,
        n_suits = NULL,
        n_ranks = NULL,
        die_arrangement = NULL,
        title = NULL,
        fontfamily = NULL,
        initialize = function(cfg=list()) {
            private$cfg <- cfg
            self$n_suits <- get_n_suits(cfg)
            self$n_ranks <- get_n_ranks(cfg)
            self$i_unsuit <- self$n_suits + 1
            self$die_arrangement <- get_die_arrangement(cfg)
            self$fontfamily <- get_fontfamily(cfg)
            self$title <- cfg$title
        },
        as_list = function() { private$cfg },
        print = function() {
            for(name in names(private$cfg)) {
                cat(paste0("$", name, " : ", private$cfg[[name]]), "\n")
            }
        },
        get_grob = function(piece_side, i_s, i_r) {
            key <- opt_cache_key(piece_side, i_s, i_r, "grob")
            if(!is.null(private$cache[[key]])) {
                private$cache[[key]]
            } else {
                default_fn <- get_style_element("grob_fn", piece_side, private$cfg, 
                                                basicPieceGrobFn, i_s, i_r)
                grobFn <- switch(piece_side,
                                 die_layoutLF = dieLayoutGrobLF,
                                 die_layoutRF = dieLayoutGrobRF,
                                 suitdie_layoutLF = suitdieLayouGrobtLF,
                                 suitdie_layoutRF = suitdieLayoutGrobRF,
                                 suitrankdie_layoutLF = suitrankdieLayoutGrobLF,
                                 suitrankdie_layoutRF = suitrankdieLayoutGrobRF,
                                 pawn_layout = pawnLayoutGrob,
                                 pyramid_layout = pyramidLayoutGrob,
                                 pyramid_top = pyramidTopGrob,
                                 default_fn)
                if (is.character(grobFn))
                    grobFn <- match.fun(grobFn)
                grob <- grobFn(piece_side, i_s, i_r, self)
                private$cache[[key]] <- grob
                grob
            }
        },
        get_piece_opt = function(piece_side, i_s=NULL, i_r=NULL) {
            if(is.null(i_r)) { i_r <- 1 }
            if(is.null(i_s)) { i_s <- self$i_unsuit }
            key <- opt_cache_key(piece_side, i_s, i_r, "piece_opt")
            if(!is.null(private$cache[[key]])) {
                return(private$cache[[key]])
            }
            # Shape
            shape <- get_shape(piece_side, i_s, i_r, private$cfg)
            shape_r <- get_shape_r(piece_side, i_s, i_r, private$cfg)
            shape_t <- get_shape_t(piece_side, i_s, i_r, private$cfg)

            # Additional colors
            background_col <- get_background_color(piece_side, i_s, i_r, private$cfg)
            border_col <- get_border_color(piece_side, i_s, i_r, private$cfg)
            gridline_col <- get_gridline_color(piece_side, i_s, i_r, private$cfg)
            mat_col <- get_mat_color(piece_side, i_s, i_r, private$cfg)
            mat_width <- get_mat_width(piece_side, i_s, i_r, private$cfg)

            # Overall scaling factor
            scale <- get_scale(private$cfg)

            # Directional mark symbol
            dm_col <- get_dm_color(piece_side, i_s, i_r, private$cfg)
            dm_scale <- get_dm_scale(piece_side, i_s, i_r, private$cfg)
            dm_fontfamily <- get_dm_fontfamily(piece_side, i_s, i_r, private$cfg)
            dm_fontface <- get_dm_fontface(piece_side, i_s, i_r, private$cfg)
            dm_fontsize <- scale * dm_scale * get_dm_fontsize(piece_side, i_s, i_r, private$cfg)
            dm_text <- get_dm_text(piece_side, i_s, i_r, private$cfg)
            dm_t <- get_dm_t(piece_side, i_s, i_r, private$cfg)
            dm_r <- get_dm_r(piece_side, i_s, i_r, private$cfg)
            dm_x <- to_x(dm_t, dm_r) + 0.5
            dm_y <- to_y(dm_t, dm_r) + 0.5

            # Primary symbol
            ps_col <- get_ps_color(piece_side, i_s, i_r, private$cfg)
            ps_scale <- get_ps_scale(piece_side, i_s, i_r, private$cfg)
            ps_fontfamily <- get_ps_fontfamily(piece_side, i_s, i_r, private$cfg)
            ps_fontface <- get_ps_fontface(piece_side, i_s, i_r, private$cfg)
            ps_fontsize <- scale * ps_scale * get_ps_fontsize(piece_side, i_s, i_r, private$cfg)
            ps_text <- get_ps_text(piece_side, i_s, i_r, private$cfg)
            ps_t <- get_ps_t(piece_side, i_s, i_r, private$cfg)
            ps_r <- get_ps_r(piece_side, i_s, i_r, private$cfg)
            ps_x <- to_x(ps_t, ps_r) + 0.5
            ps_y <- to_y(ps_t, ps_r) + 0.5

            opt <- list(shape=shape, shape_r=shape_r, shape_t=shape_t, 
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
            private$cache[[key]] <- opt
            opt
        },
        get_pp_width = function(piece_side, i_r=1) {
            key <- opt_cache_key(piece_side, NA, i_r, "pp_width")
            if(!is.null(private$cache[[key]])) {
                return(private$cache[[key]])
            }
            if (grepl("die_layout", piece_side)) {
                die_width <- get_style_element("width", "die_face", private$cfg, DIE_WIDTH)
                return (4 * die_width)
            }
            if (grepl("pyramid_layout", piece_side)) {
                pyramid_height <- get_style_element("height", "pyramid_face", private$cfg, PYRAMID_HEIGHTS[i_r], i_r=i_r)
                return (pyramid_height)
            }
            piece <- get_piece(piece_side)
            default <- switch(piece, 
                              belt = BELT_WIDTH,
                              coin = COIN_WIDTH,
                              die = DIE_WIDTH,
                              matchstick = MATCHSTICK_WIDTHS[i_r],
                              pawn = PAWN_WIDTH,
                              pyramid = PYRAMID_WIDTHS[i_r],
                              saucer = SAUCER_WIDTH,
                              suitdie = DIE_WIDTH,
                              tile = TILE_WIDTH,
                              stop(paste("Don't know width of piece", piece)))
            width <- get_style_element("width", piece_side, private$cfg, default, i_r=i_r)
            private$cache[[key]] <- width
            width
        },
        get_pp_height = function(piece_side, i_r=1) {
            key <- opt_cache_key(piece_side, NA, i_r, "pp_height")
            if(!is.null(private$cache[[key]])) {
                return(private$cache[[key]])
            }
            if (grepl("die_layout", piece_side)) {
                die_height <- get_style_element("height", "die_face", private$cfg, DIE_WIDTH)
                return (3 * die_height)
            }
            if (grepl("pyramid_layout", piece_side)) {
                pyramid_height <- get_style_element("height", "pyramid_face", private$cfg, PYRAMID_HEIGHTS[i_r], i_r=i_r)
                pyramid_width <- get_style_element("width", "pyramid_face", private$cfg, PYRAMID_WIDTHS[i_r], i_r=i_r)
                pyramid_diagonal <- sqrt(pyramid_height^2 + (0.5*pyramid_width)^2)
                return (2 * pyramid_diagonal)
            }
            if (grepl("pyramid_top", piece_side)) {
                pyramid_width <- get_style_element("width", "pyramid_face", private$cfg, PYRAMID_WIDTHS[i_r], i_r=i_r)
                return(pyramid_width)
            }
            if (grepl("pawn_layout", piece_side)) {
                pawn_height <- get_style_element("height", "pawn_face", private$cfg, PAWN_HEIGHT)
                return((2.5 / (7/8)) * pawn_height)
            }
            width <- self$get_pp_width(piece_side, i_r)
            piece <- get_piece(piece_side)
            if (piece == "matchstick") {
                W <- ifelse(i_r == 1, 0.5*width, width)
                S <- 0.5 * self$get_pp_width("tile_face")
                default <- (c(2*W, S-W, sqrt(2)*S-W, 2*S-W, sqrt(5*S^2)-W, 2*sqrt(2)*S-W)[i_r])
            }
            default <- switch(piece, 
                              belt = BELT_HEIGHT,
                              coin = width,
                              die = width,
                              matchstick = default,
                              pawn = PAWN_HEIGHT,
                              pyramid = 1.538842 * width,
                              saucer = width,
                              suitdie = width,
                              tile = width,
                              stop(paste("Don't know height of piece", piece))) #nocov
            height <- get_style_element("height", piece_side, private$cfg, default, i_r=i_r)
            private$cache[[key]] <- height
            height
        }
        ),
    private = list(cfg = NULL, cache = list())
)

#' Configuration list R6 object
#'
#' \code{pp_cfg} and \code{as_pp_cfg} creates piecepack configuration list R6 object.
#' \code{is_pp_cfg} returns \code{TRUE} if object is a piecepack configuration list R6 object.
#' \code{as.list} will convert it into a list.
#'
#' @param cfg List of configuration options
#' @examples
#'  \donttest{
#'    cfg <- list()
#'    system.time(replicate(500, grid.piece("tile_face", 4, 4, cfg)))
#'    cfg <- pp_cfg(cfg)
#'    system.time(replicate(500, grid.piece("tile_face", 4, 4, cfg)))
#'  }
#'   
#' @exportClass pp_cfg
#' @export
pp_cfg <- function(cfg=list()) {
    if(is_pp_cfg(cfg)) 
        cfg
    else
        Config$new(cfg)
}

#' @rdname pp_cfg
#' @export
is_pp_cfg <- function(cfg) {
    "pp_cfg" %in% class(cfg)
}

#' @rdname pp_cfg
#' @export
as_pp_cfg <- pp_cfg

#' @export
as.list.pp_cfg <- function(x, ...) {
    x$as_list()
}
