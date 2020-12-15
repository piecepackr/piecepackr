#' Configuration list R6 object
#'
#' \code{pp_cfg} and \code{as_pp_cfg} creates piecepack configuration list R6 object.
#' \code{is_pp_cfg} returns \code{TRUE} if object is a piecepack configuration list R6 object.
#' \code{as.list} will convert it into a list.
#'
#' \code{pp_cfg} objects serve the following purposes:\enumerate{
#' \item{Customize the appearance of pieces drawn by \code{grid.piece}.}
#' \item{Speed up the drawing of graphics through use of caching.}
#' \item{Allow the setting and querying of information about the board game components
#'       that maybe of use to developers\enumerate{
#'          \item{Number of suits}
#'          \item{Number of ranks}
#'          \item{Suit colors}
#'          \item{Which types of components are included and/or properly supported}
#'          \item{What would be a good color to use when adding annotations on top of these components.}
#'          \item{Title, Description, Copyright, and Credit metadata}
#'       }}}
#'
#' @section \code{pp_cfg} R6 Class Method Arguments:\describe{
#'   \item{\code{piece_side}}{A string with piece and side separated by a underscore e.g. "coin_face".}
#'   \item{\code{suit}}{Number of suit (starting from 1).}
#'   \item{\code{rank}}{Number of rank (starting from 1).}
#'   \item{\code{type}}{Which type of grob to return, either \code{"normal"}, \code{"picture"}, or \code{"raster"}.}

#' }
#'
#' @section \code{pp_cfg} R6 Class Methods:\describe{
#'   \item{\code{get_grob}}{Returns a \code{grid} \dQuote{grob} for drawing the piece.}
#'   \item{\code{get_piece_opt}}{Returns a list with info useful for drawing the piece.}
#'   \item{\code{get_suit_color}}{Returns the suit colors.}
#'   \item{\code{get_width}, \code{get_height}, \code{get_depth}}{
#'         Dimensions (of the bounding cube) of the piece in inches}
#' }
#'
#' @seealso \url{https://trevorldavis.com/piecepackr/configuration-lists.html} for more details
#'      about \code{piecepackr} configuration lists.
#'  \code{\link{game_systems}} for functions that return
#'      configuration list objects for several game systems.
#' @param cfg List of configuration options
#' @examples
#'  cfg <- pp_cfg(list(invert_colors=TRUE))
#'  as.list(cfg)
#'  is_pp_cfg(cfg)
#'  as_pp_cfg(list(suit_color="darkred,black,darkgreen,darkblue,grey"))
#'  cfg$get_suit_color(suit=3)
#'  cfg$annotation_color
#'  cfg$has_matchsticks
#'  cfg$has_matchsticks <- TRUE
#'  cfg$has_matchsticks
#'  cfg$get_width("tile_back")
#'  cfg$get_height("die_face")
#'  cfg$get_depth("coin_face")
#'  \donttest{
#'    cfg <- list()
#'    system.time(replicate(100, grid.piece("tile_face", 4, 4, cfg)))
#'    cfg <- pp_cfg(list())
#'    system.time(replicate(100, grid.piece("tile_face", 4, 4, cfg)))
#'  }
#'
#' @export
pp_cfg <- function(cfg=list()) {
    if (is_pp_cfg(cfg))
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
opt_cache_key <- function(piece_side, suit, rank, type) {
    paste(piece_side, suit, rank, type, sep=".")
}

get_n_suits <- function(cfg=list()) {
    if (is.null(cfg[["n_suits"]])) {
        length(get_suit_symbols(cfg=cfg, expand=FALSE)) - 1
    } else {
        cfg[["n_suits"]]
    }
}

get_n_ranks <- function(cfg=list()) {
    if (is.null(cfg[["n_ranks"]])) {
        length(get_rank_symbols(cfg=cfg, expand=FALSE))
    } else {
        cfg[["n_ranks"]]
    }
}

get_i_unsuit <- function(cfg=list()) get_n_suits(cfg) + 1


#' @import R6
Config <- R6Class("pp_cfg",
    public = list(
        i_unsuit = NULL,
        n_suits = NULL,
        n_ranks = NULL,
        coin_arrangement = NULL,
        die_arrangement = NULL,
        title = NULL,
        fontfamily = NULL,
        copyright = NULL,
        credit = NULL,
        description = NULL,
        annotation_color = NULL,
        has_bits = FALSE,
        has_boards = FALSE,
        has_cards = FALSE,
        has_coins = TRUE,
        has_dice = TRUE,
        has_matchsticks = FALSE,
        has_pawns = TRUE,
        has_pyramids = FALSE,
        has_saucers = FALSE,
        has_tiles = TRUE,
        cache_grob = TRUE,
        cache_obj_fn = TRUE,
        cache_piece_opt = TRUE,
        cache_shadow = TRUE,
        initialize = function(cfg=list()) {
            warn_cfg(cfg)
            private$cfg <- cfg
            self$n_suits <- get_n_suits(cfg)
            self$n_ranks <- get_n_ranks(cfg)
            self$i_unsuit <- self$n_suits + 1
            self$coin_arrangement <- get_coin_arrangement(cfg)
            self$die_arrangement <- get_die_arrangement(cfg)
            self$fontfamily <- get_fontfamily(cfg)
            self$title <- cfg$title
            self$copyright <- cfg$copyright
            self$credit <- cfg$credit
            self$description <- cfg$description
            self$annotation_color <- ifelse(is.null(cfg$annotation_color),
                                            "black", cfg$annotation_color)
        },
        get_grob = function(piece_side, suit, rank, type = "normal", ...) {
            switch(type,
                   normal = private$get_grob_normal(piece_side, suit, rank),
                   picture = private$get_grob_picture(piece_side, suit, rank),
                   raster = to_rasterGrob(self$get_raster(piece_side, suit, rank, ...)))
        },
        get_piece_opt = function(piece_side, suit=NA, rank=NA) {
            if (is.na(rank)) rank <- 1
            if (is.na(suit)) suit <- self$i_unsuit
            key <- opt_cache_key(piece_side, suit, rank, "piece_opt")
            if (!is.null(private$cache[[key]])) {
                return(private$cache[[key]])
            }
            opt <- get_piece_opt_helper(piece_side, suit, rank, private$cfg)
            if (self$cache_piece_opt)
                private$cache[[key]] <- opt
            opt
        },
        get_suit_color = function(suit=NULL) {
            if (is.null(suit)) suit <- seq(self$n_suits)
            colors <- character(length(suit))
            for (ii in seq(along.with=suit)) {
                colors[ii] <- get_suit_color_helper("pawn_face", suit[ii], rank=1, private$cfg)
            }
            colors
        },
        get_width = function(piece_side, suit=1, rank=1) {
            key <- opt_cache_key(piece_side, suit, rank, "width")
            if (!is.null(private$cache[[key]])) {
                return(private$cache[[key]])
            }
            if (grepl("die_layout", piece_side)) {
                die_width <- self$get_width("die_face")
                return(4 * die_width)
            }
            if (grepl("pyramid_layout", piece_side)) {
                pyramid_height <- self$get_height("pyramid_face", rank=rank)
                return(pyramid_height)
            }
            if (piece_side == "preview_layout") {
                tile_width <- self$get_width("tile_face")
                return(3 * tile_width)
            }
            piece <- get_piece(piece_side)
            if (grepl("_left$|_right$", piece_side) && piece != "pyramid") {
                ps <- paste0(piece, "_face")
                return(self$get_height(ps, suit=suit, rank=rank))
            }
            default <- switch(piece,
                              belt = 0.75 * pi, # so can wrap around 3/4" diameter pawns
                              bit = 0.75,
                              board = 8,
                              card = 2.5,
                              coin = 0.75,
                              die = 0.5,
                              matchstick = MATCHSTICK_WIDTHS[rank],
                              pawn = 0.5,
                              pyramid = PYRAMID_WIDTHS[rank],
                              saucer = 0.75, # better than 7/8" for diagrams of hex games played with coin+pawn
                              suitdie = 0.5,
                              tile = 2,
                              stop(paste("Don't know width of piece", piece)))
            width <- get_style_element("width", piece_side, private$cfg, default, suit, rank)
            private$cache[[key]] <- width
            width
        },
        get_height = function(piece_side, suit=1, rank=1) {
            key <- opt_cache_key(piece_side, suit, rank, "height")
            if (!is.null(private$cache[[key]])) {
                return(private$cache[[key]])
            }
            if (grepl("die_layout", piece_side)) {
                die_height <- self$get_height("die_face")
                return(3 * die_height)
            }
            if (grepl("pyramid_layout", piece_side)) {
                pyramid_height <- self$get_height("pyramid_face", rank=rank)
                pyramid_width <- self$get_width("pyramid_face", rank=rank)
                pyramid_diagonal <- sqrt(pyramid_height^2 + (0.5*pyramid_width)^2)
                return(2 * pyramid_diagonal)
            }
            if (grepl("pyramid_top", piece_side)) {
                pyramid_width <- self$get_width("pyramid_face", rank=rank)
                return(pyramid_width)
            }
            if (grepl("pawn_layout", piece_side)) {
                pawn_height <- self$get_height("pawn_face")
                return((2.5 / (7/8)) * pawn_height)
            }
            if (piece_side == "preview_layout") {
                tile_height <- self$get_height("tile_face")
                return(3 * tile_height)
            }
            piece <- get_piece(piece_side)
            if (grepl("_top$|_base$|_left$|_right$", piece_side) && piece != "pyramid") {
                ps <- paste0(piece, "_face")
                return(self$get_depth(ps, suit=suit, rank=rank))
            }
            width <- self$get_width(piece_side, suit, rank)
            if (piece == "matchstick") {
                W <- ifelse(rank == 1, 0.5*width, width)
                S <- 0.5 * self$get_width("tile_face")
                ms_default <- (c(2*W, S-W, sqrt(2)*S-W, 2*S-W, sqrt(5*S^2)-W, 2*sqrt(2)*S-W)[rank])
            }
            default <- switch(piece,
                              belt = 0.5,
                              bit = width,
                              board = width,
                              card = 1.4 * width,
                              coin = width,
                              die = width,
                              matchstick = ms_default,
                              pawn = 0.875,
                              pyramid = 1.538842 * width,
                              saucer = width,
                              suitdie = width,
                              tile = width,
                              stop(paste("Don't know height of piece", piece))) #nocov
            height <- get_style_element("height", piece_side, private$cfg, default, suit, rank)
            private$cache[[key]] <- height
            height
        },
        get_depth = function(piece_side, suit=1, rank=1) {
            key <- opt_cache_key(piece_side, suit, rank, "depth")
            if (!is.null(private$cache[[key]])) {
                return(private$cache[[key]])
            }
            width <- self$get_width(piece_side, suit, rank)
            if (grepl("pyramid", piece_side)) {
                height <- self$get_height("pyramid_face", rank=rank)
                if (piece_side == "pyramid_top") {
                    return(sqrt(height^2 - (0.5*width)^2))
                } else {
                    theta <- 2 * asin(0.5 * width / height)
                    return(sin(theta) * height)
                }
            }
            piece <- get_piece(piece_side)
            if (grepl("_top$|_base$", piece_side)) {
                ps <- paste0(piece, "_face")
                return(self$get_height(ps, suit=suit, rank=rank))
            } else if (grepl("_left$|_right$", piece_side)) {
                ps <- paste0(piece, "_face")
                return(self$get_width(ps, suit=suit, rank=rank))
            }
            default <- switch(piece,
                              bit = 0.25,
                              board = 0.25,
                              card = 0.01181102, # 3 mm
                              coin = 0.125,
                              die = width,
                              matchstick = width,
                              pawn = 0.25,
                              saucer = 0.125,
                              suitdie = width,
                              tile = 0.25,
                              0)
            depth <- get_style_element("depth", piece_side, private$cfg, default, suit, rank)
            private$cache[[key]] <- depth
            depth
        },
        as_list = function() private$cfg,
        print = function() {
            for (name in names(private$cfg)) {
                if (is.function(private$cfg[[name]])) {
                    cat(paste0("$", name, " : ", "a function", "\n"))
                } else {
                    cat(paste0("$", name, " : ", private$cfg[[name]]), "\n")
                }
            }
        },
        get_op_grob = function(piece_side, suit, rank,
                               x, y, z, angle, type,
                               width, height, depth,
                               op_scale, op_angle) {
            key <- opt_cache_key(piece_side, suit, rank, "op_grob")
            if (!is.null(private$cache[[key]])) {
                grobFn <- private$cache[[key]]
            } else {
                default_fn <- switch(piece_side,
                                     pyramid_top = basicPyramidTop,
                                     pyramid_face = basicPyramidSide,
                                     pyramid_back = basicPyramidSide,
                                     pyramid_left = basicPyramidSide,
                                     pyramid_right = basicPyramidSide,
                                     basicOpGrob)
                grobFn <- get_style_element("op_grob_fn", piece_side, private$cfg,
                                                default_fn, suit, rank)
                if (self$cache_shadow) private$cache[[key]] <- grobFn
            }
            grobFn(piece_side, suit, rank, self,
                   x, y, z, angle, type, width, height, depth,
                   op_scale, op_angle)
        },
        get_raster = function(piece_side, suit, rank, res=72) {
            grob <- private$get_grob_normal(piece_side, suit, rank)
            width <- self$get_width(piece_side, suit, rank)
            height <- self$get_height(piece_side, suit, rank)
            png_file <- tempfile(fileext=".png")
            on.exit(unlink(png_file))
            current_dev <- grDevices::dev.cur()
            if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
            png(png_file, width=width, height=height, units="in", res=res, bg="transparent")
            grid.draw(grob)
            invisible(grDevices::dev.off())
            as.raster(png::readPNG(png_file))
        },
        get_shadow_fn = function(piece_side, suit, rank) {
            key <- opt_cache_key(piece_side, suit, rank, "shadow")
            if (!is.null(private$cache[[key]])) {
                private$cache[[key]]
            } else {
                default_fn <- get_style_element("shadow_fn", piece_side, private$cfg,
                                                basicShadowGrob, suit, rank)
                grobFn <- switch(piece_side,
                                 pyramid_top = function(...) nullGrob(),
                                 default_fn)
                if (self$cache_shadow) private$cache[[key]] <- grobFn
                grobFn
            }
        },
        rayrender = function(piece_side, suit, rank,
                             x, y, z, angle, axis_x, axis_y,
                             width, height, depth,
                             scale = 1, res = 72) {
            key <- opt_cache_key(piece_side, suit, rank, "rayrender_fn")
            if (!is.null(private$cache[[key]])) {
                rayrender_fn <- private$cache[[key]]
            } else {
                rayrender_fn <- get_style_element("rayrender_fn", piece_side, private$cfg,
                                                  rr_piece_helper, suit, rank)
                if (self$cache_obj_fn) private$cache[[key]] <- rayrender_fn
            }
            rayrender_fn(piece_side, suit, rank, self,
                         x, y, z,
                         angle, axis_x, axis_y,
                         width, height, depth,
                         scale = scale, res = res)
        },
        rgl = function(piece_side, suit, rank,
                       x, y, z, angle, axis_x, axis_y,
                       width, height, depth,
                       scale = 1, res = 72,
                       alpha = 1.0, lit = FALSE,
                       shininess = 50.0, textype = "rgba") {
            key <- opt_cache_key(piece_side, suit, rank, "rgl_fn")
            if (!is.null(private$cache[[key]])) {
                rgl_fn <- private$cache[[key]]
            } else {
                rgl_fn <- get_style_element("shadow_fn", piece_side, private$cfg,
                                                rgl_piece_helper, suit, rank)
                if (self$cache_obj_fn) private$cache[[key]] <- rgl_fn
            }
            rgl_fn(piece_side, suit, rank, self,
                   x, y, z,
                   angle, axis_x, axis_y,
                   width, height, depth,
                   scale = scale, res = res,
                   alpha = alpha, lit = lit,
                   shininess = shininess, textype)
        },
        save_obj = function(piece_side, suit, rank,
                            x, y, z, angle, axis_x, axis_y,
                            width, height, depth,
                            filename = tempfile(fileext = ".obj"), res = 72) {
            key <- opt_cache_key(piece_side, suit, rank, "obj_fn")
            if (!is.null(private$cache[[key]])) {
                obj_fn <- private$cache[[key]]
            } else {
                if (grepl("tile|coin|pawn|matchstick|bit|board|card|saucer", piece_side)) {
                    default_fn <- save_2s_obj
                } else if (grepl("die", piece_side)) {
                    default_fn <- save_die_obj
                } else if (piece_side == "pyramid_top") {
                    default_fn <- save_pt_obj
                } else if (grepl("pyramid", piece_side)) {
                    default_fn <- save_ps_obj
                } else {
                    default_fn <- NULL
                }
                obj_fn <- get_style_element("obj_fn", piece_side, private$cfg, default_fn, suit, rank)
                if (is.null(obj_fn)) {
                    stop("Don't know how to export ", piece_side, " to Wavefront OBJ format.")
                }
                if (self$cache_obj_fn) private$cache[[key]] <- obj_fn
            }
            obj_fn(piece_side, suit, rank, self,
                   x = x, y = y, z = z,
                   angle = angle, axis_x = axis_x, axis_y = axis_y,
                   width = width, height = height, depth = depth,
                   filename = filename, res = res)
        },
        # Deprecated public methods
        get_pictureGrob = function(piece_side, suit, rank) {
            .Deprecated('pp_cfg()$get_grob(piece_side, suit, rank, type = "picture")')
            private$get_grob_picture(piece_side, suit, rank)
        }),
    active = list(
        has_piecepack = function(value) {
            if (missing(value)) {
                return(self$has_coins && self$has_tiles && self$has_pawns && self$has_dice)
            } else {
               if (!is.logical(value)) stop(paste(value, "is not logical"))
                self$has_coins <- value
                self$has_tiles <- value
                self$has_pawns <- value
                self$has_dice  <- value
            }
        }),
    private = list(cfg = NULL, cache = list(),
        get_grob_normal = function(piece_side, suit, rank) {
            suit <- ifelse(has_suit(piece_side), ifelse(is.na(suit), 1, suit), self$i_unsuit)
            suit <- ifelse(suit > self$i_unsuit+1, self$i_unsuit+1, suit)
            rank <- ifelse(has_rank(piece_side), ifelse(is.na(rank), 1, rank), 0)
            key <- opt_cache_key(piece_side, suit, rank, "grob")
            if (!is.null(private$cache[[key]])) {
                private$cache[[key]]
            } else {
                default_grob_fn <- switch(piece_side,
                                          board_face = checkeredBoardGrobFn(8, 8),
                                          board_back = linedBoardGrobFn(8, 8),
                                          card_face = cardGrobFn(-1),
                                          card_back = cardGrobFn(-1),
                                          basicPieceGrob)
                default_fn <- get_style_element("grob_fn", piece_side, private$cfg,
                                                default_grob_fn, suit, rank)
                grob_fn <- switch(piece_side,
                                 die_layoutLF = dieLayoutGrobLF,
                                 die_layoutRF = dieLayoutGrobRF,
                                 suitdie_layoutLF = suitdieLayouGrobtLF,
                                 suitdie_layoutRF = suitdieLayoutGrobRF,
                                 suitrankdie_layoutLF = suitrankdieLayoutGrobLF,
                                 suitrankdie_layoutRF = suitrankdieLayoutGrobRF,
                                 pawn_layout = pawnLayoutGrob,
                                 preview_layout = previewLayoutGrob,
                                 pyramid_layout = pyramidLayoutGrob,
                                 pyramid_top = pyramidTopGrob,
                                 default_fn)
                if (is.character(grob_fn))
                    grob_fn <- match.fun(grob_fn)
                grob <- grob_fn(piece_side, suit, rank, self)
                if (self$cache_grob) private$cache[[key]] <- grob
                grob
            }
        },
        get_grob_picture = function(piece_side, suit, rank) {
            grob <- private$get_grob_normal(piece_side, suit, rank)
            width <- self$get_width(piece_side, suit, rank)
            height <- self$get_height(piece_side, suit, rank)
            as_picture(grob, width, height)
        })
)
