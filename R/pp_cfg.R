#' Configuration list R6 object
#'
#' `pp_cfg()` and `as_pp_cfg()` create piecepack configuration list R6 objects.
#' `is_pp_cfg()` returns `TRUE` if object is a piecepack configuration list R6 object.
#' `as.list()` will convert it into a list.
#'
#' `pp_cfg` R6 class objects serve the following purposes:
#'
#' * Customize the appearance of pieces drawn by `grid.piece()`.
#' * Speed up the drawing of graphics through use of caching.
#' * Allow the setting and querying of information about the board game components
#'   that maybe of use to developers:
#'
#'   + Number of suits
#'   + Number of ranks
#'   + Suit colors
#'   + Which types of components are included and/or properly supported
#'   + What would be a good color to use when adding annotations on top of these components.
#'   + Title, Description, Copyright, License, and Credit metadata
#'
#' @section `pp_cfg` R6 Class Method Arguments:\describe{
#'   \item{`piece_side`}{A string with piece and side separated by a underscore e.g. "coin_face".}
#'   \item{`suit`}{Number of suit (starting from 1).}
#'   \item{`rank`}{Number of rank (starting from 1).}
#'   \item{`type`}{Which type of grob to return, either `"normal"`, `"picture"`, `"raster"`, or `"transformation"`.}
#'   \item{`scale`}{"scale" factor}
#'   \item{`alpha`}{"alpha" value}
#' }
#'
#' @section `pp_cfg` R6 Class Methods:\describe{
#'   \item{`get_grob()`}{Returns a \code{grid} \dQuote{grob} for drawing the piece.}
#'   \item{`get_piece_opt()`}{Returns a list with info useful for drawing the piece.}
#'   \item{`get_suit_color()`}{Returns the suit colors.}
#'   \item{`get_width()`, `get_height()`, `get_depth()`}{
#'         Dimensions (of the bounding cube) of the piece in inches}
#' }
#'
#' @section \code{pp_cfg} R6 Class Fields and Active Bindings:\describe{
#'   \item{`annotation_color`}{Suggestion of a good color to annotate with}
#'   \item{`cache`}{Cache object which stores intermediate graphical calculations.
#'                  Default is a memory-cache that does not prune.
#'                  This can be replaced by another cache that
#'                  implements the cache API used by the `cachem` package}
#'   \item{`cache_grob`}{Whether we should cache (2D) grobs}
#'   \item{`cache_grob_with_bleed_fn`}{Whether we should cache the grob with bleed functions}
#'   \item{`cache_piece_opt`}{Whether we should cache piece opt information}
#'   \item{`cache_op_fn`}{Whether we should cache the oblique projection functions}
#'   \item{`cache_obj_fn`}{Whether we should cache any 3D rendering functions}
#'   \item{`copyright`}{Design copyright information}
#'   \item{`credit`}{Design credits}
#'   \item{`description`}{Design description}
#'   \item{`fontfamily`}{Main font family}
#'   \item{`has_bits`}{Whether we should assume this supports "bit" pieces}
#'   \item{`has_boards`}{Whether we should assume this supports "board" pieces}
#'   \item{`has_cards`}{Whether we should assume this supports "card" pieces}
#'   \item{`has_coins`}{Whether we should assume this supports "coin" pieces}
#'   \item{`has_dice`}{Whether we should assume this supports "die" pieces}
#'   \item{`has_matchsticks`}{Whether we should assume this supports "matchstick" pieces}
#'   \item{`has_pawns`}{Whether we should assume this supports "pawn" pieces}
#'   \item{`has_piecepack`}{Binding which simultaneously checks/sets
#'                         `has_coins`, `has_tiles`, `has_pawns`, `has_dice`}
#'   \item{`has_pyramids`}{Whether we should assume this supports "pyramid" pieces}
#'   \item{`has_saucers`}{Whether we should assume this supports "saucer" pieces}
#'   \item{`has_tiles`}{Whether we should assume this supports "tile" pieces}
#'   \item{`spdx_id`}{SPDX Identifier for graphical design license.
#'                    See \url{https://spdx.org/licenses/} for full list.}
#'   \item{`title`}{Design title}
#' }
#'
#' @section Defunct `pp_cfg` R6 Class attributes which have been removed:\describe{
#'    \item{`cache_shadow`}{Use `cache_op_fn` instead}
#'    \item{`i_unsuit`}{Instead add `1L` to `n_suits`}
#'    \item{`get_pictureGrob()`}{Use `get_grob(..., type = "picture")` instead}
#'    \item{`get_shadow_fn`}{`get_op_grob()` returns complete oblique projection grob}
#' }
#'
#' @seealso [game_systems()] for functions that return configuration list
#'  objects for several game systems.
#'  <https://trevorldavis.com/piecepackr/configuration-lists.html> for more details
#'  about `piecepackr` configuration lists.
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
#'  \donttest{# May take more than 5 seconds on CRAN servers
#'  # `pp_cfg()` objects use a cache to speed up repeated drawing
#'  pdf(tempfile(fileext = ".pdf"))
#'  cfg <- list()
#'  system.time(replicate(100, grid.piece("tile_back", 4, 4, cfg)))
#'  cfg <- pp_cfg(list())
#'  system.time(replicate(100, grid.piece("tile_back", 4, 4, cfg)))
#'  invisible(dev.off())
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
is_pp_cfg <- function(cfg) inherits(cfg, "pp_cfg")

#' @rdname pp_cfg
#' @export
as_pp_cfg <- pp_cfg

#' @export
as.list.pp_cfg <- function(x, ...) {
    x$as_list()
}

default_lacks_suit <- c("tile_back", "saucer_back", "coin_face", "card_back")
default_lacks_rank <- c("tile_back", "coin_back", "card_back",
                        "pawn_face", "pawn_back", "belt_face",
                        "saucer_face", "saucer_back", "suitdie_face")
impute_rank <- function(piece_side, rank, cfg) {
    ifelse(piece_side %in% cfg$lacks_rank,
           cfg$n_ranks+1L,
           ifelse(is.na(rank), 1L, min(rank, cfg$n_ranks+2L)))
}
impute_suit <- function(piece_side, suit, cfg) {
    ifelse(piece_side %in% cfg$lacks_suit,
           cfg$n_suits+1L,
           ifelse(is.na(suit), 1L, min(suit, cfg$n_suits+2L)))
}

Config <- R6Class("pp_cfg",
    public = list(
        cache = NULL,
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
        initialize = function(cfg=list()) {
            warn_cfg(cfg)
            self$cache <- Cache$new()
            private$cfg <- cfg
            private$cfg$n_suits <- cfg$n_suits %||% (length(get_suit_symbols(cfg=cfg, expand=FALSE)) - 1L)
            private$cfg$n_ranks <- cfg$n_ranks %||% length(get_rank_symbols(cfg=cfg, expand=FALSE))
            private$cfg$lacks_rank <- cleave2(cfg$lacks_rank %||% default_lacks_rank)
            private$cfg$lacks_suit <- cleave2(cfg$lacks_suit %||% default_lacks_suit)
            # so different cfg objects can share the same cache (with high probability)
            private$prefix <- as.hexmode(sample.int(2147483647L, 1L, useHash=TRUE))
        },
        get_grob = function(piece_side, suit, rank,
                            type = c("normal", "picture", "raster", "transformation"),
                            ...) {
            type <- match.arg(type)
            grob <- switch(type,
                           normal = private$get_grob_normal(piece_side, suit, rank),
                           picture = private$get_grob_picture(piece_side, suit, rank),
                           raster = to_rasterGrob(self$get_raster(piece_side, suit, rank, ...)),
                           transformation = private$get_grob_transformation(piece_side, suit, rank))
            grob
        },
        get_grob_with_bleed = function(piece_side, suit, rank) {
            rank <- impute_rank(piece_side, rank, self)
            suit <- impute_suit(piece_side, suit, self)
            key <- private$opt_cache_key(piece_side, suit, rank, "grob_with_bleed_fn")
            grob_fn <- self$cache$get(key, key_missing())
            if (is.key_missing(grob_fn)) {
                default_grob_fn <- basicGrobWithBleed
                grob_fn <- get_style_element("grob_with_bleed_fn", piece_side, private$cfg,
                                             default_grob_fn, suit, rank)
                if (is.character(grob_fn))
                    grob_fn <- match.fun(grob_fn)
                if (self$cache_grob_with_bleed_fn) self$cache$set(key, grob_fn)
            }
            args <- list(piece_side = piece_side, suit = suit, rank = rank,
                         cfg = self)
            do.call(grob_fn, args)
        },
        get_piece_opt = function(piece_side, suit=NA, rank=NA) {
            if (is.na(rank)) rank <- 1L
            if (is.na(suit)) suit <- self$n_suits + 1L
            key <- private$opt_cache_key(piece_side, suit, rank, "piece_opt")
            opt <- self$cache$get(key, key_missing())
            if (is.key_missing(opt)) {
                opt <- get_piece_opt_helper(piece_side, suit, rank, private$cfg)
                if (self$cache_piece_opt) self$cache$set(key, opt)
            }
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
            key <- private$opt_cache_key(piece_side, suit, rank, "width")
            width <- self$cache$get(key, key_missing())
            if (!is.key_missing(width))
                return(width)
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
                              abort(paste("Don't know width of piece", piece)))
            width <- get_style_element("width", piece_side, private$cfg, default, suit, rank)
            self$cache$set(key, width)
            width
        },
        get_height = function(piece_side, suit=1, rank=1) {
            key <- private$opt_cache_key(piece_side, suit, rank, "height")
            height <- self$cache$get(key, key_missing())
            if (!is.key_missing(height))
                return(height)
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
                              abort(paste("Don't know height of piece", piece))) #nocov
            height <- get_style_element("height", piece_side, private$cfg, default, suit, rank)
            self$cache$set(key, height)
            height
        },
        get_depth = function(piece_side, suit=1, rank=1) {
            key <- private$opt_cache_key(piece_side, suit, rank, "depth")
            depth <- self$cache$get(key, key_missing())
            if (!is.key_missing(depth))
                return(depth)
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
            self$cache$set(key, depth)
            depth
        },
        as_list = function() private$cfg,
        print = function() {
            for (name in sort(names(private$cfg))) {
                if (is.function(private$cfg[[name]])) {
                    cat(paste0("$", name, " : ", "a function", "\n"))
                } else {
                    cat(paste0("$", name, " : ", paste(private$cfg[[name]], collapse=",")), "\n")
                }
            }
        },
        get_op_grob = function(piece_side, suit, rank,
                               x, y, z, angle, type,
                               width, height, depth,
                               op_scale, op_angle) {
            key <- private$opt_cache_key(piece_side, suit, rank, "op_grob")
            grob_fn <- self$cache$get(key, key_missing())
            if (is.key_missing(grob_fn)) {
                default_fn <- switch(piece_side,
                                     die_face = basicDieGrob,
                                     pyramid_top = basicPyramidTop,
                                     pyramid_face = basicPyramidSide,
                                     pyramid_back = basicPyramidSide,
                                     pyramid_left = basicPyramidSide,
                                     pyramid_right = basicPyramidSide,
                                     basicTokenGrob)
                grob_fn <- get_style_element("op_grob_fn", piece_side, private$cfg,
                                                default_fn, suit, rank)
                if (self$cache_op_fn) self$cache$set(key, grob_fn)
            }
            args <- list(piece_side = piece_side,
                         suit = suit,
                         rank = rank,
                         cfg = self,
                         x = x, y = y , z = z,
                         angle = angle, type = type,
                         width = width, height = height, depth = depth,
                         op_scale = op_scale, op_angle = op_angle)
            grob <- do.call(grob_fn, args)
        },
        get_raster = function(piece_side, suit, rank, res=72) {
            grob <- private$get_grob_normal(piece_side, suit, rank)
            width <- self$get_width(piece_side, suit, rank)
            height <- self$get_height(piece_side, suit, rank)
            png_file <- tempfile(fileext=".png")
            on.exit(unlink(png_file))
            current_dev <- grDevices::dev.cur()
            if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
            args <- list(filename=png_file, width=width, height=height,
                         units="in", res=res, bg="transparent")
            if (capabilities("cairo"))
                args$type <- "cairo"
            do.call(png, args)
            grid.draw(grob)
            invisible(grDevices::dev.off())
            as.raster(png::readPNG(png_file))
        },
        rayrender = function(piece_side, suit, rank,
                             x, y, z, angle, axis_x, axis_y,
                             width, height, depth,
                             scale = 1, res = 72) {
            key <- private$opt_cache_key(piece_side, suit, rank, "rayrender_fn")
            rayrender_fn <- self$cache$get(key, key_missing())
            if (is.key_missing(rayrender_fn)) {
                rayrender_fn <- get_style_element("rayrender_fn", piece_side, private$cfg,
                                                  rr_piece_helper, suit, rank)
                if (self$cache_obj_fn) self$cache$set(key, rayrender_fn)
            }
            args <- list(piece_side = piece_side,
                         suit = suit, rank = rank, cfg = self,
                         x = x, y = y, z = z,
                         angle = angle, axis_x = axis_x, axis_y = axis_y,
                         width = width, height = height, depth = depth,
                         scale = scale, res = res)
            do.call(rayrender_fn, args)
        },
        rayvertex = function(piece_side, suit, rank,
                             x, y, z, angle, axis_x, axis_y,
                             width, height, depth,
                             scale = 1, res = 72) {
            key <- private$opt_cache_key(piece_side, suit, rank, "rayvertex_fn")
            rayvertex_fn <- self$cache$get(key, key_missing())
            if (is.key_missing(rayvertex_fn)) {
                rayvertex_fn <- get_style_element("rayvertex_fn", piece_side, private$cfg,
                                                  rv_piece_helper, suit, rank)
                if (self$cache_obj_fn) self$cache$set(key, rayvertex_fn)
            }
            args <- list(piece_side = piece_side,
                         suit = suit, rank = rank, cfg = self,
                         x = x, y = y, z = z,
                         angle = angle, axis_x = axis_x, axis_y = axis_y,
                         width = width, height = height, depth = depth,
                         scale = scale, res = res)
            do.call(rayvertex_fn, args)
        },
        rgl = function(piece_side, suit, rank,
                       x, y, z,
                       angle, axis_x, axis_y,
                       width, height, depth,
                       scale = 1, res = 72,
                       alpha = 1.0, lit = FALSE,
                       shininess = 50.0, textype = "rgba") {
            key <- private$opt_cache_key(piece_side, suit, rank, "rgl_fn")
            rgl_fn <- self$cache$get(key, key_missing())
            if (is.key_missing(rgl_fn)) {
                rgl_fn <- get_style_element("rgl_fn", piece_side, private$cfg,
                                                rgl_piece_helper, suit, rank)
                if (self$cache_obj_fn) self$cache$set(key, rgl_fn)
            }
            args <- list(piece_side = piece_side,
                         suit = suit, rank = rank, cfg = self,
                         x = x, y = y, z = z,
                         angle = angle, axis_x = axis_x, axis_y = axis_y,
                         width = width, height = height, depth = depth,
                         scale = scale, res = res, alpha = alpha,
                         lit = lit, shininess = shininess, textype = textype)
            do.call(rgl_fn, args)
        },
        save_obj = function(piece_side, suit, rank,
                            x, y, z, angle, axis_x, axis_y,
                            width, height, depth,
                            filename = tempfile(fileext = ".obj"), res = 72) {
            key <- private$opt_cache_key(piece_side, suit, rank, "obj_fn")
            obj_fn <- self$cache$get(key, key_missing())
            if (is.key_missing(obj_fn)) {
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
                    abort(paste("Don't know how to export ", piece_side, " to Wavefront OBJ format."))
                }
                if (self$cache_obj_fn) self$cache$set(key, obj_fn)
            }
            args <- list(piece_side = piece_side,
                         suit = suit, rank = rank, cfg = self,
                         x = x, y = y, z = z,
                         angle = angle, axis_x = axis_x, axis_y = axis_y,
                         width = width, height = height, depth = depth,
                         filename = filename, res = res)
            do.call(obj_fn, args)
        }),
    active = list(
        annotation_color = function(value) {
            if (missing(value)) {
                private$cfg$annotation_color %||% "black"
            } else {
                stopifnot(is.character(value))
                private$cfg$annotation_color <- value
            }
        },
        cache_grob = function(value) {
            if (missing(value)) {
                private$cache_grob_bool
            } else {
                private$cache_type(value, "cache_grob_bool", "grob$")
            }
        },
        cache_grob_with_bleed_fn = function(value) {
            if (missing(value)) {
                private$cache_grob_with_bleed_fn_bool
            } else {
                private$cache_type(value, "cache_grob_with_bleed_fn_bool", "grob_with_bleed_fn$")
            }
        },
        cache_obj_fn = function(value) {
            if (missing(value)) {
                private$cache_obj_fn_bool
            } else {
                private$cache_type(value, "cache_obj_fn_bool", "obj_fn$|rgl_fn$|rayrender_fn$")
            }
        },
        cache_piece_opt = function(value) {
            if (missing(value)) {
                private$cache_piece_opt_bool
            } else {
                private$cache_type(value, "cache_piece_opt_bool", "piece_opt$")
            }
        },
        cache_op_fn = function(value) {
            if (missing(value)) {
                private$cache_op_fn_bool
            } else {
                private$cache_type(value, "cache_op_fn_bool", "shadow$|op_grob_fn$")
            }
        },
        coin_arrangement = function(value) {
            if (missing(value)) {
                as.numeric(private$cfg$coin_arrangement %||% 180)
            } else {
                private$cfg$coin_arrangement <- value
            }
        },
        copyright = function(value) {
            if (missing(value)) {
                private$cfg$copyright
            } else {
                stopifnot(is.character(value))
                private$cfg$copyright <- value
            }
        },
        credit = function(value) {
            if (missing(value)) {
                private$cfg$credit
            } else {
                stopifnot(is.character(value))
                private$cfg$credit <- value
            }
        },
        description = function(value) {
            if (missing(value)) {
                private$cfg$description
            } else {
                stopifnot(is.character(value))
                private$cfg$description <- value
            }
        },
        die_arrangement = function(value) {
            if (missing(value)) {
                private$cfg$die_arrangement %||% "counter_down"
            } else {
                stopifnot(is.character(value))
                private$cfg$die_arrangement <- value
            }
        },
        fontfamily = function(value) {
            if (missing(value))
                private$cfg$fontfamily %||% "sans"
            else
                warn("Must set 'fontfamily' at initialization")
        },
        has_piecepack = function(value) {
            if (missing(value)) {
                self$has_coins && self$has_tiles && self$has_pawns && self$has_dice
            } else {
                stopifnot(is.logical(value))
                self$has_coins <- value
                self$has_tiles <- value
                self$has_pawns <- value
                self$has_dice  <- value
            }
        },
        lacks_rank = function(value) {
            if (missing(value))
                private$cfg$lacks_rank
            else
                warn("Must set 'lacks_rank' at initialization")
        },
        lacks_suit = function(value) {
            if (missing(value))
                private$cfg$lacks_suit
            else
                warn("Must set 'lacks_suit' at initialization")
        },
        n_ranks = function(value) {
            if (missing(value))
                private$cfg$n_ranks
            else
                warn("Must set 'n_ranks' at initialization")
        },
        n_suits = function(value) {
            if (missing(value))
                private$cfg$n_suits
            else
                warn("Must set 'n_suits' at initialization")
        },
        spdx_id = function(value) {
            if (missing(value)) {
                private$cfg$spdx_id
            } else {
                stopifnot(is.character(value))
                check_spdx_id(value)
                private$cfg$spdx_id <- value
            }
        },
        title = function(value) {
            if (missing(value)) {
                private$cfg$title
            } else {
                stopifnot(is.character(value))
                private$cfg$title <- value
            }
        }),
    private = list(cfg = NULL, prefix = NULL,
                   cache_grob_bool = TRUE,
                   cache_grob_with_bleed_fn_bool = TRUE,
                   cache_obj_fn_bool = TRUE,
                   cache_piece_opt_bool = TRUE,
                   cache_op_fn_bool = TRUE,
                   n_suits_val = NULL,
                   n_ranks_val = NULL,
        get_grob_normal = function(piece_side, suit, rank) {
            rank <- impute_rank(piece_side, rank, self)
            suit <- impute_suit(piece_side, suit, self)
            key <- private$opt_cache_key(piece_side, suit, rank, "grob")
            grob <- self$cache$get(key, key_missing())
            if (is.key_missing(grob)) {
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

                args <- list(piece_side = piece_side,
                             suit = suit, rank = rank, cfg = self)
                grob <- do.call(grob_fn, args)
                if (!inherits(grob, "pp_grobCoords"))
                    class(grob) <- c("pp_grobCoords", class(grob))
                if (self$cache_grob) self$cache$set(key, grob)
            }
            grob
        },
        get_grob_picture = function(piece_side, suit, rank) {
            grob <- private$get_grob_normal(piece_side, suit, rank)
            width <- self$get_width(piece_side, suit, rank)
            height <- self$get_height(piece_side, suit, rank)
            as_picture(grob, width, height)
        },
        get_grob_transformation = function(piece_side, suit, rank) {
            grob <- private$get_grob_normal(piece_side, suit, rank)
            width <- self$get_width(piece_side, suit, rank)
            height <- self$get_height(piece_side, suit, rank)
            vp <- viewport(width = inch(width), height = inch(height))
            affiner::affineGrob(grob, vp_define = vp)
        },
        opt_cache_key = function(piece_side, suit, rank, type) {
            paste(private$prefix, piece_side, suit, rank, type, sep="-")
        },
        cache_type = function(value, private_key, cache_key) {
            if (isTRUE(value)) {
                private[[private_key]] <- TRUE
            } else if (isFALSE(value)) {
                private[[private_key]] <- FALSE
                keys <- grep(cache_key, self$cache$keys(), value = TRUE)
                for (key in keys) self$cache$remove(key)
            } else {
                abort("value was not a boolean")
            }
        })
)
