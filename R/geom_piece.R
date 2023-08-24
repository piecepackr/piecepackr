#' Draw board game pieces with ggplot2
#'
#' `geom_piece()` creates a `ggplot2` geom.
#' `aes_piece()` takes a data frame and generates
#' an appropriate `ggplot2::aes()` mapping.
#'
#' `geom_piece()` requires a fixed scale coordinate system with an aspect
#' ratio of 1 as provided by `ggplot2::coord_fixed()`.
#' `geom_piece()` also requires that `cfg` is a character vector (and not a `pp_cfg()` object).
#' In particular if using `op_transform()` one should set its argument `cfg_class = "character"`
#' if intending for use with `geom_piece()`.
#'
#' @section Aesthetics:
#' `geom_piece()` understands the following aesthetics (required aesthetics are in bold).
#' See [pieceGrob()] for more details.
#'
#' * **`x`**
#' * **`y`**
#' * `z`
#' * `piece_side`
#' * `rank`
#' * `suit`
#' * `cfg`
#' * `width`
#' * `height`
#' * `depth`
#' * `angle`
#' * `scale`
#' * `type`
#'
#' @inheritParams ggplot2::geom_rect
#' @inheritParams pieceGrob
#' @inheritParams render_piece
#' @param  ... Aesthetics, used to set an aesthetic to a fixed value.
#' @seealso `geom_piece()` is a wrapper around [pieceGrob()].
#'         [scale_x_piece()] and [scale_y_piece()] are wrappers
#'         around [ggplot2::scale_x_continuous()] and [ggplot2::scale_y_continuous()]
#'         with better defaults for board game diagrams.
#' @examples
#' if (require("ggplot2", quietly = TRUE) && require("tibble", quietly = TRUE)) {
#'   envir <- game_systems("sans")
#'   df_board <- tibble(piece_side = "board_face", suit = 3, rank = 8,
#'                  x = 4.5, y = 4.5)
#'   df_w <- tibble(piece_side = "bit_face", suit = 6, rank = 1,
#'                  x = rep(1:8, 2), y = rep(1:2, each=8))
#'   df_b <- tibble(piece_side = "bit_face", suit = 1, rank = 1,
#'                  x = rep(1:8, 2), y = rep(7:8, each=8))
#'   df <- rbind(df_board, df_w, df_b)
#'   # 2D example
#'   # `cfg` must be a character vector for `geom_piece()`
#'   ggplot(df, aes_piece(df)) +
#'       geom_piece(cfg = "checkers1", envir = envir) +
#'       coord_fixed() +
#'       scale_x_piece() +
#'       scale_y_piece() +
#'       theme_minimal(28) +
#'       theme(panel.grid = element_blank())
#' }
#' if (require("ggplot2", quietly = TRUE) && require("tibble", quietly = TRUE)) {
#'   # 3D "oblique" projection example
#'   # `cfg_class` must be "character" when using with `geom_piece()`
#'   df3d <- op_transform(df, cfg = "checkers1", envir = envir,
#'                        op_angle = 45, cfg_class = "character")
#'   ggplot(df3d, aes_piece(df3d)) +
#'       geom_piece(cfg = "checkers1", envir = envir,
#'                  op_angle = 45, op_scale = 0.5) +
#'       coord_fixed() +
#'       theme_void()
#' }
#' @export
geom_piece <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       envir = getOption("piecepackr.envir", piecepackr::game_systems()),
                       op_scale = getOption("piecepackr.op_scale", 0),
                       op_angle = getOption("piecepackr.op_angle", 45),
                       inherit.aes = TRUE) {
    assert_suggested("ggplot2")

    # assert cfg is character
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPiece,
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = list(
            envir = envir,
            op_scale = op_scale,
            op_angle = op_angle,
            ...
        )
    )
}

#' ggplot2 game diagram scales
#'
#' `scale_x_piece()` and `scale_y_piece()` are wrappers
#' around [ggplot2::scale_x_continuous()] and
#' [ggplot2::scale_y_continuous()] with "better"
#' defaults for board game diagrams.
#' `label_letter()` labels breaks with letters
#' and `label_counting()` labels breaks with positive integers
#' to more easily generate (i.e. chess) algebraic notation coordinates.
#' `breaks_counting()` generates breaks of just the positive integers within the limits.
#' @param ... Passed to [ggplot2::scale_x_continuous()] or [ggplot2::scale_y_continuous()].
#' @inheritParams ggplot2::scale_x_continuous
#' @examples
#' if (require("ggplot2", quietly = TRUE) && require("tibble", quietly = TRUE)) {
#'   envir <- game_systems("sans")
#'   df_board <- tibble(piece_side = "board_face", suit = 3, rank = 8,
#'                  x = 4.5, y = 4.5)
#'   df_w <- tibble(piece_side = "bit_face", suit = 6, rank = 1,
#'                  x = rep(1:8, 2), y = rep(1:2, each=8))
#'   df_b <- tibble(piece_side = "bit_face", suit = 1, rank = 1,
#'                  x = rep(1:8, 2), y = rep(7:8, each=8))
#'   df <- rbind(df_board, df_w, df_b)
#'
#'   # `cfg` must be a character vector for `geom_piece()`
#'   ggplot(df, aes_piece(df)) +
#'       geom_piece(cfg = "checkers1", envir = envir) +
#'       coord_fixed() +
#'       scale_x_piece() +
#'       scale_y_piece() +
#'       theme_minimal(28) +
#'       theme(panel.grid = element_blank())
#' }
#' @return `scale_x_piece()` and `scale_y_piece()` return ggplot2 scale objects.
#'         `label_letter()` and `label_counting()` return functions suitable for use with the `labels` scale argument.
#'         `breaks_counting()` returns a function suitable for use with the `breaks` scale argument.
#' @rdname scale_piece
#' @export
scale_x_piece <- function(..., name = NULL,
                          breaks = breaks_counting(),
                          minor_breaks = NULL,
                          labels = label_letter()) {
    assert_suggested("ggplot2")
    ggplot2::scale_x_continuous(..., name = name,
                                breaks = breaks,
                                minor_breaks = minor_breaks,
                                labels = labels)
}

#' @rdname scale_piece
#' @export
scale_y_piece <- function(..., name = NULL,
                          breaks = breaks_counting(),
                          minor_breaks = NULL,
                          labels = label_counting()) {
    assert_suggested("ggplot2")
    ggplot2::scale_y_continuous(..., name = name,
                                breaks = breaks,
                                minor_breaks = minor_breaks,
                                labels = labels)
}

#' @rdname scale_piece
#' @export
label_letter <- function() {
    function(x) letters[seq_along(x)]
}

#' @rdname scale_piece
#' @export
label_counting <- function() {
    function(x) as.character(seq_along(x))
}

#' @rdname scale_piece
#' @export
breaks_counting <- function() {
    function(x) {
        seq.int(from = max(1L, ceiling(x[1])),
                to = floor(x[2]),
                by = 1L)
    }
}

# GeomPiece is defined in `.onLoad()` in `hooks.R` so {ggplot2} can be Suggests instead of Imports
# Because it is defined in `.onLoad()` {covr} can't see it even though it is implicitly tested
# in `geom_piece()` tests
create_GeomPiece <- function() { # nocov start
    ggplot2::ggproto(
    "GeomPiece",
    ggplot2::Geom,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(
        piece_side = "tile_back",
        rank = 1L,
        suit = 1L,
        cfg = "piecepack",
        z = NA,
        width = NA,
        height = NA,
        depth = NA,
        angle = 0,
        scale = 1,
        type = "normal"
    ),
    draw_key = function(self, data, params, size) {
        # data Single row data frame containing scaled aesthetics
        # params A list of additional parameters supplied to the geom
        # size Width and height of key in mm
        ggplot2::zeroGrob()
    },
    draw_panel = function(self, data, panel_params, coord,
                          envir, op_scale, op_angle) {
        if (coord$is_free()) {
            abort("'geom_piece()' will not work correctly if not using a fixed scale.")
        }
        if (hasName(coord, "ratio") && coord$ratio != 1) {
            abort("'geom_piece()' will not work correctly if not using an aspect ratio of 1.")
        }
        data <- gg_impute_missing_data(data, envir, panel_params)
        coord <- coord$transform(data, panel_params)
        grob_piece(coord, panel_params, envir, op_scale, op_angle)
    },
    setup_data = function(data, params) {
        l <- aabb_piece(data, envir = params$envir,
                        op_scale = params$op_scale,
                        op_angle = params$op_angle)
        data$xmin <- l$x_op[1]
        data$xmax <- l$x_op[2]
        data$ymin <- l$y_op[1]
        data$ymax <- l$y_op[2]
        data
    }
  )
} # nocov end

#' @rdname geom_piece
#' @export
aes_piece <- function(df) {
    assert_suggested("ggplot2")
    stopifnot(hasName(df, "x"), hasName(df, "y"))
    mapping <- ggplot2::aes()
    aesthetics <- c("x", "y", "z", "piece_side", "rank", "suit", "cfg",
                    "width", "height", "depth", "angle", "scale", "type")
    for (a in aesthetics) {
        if (hasName(df, a)) mapping[[a]] <- as.name(a)
    }
    mapping
}

grob_piece <- function(coord, panel_params, envir, op_scale, op_angle) {
    grid::gTree(coord = coord,
                panel_params = panel_params,
                envir = envir,
                op_scale = op_scale, op_angle = op_angle,
                cl = "geom_piece_grob")
}

#' @import grid
#' @export
makeContent.geom_piece_grob <- function(x) {
    grob <- piecepackr::pmap_piece(x$df, piecepackr::pieceGrob,
                           default.units = "inches", draw = FALSE,
                           op_scale = x$op_scale, op_angle = x$op_angle,
                           envir = x$envir, name = "geom_piece")
    setChildren(x, gList(grob))
}

#' @export
makeContext.geom_piece_grob <- function(x) {
    x_width_native <- diff(x$panel_params$x$continuous_range)
    x_width_in <- grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
    gp_scale <- x_width_in / x_width_native
    x$gp <- grid::gpar(cex = gp_scale, lex = gp_scale)
    x$df <- gg_to_in(x$coord, gp_scale)
    x
}

# nolint start
# CoordPiece <- ggplot2::ggproto(
#   "CoordPiece",
#   ggplot2::CoordFixed
# )
# coord_piece <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
#   ggproto(NULL, CoordPiece,
#     limits = list(x = xlim, y = ylim),
#     ratio = 1,
#     expand = expand,
#     clip = clip
#   )
# }
# coord_piece() algebraic?
# nolint end

gg_to_in <- function(df, scale) {
    df$x <- convertX(unit(df$x, "npc"), "in", valueOnly = TRUE)
    df$y <- convertY(unit(df$y, "npc"), "in", valueOnly = TRUE)
    df$z <- scale * df$z
    df$width <- scale * df$width
    df$height <- scale * df$height
    df$depth <- scale * df$depth
    df
}

gg_impute_missing_data <- function(data, envir, panel_params) {
    # impute missing width, height, depth, and z
    i_w <- which(is.na(data$width))
    data$width[i_w] <- purrr::pmap_dbl(data[i_w,], gg_get_width, envir = envir)
    i_h <- which(is.na(data$height))
    data$height[i_h] <- purrr::pmap_dbl(data[i_h,], gg_get_height, envir = envir)
    i_d <- which(is.na(data$depth))
    data$depth[i_d] <- purrr::pmap_dbl(data[i_d,], gg_get_depth, envir = envir)
    i_z <- which(is.na(data$z))
    data$z[i_z] <- 0.5 * data$depth[i_d]
    data
}
gg_get_width <- function(piece_side, suit, rank, cfg, ..., envir) {
    envir[[cfg]]$get_width(piece_side, suit, rank)
}
gg_get_height <- function(piece_side, suit, rank, cfg, ..., envir) {
    envir[[cfg]]$get_height(piece_side, suit, rank)
}
gg_get_depth <- function(piece_side, suit, rank, cfg, ..., envir) {
    envir[[cfg]]$get_depth(piece_side, suit, rank)
}
