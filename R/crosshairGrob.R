#' Crosshair Grob
#'
#' `grid.crosshair()` draws \dQuote{crosshair(s)} to the active graphics device.
#' `crosshairGrob()` is its grid grob counterpart.
#' Intended for use in adding crosshairs at the corners of
#' game pieces in print-and-play layouts.
#'
#' @inheritParams pieceGrob
#' @param ch_width Width/height of `ch_grob`'s viewport.
#' @param ch_grob Crosshair grob.  Will be drawn in each corner of the piece in a viewport with `ch_width` width and `ch_width` height.
#'                `segmentsCrosshairGrob()` simply wraps [grid::segmentsGrob()] while
#'                `squaresCrosshairGrob()` wraps [grid::rectGrob()] and alternates
#'                 black/white squares for visibility on both light and dark backgrounds.
#' @return A grid grob.
#' @examples
#' if (requireNamespace("grid", quietly = TRUE) &&
#'     piecepackr:::device_supports_unicode()) {
#'   cfg <- pp_cfg(list(border_color = NA))
#'   grid::grid.newpage()
#'   df <- data.frame(piece_side = "tile_face", suit = 2, rank = 2,
#'                    x = 2, y = 2, angle = 0,
#'                    stringsAsFactors = FALSE)
#'   pmap_piece(df, grid.piece, cfg = cfg, default.units = "in")
#'   pmap_piece(df, grid.crosshair, cfg = cfg, default.units = "in")
#' }
#' if (requireNamespace("grid", quietly = TRUE) &&
#'     piecepackr:::device_supports_unicode()) {
#'   grid::grid.newpage()
#'   pmap_piece(df, grid.piece, cfg = cfg, default.units = "in")
#'   pmap_piece(df, grid.crosshair, cfg = cfg, default.units = "in",
#'              ch_grob = segmentsCrosshairGrob())
#' }
#' @name grid.crosshair
NULL

#' @rdname grid.crosshair
#' @export
crosshairGrob <- function(...,
                         piece_side = "tile_back", suit = NA, rank = NA,
                         cfg = getOption("piecepackr.cfg", pp_cfg()),
                         x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                         angle = 0, width = NA, height = NA, scale = 1,
                         default.units = "npc",
                         envir = getOption("piecepackr.envir"),
                         name = NULL, gp = gpar(), vp = NULL,
                         ch_width = unit(1/6, "in"),
                         ch_grob = squaresCrosshairGrob()) {

    if (is.na(width) || is.na(height)) {
        cfg <- get_cfg(cfg, envir)
        rank <- impute_rank(piece_side, rank, cfg)
        suit <- impute_suit(piece_side, suit, cfg)
        if (is.na(width)) width <- inch(cfg$get_width(piece_side, suit, rank))
        if (is.na(height)) height <- inch(cfg$get_height(piece_side, suit, rank))
    }

    if (is.na(angle))
        angle <- 0
    else
        angle <- angle %% 360

    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    if (!is.unit(width)) width <- unit(width, default.units)
    if (!is.unit(height)) height <- unit(height, default.units)
    if (!is.unit(ch_width)) ch_width <- unit(ch_width, default.units)

    width <- scale * width
    height <- scale * height

    gTree(x=x, y=y, angle=angle,
          width=width, height=height,
          ch_width=ch_width, ch_grob=ch_grob,
          name = name, gp = gp, vp = vp,
          cl = "pp_crosshair")
}

#' @rdname grid.crosshair
#' @param ... `crosshairGrob()` ignores; `grid.crosshair()` passes to `crosshairGrob()`.
#' @export
grid.crosshair <- function(..., draw = TRUE) {
    grob <- crosshairGrob(...)
    if (draw) {
        grid.draw(grob)
        invisible(grob)
    } else {
        grob
    }
}

#' @rdname grid.crosshair
#' @export
segmentsCrosshairGrob <- function(..., gp = gpar()) {
    col <- gp$col %||% "black"
    lwd <- gp$lwd %||% 1
    lty <- gp$lty %||% "solid"

    gp <- gpar(col = col, lwd = lwd, lty = lty)
    gList(segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,
                       gp = gp, default.units = "snpc"),
          segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
                       gp = gp, default.units = "snpc"))
}

#' @rdname grid.crosshair
#' @export
squaresCrosshairGrob <- function(..., gp = gpar()) {
    fill <- gp$fill%||% c("black", "white")
    if (length(fill) == 1L)
        fill <- c(fill, "white")

    gp1 <- gpar(col = NA, fill = fill[1L])
    gp2 <- gpar(col = NA, fill = fill[2L])
    gList(rectGrob(x = c(0.1, 0.3, 0.5, 0.5, 0.5, 0.5, 0.5, 0.7, 0.9),
                   y = c(0.5, 0.5, 0.1, 0.3, 0.5, 0.7, 0.9, 0.5, 0.5), 
                   width = 0.1, height = 0.1, gp = gp1, default.units = "snpc"),
          rectGrob(x = c(0.2, 0.4, 0.5, 0.5, 0.5, 0.5, 0.6, 0.8),
                   y = c(0.5, 0.5, 0.2, 0.4, 0.6, 0.8, 0.5, 0.5),
                   width = 0.1, height = 0.1, gp = gp2, default.units = "snpc"))
}

#' @export
makeContent.pp_crosshair <- function(x) {
    vp_piece <- viewport(x=x$x, y=x$y, angle=x$angle, width=x$width, height=x$height)

    vp_ll <- viewport(x=0, y=0, width=x$ch_width, height=x$ch_width)
    vp_ul <- viewport(x=0, y=1, width=x$ch_width, height=x$ch_width)
    vp_lr <- viewport(x=1, y=0, width=x$ch_width, height=x$ch_width)
    vp_ur <- viewport(x=1, y=1, width=x$ch_width, height=x$ch_width)

    ch_ll <- grobTree(x$ch_grob, name = "crosshair_ll", vp = vpStack(vp_piece, vp_ll))
    ch_ul <- grobTree(x$ch_grob, name = "crosshair_ul", vp = vpStack(vp_piece, vp_ul))
    ch_lr <- grobTree(x$ch_grob, name = "crosshair_lr", vp = vpStack(vp_piece, vp_lr))
    ch_ur <- grobTree(x$ch_grob, name = "crosshair_ur", vp = vpStack(vp_piece, vp_ur))

    gl <- gList(ch_ll, ch_ul, ch_lr, ch_ur)
    setChildren(x, gl)
}
