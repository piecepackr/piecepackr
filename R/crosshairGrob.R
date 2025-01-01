#' Draw crosshairs with grid
#'
#' `grid.crosshair()` draws crosshairs at the corners of a rectangular area.
#' `crosshairGrob()` is its grid grob counterpart.
#' They are intended for use in adding crosshairs at the corners of
#' game pieces in print-and-play layouts.
#'
#' One can use the lower level `segmentsCrosshairGrob()` and `squaresCrosshairGrob()`
#' (which can be drawn with [grid::grid.draw()])
#' to add individual crosshairs to specified (x,y) locations.
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

    if (is_angle(angle))
        angle <- as.double(angle, "degrees")

    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    if (!is.unit(width)) width <- unit(width, default.units)
    if (!is.unit(height)) height <- unit(height, default.units)
    if (!is.unit(ch_width)) ch_width <- unit(ch_width, default.units)

    width <- scale * width
    height <- scale * height

    gTree(piece_side=piece_side, suit=suit, rank=rank, cfg=cfg,
          x=x, y=y, angle=angle,
          width=width, height=height,
          default.units=default.units, envir=envir,
          scale=scale,
          ch_width=ch_width, ch_grob=ch_grob,
          name = name, gp = gp, vp = vp,
          cl = "pp_crosshair")
}

#' @export
makeContent.pp_crosshair <- function(x) {
    nn <- max(lengths(list(x$piece_side, x$suit, x$rank, x$x, x$y, x$angle,
                           x$width, x$height,
                           x$scale, x$bleed,
                           x$ch_width)))
    piece_side <- rep(x$piece_side, length.out=nn)
    suit <- rep(x$suit, length.out=nn)
    rank <- rep(x$rank, length.out=nn)
    xc <- rep(x$x, length.out=nn)
    yc <- rep(x$y, length.out=nn)
    angle <- rep(x$angle, length.out=nn)

    width <- rep(x$width, length.out=nn)
    height <- rep(x$height, length.out=nn)

    scale <- rep(x$scale, length.out=nn)

    cfg <- get_cfg(x$cfg, x$envir)
    cfg <- rep(c(cfg), length.out=nn)

    ch_width <- rep(x$ch_width, length.out=nn)

    gl <- gList()
    for (i in seq(nn)) {
        name <- paste0(".", i)
        gl[[i]] <- crosshairGrobHelper(piece_side[i], suit[i], rank[i], cfg[[i]],
                                       xc[i], yc[i], angle[i],
                                       width[i], height[i], x$default.units,
                                       scale[i], name,
                                       ch_width[i], x$ch_grob)
    }
    setChildren(x, gl)
}

crosshairGrobHelper <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(),
                               x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                               angle=0, width=NA, height=NA,
                               default.units = "npc",
                               scale=1, name="",
                               ch_width=unit(0.25, "mm"),
                               ch_grob=unit(0.125, "in")) {
    if (is.na(width) || is.na(height)) {
        cfg <- as_pp_cfg(cfg)
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

    vp_piece <- viewport(x=x, y=y, angle=angle, width=width, height=height)

    vp_ll <- viewport(x=0, y=0, width=ch_width, height=ch_width)
    vp_ul <- viewport(x=0, y=1, width=ch_width, height=ch_width)
    vp_lr <- viewport(x=1, y=0, width=ch_width, height=ch_width)
    vp_ur <- viewport(x=1, y=1, width=ch_width, height=ch_width)

    ch_ll <- grobTree(ch_grob, name = "crosshair_ll", vp = vp_ll)
    ch_ul <- grobTree(ch_grob, name = "crosshair_ul", vp = vp_ul)
    ch_lr <- grobTree(ch_grob, name = "crosshair_lr", vp = vp_lr)
    ch_ur <- grobTree(ch_grob, name = "crosshair_ur", vp = vp_ur)

    name <- paste0("crosshair", name)

    grobTree(ch_ll, ch_ul, ch_lr, ch_ur, vp = vp_piece, name = name)
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
segmentsCrosshairGrob <- function(...,
                                  x = unit(0.5, "npc"),
                                  y = unit(0.5, "npc"),
                                  width = unit(1, "snpc"),
                                  height = unit(1, "snpc"),
                                  default.units = "npc",
                                  name = NULL, gp = gpar(), vp = NULL) {
    col <- gp$col %||% gp$fill %||% "black"
    lwd <- gp$lwd %||% 1
    lty <- gp$lty %||% "solid"

    gp <- gpar(col = col, lwd = lwd, lty = lty)

    nn <- max(lengths(list(x, y, width, height)))
    x <- rep(x, length.out = nn)
    y <- rep(y, length.out = nn)
    width <- rep(width, length.out = nn)
    height <- rep(height, length.out = nn)
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.unit(width))
        width <- unit(width, default.units)
    if (!is.unit(height))
        height <- unit(height, default.units)

    gl <- gList()
    for (i in seq(nn)) {
        vpi <- viewport(x = x[i], y = y[i], width = width[i], height = height[i])
        gl[[i]] <- grobTree(segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,
                                         default.units = "snpc"),
                            segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
                                         default.units = "snpc"),
                            name = paste0("crosshair.", i), vp = vpi)
    }
    gTree(name = name, gp = gp, vp = vp, children = gl, cl = "pp_segments_crosshair")
}

#' @rdname grid.crosshair
#' @export
squaresCrosshairGrob <- function(...,
                                 x = unit(0.5, "npc"),
                                 y = unit(0.5, "npc"),
                                 width = unit(1, "snpc"),
                                 height = unit(1, "snpc"),
                                 default.units = "npc",
                                 name = NULL, gp = gpar(), vp = NULL) {
    fill <- gp$fill %||% gp$col %||% c("black", "white")
    if (length(fill) == 1L)
        fill <- c(fill, opposite_col(fill))

    gp1 <- gpar(col = NA, fill = fill[1L])
    gp2 <- gpar(col = NA, fill = fill[2L])

    nn <- max(lengths(list(x, y, width, height)))
    x <- rep(x, length.out = nn)
    y <- rep(y, length.out = nn)
    width <- rep(width, length.out = nn)
    height <- rep(height, length.out = nn)
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.unit(width))
        width <- unit(width, default.units)
    if (!is.unit(height))
        height <- unit(height, default.units)

    gl <- gList()
    for (i in seq(nn)) {
        vpi <- viewport(x = x[i], y = y[i], width = width[i], height = height[i])
        gl[[i]] <- grobTree(rectGrob(x = c(0.1, 0.3, 0.5, 0.5, 0.5, 0.5, 0.5, 0.7, 0.9),
                                     y = c(0.5, 0.5, 0.1, 0.3, 0.5, 0.7, 0.9, 0.5, 0.5),
                                     width = 0.1, height = 0.1, gp = gp1, default.units = "snpc"),
                            rectGrob(x = c(0.2, 0.4, 0.5, 0.5, 0.5, 0.5, 0.6, 0.8),
                                     y = c(0.5, 0.5, 0.2, 0.4, 0.6, 0.8, 0.5, 0.5),
                                     width = 0.1, height = 0.1, gp = gp2, default.units = "snpc"),
                            name = paste0("crosshair.", i), vp = vpi)
    }
    gTree(name = name, gp = gp, vp = vp, children = gl, cl = "pp_squares_crosshair")
}

# Opposite color in RGB space
opposite_col <- function(col) {
    stopifnot(length(col) == 1L)
    x <- grDevices::col2rgb(col)
    grDevices::rgb(255 - x[[1, 1]], 255 - x[[2, 1]], 255 - x[[3, 1]], maxColorValue=255)
}
