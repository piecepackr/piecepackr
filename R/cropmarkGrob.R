#' Draw crop marks with grid
#'
#' `grid.cropmark()` draws \dQuote{crop marks} to the active graphics device.
#' `cropmarkGrob()` is its grid grob counterpart.
#' Intended for use in adding crop marks around
#' game pieces in print-and-play layouts.
#'
#' @inheritParams pieceGrob
#' @param bleed Bleed zone size to assume:
#'              \itemize{
#'                 \item{If `bleed` is a [grid::unit()] simply use it}
#'                 \item{If `bleed` is numeric then convert via `grid::unit(bleed, default.units)`}
#'                 \item{If `bleed` is `TRUE` assume 1/8 inch bleed zone size}
#'                 \item{If `bleed` is `FALSE` assume 0 inch bleed zone size}
#'              }
#' @param cm_select A string of integers from "1" to "8" indicating which
#'                  crop marks to draw.  "1" represents the top right crop mark
#'                  then we proceeding clockwise to "8" which represents the
#'                  top left crop mark.
#'                  Default "12345678" draws all eight crop marks.
#' @param cm_width Width of crop mark.
#' @param cm_length Length of crop mark.
#' @return A grid grob.
#' @examples
#' if (requireNamespace("grid", quietly = TRUE) &&
#'     piecepackr:::device_supports_unicode()) {
#'   cfg <- pp_cfg(list(mat_color = "pink", mat_width=0.05, border_color=NA))
#'   grid::grid.newpage()
#'   df <- data.frame(piece_side = "tile_face", suit = 2, rank = 2,
#'                    x = 2, y = 2, angle = 0,
#'                    stringsAsFactors = FALSE)
#'   pmap_piece(df, grid.cropmark, cfg = cfg, default.units = "in")
#'   pmap_piece(df, grid.piece, cfg = cfg, default.units = "in", bleed=TRUE)
#' }
#' if (requireNamespace("grid", quietly = TRUE) &&
#'     piecepackr:::device_supports_unicode()) {
#'   grid::grid.newpage()
#'   df <- data.frame(piece_side = "coin_back", suit = 2, rank = 2,
#'                    x = 2, y = 2, angle = 0,
#'                    stringsAsFactors = FALSE)
#'   pmap_piece(df, grid.cropmark, cfg = cfg, default.units = "in", bleed=TRUE)
#'   pmap_piece(df, grid.piece, cfg = cfg, default.units = "in", bleed=TRUE)
#' }
#' @name grid.cropmark
NULL

#' @rdname grid.cropmark
#' @export
cropmarkGrob <- function(...,
                         piece_side = "tile_back", suit = NA, rank = NA,
                         cfg = getOption("piecepackr.cfg", pp_cfg()),
                         x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                         angle = 0, width = NA, height = NA, scale = 1,
                         default.units = "npc",
                         envir = getOption("piecepackr.envir"),
                         name = NULL, gp = gpar(), vp = NULL,
                         bleed = unit(0.125, "in"),
                         cm_select = "12345678",
                         cm_width = unit(0.25, "mm"),
                         cm_length = unit(0.125, "in")) {
    if (is_angle(angle))
        angle <- as.double(angle, "degrees")

    gTree(piece_side=piece_side, suit=suit, rank=rank, cfg=cfg,
          x=x, y=y, angle=angle,
          width=width, height=height,
          default.units=default.units, envir=envir,
          scale=scale, bleed=bleed,
          cm_select=cm_select, cm_width=cm_width, cm_length=cm_length,
          name = name, gp = gp, vp = vp,
          cl = "pp_cropmark")
}

#' @rdname grid.cropmark
#' @param ... `cropmarkGrob()` ignores; `grid.cropmark()` passes to `cropmarkGrob()`.
#' @export
grid.cropmark <- function(..., draw = TRUE) {
    grob <- cropmarkGrob(...)
    if (draw) {
        grid.draw(grob)
        invisible(grob)
    } else {
        grob
    }
}

#' @export
makeContext.pp_cropmark <- function(x) {
    x$gp$fill <- x$gp$fill %||% x$gp$col %||% get.gpar("col")$col
    x$gp$col <- NA
    x
}

#' @export
makeContent.pp_cropmark <- function(x) {
    nn <- max(lengths(list(x$piece_side, x$suit, x$rank, x$x, x$y, x$angle,
                           x$width, x$height,
                           x$scale, x$bleed,
                           x$cm_select, x$cm_width, x$cm_length)))
    piece_side <- rep(x$piece_side, length.out=nn)
    suit <- rep(x$suit, length.out=nn)
    rank <- rep(x$rank, length.out=nn)
    xc <- rep(x$x, length.out=nn)
    yc <- rep(x$y, length.out=nn)
    angle <- rep(x$angle, length.out=nn)

    width <- rep(x$width, length.out=nn)
    height <- rep(x$height, length.out=nn)

    scale <- rep(x$scale, length.out=nn)
    bleed <- rep(x$bleed, length.out=nn)

    cfg <- get_cfg(x$cfg, x$envir)
    cfg <- rep(c(cfg), length.out=nn)

    cm_select <- rep(x$cm_select, length.out=nn)
    cm_width <- rep(x$cm_width, length.out=nn)
    cm_length <- rep(x$cm_length, length.out=nn)

    gl <- gList()
    for (i in seq(nn)) {
        name <- paste0(".", i)
        gl[[i]] <- cropmarkGrobHelper(piece_side[i], suit[i], rank[i], cfg[[i]],
                                      xc[i], yc[i], angle[i],
                                      width[i], height[i], x$default.units,
                                      scale[i], name, bleed[i],
                                      cm_select[i], cm_width[i], cm_length[i])
    }
    setChildren(x, gl)
}

cropmarkGrobHelper <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(),
                               x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                               angle=0, width=NA, height=NA,
                               default.units = "npc",
                               scale=1, name="", bleed=FALSE,
                               cm_select="12345678",
                               cm_width=unit(0.25, "mm"),
                               cm_length=unit(0.125, "in")) {

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

    if (isTRUE(bleed)) bleed <- unit(0.125, "in")
    if (isFALSE(bleed)) bleed <- unit(0, "in")
    if (!is.unit(bleed)) {
        bleed <- unit(bleed, default.units)
    }

    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    if (!is.unit(width)) width <- unit(width, default.units)
    if (!is.unit(height)) height <- unit(height, default.units)
    if (!is.unit(cm_width)) cm_width <- unit(cm_width, default.units)
    if (!is.unit(cm_length)) cm_length <- unit(cm_length, default.units)

    width <- scale * width
    height <- scale * height
    vp <- viewport(x=x, y=y, angle=angle,
                   width=width + 2 * bleed + 2 * cm_length,
                   height=height + 2 * bleed + 2 * cm_length)

    xc <- unit(0.5, "npc")
    yc <- unit(0.5, "npc")

    if (grepl("1", cm_select)) {
        cm1 <- rectGrob(x = xc + 0.5 * width,
                        y = yc + 0.5 * height + bleed + 0.5 * cm_length,
                        width = cm_width, height = cm_length,
                        name = "crop_mark_1")
    } else {
        cm1 <- nullGrob(name = "crop_mark_1")
    }

    if (grepl("2", cm_select)) {
        cm2 <- rectGrob(x = xc + 0.5 * width + bleed + 0.5 * cm_length,
                        y = yc + 0.5 * height,
                        width = cm_length, height = cm_width,
                        name = "crop_mark_2")
    } else {
        cm2 <- nullGrob(name = "crop_mark_2")
    }

    if (grepl("3", cm_select)) {
        cm3 <- rectGrob(x = xc + 0.5 * width + bleed + 0.5 * cm_length,
                        y = yc - 0.5 * height,
                        width = cm_length, height = cm_width,
                        name = "crop_mark_3")
    } else {
        cm3 <- nullGrob(name = "crop_mark_3")
    }

    if (grepl("4", cm_select)) {
        cm4 <- rectGrob(x = xc + 0.5 * width,
                        y = yc - 0.5 * height - bleed - 0.5 * cm_length,
                        width = cm_width, height = cm_length,
                        name = "crop_mark_4")
    } else {
        cm4 <- nullGrob(name = "crop_mark_4")
    }

    if (grepl("5", cm_select)) {
        cm5 <- rectGrob(x = xc - 0.5 * width,
                        y = yc - 0.5 * height - bleed - 0.5 * cm_length,
                        width = cm_width, height = cm_length,
                        name = "crop_mark_5")
    } else {
        cm5 <- nullGrob(name = "crop_mark_5")
    }

    if (grepl("6", cm_select)) {
        cm6 <- rectGrob(x = xc - 0.5 * width - bleed - 0.5 * cm_length,
                        y = yc - 0.5 * height,
                        width = cm_length, height = cm_width,
                        name = "crop_mark_6")
    } else {
        cm6 <- nullGrob(name = "crop_mark_6")
    }

    if (grepl("7", cm_select)) {
        cm7 <- rectGrob(x = xc - 0.5 * width - bleed - 0.5 * cm_length,
                        y = yc + 0.5 * height,
                        width = cm_length, height = cm_width,
                        name = "crop_mark_7")
    } else {
        cm7 <- nullGrob(name = "crop_mark_7")
    }

    if (grepl("8", cm_select)) {
        cm8 <- rectGrob(x = xc - 0.5 * width,
                        y = yc + 0.5 * height + bleed + 0.5 * cm_length,
                        width = cm_width, height = cm_length,
                        name = "crop_mark_8")
    } else {
        cm8 <- nullGrob(name = "crop_mark_8")
    }
    name <- paste0("crop_mark", name)

    grobTree(cm1, cm2, cm3, cm4, cm5, cm6, cm7, cm8, name = name, vp=vp)
}
