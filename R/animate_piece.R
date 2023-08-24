#' Animate board game pieces
#'
#' `animate_piece()` animates board game pieces.
#' @param dfs A list of data frames of game data to plot.
#' @param file Filename to save animation unless \code{NULL}
#'             in which case it uses the current graphics device.
#' @param annotate If `TRUE` or `"algebraic"` annotate the plot
#'                  with \dQuote{algrebraic} coordinates,
#'                 if `FALSE` or `"none"` don't annotate,
#'                 if `"cartesian"` annotate the plot with \dQuote{cartesian} coordinates.
#' @param ... Arguments to \code{pmap_piece}
#' @param .f Low level graphics function to use e.g. [grid.piece()], [piece3d()], [piece()], or [piece_mesh()].
#' @param cfg A piecepackr configuration list
#' @param envir Environment (or named list) of piecepackr configuration lists
#' @param n_transitions Integer, if over zero (the default)
#'                how many transition frames to add between moves.
#' @param n_pauses Integer, how many paused frames per completed move.
#' @param fps Double, frames per second.
#' @param width Width of animation (in inches).  Inferred by default.
#' @param height Height of animation (in inches).  Inferred by default.
#' @param ppi Resolution of animation in pixels per inch.
#'            By default set so image max 600 pixels wide or tall.
#' @param new_device If \code{file} is \code{NULL} should we open up a new graphics device?
#' @param annotation_scale Multiplicative factor that scales (stretches) any annotation coordinates.
#'                         By default uses `attr(df, "scale_factor") %||% 1`.
#' @return Nothing, as a side effect creates an animation.
#' @examples
#'   # Basic tic-tac-toe animation
#'   dfs <- list()
#'   d.frame <- function(piece_side = "bit_back", ..., rank = 1L) {
#'                  data.frame(piece_side = piece_side, ..., rank = rank,
#'                             cfg = "checkers1", stringsAsFactors = FALSE)
#'   }
#'   df <- d.frame("board_back", suit = 2L, rank = 3L, x = 2, y = 2, id = "1")
#'   dfs[[1L]] <- df
#'   df <- rbind(df, d.frame(suit = 1L, x = 2, y = 2, id = "2"))
#'   dfs[[2L]] <- df
#'   df <- rbind(df, d.frame(suit = 2L, x = 1, y = 2, id = "3"))
#'   dfs[[3L]] <- df
#'   df <- rbind(df, d.frame(suit = 1L, x = 3, y = 1, id = "4"))
#'   dfs[[4L]] <- df
#'   df <- rbind(df, d.frame(suit = 2L, x = 1, y = 3, id = "5"))
#'   dfs[[5L]] <- df
#'   df <- rbind(df, d.frame(suit = 1L, x = 1, y = 1, id = "6"))
#'   dfs[[6L]] <- df
#'   df <- rbind(df, d.frame(suit = 2L, x = 3, y = 3, id = "7"))
#'   dfs[[7L]] <- df
#'   df <- rbind(df, d.frame(suit = 1L, x = 2, y = 1, id = "8"))
#'   dfs[[8L]] <- df
#'
#'   ## Press enter to walk through moves in a "game" in new graphics device
#'   if (interactive()) {
#'       animate_piece(dfs, file = NULL)
#'   }
#'
#'   ## Save GIF of game with animation transitions
#'   \dontrun{# May take more than 5 seconds on CRAN servers
#'   if ((requireNamespace("animation", quietly = TRUE) ||
#'        requireNamespace("gifski", quietly = TRUE)) &&
#'       requireNamespace("tweenr", quietly = TRUE)) {
#'       file <- tempfile("tic-tac-toe", fileext = ".gif")
#'       animate_piece(dfs, file = file,
#'                     n_transitions = 5L, n_pauses = 2L, fps = 9)
#'   }
#'   }
#'
#' @export
animate_piece <- function(dfs, file = "animation.gif", ...,
                          annotate = TRUE,
                          .f = piecepackr::grid.piece,
                          cfg = getOption("piecepackr.cfg", NULL),
                          envir = getOption("piecepackr.envir", game_systems("sans")),
                          n_transitions = 0L, n_pauses = 1L, fps = n_transitions + n_pauses,
                          width = NULL, height = NULL, ppi = NULL,
                          new_device = TRUE, annotation_scale = NULL) {

    if (n_transitions > 0L)
        assert_suggested("tweenr")

    ce <- default_cfg_envir(cfg, envir)
    cfg <- ce$cfg
    envir <- ce$envir

    ranges <- lapply(dfs, aabb_piece, cfg = cfg, envir = envir, ...)
    if (n_transitions > 0L) {
        dfs <- get_tweened_dfs(dfs, n_transitions, n_pauses, ..., cfg = cfg, envir = envir)
    } else if (n_pauses != 1) {
        dfs <- rep(dfs, each = n_pauses)
    }

    #### Add grid and comment annotations
    xmax_op <- max(sapply(ranges, function(r) r$x_op[2]), na.rm = TRUE)
    ymax_op <- max(sapply(ranges, function(r) r$y_op[2]), na.rm = TRUE)
    xmin_op <- min(sapply(ranges, function(r) r$x_op[1]), na.rm = TRUE)
    ymin_op <- min(sapply(ranges, function(r) r$y_op[1]), na.rm = TRUE)
    xmax <- max(sapply(ranges, function(r) r$x[2]), na.rm = TRUE)
    ymax <- max(sapply(ranges, function(r) r$y[2]), na.rm = TRUE)
    xoffset <- min2offset(xmin_op)
    yoffset <- min2offset(ymin_op)
    if (is.null(width)) width <- xmax_op + xoffset + 0.50
    if (is.null(height)) height <- ymax_op + yoffset + 0.50
    m <- max(width, height)
    if (is.null(ppi)) ppi <- round(600 / m, 0)
    # mp4 needs even height / weight
    height <- ceiling(ppi * height)
    width <- ceiling(ppi * width)
    height <- height + (height %% 2)
    width <- width + (width %% 2)
    plot_fn <- plot_fn_helper(.f, xmax, ymax, xoffset, yoffset, width, height, m, ppi, envir,
                              annotate, annotation_scale)
    animation_fn(file, new_device)(lapply(dfs, plot_fn, ...), file, width, height, 1 / fps, ppi)
    invisible(NULL)
}
#### How to handle empty tibbles??
animation_fn <- function(file, new_device = TRUE) {
    if (is.null(file)) {
        function(expr, file, width, height, delay, res) {
            if (new_device) dev.new(width = width / res, height = height / res, unit = "in", noRstudioGD = TRUE)
            devAskNewPage(TRUE)
            eval(expr)
            devAskNewPage(getOption("device.ask.default"))
        }
    } else if (grepl(".html$", file)) {
        assert_suggested("animation")
        function(expr, file, width, height, delay, res) {
            animation::saveHTML(expr, htmlfile = file, interval = delay, img.name = file,
                                ani.height = height, ani.width = width, ani.res = res,
                                ani.dev = "png", ani.type = "png",
                                title = "Animated game", verbose = FALSE)
        }
    } else if (grepl(".gif$", file)) {
        if (requireNamespace("gifski", quietly = TRUE)) {
            function(expr, file, width, height, delay, res) {
                gifski::save_gif(expr, file, width, height, delay, res = res, progress = FALSE)
            }
        } else if (requireNamespace("animation", quietly = TRUE)) {
            function(expr, file, width, height, delay, res) {
                animation::saveGIF(expr, movie.name = file, interval = delay, img.name = file,
                                   ani.height = height, ani.width = width, ani.res = res,
                                   ani.dev = "png", ani.type = "png")
            }
        } else {
            abort(c("At least one the suggested packages 'gifski' or 'animation' is required to use 'animate_game()'.",
                    i = "Use 'install.packages(\"gifski\")' and/or 'install.packages(\"animation\")' to install them."),
                  class = "piecepackr_suggested_package")
        }
    } else if (grepl(".bmp$|.jpg$|.jpeg$|.png$|.tiff$", file)) {
        stopifnot(has_c_integer_format(file))
        function(expr, file, width, height, delay, res) {
            pp_device(file, width = width / res, height = height / res, res=res)
            eval(expr)
            grDevices::dev.off()
        }
    } else {
        assert_suggested("animation")
        function(expr, file, width, height, delay, res) {
            animation::saveVideo(expr, video.name = file, interval = delay, img.name = file,
                                ani.height = height, ani.width = width, ani.res = res,
                                ani.dev = "png", ani.type = "png",
                                title = "Animated game", verbose = FALSE)
        }
    }
}

get_tweened_dfs <- function(dfs, n_transitions = 0L, n_pauses = 1L, ...) {
    n <- length(dfs)
    if (n < 2) return(rep(dfs, n_pauses))
    new_dfs <- list()
    for (i in seq_len(n - 1)) {
        new_dfs <- append(new_dfs, rep(dfs[i], n_pauses))
        new_dfs <- append(new_dfs, tween_dfs(dfs[[i]], dfs[[i+1]], n_transitions))
    }
    new_dfs <- append(new_dfs, rep(dfs[n], n_pauses))
    for (i in seq_along(new_dfs))
        attr(new_dfs[[i]], "scale_factor") <- attr(dfs[[1]], "scale_factor")
    new_dfs
}

tween_dfs <- function(df1, df2, n_transitions = 0L) {
    stopifnot(names(df1) == names(df2))
    df_id_cfg <- get_id_cfg(df1, df2)
    if (nrow(df1) == 0 && nrow(df2) == 0) return(rep(list(df1), n_transitions))
    dfs <- init_dfs(df1, df2)
    df <- tweenr::tween_state(dfs[[1]], dfs[[2]], ease = "cubic-in-out",
                              nframes = n_transitions + 2L, id = .data$id)
    df <- merge(df, df_id_cfg, by = "id", all.x = TRUE, sort = FALSE)
    id_frames <- as.list(seq.int(max(df$.frame)))
    l <- lapply(id_frames, function(id_frame) df[which(df$.frame == id_frame), , drop = FALSE])
    l <- head(l, -1L)
    l <- tail(l, -1L)
    l
}
init_dfs <- function(df1, df2) {
    if (nrow(df1) == 0) {
        df1i <- get_tweenr_df(df2)
        df1i$scale <- 0
    } else {
        df1i <- get_tweenr_df(df1)
    }
    if (nrow(df2) == 0) {
        df2i <- get_tweenr_df(df1)
        df2i$scale <- 0
    } else {
        df2i <- get_tweenr_df(df2)
    }
    df2_anti <- df2i[which(match(df2i$id, df1i$id, 0) == 0), , drop = FALSE]
    df2_anti$scale <- rep(0, nrow(df2_anti))
    df1 <- rbind(df1i, df2_anti) # 'added' pieces
    df1_anti <- df1i[which(match(df1i$id, df2i$id, 0) == 0), , drop = FALSE]
    df1_anti$scale <- rep(0, nrow(df1_anti))
    df2 <- df2i # 'removed' pieces
    while (nrow(df1_anti)) {
        row <- df1_anti[1L, ]
        df1_anti <- df1_anti[-1L, ]
        prev_index <- find_good_prev_index(row, df1i, df2i)
        df2 <- insert_df(df2, row, prev_index)
    }
    df1 <- df1[match(df2$id, df1$id), ] # re-sort to match df2
    list(df1, df2)
}
find_good_prev_index <- function(row, df1i, df2i) {
    prev_index <- Inf
    index <- which(df1i$id == row$id)
    while (is.infinite(prev_index)) {
        index <- index - 1
        if (index == 0) {
            prev_index <- 0
        } else {
            prev <- df1i[index, ]
            index2 <- which(df2i$id == prev$id)
            if (length(index2)) {
                prev2 <- df2i[index2, ]
                if (nigh(prev$x, prev2$x) && nigh(prev$y, prev2$y)) {
                    prev_index <- index2
                }
            }
        }
    }
    prev_index
}
get_id_cfg <- function(df1, df2) {
    df <- rbind(df1[, c("id", "cfg")],
                df2[, c("id", "cfg")])
    unique(df)
}
get_tweenr_df <- function(df, ...) {
    stopifnot(all(hasName(df, c("id", "piece_side", "rank", "suit", "x", "y"))))
    if (!hasName(df, "alpha")) df$alpha <- 1
    if (!hasName(df, "angle")) df$angle <- 0
    if (!hasName(df, "scale")) df$scale <- 1

    if (hasName(df, "z"))
        columns <- c("id", "piece_side", "suit", "rank", "x", "y", "z", "angle", "scale", "alpha")
    else
        columns <- c("id", "piece_side", "suit", "rank", "x", "y", "angle", "scale", "alpha")

    as.data.frame(df[, columns])
}
# Insert `df2` into `df1` after `index`
# index = 0 means instead at beginning
insert_df <- function(df1, df2, index = nrow(df1)) {
    if (index == 0L) {
        rbind(df2, df1)
    } else if (index == nrow(df1)) {
        rbind(df1, df2)
    } else {
        rbind(df1[seq(index), ], df2, df1[-seq(index), ])
    }
}

