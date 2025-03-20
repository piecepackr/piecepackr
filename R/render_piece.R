#' Render image of game pieces
#'
#' `render_piece()` renders an image of game pieces to a file or graphics device.
#' It is a wrapper around `pmap_piece()` that can auto-size files and graphic devices,
#' apply axes offsets, annotate coordinates, and set up `rayrender` / `rayvertex` scenes.
#' @inheritParams aabb_piece
#' @param file Filename to save image to unless `NULL`
#'             in which case it either uses the current graphics device or opens a new device
#'             (depending on `open_device` argument).
#' @param ... Arguments to [pmap_piece()]
#' @param .f Low level graphics function to use e.g. [grid.piece()], [piece3d()], [piece_mesh()], or [piece()].
#' @param cfg A piecepackr configuration list
#' @param envir Environment (or named list) of piecepackr configuration lists
#' @param width Width of image (in inches).  Inferred by default.
#' @param height Height of image (in inches).  Inferred by default.
#' @param ppi Resolution of image in pixels per inch.
#' @param bg Background color (use `"transparent"` for transparent)
#' @param xoffset Number to add to the `x` column in `df`.  Inferred by default.
#' @param yoffset Number to add to the `y` column in `df`.  Inferred by default.
#' @param annotate If `TRUE` or `"algebraic"` annotate the plot
#'                 with \dQuote{algrebraic} coordinates,
#'                 if `FALSE` or `"none"` don't annotate,
#'                 if `"cartesian"` annotate the plot with \dQuote{cartesian} coordinates.
#' @param annotation_scale Multiplicative factor that scales (stretches) any annotation coordinates.
#'                         By default uses `attr(df, "scale_factor") %||% 1`.
#' @param dev Graphics device function to use if `open_device` is `FALSE`.
#'            If `NULL` infer a reasonable choice from `file`.
#' @param dev.args Additional arguments to pass to `dev` (besides `filename`, `width`, and `height`).
#'                 Will filter out any names that aren't in `formals(dev)`.
#' @param open_device If `TRUE` open a new graphics device otherwise draw in the active graphics.
#' @param close_device If `TRUE` close the graphics device (if `open_device = TRUE` close the newly opened device otherwise close the previously existing graphics device).
#' @param image Class of image object to return in the `"image"` field of returned list.
#'              If `"NULL"` (the default) return `NULL`.
#'              If `"raster"` or `"nativeRaster" try to return a raster object of the image
#'              using [grDevices::dev.capture()] or `as.raster(magick::image_read())`.
#' @param xbreaks,ybreaks Subset (of integers) to provide axis labels for if `annotate` is `TRUE`.
#'                        If `NULL` infer a reasonable choice.
#' @param new_device If `FALSE` draw in the active graphics device instead of opening a new graphics device.  This argument is deprecated.  Use the `open_device` argument instead.
#' @return An invisible list of the dimensions of the image and
#'         possibly an image object specified by `image`.
#'         As a side effect may save a file and/or open/close a graphics device.
#' @seealso This function is a wrapper around [pmap_piece()].
#' @examples
#'  df_board <- data.frame(piece_side = "board_face", suit = 3, rank = 5,
#'                         x = 3.0, y = 3.0, stringsAsFactors = FALSE)
#'  df_w <- data.frame(piece_side = "bit_back", suit = 6, rank = 1,
#'                     x = rep(1:5, 2), y = rep(1:2, each=5),
#'                     stringsAsFactors = FALSE)
#'  df_b <- data.frame(piece_side = "bit_back", suit = 1, rank = 1,
#'                     x = rep(1:5, 2), y = rep(4:5, each=5),
#'                     stringsAsFactors = FALSE)
#'  df <- rbind(df_board, df_w, df_b)
#'  df$cfg <- "checkers1"
#'
#'  if (requireNamespace("grid", quietly = TRUE)) {
#'    render_piece(df, open_device = FALSE)
#'  }
#'  if (requireNamespace("grid", quietly = TRUE)) {
#'    grid::grid.newpage()
#'    render_piece(df, open_device = FALSE,
#'                 op_scale = 0.5, trans = op_transform,
#'                 annotate = "algrebraic")
#'  }
#'  \dontrun{# May take more than 5 seconds on CRAN servers
#'  if (require(rayvertex)) {
#'    envir3d <- game_systems("sans3d")
#'    render_piece(df, .f = piece_mesh, envir = envir3d,
#'                 open_device = FALSE,
#'                 op_scale = 0.5, trans = op_transform)
#'  }
#'  }
#' @export
render_piece <- function(df, file = NULL, ...,
                         .f = piecepackr::grid.piece,
                         cfg = getOption("piecepackr.cfg", NULL),
                         envir = getOption("piecepackr.envir", game_systems("sans")),
                         width = NULL, height = NULL,
                         ppi = 72, bg = "white",
                         xoffset = NULL, yoffset = NULL,
                         annotate = FALSE, annotation_scale = NULL,
                         dev = NULL,
                         dev.args = list(res = ppi, bg = bg, units = "in"),
                         open_device = new_device,
                         close_device = open_device && (!is.null(file) || !is.null(dev)),
                         image = c("NULL", "raster", "nativeRaster"),
                         xbreaks = NULL, ybreaks = NULL,
                         new_device = TRUE) {
    if (!missing(new_device)) {
        warn("The argument `new_device` is deprecated.  Use `open_device` instead.",
             class = "deprecatedWarning")
    }
    stopifnot(is.null(dev) || is.function(dev))
    image <- match.arg(image)
    if (image == "NULL")
        image <- NULL

    # Make sure if we close the graphics device we return to any previous device
    if (close_device) {
        if (open_device) {
            current_dev <- grDevices::dev.cur()
            if(current_dev > 1L)
                on.exit(grDevices::dev.set(current_dev), add = TRUE)
        } else {
            if (length(dev.list()) > 1L) {
                prev_dev <- grDevices::dev.prev()
                on.exit(grDevices::dev.set(prev_dev), add = TRUE)
            }
        }
    }

    ce <- default_cfg_envir(cfg, envir)
    cfg <- ce$cfg
    envir <- ce$envir

    dfr <- aabb_piece(df, cfg = cfg, envir = envir, ...)
    xmax <- dfr$x[2]
    ymax <- dfr$y[2]
    xoffset <- xoffset %||% min2offset(dfr$x_op[1])
    yoffset <- yoffset %||% min2offset(dfr$y_op[1])
    if (is.null(width)) width <- dfr$x_op[2] + xoffset + 0.50
    if (is.null(height)) height <- dfr$y_op[2] + yoffset + 0.50
    if (is.na(width)) width <- 1
    if (is.na(height)) height <- 1
    m <- max(width, height)

    if (open_device) {
        if (is.null(dev)) {
            dev <- pp_device_fn(file)
        }
        args <- c(list(filename = file, width = width, height = height),
                  dev.args)
        args <- args[names(args) %in% names(formals(dev))]
        do.call(dev, args)
    }

    # plot_fn_helper expected width and height in pixels
    width <- ppi * width
    height <- ppi * height
    l <- list(width = width, height = height, image = NULL)

    fn <- plot_fn_helper(.f, xmax, ymax, xoffset, yoffset, width, height, m, ppi, envir,
                         annotate, annotation_scale, xbreaks, ybreaks)
    fn(df, ...)

    if (!is.null(image)) {
        stopifnot(image %in% c("raster", "nativeRaster"))
        native <- ifelse(image == "nativeRaster", TRUE, FALSE)
        l$image <- suppressWarnings(grDevices::dev.capture(native = native))
        if (is.null(l$image) && is.null(file)) {
            abort("`dev.capture()` failed to capture a raster image with this graphics device.")
        }
    }

    if (close_device) {
        grDevices::dev.off()
    }

    # Fallback to {magick} to capture an image if `dev.capture()` failed
    if (!is.null(image) && is.null(l$image)) {
        assert_suggested("magick")
        l$image <- grDevices::as.raster(magick::image_read(file), native = native)
    }
    invisible(l)
}

plot_fn_helper <- function(.f = grid.piece, xmax, ymax, xoffset, yoffset,
                           width, height, m, ppi, envir,
                           annotate, annotation_scale, xbreaks, ybreaks) {
    if (identical(.f, grid.piece)) {
        function(df, ..., scale = 1) {
            annotation_scale <- annotation_scale %||% attr(df, "scale_factor") %||% 1
            df$x <- df$x + xoffset
            df$y <- df$y + yoffset
            df$scale <- if (hasName(df, "scale")) scale * df$scale else scale
            grid::grid.newpage()
            pmap_piece(df, default.units = "in", ..., envir = envir)
            annotate_plot(annotate, xmax, ymax, xoffset, yoffset, annotation_scale,
                          xbreaks, ybreaks)
        }
    } else if (identical(.f, piece3d)) {
        assert_suggested("rgl")
        if (Sys.which("wmctrl") != "") {
            cmd <- paste0("wmctrl -r RGL -e 0,-1,-1,", ceiling(width), ",", ceiling(height))
            system(cmd)
        }
        f <- tempfile(fileext=".png")
        function(df, ..., scale = 1) {
            df$scale <- if (hasName(df, "scale")) scale * df$scale else scale
            rgl::clear3d()
            rgl::points3d(x = rep(c(0, xmax), 2), y = rep(c(0, ymax), each = 2), z = 0, alpha = 0)
            pmap_piece(df, piece3d, ..., envir = envir)
            Sys.sleep(2)
            rgl::snapshot3d(f, top = FALSE, webshot = FALSE)
            grid::grid.newpage()
            grid::grid.raster(png::readPNG(f))
        }
    } else if (identical(.f, piece)) {
        assert_suggested("rayrender")
        function(df, ..., scale = 1,
                 fov = 20, samples=100, lookat = NULL, lookfrom = NULL, clamp_value = Inf,
                 table = NA, interactive = TRUE) {
            df$scale <- if (hasName(df, "scale")) scale * df$scale else scale
            df$x <- df$x + xoffset
            df$y <- df$y + yoffset
            l <- pmap_piece(df, piece, ..., envir = envir)
            if (all(is.na(table))) {
                table <- rayrender::sphere(z=-1e3, radius=1e3, material=rayrender::diffuse(color="green"))
                light <- rayrender::sphere(x=0.5*width/ppi, y=-4, z=max(2.0*m+1, 20),
                                           material=rayrender::light(intensity=420))
                table <- rayrender::add_object(table, light)
            }
            scene <- Reduce(rayrender::add_object, l, init=table)
            if (is.null(lookat)) lookat <- c(0.5*width/ppi, 0.5*height/ppi, 0)
            if (is.null(lookfrom)) lookfrom <- c(0.5*width/ppi, -2.0*m, 2.0*m)
            rayrender::render_scene(scene,
                                    fov = fov, samples = samples,
                                    lookat = lookat, lookfrom = lookfrom, clamp_value = clamp_value,
                                    width = width, height = height, interactive = interactive)
        }
    } else if (identical(.f, piece_mesh)) {
        assert_suggested("rayvertex")
        function(df, ..., scale = 1,
                 fov = 20, lookat = NULL, lookfrom = NULL,
                 table = NA, light_info = NA) {
            df$scale <- if (hasName(df, "scale")) scale * df$scale else scale
            df$x <- df$x + xoffset
            df$y <- df$y + yoffset
            l <- pmap_piece(df, piece_mesh, ..., envir = envir)
            if (all(is.na(table))) {
                table <- rayvertex::sphere_mesh(c(0, 0, -1e3), radius=1e3,
                                                material=rayvertex::material_list(diffuse="green"))
            }
            if (all(is.na(light_info))) {
                light_info <- rayvertex::directional_light(c(0.5*width/ppi, -4, max(2.0*m+1, 20)), intensity = 2.5)
            }
            scene <- Reduce(rayvertex::add_shape, l, init=table)
            if (is.null(lookat)) lookat <- c(0.5*width/ppi, 0.5*height/ppi, 0)
            if (is.null(lookfrom)) lookfrom <- c(0.5*width/ppi, -2.0*m, 2.0*m)
            rayvertex::rasterize_scene(scene, width = width, height = height,
                                       fov = fov, lookat = lookat, lookfrom = lookfrom,
                                       light_info = light_info)
        }
    } else {
        .f
    }
}

min2offset <- function(min, lbound = 0.5) {
    if (is.na(min)) {
        NA_real_
    } else if (min < lbound) {
        lbound - min
    } else {
        0
    }
}

annotate_plot <- function(annotate, xmax, ymax, xoffset = 0, yoffset = 0,
                          annotation_scale = 1, xbreaks = NULL, ybreaks = NULL) {
        if (isFALSE(annotate) || annotate == "none" || is.na(xmax) || is.na(ymax))
            return(invisible(NULL))
        gp <- gpar(fontsize = 18, fontface = "bold")

        if (is.null(xbreaks)) {
            x_coords <- seq(annotation_scale, floor(xmax), by = annotation_scale)
        } else {
            xbreaks <- as.integer(xbreaks)
            x_coords <- seq(annotation_scale, by = annotation_scale,
                            length.out = max(xbreaks))
        }
        if (annotate == "cartesian")
            l <- as.character(seq_along(x_coords))
        else
            l <- letters[seq_along(x_coords)]
        if (!is.null(xbreaks)) {
            x_coords <- x_coords[xbreaks]
            l <- l[xbreaks]
        }
        l <- str_pad(l, max(str_count(l)))
        grid.text(l, x = x_coords + xoffset, y = 0.25, default.units = "in", gp = gp)

        if (is.null(ybreaks)) {
            y_coords <- seq(annotation_scale, floor(ymax), by = annotation_scale)
        } else {
            ybreaks <- as.integer(ybreaks)
            y_coords <- seq(annotation_scale, by = annotation_scale,
                            length.out = max(ybreaks))
        }
        n <- as.character(seq_along(y_coords))
        n <- str_pad(n, max(str_count(n)))
        if (!is.null(ybreaks)) {
            y_coords <- y_coords[ybreaks]
            l <- l[ybreaks]
        }
        grid.text(n, x = 0.25, y = y_coords + yoffset, default.units = "in", gp = gp)

        invisible(NULL)
}
