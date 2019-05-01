#' Draw piecepack pieces using grid
#' 
#' \code{grid.piece} draws a piecepack pieces onto the graphics device.  
#' \code{pieceGrob} is its \code{grid} \code{grob} counterpart.
#' \code{pmap_piece} operates on the rows of a data frame  
#'     applying \code{pieceGrob} to each row.
#' 
#' @param piece_side A string with piece and side separated by a underscore e.g. "coin_face"
#' @param cfg Piecepack configuration list or \code{pp_cfg} object, 
#'        a list of \code{pp_cfg} objects,
#'        or a character vector of \code{pp_cfg} objects
#' @param suit Number of suit (highest rank starting from 1).  
#'        The number above the total number of suits is the neutral "unsuit".
#'        and the next number above that is "no suits".
#' @param rank Number of rank (lowest rank starting from 1)
#' @param x Where to place piece on x axis of viewport
#' @param y Where to place piece on y axis of viewport
#' @param angle Angle to draw piece at
#' @param use_pictureGrob If \code{TRUE} instead of directly returning the grob first 
#'            export to (temporary) svg and then re-import as a \code{grImport2::pictureGrob}.  
#'            This is useful if drawing pieces really big or small and don't want
#'            to play with re-configuring fontsizes.
#' @param width Width of piece
#' @param height Height of piece
#' @param default.units  A string indicating the default units to use if 
#'   'x', 'y', 'width', and/or 'height' are only given as numeric vectors.
#' @param envir Environment (or named list) containing configuration list(s).
#' @param name A character identifier (for grid)
#' @param gp An object of class ‘gpar’, typically the output from a call
#'        to the function ‘gpar’.  This is basically a list of
#'        graphical parameter settings.
#' @param draw A logical value indicating whether graphics output should be produced.
#' @param vp A \code{grid} viewport object (or NULL).
#' @param .l A list of vectors, such as a data frame. The length of \code{.l}
#'           determines the number of arguments that \code{grid.piece_wrapper}
#'           will be called  with. List names will be used if present.
#' @param ... Extra arguments to pass to \code{pieceGrob}.
#' @return A \code{grob} object.  If \code{draw} is \code{TRUE} then as a side effect
#'         will also draw it to the graphics device.
#' @name grid.piece
NULL

#' @rdname grid.piece
#' @export
pmap_piece <- function(.l, ..., draw=TRUE, name=NULL, gp=NULL, vp=NULL) {
    ll <- purrr::pmap(.l, pieceGrob_wrapper, ..., draw=FALSE)
    grob <- gTree(children=as.gList(ll), name=name, gp=gp, vp=vp)
    if (draw)
        grid.draw(grob)
    else
        grob
}

as.gList <- function(ll) {
    gl <- gList()
    for (ii in seq(ll)) {
        gl[[ii]] <- ll[[ii]]
    }
    gl
}

pieceGrob_wrapper <- function(piece_side="tile_back", suit=NA, rank=NA, 
                           cfg=pp_cfg(), x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                           angle=NA, use_pictureGrob=FALSE, 
                           width=NA, height=NA, 
                           default.units="npc", envir=NULL, ...) {
    pieceGrob(piece_side, suit, rank, cfg, x, y, 
                   angle, use_pictureGrob, width, height, default.units, envir)
}

as_picture <- function(grob, width, height) {
    svg_file <- tempfile(fileext=".svg")
    on.exit(unlink(svg_file))
    svg(svg_file, width=width, height=height)
    grid.draw(grob)
    invisible(dev.off())
    pictureGrob(readPicture(svg_file, warn=FALSE))
}

pieceGrobHelper <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(), 
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                           angle=0, use_pictureGrob=FALSE,
                           width=NA, height=NA, default.units = "npc") {
    cfg <- as_pp_cfg(cfg)
    suit <- ifelse(has_suit(piece_side), ifelse(is.na(suit), 1, suit), cfg$i_unsuit)
    rank <- ifelse(has_rank(piece_side), ifelse(is.na(rank), 1, rank), 0)
    if(!is.unit(x)) { x <- unit(x, default.units) }
    if(!is.unit(y)) { y <- unit(y, default.units) }
    if(is.na(angle)) { angle <- 0 }
    if(is.na(width)) { width <- inch(cfg$get_width(piece_side, suit, rank)) }
    if(is.na(height)) { height <- inch(cfg$get_height(piece_side, suit, rank)) }
    if(!is.unit(width)) { width <- unit(width, default.units) }
    if(!is.unit(height)) { height <- unit(height, default.units) }

    grob <- cfg$get_grob(piece_side, suit, rank)
    if (use_pictureGrob) {
        pp_width <- cfg$get_width(piece_side, suit, rank)
        pp_height <- cfg$get_height(piece_side, suit, rank)
        grob <- as_picture(grob, pp_width, pp_height)
    }
    cvp <- viewport(x, y, width, height, angle=angle)
    grobTree(grob, vp=cvp)
    
}

#' @rdname grid.piece
#' @export
pieceGrob <- function(piece_side="tile_back", suit=NA, rank=NA, 
                         cfg=pp_cfg(), 
                         x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                         angle=0, use_pictureGrob=FALSE,
                         width=NA, height=NA, 
                         default.units = "npc", envir=NULL,
                         name=NULL, gp=NULL, vp=NULL) {

    nn <- max(lengths(list(piece_side, suit, rank, x, y, angle, use_pictureGrob, width, height)))
    piece_side <- rep(piece_side, length.out=nn)
    suit <- rep(suit, length.out=nn)
    rank <- rep(rank, length.out=nn)
    x <- rep(x, length.out=nn)
    y <- rep(y, length.out=nn)
    angle <- rep(angle, length.out=nn)
    use_pictureGrob <- rep(use_pictureGrob, length.out=nn)
    width <- rep(width, length.out=nn)
    height <- rep(height, length.out=nn)

    if (is_pp_cfg(cfg)) {
        cfg <- rep(c(cfg), length.out=nn)
    } else if (is.character(cfg)) {
        if(!is.null(envir)) { 
            envir=as.environment(envir) 
            cfg <- lapply(cfg, function(cc) as_pp_cfg(envir[[cc]]))
        } else {
            cfg <- lapply(cfg, function(cc) as_pp_cfg(dynGet(cc)))
        }
        cfg <- rep(cfg, length.out=nn)
    } else {
        if (is.list(cfg)) {
            if (!("pp_cfg" %in% sapply(cfg, class)))
                cfg <- c(pp_cfg(cfg))
            cfg <- rep(cfg, length.out=nn)
        } else {
            stop("Don't know how to parse cfg argument") # nocov
        }
    }

    gl <- gList()
    for(ii in seq(nn)) {
        gl[[ii]] <- pieceGrobHelper(piece_side[ii], suit[ii], rank[ii], cfg[[ii]],
                                        x[ii], y[ii], angle[ii], use_pictureGrob[ii],
                                        width[ii], height[ii], default.units)
    }
    gTree(children=gl, name=name, gp=gp, vp=vp)
}

#' @rdname grid.piece
#' @export
grid.piece <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=list(), 
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                           angle=0, use_pictureGrob=FALSE,
                           width=NA, height=NA, 
                           default.units = "npc", envir=NULL,
                           name=NULL, gp=NULL, draw=TRUE, vp=NULL) {
    grob <- pieceGrob(piece_side, suit, rank, cfg, 
                          x, y, angle, use_pictureGrob, width, height, default.units, 
                          envir, name, gp, vp)
    if (draw) { 
        grid.draw(grob)
    } else {
        grob
    }
}
