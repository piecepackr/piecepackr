#' Draw piecepack pieces using grid
#' 
#' \code{grid.piece} draws a piecepack pieces onto the graphics device.  
#' \code{grobpiece} is its \code{grid} \code{grob} counterpart.
#' \code{pmap_piece} operates on the rows of a data frame  
#'     applying \code{grid.piece} to each row.
#' 
#' @param piece_side A string with piece and side separated by a underscore e.g. "coin_face"
#' @param cfg Piecepack configuration list or \code{pp_cfg} object, 
#'        a list of \code{pp_cfg} objects,
#'        or a character vector of \code{pp_cfg} objects
#' @param i_s Number of suit
#' @param i_r Number of rank
#' @param x Where to place piece on x axis of viewport
#' @param y Where to place piece on y axis of viewport
#' @param rot Angle to draw piece at
#' @param svg If \code{TRUE} instead of drawing directly into graphics device
#'            export to svg, re-import svg, and then draw it to graphics device.  
#'            This is useful if drawing really big or small and don't want
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
#' @param ... For \code{pmap_piece} extra arguments to pass to \code{grid.piece_wrapper},
#'            for \code{grid.piece_wrapper} any extra arguments are ignored.
#' @return A \code{grob} object.  If \code{draw} is \code{TRUE} will also draw it to the graphics device.
#' @name grid.piece
NULL

#' @rdname grid.piece
#' @export
pmap_piece <- function(.l, ..., draw=TRUE, name=NULL, gp=NULL, vp=NULL) {
    ll <- purrr::pmap(.l, grid.piece_wrapper, ..., draw=FALSE)
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

grid.piece_wrapper <- function(..., piece_side="tile_back", i_s=NA, i_r=NA, 
                                   cfg=pp_cfg(), envir=NULL, 
                                   x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                                   rot=NA, svg=FALSE, 
                                   width=NA, height=NA, default.units="npc", draw=TRUE) {
    grid.piece(piece_side, i_s, i_r, cfg, x, y, 
                   rot, svg, width, height, default.units, envir, draw=draw)
}


pieceGrobHelper <- function(piece_side="tile_back", i_s=NA, i_r=NA, cfg=pp_cfg(), 
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                           rot=0, svg=FALSE,
                           width=NA, height=NA, default.units = "npc") {
    cfg <- as_pp_cfg(cfg)
    i_s <- ifelse(has_suit(piece_side), ifelse(is.na(i_s), 1, i_s), cfg$i_unsuit)
    i_r <- ifelse(has_rank(piece_side), ifelse(is.na(i_r), 1, i_r), 0)
    if(!is.unit(x)) { x <- unit(x, default.units) }
    if(!is.unit(y)) { y <- unit(y, default.units) }
    if(is.na(rot)) { rot <- 0 }
    if(is.na(width)) { width <- inch(cfg$get_pp_width(piece_side, i_r)) }
    if(is.na(height)) { height <- inch(cfg$get_pp_height(piece_side, i_r)) }
    if(!is.unit(width)) { width <- unit(width, default.units) }
    if(!is.unit(height)) { height <- unit(height, default.units) }

    grob <- cfg$get_grob(piece_side, i_s, i_r)
    if (svg) {
        svg_file <- tempfile(fileext=".svg")
        on.exit(unlink(svg_file))
        pp_width=cfg$get_pp_width(piece_side, i_r)
        pp_height=cfg$get_pp_height(piece_side, i_r)

        svg(svg_file, width=pp_width, height=pp_height)
        grid.draw(grob)
        invisible(dev.off())

        grob <- pictureGrob(readPicture(svg_file, warn=FALSE))
    }
    cvp <- viewport(x, y, width, height, angle=rot)
    grobTree(grob, vp=cvp)
    
}

#' @rdname grid.piece
#' @export
pieceGrob <- function(piece_side="tile_back", i_s=NA, i_r=NA, 
                         cfg=pp_cfg(), 
                         x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                         rot=0, svg=FALSE,
                         width=NA, height=NA, 
                         default.units = "npc", envir=NULL,
                         name=NULL, gp=NULL, vp=NULL) {

    nn <- max(lengths(list(piece_side, i_s, i_r, x, y, rot, svg, width, height)))
    piece_side <- rep(piece_side, length.out=nn)
    i_s <- rep(i_s, length.out=nn)
    i_r <- rep(i_r, length.out=nn)
    x <- rep(x, length.out=nn)
    y <- rep(y, length.out=nn)
    rot <- rep(rot, length.out=nn)
    svg <- rep(svg, length.out=nn)
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
        gl[[ii]] <- pieceGrobHelper(piece_side[ii], i_s[ii], i_r[ii], cfg[[ii]],
                                        x[ii], y[ii], rot[ii], svg[ii],
                                        width[ii], height[ii], default.units)
    }
    gTree(children=gl, name=name, gp=gp, vp=vp)
}

#' @rdname grid.piece
#' @export
grid.piece <- function(piece_side="tile_back", i_s=NA, i_r=NA, cfg=list(), 
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                           rot=0, svg=FALSE,
                           width=NA, height=NA, 
                           default.units = "npc", envir=NULL,
                           name=NULL, gp=NULL, draw=TRUE, vp=NULL) {
    grob <- pieceGrob(piece_side, i_s, i_r, cfg, 
                          x, y, rot, svg, width, height, default.units, 
                          envir, name, gp, vp)
    if (draw) { 
        grid.draw(grob)
    } else {
        grob
    }
}
