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
#' @param z z-coordinate of the piece.  Has no effect if \code{op_scale} is \code{0}.
#' @param angle Angle (on xy plane) to draw piece at
#' @param use_pictureGrob If \code{TRUE} instead of directly returning the grob first 
#'            export to (temporary) svg and then re-import as a \code{grImport2::pictureGrob}.  
#'            This is useful if drawing pieces really big or small and don't want
#'            to play with re-configuring fontsizes.
#' @param width Width of piece
#' @param height Height of piece
#' @param depth Depth (thickness) of piece.  Has no effect if \code{op_scale} is \code{0}.
#' @param op_scale How much to scale the depth of the piece in the oblique projection (viewed from the top of the board).
#'              \code{0} (the default) leads to an \dQuote{orthographic} projection,
#'              \code{0.5} is the most common scale used in the \dQuote{cabinet} projection, 
#'               and \code{1.0} is the scale used in the \dQuote{cavalier} projection.
#' @param op_angle What is the angle of the oblique projection?  Has no effect if \code{op_scale} is \code{0}.
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
#' @examples
#'   if (require("grid")) {
#'        draw_pp_diagram <- function(cfg=pp_cfg(), op_scale=0) {
#'            g.p <- function(...) { 
#'                grid.piece(..., op_scale=op_scale, cfg=cfg, default.units="in") 
#'            }
#'            g.p("tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1))
#'            g.p("tile_back", x=0.5+3, y=0.5+1, z=1/4+1/8)
#'            g.p("tile_back", x=0.5+3, y=0.5+1, z=2/4+1/8)
#'            g.p("die_face", suit=3, rank=5, x=1, y=1, z=1/4+1/4)
#'            g.p("pawn_face", x=1, y=4, z=1/4+1/2, angle=90)
#'            g.p("coin_back", x=3, y=4, z=1/4+1/16, angle=180)
#'            g.p("coin_back", suit=4, x=3, y=4, z=1/4+1/8+1/16, angle=180)
#'            g.p("coin_back", suit=2, x=3, y=1, z=3/4+1/8, angle=90)
#'        }
#'
#'        # default piecepack, orthogonal projection
#'        draw_pp_diagram(cfg=pp_cfg())
#'        
#'        # custom configuration, orthogonal projection
#'        grid.newpage()
#'        dark_colorscheme <- list(suit_color="darkred,black,darkgreen,darkblue,black",
#'                             invert_colors.suited=TRUE, border_color="black", border_lex=2)
#'        traditional_ranks <- list(use_suit_as_ace=TRUE, rank_text=",a,2,3,4,5")
#'        cfg <- c(dark_colorscheme, traditional_ranks)
#'        draw_pp_diagram(cfg=pp_cfg(cfg))
#'        
#'        # custom configuration, oblique projection
#'        grid.newpage()
#'        cfg3d <- list(width.pawn=0.75, height.pawn=0.75, depth.pawn=1, 
#'                           dm_text.pawn="", shape.pawn="convex6", invert_colors.pawn=TRUE,
#'                           edge_color.coin="tan", edge_color.tile="tan")
#'        cfg <- pp_cfg(c(cfg, cfg3d))
#'        draw_pp_diagram(cfg=pp_cfg(cfg), op_scale=0.5)
#'        
#'        # pmap_piece lets you use data frame input
#'        grid.newpage()
#'        df_tiles <- data.frame(piece_side="tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1),
#'                               suit=NA, angle=NA, z=NA, stringsAsFactors=FALSE)
#'        df_coins <- data.frame(piece_side="coin_back", x=rep(4:1, 4), y=rep(4:1, each=4),
#'                               suit=1:16%%2+rep(c(1,3), each=8),
#'                               angle=rep(c(180,0), each=8), z=1/4+1/16, stringsAsFactors=FALSE)
#'        df <- rbind(df_tiles, df_coins)
#'        pmap_piece(df, cfg=cfg, op_scale=0.5, default.units="in")
#'   }
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

pieceGrob_wrapper <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(), 
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                           angle=NA, use_pictureGrob=FALSE, 
                           width=NA, height=NA, depth=NA,
                           op_scale=0, op_angle=45,
                           default.units="npc", envir=NULL, ...) {
    pieceGrob(piece_side, suit, rank, cfg, x, y, z, 
                   angle, use_pictureGrob, width, height, depth,
                   op_scale, op_angle, default.units, envir)
}


pieceGrobHelper <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=pp_cfg(), 
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                           angle=0, use_pictureGrob=FALSE,
                           width=NA, height=NA, depth=NA, 
                           op_scale=0, op_angle=45,
                           default.units = "npc") {
    cfg <- as_pp_cfg(cfg)
    suit <- ifelse(has_suit(piece_side), ifelse(is.na(suit), 1, suit), cfg$i_unsuit)
    suit <- ifelse(suit > cfg$i_unsuit+1, cfg$i_unsuit+1, suit)
    rank <- ifelse(has_rank(piece_side), ifelse(is.na(rank), 1, rank), 0)
    if(is.na(angle)) { angle <- 0 }
    if(is.na(width)) { width <- inch(cfg$get_width(piece_side, suit, rank)) }
    if(is.na(height)) { height <- inch(cfg$get_height(piece_side, suit, rank)) }
    if(is.na(depth)) { depth <- inch(cfg$get_depth(piece_side, suit, rank)) }
    if(is.na(z)) { z <- 0.5 * depth }
    if(!is.unit(x)) { x <- unit(x, default.units) }
    if(!is.unit(y)) { y <- unit(y, default.units) }
    if(!is.unit(z)) { z <- unit(z, default.units) }
    if(!is.unit(width)) { width <- unit(width, default.units) }
    if(!is.unit(height)) { height <- unit(height, default.units) }
    if(!is.unit(depth)) { depth <- unit(depth, default.units) }
    if (use_pictureGrob) 
        grob <- cfg$get_pictureGrob(piece_side, suit, rank)
    else
        grob <- cfg$get_grob(piece_side, suit, rank)
    if (op_scale < 0.01) {
        cvp <- viewport(x, y, width, height, angle=angle)
        grobTree(grob, vp=cvp)
    } else {
        xp <- op_x(x, y, z+0.5*depth, op_angle, op_scale)
        yp <- op_y(x, y, z+0.5*depth, op_angle, op_scale)
        cvp <- viewport(xp, yp, width, height, angle=angle)
        grob <- grobTree(grob, vp=cvp)
        shadow_fn <- cfg$get_shadow_fn(piece_side, suit, rank)
        shadow <- shadow_fn(piece_side, suit, rank, cfg, 
                            x, y, z, angle, width, height, depth,
                            op_scale, op_angle, default.units) 
        grobTree(shadow, grob)
    }
}

op_x <- function(x, y, z, op_angle=45, op_scale=0) {
    x + op_scale * z* cos((op_angle) * 2 * pi / 360)  

}
op_y <- function(x, y, z, op_angle=45, op_scale=0) {
    y + op_scale * z * sin((op_angle) * 2 * pi / 360) 
}

#' @rdname grid.piece
#' @export
pieceGrob <- function(piece_side="tile_back", suit=NA, rank=NA, 
                         cfg=pp_cfg(), 
                         x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                         angle=0, use_pictureGrob=FALSE,
                         width=NA, height=NA, depth=NA,
                         op_scale=0, op_angle=45,
                         default.units = "npc", envir=NULL,
                         name=NULL, gp=NULL, vp=NULL) {

    nn <- max(lengths(list(piece_side, suit, rank, x, y, z, angle, use_pictureGrob, width, height, depth)))
    piece_side <- rep(piece_side, length.out=nn)
    suit <- rep(suit, length.out=nn)
    rank <- rep(rank, length.out=nn)
    x <- rep(x, length.out=nn)
    y <- rep(y, length.out=nn)
    z <- rep(z, length.out=nn)
    angle <- rep(angle, length.out=nn)
    use_pictureGrob <- rep(use_pictureGrob, length.out=nn)
    width <- rep(width, length.out=nn)
    height <- rep(height, length.out=nn)
    depth <- rep(depth, length.out=nn)

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
                                        x[ii], y[ii], z[ii], angle[ii], use_pictureGrob[ii],
                                        width[ii], height[ii], depth[ii], 
                                        op_scale, op_angle, default.units)
    }
    gTree(children=gl, name=name, gp=gp, vp=vp)
}

#' @rdname grid.piece
#' @export
grid.piece <- function(piece_side="tile_back", suit=NA, rank=NA, cfg=list(), 
                           x=unit(0.5, "npc"), y=unit(0.5, "npc"), z=NA,
                           angle=0, use_pictureGrob=FALSE,
                           width=NA, height=NA, depth=NA,
                           op_scale=0, op_angle=45,
                           default.units = "npc", envir=NULL,
                           name=NULL, gp=NULL, draw=TRUE, vp=NULL) {
    grob <- pieceGrob(piece_side, suit, rank, cfg, 
                          x, y, z, angle, use_pictureGrob, width, height, depth,
                          op_scale, op_angle, default.units, 
                          envir, name, gp, vp)
    if (draw) { 
        grid.draw(grob)
    } else {
        grob
    }
}
