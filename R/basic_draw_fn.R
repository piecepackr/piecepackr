add_gridlines <- function(col, shape, shape_t) {
    if (is_color_invisible(col)) return (invisible(NULL))
    o <- 0.02
    if (shape == "rect") {
        lwd <- 8
        gp_gl <- gpar(col=col, lwd=lwd, lineend="butt")
        grid.lines(x=0.5, gp=gp_gl)
        grid.lines(y=0.5, gp=gp_gl)
        # seg(0.5, 0+o, 0.5, 1-o, col, lwd=lwd, lineend="square")
        # seg(0+o, 0.5, 1-o, 0.5, col, lwd=lwd, lineend="square")
    } else if (shape %in% c("circle", "kite", "halma")) {
        stop(paste("Don't know how to add grid lines to shape", shape))
    } else {
        o <- 0.01
        lwd <- 4
        n_vertices <- get_n_vertices(shape)
        t <- seq(0, 360, length.out=n_vertices+1) + shape_t
        t <- t[1:(length(t)-1)]
        nt <- length(t)
        n <- floor(nt / 2)
        r <- 0.5 - o
        x <- 0.5 + to_x(t, r)
        y <- 0.5 + to_y(t, r)
        for (ii in 1:nt) {
            i_next <- ii+n
            if (i_next > nt)
                i_next <- i_next %% nt
            seg(x[ii], y[ii], x[i_next], y[i_next] , col, lwd=lwd)
        }
    }
}

# add_checkers <- function(col, shape, shape_t) {
#     if (is_color_invisible(col)) return (invisible(NULL))
#     if (shape == "rect") {
#         grid.rect(x=0.25, y=0.25, width=0.5, height=0.5, gp=gpar(col=NA, fill=col))
#         grid.rect(x=0.75, y=0.75, width=0.5, height=0.5, gp=gpar(col=NA, fill=col))
#     } else if (shape %in% c("circle", "kite", "halma")) {
#         stop(paste("Don't know how to add checkers to shape", shape))
#     } else {
#         n_vertices <- get_n_vertices(shape)
#         t <- seq(0, 360, length.out=n_vertices+1) + shape_t
#         nt <- length(t) - 1
#         r <- 0.5
#         x <- 0.5 + to_x(t, r)
#         y <- 0.5 + to_y(t, r)
#         for (ii in 1:nt) {
#             if( ii %% 2) {
#                 xs <- c(0.5, x[ii], x[ii+1])
#                 ys <- c(0.5, y[ii], y[ii+1])
#                 grid.polygon(xs, ys, gp=gpar(col=NA, fill=col))
#             }
#         }
#     }
# }

is_color_invisible <- function(color) {
    if (is.na(color))
        return (TRUE)
    if (color == "transparent")
        return (TRUE)
    return (FALSE)
}

# add_hexlines <- function(col, shape, omit_direction=FALSE) {
#     if(is_color_invisible(col)) return (invisible(NULL))
#     if (shape != "rect") {
#         stop(paste("Don't know how to add hexlines to shape", shape))
#     }
#     ho <- 0.25
#     hl_size <- 4
#     if (omit_direction %in% 1:2)  # upper left
#         NULL
#     else
#         seg(0, 1 - ho, ho, 1, col, lwd=hl_size) 
#     if (omit_direction %in% 3:4)  # lower left
#         NULL
#     else
#         seg(0, ho, ho, 0, col, lwd=hl_size) 
#     if (omit_direction %in% 5:6)  # lower right
#         NULL
#     else
#         seg(1, ho, 1 - ho, 0, col, lwd=hl_size) 
#     if (omit_direction %in% 7:8)  # upper right
#         NULL
#     else
#         seg(1, 1 - ho, 1 - ho, 1, col, lwd=hl_size) 
# }

add_mat <- function(col, shape, t, width) {
    if (is_color_invisible(col) || all(width==0))
        return (invisible(NULL))
    gp_mat <- gpar(col=NA, fill=col)
    if (shape == "rect") {
        grid.rect_mat_fn(width)(gp=gp_mat)
    } else if (shape == "circle") {
        grid.convex_mat_fn(60, 0, width[1])(gp=gp_mat)
    } else if (grepl("^convex", shape)) {
        grid.convex_mat_fn(get_n_vertices(shape), t, width[1])(gp=gp_mat)
    } else {
        stop(paste("Don't know how to add mat to shape", shape))
    }
}

draw_component_basic <- function(component_side, i_s, i_r, cfg) {
    opt <- get_component_opt(component_side, i_s, i_r, cfg)

    shape_fn <- get_grid_shape(opt$shape, opt$shape_t, opt$shape_r)

    # Background
    shape_fn(gp=gpar(col=NA, fill=opt$background_col))

    # Gridlines, Mat
    add_gridlines(opt$gridline_col, opt$shape, opt$shape_t)
    add_mat(opt$mat_col, opt$shape, opt$shape_t, opt$mat_width)

    # Primary symbol
    gp_ps <- gpar(col=opt$ps_col, fontsize=opt$ps_fontsize, 
                  fontfamily=opt$ps_fontfamily, fontface=opt$ps_fontface)
    grid.text(opt$ps_text, x=opt$ps_x, y=opt$ps_y, gp=gp_ps)

    # Directional mark
    gp_dm <- gpar(col=opt$dm_col, fontsize=opt$dm_fontsize, 
                  fontfamily=opt$dm_fontfamily, fontface=opt$ps_fontface)
    grid.text(opt$dm_text, x=opt$dm_x, y=opt$dm_y, gp=gp_dm)

    # Border col
    shape_fn(gp=gpar(col=opt$border_col, fill=NA))

    invisible(NULL)
}
