#' @importFrom grImport2 pictureGrob readPicture
cc_file <- pictureGrob(readPicture(system.file("extdata/by-sa-svg.svg", package="piecepackr")))

is_odd <- function(x) { as.logical(x %% 2) }

#' Inch utility function
#'
#' This utility function is equivalent to \code{grid::unit(x, "in")}.
#' 
#' @param x Number representing number of inches
#' @export
inch <- function(x) { unit(x, "in") }

get_pp_die_arrangement <- function(component_side=NA, cfg=list()) {
    get_style_element("pp_die_arrangement", component_side, cfg, "counter")
}

## Layout functions
x_die_layoutRF <- c(1/4, 2/4, 2/4, 3/4, 3/4, 4/4) - 1/8
x_die_layoutLF <- c(4/4, 3/4, 3/4, 2/4, 2/4, 1/4) - 1/8
y_die_layout <- c(1/3, 1/3, 2/3, 2/3, 3/3, 3/3) - 1/6

draw_die_layoutRF <- function(component_side, i_s, i_r, cfg) {
    draw_piecepack_die(i_s, cfg)
}

draw_die_layoutLF <- function(component_side, i_s, i_r, cfg) {
    draw_piecepack_die(i_s, cfg, flip=TRUE)
}

draw_suitdie_layoutRF <- function(component_side, i_s, i_r, cfg) {
    draw_suit_die(cfg)
}

draw_suitdie_layoutLF <- function(component_side, i_s, i_r, cfg) {
    draw_suit_die(cfg, flip=TRUE)
}

draw_suitrankdie_layoutRF <- function(component_side, i_s, i_r, cfg) {
    draw_suitrank_die(cfg)
}
draw_suitrankdie_layoutLF <- function(component_side, i_s, i_r, cfg) {
    draw_suitrank_die(cfg, flip=TRUE)
}

draw_piecepack_die <- function(i_s, cfg, flip=FALSE) {
    angle <- rep(c(-90, 0), 3)
    if (flip) {
        x <- x_die_layoutLF
        angle <- -angle
    } else {
        x <- x_die_layoutRF
    }
    if (get_pp_die_arrangement(cfg=cfg) == "opposites_sum_to_5") {
        i_r <- c(1, 2, 3, 6, 5, 4)
        i_s <- rep(i_s, length.out=6)[i_r]
    } else {
        i_r <- 1:6
    }
    df <- tibble::tibble(component_side="die_face", i_s, i_r, x, y=y_die_layout, angle)
    draw_components(df, cfg=cfg)
}

draw_suit_die <- function(cfg, flip=FALSE) {
    angle <- rep(c(-90, 0), 3)
    if (flip) {
        x <- x_die_layoutLF
        angle <- -angle
    } else {
        x <- x_die_layoutRF
    }
    i_s <- get_suit_die_suits(cfg)
    df <- tibble::tibble(component_side="suitdie_face", i_s, x, y=y_die_layout, angle)
    draw_components(df, cfg=cfg)
}

get_suit_die_suits <- function(cfg) {
    n_suits <- get_n_suits(cfg)
    if (n_suits < 4) {
        rep(n_suits:1, length.out=6)
    } else if (n_suits == 4) {
        c(5,6,4:1)
    } else if (n_suits > 4) {
        6:1
    } 
}

draw_suitrank_die <- function(cfg, flip=FALSE) {
    i_s <- get_suit_die_suits(cfg)
    draw_piecepack_die(i_s, cfg, flip)
}

draw_pawn_layout <- function(component_side, i_s, i_r, cfg) {
    suppressWarnings({
        denominator <- PAWN_HEIGHT + PAWN_BASE
        y <- (PAWN_HEIGHT/2 + PAWN_BASE) / denominator
        height = PAWN_HEIGHT / denominator
        pushViewport(viewport(y=0.25, height=0.5))
        pushViewport(viewport(y=y, height=height))
        draw_component("pawn_face", cfg, i_s)
        popViewport()
        popViewport()
        pushViewport(viewport(y=0.75, height=0.5, name="pawn_rear", angle=180))
        pushViewport(viewport(y=y, height=height, name="pawn_back"))
        draw_component("pawn_back", cfg, i_s)
        popViewport()
        popViewport()
    })
    border_col <- get_border_color("pawn_face", i_s, 0, cfg)
    grid.lines(y=0.5, gp=gpar(col=border_col, fill=NA, lty="dashed"))
    grid.rect(gp=gpar(col=border_col, fill=NA))
    ll <- 0.07
    seg(0.5, 0, 0.5, ll, border_col)
    seg(0.5, 1, 0.5, 1-ll, border_col)
}

draw_pyramid_layout <- function(component_side, i_s, i_r, cfg) {
    suppressWarnings({
        t <- c(72, 36, 0, -36, -72)
        r <- 0.5
        x <- to_x(t, r)
        y <- 0.5 + 0.5*to_y(t, r)
        components <- c("pyramid_face", "pyramid_right", "pyramid_back", "pyramid_left", "pyramid_face")
        angles <- c(90+72, 90+36, 90, 90-36, 90-72)
        for(ii in 1:5) {
            pushViewport(viewport(width=inch(PYRAMID_WIDTHS[i_r]), height=inch(PYRAMID_HEIGHTS[i_r]), angle=angles[ii], x=x[ii], y=y[ii]))
            draw_component(components[ii], cfg, i_s, i_r)
            popViewport()
        }
    })
}

LETTER_WIDTH <- 8.5
LETTER_HEIGHT <- 11
A4_WIDTH <- 8.27
A4_HEIGHT <- 11.69
A5W <- 5
A5H <- 7.5

pp_pdf <- function(filename, family, paper) {
    if (paper == "letter") {
        cairo_pdf(filename, onefile=TRUE, width=LETTER_HEIGHT, height=LETTER_WIDTH, family=family)
    } else if (paper == "A4") {
        cairo_pdf(filename, onefile=TRUE, width=A4_HEIGHT, height=A4_WIDTH, family=family)
    } else if (paper == "A5") {
        cairo_pdf(filename, onefile=TRUE, width=A4_HEIGHT/2, height=A4_WIDTH, family=family)
    } else {
        stop(paste("Don't know how to handle paper", paper))
    }
}

#' Make print-and-play piecepack pdf
#'
#' Makes a print-and-play piecepack pdf.
#'
#' @param cfg Piecepack configuration list
#' @param output_filename Filename of PnP output
#' @param size PnP output size (currently either "letter" or "A4")
#' @param components Character vector of desired PnP components (default everything)
#' @export
make_pnp <- function(cfg=list(), output_filename="pdf/decks/piecepack_deck.pdf", size="letter", 
                     components=c("piecepack", "misc")) {
    unlink(output_filename)
    directory <- dirname(output_filename)
    dir.create(directory, recursive=TRUE, showWarnings=FALSE)

    n_suits <- get_n_suits(cfg)
    i_unsuit <- n_suits + 1
    cfg <- pp_cfg(cfg)
    if (size == "letter") {
        xl <- inch(LETTER_HEIGHT / 2 - A5W / 2)
        xr <- inch(LETTER_HEIGHT / 2 + A5W / 2)
    } else if (size == "A4") {
        xl <- inch(A4_HEIGHT / 2 - A5W / 2)
        xr <- inch(A4_HEIGHT / 2 + A5W / 2)
    } else { # size == "A5"
        xl <- 0.5
        xr <- 0.5
    }

    pp_pdf(output_filename, get_fontfamily(cfg), size)

    grid.newpage()
    pushViewport(viewport(x=xl, width=A5W))
    draw_a5_title(cfg$title)
    popViewport()
    if (size == "A5") { grid.newpage() }
    pushViewport(viewport(x=xr, width=A5W))
    draw_a5_blank()
    popViewport()

    if ("piecepack" %in% components) {
        for (i_s in 1:n_suits) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_piecepack(i_s, cfg, front=TRUE)
            popViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_piecepack(i_s, cfg, front=FALSE)
            popViewport()
        }
        if (is_odd(n_suits)) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_piecepack(i_unsuit+1, cfg, front=TRUE)
            popViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_piecepack(i_unsuit+1, cfg, front=FALSE)
            popViewport()
        }
    }

    #### Fine-tune between pyramids, matchsticks, and misc.
    #### Add misc. accessories
    if ("misc" %in% components) {
        if (n_suits <= 6) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_matchsticks(cfg, TRUE)
            popViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_matchsticks(cfg, FALSE)
            popViewport()
        }
        if (n_suits >= 4) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_pyramids(1:2, cfg, TRUE)
            popViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_pyramids(3:4, cfg, FALSE)
            popViewport()
        }
        if (n_suits > 4) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_pyramids(5:6, cfg, TRUE)
            popViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_blank()
            popViewport()
        }
    }
    invisible(dev.off())
}

title_text <- function(label, x, y) {
    gp <- gpar(fontsize=9, fontfamily="sans")
    grid.text(label, x, y, just="center", gp=gp)
}

draw_a5_title <- function(title="Piecepack collection") {
    pushViewport(viewport(width=inch(A5W), height=inch(A5H)))
    grid.rect(gp=gpar(color="brown"))
    header_height <- 1.5
    y_header <- A5H - header_height/2

    pushViewport(viewport(y=inch(y_header), width=inch(A5W), height=inch(header_height)))
    # title
    pushViewport(viewport(y=0.85, height=0.2))
    gp_title <- gpar(fontsize=15, fontfamily="sans", fontface="bold")
    grid.text(title, just="center", gp=gp_title)
    popViewport()

    # CC images
    width_image = 0.14
    pushViewport(viewport(x=width_image/2, y=0.4, width=width_image, height=0.5))
    grid.draw(cc_file)
    popViewport()
    pushViewport(viewport(x=1-width_image/2, y=0.4, width=width_image, height=0.5))
    grid.draw(cc_file)
    popViewport()

    # text
    pushViewport(viewport(x=0.5, y=0.4, width=1-2*width_image, height=0.8))
    title_text("Generated by the piecepackr R package", x=0.5, y=0.8)
    title_text("\u00a9 2016-2019 Trevor L Davis. Some Rights Reserved.", x=0.5, y=0.6)
    title_text("This work is licensed under a CC BY-SA 4.0 license:", x=0.5, y=0.4)
    title_text("https://creativecommons.org/licenses/by-sa/4.0", x=0.5, y=0.2)
    popViewport()

    popViewport()
    popViewport()
}

draw_a5_blank <- function() {
    pushViewport(viewport(width=inch(A5W), height=inch(A5H)))
    grid.rect(gp=gpar(color="brown"))
    grid.text("Intentionally left blank")
    popViewport()
}

draw_a5_matchsticks <- function(cfg=list(), front=TRUE) {
    n_suits <- get_n_suits(cfg) 
    pushViewport(viewport(width=inch(A5W), height=inch(A5H)))
    grid.rect(gp=gpar(color="brown"))
    y1t <- A5H - 0.5* MATCHSTICK_HEIGHTS[1]
    y1b <- A5H - 1.5* MATCHSTICK_HEIGHTS[1]
    x1s <- (0.5 + 0:(2*n_suits-1)) * MATCHSTICK_WIDTHS[1]
    xs <-  (0.5 + 0:(4*n_suits-1)) * MATCHSTICK_WIDTHS[2]
    y2  <- y1b - 0.5*MATCHSTICK_HEIGHTS[1] - 0.5*MATCHSTICK_HEIGHTS[2]
    y3  <- y2  - 0.5*MATCHSTICK_HEIGHTS[2] - 0.5*MATCHSTICK_HEIGHTS[3]
    y5  <- y3  - 0.5*MATCHSTICK_HEIGHTS[3] - 0.5*MATCHSTICK_HEIGHTS[5]
    y6  <- y5  - 0.5*MATCHSTICK_HEIGHTS[5] - 0.5*MATCHSTICK_HEIGHTS[6]
    y4s <- A5H - (0.5 + 0:3)*MATCHSTICK_HEIGHTS[4]
    x4s <- 4*n_suits * MATCHSTICK_WIDTHS[2] + (0.5 + 0:(n_suits-1)) * MATCHSTICK_WIDTHS[6]

    x = c(rep(x1s,each=2), rep(xs, 4), rep(x4s, each=4))
    y = c(rep(c(y1t, y1b), 2*n_suits), rep(c(y2, y3, y5, y6), each=4*n_suits), rep(c(y4s), n_suits))
    i_s = rep(rep(1:n_suits, each=4), 6)
    i_r = rep(c(1:3,5:6,4), each=4*n_suits)
    if (front) {
        component_side = "matchstick_face"
    } else {
        x <- A5W - x
        component_side = "matchstick_back"

    }
    df <- tibble::tibble(component_side, x, y, i_s, i_r)
    draw_components(df, cfg=cfg, units="inches")
    popViewport()
}

draw_a5_piecepack <- function(i_s, cfg=list(), front=TRUE) {
    i_unsuit <- get_i_unsuit(cfg)
    pushViewport(viewport(width=inch(A5W), height=inch(A5H)))
    grid.rect(gp=gpar(color="brown"))
    xtl <- 1.5 * TILE_WIDTH
    xtr <- 0.5 * TILE_WIDTH
    ytb <- 0.5 * TILE_WIDTH
    ytm <- 1.5 * TILE_WIDTH
    ytt <- 2.5 * TILE_WIDTH
    xc <-A5W - 0.25 * TILE_WIDTH
    # ycs <- 0.50 + seq(0, 2.50, 0.50) * TILE_WIDTH
    ycs <- (0.50 + seq(0, 5)) * 0.8
    # xdl <- A5W - 4 * DIE_WIDTH
    # xdm <- A5W - 6 * DIE_WIDTH
    xdr <- A5W - 8 * DIE_WIDTH
    yd <- A5H - 1.5 * DIE_WIDTH
    xp <- DIE_LAYOUT_WIDTH + 0.5 * PAWN_LAYOUT_HEIGHT
    yp <- A5H - 0.5 * PAWN_WIDTH
    xb <- DIE_LAYOUT_WIDTH + 0.5 * BELT_WIDTH
    yb <- A5H - PAWN_WIDTH - 0.5 * BELT_HEIGHT
    xsl <- A5W - 0.75 * TILE_WIDTH
    xsr <- A5W - 0.25 * TILE_WIDTH
    ysb <- 2.75 * TILE_WIDTH 
    x <- c(rep(c(xtl, xtr), 3), rep(xc, 6), xdr, xp, xb, xsr)
    y <- c(rep(c(ytt, ytm, ytb), each=2), ycs, yd, yp, yb, ysb)
    if (front) {
        x <- A5W - x
        component_side <- c(rep("tile_face", 6), rep("coin_back", 6),
                            rep("die_layoutRF", 1), "pawn_layout", "belt_face", "saucer_face")
        i_ss <- c(rep(i_s, 16))
        i_r <- c(1:6, rep(NA, 6), rep(NA, 4))
        angle = c(rep(0, 13), 90, rep(0, 2))
    } else {
        component_side <- c(rep("tile_back", 6), rep("coin_face", 6),
                            rep("die_layoutLF", 1), "pawn_layout", "belt_face", "saucer_back")
        i_ss <- c(rep(NA, 12), rep(i_s, 3), i_unsuit)
        i_r <- c(rep(NA, 6), 1:6, rep(NA, 4))
        angle = c(rep(0, 13), 90, rep(0, 2))
    }
    df <- tibble::tibble(component_side, x, y, i_s=i_ss, i_r, angle)
    draw_components(df, cfg=cfg, units="inches")
    popViewport()
}

draw_a5_pyramids <- function(i_s=1:2, cfg=list(), front=TRUE) {
    n_ranks <- get_n_ranks(cfg)
    n_suits <- get_n_suits(cfg) 
    pushViewport(viewport(width=inch(A5W), height=inch(A5H)))
    grid.rect(gp=gpar(color="brown"))
    y_up <- rep(cumsum(PYRAMID_LAYOUT_HEIGHTS) - 0.5*PYRAMID_LAYOUT_HEIGHTS, each=2)
    x_up <- rep(c(0.5, 1.5), 7)*rep(PYRAMID_LAYOUT_WIDTHS, each=2)
    x <- c(x_up[1:8], A5W - x_up[9:12]- rep(c(0.75,1),each=2))
    p5h <- PYRAMID_LAYOUT_HEIGHTS[5]; p6h <- PYRAMID_LAYOUT_HEIGHTS[6]
    y <- c(y_up[1:8], rep(c(p6h+0.5*p5h, 0.5*p6h), each=2))
    component_side <- "pyramid_layout"
    i_s <- rep(i_s, 6)
    i_r <- rep(1:6, each=2)
    angle <- c(rep(c(180, 0), 4), rep(c(0, 180), 2))
    df <- tibble::tibble(component_side, x, y, i_s, i_r, angle)
    draw_components(df, cfg=cfg, units="inches")
    popViewport()
}

#### Separate out add_metadata and remove
# make_collection <- function(output_filename, input_filenames, size="letter", metadata=list()) {
#     unlink(output_filename)
#     directory <- dirname(output_filename)
#     dir.create(directory, recursive=TRUE, showWarnings=FALSE)
# 
#     # add bookmarks
#     pm_filename <- tempfile(fileext=".txt")
#     make_pdfmark_txt(pm_filename, input_filenames)
# 
#     temp_pdf <- tempfile(fileext=".pdf")
#     cmd <- find_gs()
#     args <- c("-q", "-o", shQuote(temp_pdf), "-sDEVICE=pdfwrite", pm_filename, shQuote(input_filenames))
#     system2(cmd, args)
# 
#     #### What to do with metadata
#     pm_filename <- tempfile(fileext=".txt")
#     make_pdfmark_metadata(pm_filename, metadata$title, metadata$author, metadata$subject, metadata$keywords)
#     args <- c("-q", "-o", shQuote(output_filename), "-sDEVICE=pdfwrite", pm_filename, shQuote(temp_pdf))
#     system2(cmd, args)
# 
# }
# 
# make_pdfmark_txt <- function(pm_filename, input_filenames) {
#     deck_filenames <- input_filenames[-1]
#     n_sets <- length(deck_filenames) 
# 
#     n_preview <- ceiling(n_sets / 6)
#     if (is_odd(n_preview))
#         n_preview <- n_preview + 1
#     txt <- "[/Page 1 /View [/XYZ null null null] /Title (Piecepack Sets Preview) /OUT pdfmark"
#     next_page <- n_preview + 1
#     for(ii in 1:n_sets) {
#         new_txt <- sprintf("[/Page %s /View [/XYZ null null null] /Title (Piecepack Set #%s) /OUT pdfmark", next_page, ii)
#         txt <- append(txt, new_txt)
#         next_page <- next_page + get_n_pages(deck_filenames[ii])
#     }
#     writeLines(txt, pm_filename)
# }
# 
# make_pdfmark_metadata <- function(pm_filename, title="Piecepack collection", author="", subject="", keywords="piecepack") {
#     txt <- paste0("[ /Title (", title, ")\n /Author (", author, ")\n /Subject (", 
#                 subject, ")\n /Keywords (", keywords, ")\n /DOCINFO pdfmark")
# 
#     writeLines(txt, pm_filename)
# }
