#' @importFrom grImport2 pictureGrob readPicture
cc_file <- pictureGrob(readPicture(system.file("extdata/by-sa-svg.svg", package="piecepackr")))

is_odd <- function(x) { as.logical(x %% 2) }

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
#' @param pieces Character vector of desired PnP pieces (default everything)
#' @export
make_pnp <- function(cfg=list(), output_filename="piecepack.pdf", size="letter", 
                     pieces=c("piecepack", "matchsticks", "pyramids")) {
    unlink(output_filename)
    directory <- dirname(output_filename)
    dir.create(directory, recursive=TRUE, showWarnings=FALSE)

    cfg <- as_pp_cfg(cfg)
    n_suits <- cfg$n_suits

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

    pp_pdf(output_filename, cfg$fontfamily, size)

    grid.newpage()
    pushViewport(viewport(x=xl, width=A5W))
    draw_a5_title(cfg$title)
    upViewport()
    if (size == "A5") { grid.newpage() }
    pushViewport(viewport(x=xr, width=A5W))
    draw_a5_blank()
    upViewport()

    if ("piecepack" %in% pieces) {
        for (i_s in 1:n_suits) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_piecepack(i_s, cfg, front=TRUE)
            upViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_piecepack(i_s, cfg, front=FALSE)
            upViewport()
        }
        if (is_odd(n_suits)) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_piecepack(cfg$i_unsuit+1, cfg, front=TRUE)
            upViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_piecepack(cfg$i_unsuit+1, cfg, front=FALSE)
            upViewport()
        }
    }

    #### Fine-tune between pyramids, matchsticks, and misc.
    #### Add misc. accessories
    if ("matchsticks" %in% pieces) {
        if (n_suits <= 6) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_matchsticks(cfg, TRUE)
            upViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_matchsticks(cfg, FALSE)
            upViewport()
        }
    }
    if ("pyramids" %in% pieces) {
        if (n_suits >= 4) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_pyramids(1:2, cfg, TRUE)
            upViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_pyramids(3:4, cfg, FALSE)
            upViewport()
        }
        if (n_suits > 4) {
            grid.newpage()
            pushViewport(viewport(x=xl, width=A5W))
            draw_a5_pyramids(5:6, cfg, TRUE)
            upViewport()
            if (size == "A5") { grid.newpage() }
            pushViewport(viewport(x=xr, width=A5W))
            draw_a5_blank()
            upViewport()
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
    upViewport()

    # CC images
    width_image = 0.14
    pushViewport(viewport(x=width_image/2, y=0.4, width=width_image, height=0.5))
    grid.draw(cc_file)
    upViewport()
    pushViewport(viewport(x=1-width_image/2, y=0.4, width=width_image, height=0.5))
    grid.draw(cc_file)
    upViewport()

    # text
    pushViewport(viewport(x=0.5, y=0.4, width=1-2*width_image, height=0.8))
    title_text("Generated by the piecepackr R package", x=0.5, y=0.8)
    title_text("\u00a9 2016-2019 Trevor L Davis. Some Rights Reserved.", x=0.5, y=0.6)
    title_text("This work is licensed under a CC BY-SA 4.0 license:", x=0.5, y=0.4)
    title_text("https://creativecommons.org/licenses/by-sa/4.0", x=0.5, y=0.2)
    upViewport()

    upViewport()
    upViewport()
}

draw_a5_blank <- function() {
    pushViewport(viewport(width=inch(A5W), height=inch(A5H)))
    grid.rect(gp=gpar(color="brown"))
    grid.text("Intentionally left blank")
    upViewport()
}

draw_a5_matchsticks <- function(cfg=pp_cfg(), front=TRUE) {
    n_suits <- cfg$n_suits
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
        piece_side = "matchstick_face"
    } else {
        x <- A5W - x
        piece_side = "matchstick_back"

    }
    df <- tibble::tibble(piece_side, x, y, i_s, i_r)
    pmap_piece(df, cfg=cfg, default.units="inches")
    upViewport()
}

draw_a5_piecepack <- function(i_s, cfg=pp_cfg(), front=TRUE) {
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
        piece_side <- c(rep("tile_face", 6), rep("coin_back", 6),
                            rep("die_layoutRF", 1), "pawn_layout", "belt_face", "saucer_face")
        i_ss <- c(rep(i_s, 16))
        i_r <- c(1:6, rep(NA, 6), rep(NA, 4))
        rot = c(rep(0, 13), 90, rep(0, 2))
    } else {
        piece_side <- c(rep("tile_back", 6), rep("coin_face", 6),
                            rep("die_layoutLF", 1), "pawn_layout", "belt_face", "saucer_back")
        i_ss <- c(rep(NA, 12), rep(i_s, 3), cfg$i_unsuit)
        i_r <- c(rep(NA, 6), 1:6, rep(NA, 4))
        rot = c(rep(0, 13), 90, rep(0, 2))
    }
    df <- tibble::tibble(piece_side, x, y, i_s=i_ss, i_r, rot)
    pmap_piece(df, cfg=cfg, default.units="inches")
    upViewport()
}

draw_a5_pyramids <- function(i_s=1:2, cfg=pp_cfg(), front=TRUE) {
    n_ranks <- cfg$n_ranks
    n_suits <- cfg$n_suits
    pushViewport(viewport(width=inch(A5W), height=inch(A5H)))
    grid.rect(gp=gpar(color="brown"))
    y_up <- rep(cumsum(PYRAMID_LAYOUT_HEIGHTS) - 0.5*PYRAMID_LAYOUT_HEIGHTS, each=2)
    x_up <- rep(c(0.5, 1.5), 7)*rep(PYRAMID_LAYOUT_WIDTHS, each=2)
    x <- c(x_up[1:8], A5W - x_up[9:12]- rep(c(0.75,1),each=2))
    p5h <- PYRAMID_LAYOUT_HEIGHTS[5]; p6h <- PYRAMID_LAYOUT_HEIGHTS[6]
    y <- c(y_up[1:8], rep(c(p6h+0.5*p5h, 0.5*p6h), each=2))
    piece_side <- "pyramid_layout"
    i_s <- rep(i_s, 6, length.out=12)
    i_r <- rep(1:6, each=2)
    rot <- c(rep(c(180, 0), 4), rep(c(0, 180), 2))
    df <- tibble::tibble(piece_side, x, y, i_s, i_r, rot)
    pmap_piece(df, cfg=cfg, default.units="inches")
    upViewport()
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
