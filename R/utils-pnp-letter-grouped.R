INTER_W <- min(LETTER_HEIGHT, A4_HEIGHT) # 11" — letter/A4 intersection width
INTER_H <- min(LETTER_WIDTH, A4_WIDTH) # 8.27" — letter/A4 intersection height

print_and_play_paper_grouped <- function(cfg, size, pieces, arrangement, quietly, size_bleed) {
	n_suits <- cfg$n_suits
	n_ranks <- cfg$n_ranks

	stopifnot(n_ranks <= 6)
	if ("matchsticks" %in% pieces) {
		abort('"matchsticks" `pieces` not currently supported for `bleed = "grouped"`')
	}
	if ("pyramids" %in% pieces) {
		abort('"pyramids" `pieces` not currently supported for `bleed = "grouped"`')
	}
	if ("subpack" %in% pieces) {
		abort('"subpack" `pieces` not currently supported for `bleed = "grouped"`')
	}
	if (size == "A5") {
		abort('`size = "A5"` not supported for `bleed = "grouped"`')
	}

	# Draw within a viewport equal to the intersection of the letter and A4
	# paper sizes (11" x 8.27"), centered on the page — both paper sizes then
	# share the same piece coordinates.
	xl_off <- INTER_W / 2 - A5W + size_bleed$left
	xr_off <- INTER_W / 2 + size_bleed$left
	y_off <- (INTER_H - A5H) / 2 + size_bleed$bottom
	vp_draw <- viewport(
		width = inch(INTER_W + size_bleed$left + size_bleed$right),
		height = inch(INTER_H + size_bleed$top + size_bleed$bottom)
	)

	# A5 viewport centers (for front-matter grobs that use A5 coordinates)
	xl_ctr <- xl_off + A5W / 2
	xr_ctr <- xr_off + A5W / 2
	y_ctr <- y_off + A5H / 2
	vpl <- viewport(x = inch(xl_ctr), y = inch(y_ctr), width = inch(A5W), height = inch(A5H))
	vpr <- viewport(x = inch(xr_ctr), y = inch(y_ctr), width = inch(A5W), height = inch(A5H))

	gl <- list()
	pl <- list()

	## Front Matter — wrap A5 grobs in positioned viewports so they render like
	## the piecepack grobs (plain grid.draw in the loop below).
	gl <- gappend(
		gl,
		gTree(children = gList(a5_title_grob(cfg, pieces, quietly, bleed = TRUE)), vp = vpl)
	)
	gl <- gappend(
		gl,
		gTree(children = gList(a5_inst_grob_bleed(cfg, pieces, arrangement, size)), vp = vpr)
	)
	if (arrangement == "double-sided") {
		gl <- gappend(gl, gTree(children = gList(blank_grob), vp = vpl))
		gl <- gappend(gl, gTree(children = gList(blank_grob), vp = vpr))
		pl[["Front Matter"]] <- 2
	} else {
		pl[["Front Matter"]] <- 1
	}

	## Piecepack — grobs carry page/intersection coordinates directly
	if ("piecepack" %in% pieces) {
		n_pages <- n_suits
		if (is_odd(n_suits) && arrangement == "double-sided") {
			n_pages <- n_pages + 1
		}
		for (suit in seq(n_pages)) {
			gl <- gappend(
				gl,
				a5_piecepack_grob_shared(suit, cfg, TRUE, arrangement, xl_off, y_off)
			)
			gl <- gappend(
				gl,
				a5_piecepack_grob_shared(suit, cfg, FALSE, arrangement, xr_off, y_off)
			)
		}
		pl$Piecepack <- n_pages
	}

	for (ii in seq(gl)) {
		if (is_odd(ii)) {
			grid.newpage()
			pushViewport(vp_draw)
		}
		grid.draw(gl[[ii]])
		if (!is_odd(ii)) {
			upViewport()
		}
	}

	pl
}

a5_piecepack_grob_shared <- function(suit, cfg, front, arrangement, x_off, y_off) {
	BLEED <- 1 / 8
	tile_width <- cfg$get_width("tile_back")
	die_width <- cfg$get_width("die_face")
	pawn_width <- cfg$get_width("pawn_width")
	coin_diam <- cfg$get_width("coin_face")

	# Tile centers shifted 1/8" toward the outer page edge.
	# On the front (left A5) this means shifting left; the back is produced by
	# mirroring (A5W - x), which automatically shifts right on the right A5.
	# Result: tile/coin bleed zones share a cut line at x = 2*tile_width within
	# the A5, and the outer tile bleed reaches exactly the 0.25" printer safe zone.
	xtr <- 0.5 * tile_width - BLEED
	xtl <- 1.5 * tile_width - BLEED
	ytb <- 0.5 * tile_width
	ytm <- 1.5 * tile_width
	ytt <- 2.5 * tile_width

	xc <- A5W - 0.25 * tile_width
	ycs <- rev((0.50 + seq(0, 5)) * 0.8)

	xsr <- A5W - 0.25 * tile_width
	ysb <- 2.75 * tile_width

	xdr <- c(0.5, 1.5, 2.5) * die_width
	ydt <- A5H - 0.5 * die_width
	ydb <- A5H - 1.5 * die_width

	xp <- A5W - 0.5 * cfg$get_height("pawn_layout")
	yp <- A5H - 0.5 * pawn_width
	xb <- A5W - 0.5 * cfg$get_width("belt_face")
	yb <- A5H - pawn_width - 0.5 * cfg$get_height("belt_face") - 0.25

	dft <- tibble(
		piece_side = "tile_face",
		x = rep(c(xtr, xtl), 3),
		y = rep(c(ytt, ytm, ytb), each = 2),
		suit,
		rank = 1:6,
		angle = 0
	)
	dfc <- tibble(
		piece_side = "coin_back",
		x = rep(xc, 6),
		y = ycs,
		suit,
		rank = 1:6,
		angle = ifelse(front, cfg$coin_arrangement, 0)
	)
	dfd <- tibble(
		piece_side = "die_face",
		x = rep(xdr, 2),
		y = rep(c(ydt, ydb), each = 3),
		suit,
		rank = 1:6,
		angle = 0
	)
	dfp <- tibble(piece_side = "pawn_layout", x = xp, y = yp, suit, rank = NA, angle = 90)
	dfb <- tibble(piece_side = "belt_face", x = xb, y = yb, suit, rank = NA, angle = 0)
	dfs <- tibble(
		piece_side = "saucer_face",
		x = xsr,
		y = ysb,
		suit,
		rank = NA,
		angle = ifelse(front, 0, cfg$coin_arrangement)
	)

	if (!front) {
		mirror <- function(df) {
			df$x <- A5W - df$x
			df
		}
		dft <- mirror(dft)
		dft$piece_side <- "tile_back"
		dft$angle <- 90 * ((suit + dft$rank) %% 4)
		dfc <- mirror(dfc)
		dfc$piece_side <- "coin_face"
		dfd <- mirror(dfd)
		dfp <- mirror(dfp)
		dfb <- mirror(dfb)
		dfs <- mirror(dfs)
		dfs$piece_side <- "saucer_back"
	}

	# Shift from A5-local coordinates to page/intersection coordinates
	offset <- function(df) {
		df$x <- df$x + x_off
		df$y <- df$y + y_off
		df
	}
	dft <- offset(dft)
	dfc <- offset(dfc)
	dfd <- offset(dfd)
	dfp <- offset(dfp)
	dfb <- offset(dfb)
	dfs <- offset(dfs)

	# Bleed colors
	if (front) {
		tile_bc <- cfg$get_piece_opt("tile_face", suit, 1)$bleed_color
		coin_bc <- cfg$get_piece_opt("coin_back", suit, 1)$bleed_color
	} else {
		tile_bc <- cfg$get_piece_opt("tile_back", suit, 1)$bleed_color
		coin_bc <- cfg$get_piece_opt("coin_face", suit, 1)$bleed_color
	}
	die_bc <- cfg$get_piece_opt("die_face", suit, 1)$bleed_color

	# Shared bleed rect covering the 2x3 tile grid plus 1/8" on each outer edge.
	# The inner edge (shared cut with coins) falls at exactly 2*tile_width (A5-local).
	if (front) {
		tile_br_cx <- x_off + tile_width - BLEED
	} else {
		tile_br_cx <- x_off + A5W - tile_width + BLEED
	}
	bleed_tiles <- rectGrob(
		x = unit(tile_br_cx, "in"),
		y = unit(y_off + 1.5 * tile_width, "in"),
		width = unit(2 * tile_width + 2 * BLEED, "in"),
		height = unit(3 * tile_width + 2 * BLEED, "in"),
		just = "center",
		gp = gpar(fill = tile_bc, col = NA)
	)

	# Shared bleed rect for the column of 6 coins.
	# Left edge (front) is the shared tile-coin cut at 2*tile_width (A5-local);
	# right edge extends 1/8" past the coin edge.
	if (front) {
		coin_br_xl <- 2 * tile_width
		coin_br_xr <- xc + coin_diam / 2 + BLEED
	} else {
		coin_br_xl <- A5W - xc - coin_diam / 2 - BLEED
		coin_br_xr <- A5W - 2 * tile_width
	}
	bleed_coins <- rectGrob(
		x = unit(x_off + (coin_br_xl + coin_br_xr) / 2, "in"),
		y = unit(y_off + (ycs[6] + ycs[1]) / 2, "in"),
		width = unit(coin_br_xr - coin_br_xl, "in"),
		height = unit((ycs[1] - ycs[6] + coin_diam) + 2 * BLEED, "in"),
		just = "center",
		gp = gpar(fill = coin_bc, col = NA)
	)

	# Shared bleed rect for the 2x3 die grid.
	if (front) {
		die_br_xl <- -BLEED
		die_br_xr <- 3 * die_width + BLEED
	} else {
		die_br_xl <- A5W - 3 * die_width - BLEED
		die_br_xr <- A5W + BLEED
	}
	bleed_dice <- rectGrob(
		x = unit(x_off + (die_br_xl + die_br_xr) / 2, "in"),
		y = unit(y_off + (ydb + ydt) / 2, "in"),
		width = unit(die_br_xr - die_br_xl, "in"),
		height = unit(ydt - ydb + die_width + 2 * BLEED, "in"),
		just = "center",
		gp = gpar(fill = die_bc, col = NA)
	)

	cm_tiles <- pmap_piece(
		dft,
		cropmarkGrob,
		cfg = cfg,
		default.units = "in",
		bleed = TRUE,
		draw = FALSE
	)
	ps_tiles <- pmap_piece(dft, pieceGrob, cfg = cfg, default.units = "in", draw = FALSE)

	cm_coins <- pmap_piece(
		dfc,
		cropmarkGrob,
		cfg = cfg,
		default.units = "in",
		bleed = TRUE,
		draw = FALSE
	)
	ps_coins <- pmap_piece(dfc, pieceGrob, cfg = cfg, default.units = "in", draw = FALSE)

	ps_saucer <- pmap_piece(
		dfs,
		pieceGrob,
		cfg = cfg,
		default.units = "in",
		bleed = TRUE,
		draw = FALSE
	)

	if (!front && arrangement == "double-sided") {
		bleed_dice_grob <- nullGrob()
		ps_other <- nullGrob()
	} else {
		bleed_dice_grob <- bleed_dice
		ps_other <- pmap_piece(
			rbind(dfd, dfp, dfb),
			pieceGrob,
			cfg = cfg,
			default.units = "in",
			draw = FALSE
		)
	}

	# Dashed gutter line at the left edge of the right A5 (x_off on back pages)
	if (!front) {
		vline <- linesGrob(
			x = unit(c(x_off, x_off), "in"),
			y = unit(c(0, 1), "npc"),
			gp = gpar(lty = "dashed")
		)
	} else {
		vline <- nullGrob()
	}

	gList(
		bleed_tiles,
		bleed_coins,
		bleed_dice_grob,
		cm_tiles,
		cm_coins,
		ps_tiles,
		ps_coins,
		ps_other,
		ps_saucer,
		vline
	)
}
