INTER_W <- min(LETTER_HEIGHT, A4_HEIGHT) # 11" -- letter/A4 intersection width
INTER_H <- min(LETTER_WIDTH, A4_WIDTH) # 8.27" -- letter/A4 intersection height

# The tile and coin blocks otherwise run to within 0.01" of the printer safe
# zone.  Lift them slightly so a registration mark fits underneath.
REG_SHIFT <- 1 / 8
REG_SIZE <- 1 / 8

# Gutter between the two pawn tops so the pair can be folded around the edge of
# a core, the way the sheet folds around the tile material.  Measured between
# the piece edges, and sized to match the sheet's own gutter, where the front
# and back coin and saucer edges sit 2 * BLEED apart with their bleeds meeting
# at the fold.
PAWN_GUTTER <- 2 * (1 / 8)

# Top of the tile bleed; the upper registration mark just above it; and the
# solid rule above them both.  Everything the gutter fold needs -- the tiles'
# own crop marks and both registration marks -- therefore stays below the rule,
# so cutting along it frees the top row without taking any of them.
band_bottom_y <- function(cfg, y_off) y_off + 3 * cfg$get_width("tile_back") + 1 / 8
# Registration marks sit on the tiles' own crop marks, as in `bleed = TRUE`
# where `a5_tile_grob()` centres them on the outer cut line so the crop mark
# falls inside the crosshair.  Overlapping costs nothing and keeps both marks
# as far into the corners, and the rule as low, as the sheet allows.
reg_top_y <- function(cfg, y_off) band_bottom_y(cfg, y_off) + REG_SIZE / 2
reg_bot_y <- function(cfg, y_off) y_off - 1 / 8 - REG_SIZE / 2
sep_line_y <- function(cfg, y_off) reg_top_y(cfg, y_off) + REG_SIZE / 2 + 1 / 16

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
	# The two halves can be gutter-folded around the material, or cut apart and
	# laid on opposite sides of it -- but they cannot be printed on opposite
	# sides of the paper, since the dice, belt and pawns are printed once across
	# the full width and would land back to back.
	if (arrangement == "double-sided") {
		abort(c(
			'`arrangement = "double-sided"` not supported for `bleed = "grouped"`',
			i = paste(
				"The dice, belt, and pawns are printed once across the full width,",
				"so duplex printing would land them back to back."
			),
			i = paste(
				"To mount the two halves on opposite sides of the target material,",
				'cut the sheet apart along the "gutter" line instead.'
			)
		))
	}

	# Draw within a viewport equal to the intersection of the letter and A4
	# paper sizes (11" x 8.27"), centered on the page -- both paper sizes then
	# share the same piece coordinates.
	xl_off <- INTER_W / 2 - A5W + size_bleed$left
	xr_off <- INTER_W / 2 + size_bleed$left
	y_off <- (INTER_H - A5H) / 2 + size_bleed$bottom + REG_SHIFT
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

	## Front Matter -- wrap A5 grobs in positioned viewports so they render like
	## the piecepack grobs (plain grid.draw in the loop below).
	gl <- gappend(
		gl,
		gTree(children = gList(a5_title_grob(cfg, pieces, quietly, bleed = TRUE)), vp = vpl)
	)
	gl <- gappend(
		gl,
		gTree(children = gList(a5_inst_grob_grouped(cfg, pieces, arrangement, size)), vp = vpr)
	)
	pl[["Front Matter"]] <- 1

	## Piecepack -- grobs carry page/intersection coordinates directly
	if ("piecepack" %in% pieces) {
		n_pages <- n_suits
		for (suit in seq(n_pages)) {
			# The band spans both halves so it is drawn once, with the front grobs.
			gl <- gappend(
				gl,
				gList(
					gTree(children = a5_piecepack_grob_shared(suit, cfg, TRUE, xl_off, y_off)),
					gTree(
						children = band_grob_grouped(
							suit,
							cfg,
							y_off,
							size_bleed$left,
							size_bleed$bottom
						)
					)
				)
			)
			gl <- gappend(
				gl,
				a5_piecepack_grob_shared(suit, cfg, FALSE, xr_off, y_off)
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

# Crop marks are numbered clockwise from the top right in the *piece's* own
# frame: top = "18", right = "23", bottom = "45", left = "67".  Within a shared
# bleed group only the group's outer boundary should be marked -- an interior
# mark starts 1/8" past its piece's edge and so lands on the neighbour.  Tile
# backs are rotated by multiples of 90 degrees, so pick the page edges wanted
# and rotate back into the piece frame.
# The 1/6" default crosshair is a third of a 1/2" die face, and a corner rounder
# big enough to take it off a sticker that small eats 39% of each edge.  Cap the
# crosshair at a quarter of the piece's shorter side so a 3mm round clears it.
ch_width_for <- function(df, cfg) {
	vapply(
		df$piece_side,
		function(ps) min(1 / 6, 0.25 * min(cfg$get_width(ps), cfg$get_height(ps))),
		numeric(1),
		USE.NAMES = FALSE
	)
}

EDGE_MARKS <- c("18", "67", "45", "23") # page top, left, bottom, right (counter-clockwise)

cm_select_outer <- function(df, exclude = integer()) {
	tol <- 1e-8
	edges <- cbind(
		abs(df$y - max(df$y)) < tol,
		abs(df$x - min(df$x)) < tol,
		abs(df$y - min(df$y)) < tol,
		abs(df$x - max(df$x)) < tol
	)
	if (length(exclude)) {
		edges[, exclude + 1L] <- FALSE
	}
	k <- round(df$angle / 90) %% 4
	vapply(
		seq_len(nrow(df)),
		function(i) {
			page_edges <- which(edges[i, ]) - 1L
			paste0(EDGE_MARKS[((page_edges - k[i]) %% 4) + 1L], collapse = "")
		},
		character(1)
	)
}

a5_piecepack_grob_shared <- function(suit, cfg, front, x_off, y_off) {
	BLEED <- 1 / 8
	tile_width <- cfg$get_width("tile_back")
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
	dfs <- offset(dfs)

	# Bleed colors
	if (front) {
		tile_bc <- cfg$get_piece_opt("tile_face", suit, 1)$bleed_color
		coin_bc <- cfg$get_piece_opt("coin_back", suit, 1)$bleed_color
	} else {
		tile_bc <- cfg$get_piece_opt("tile_back", suit, 1)$bleed_color
		coin_bc <- cfg$get_piece_opt("coin_face", suit, 1)$bleed_color
	}

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

	# Skip the tile edge facing the coins: only 1/4" of waste separates the two
	# groups and the coin crosshairs already mark its far side.  Every tile cut
	# line is still marked -- the vertical ones from the top and bottom edges,
	# the horizontal ones from the outer edge.
	coin_edge <- if (dfc$x[1] > max(dft$x)) 3L else 1L
	dft_cm <- dft
	dft_cm$cm_select <- cm_select_outer(dft, exclude = coin_edge)
	cm_tiles <- pmap_piece(
		dft_cm,
		cropmarkGrob,
		cfg = cfg,
		default.units = "in",
		bleed = TRUE,
		draw = FALSE
	)
	ps_tiles <- pmap_piece(dft, pieceGrob, cfg = cfg, default.units = "in", draw = FALSE)

	# Coins and saucers are round, so a crosshair sits in the waste corner of the
	# bounding box rather than on the piece -- it marks the cut lattice and helps
	# centre a circular cutter without leaving ink on the finished piece.
	dfc_ch <- transform(dfc, ch_width = ch_width_for(dfc, cfg))
	dfs_ch <- transform(dfs, ch_width = ch_width_for(dfs, cfg))
	ch_coins <- pmap_piece(dfc_ch, crosshairGrob, cfg = cfg, default.units = "in", draw = FALSE)
	ch_saucer <- pmap_piece(dfs_ch, crosshairGrob, cfg = cfg, default.units = "in", draw = FALSE)
	ps_coins <- pmap_piece(dfc, pieceGrob, cfg = cfg, default.units = "in", draw = FALSE)

	ps_saucer <- pmap_piece(
		dfs,
		pieceGrob,
		cfg = cfg,
		default.units = "in",
		bleed = TRUE,
		draw = FALSE
	)

	# Dashed gutter line at the left edge of the right A5 (x_off on back pages)
	if (!front) {
		vline <- linesGrob(
			x = unit(c(x_off, x_off), "in"),
			y = unit(c(0, sep_line_y(cfg, y_off)), "in"),
			gp = gpar(lty = "dashed")
		)
	} else {
		vline <- nullGrob()
	}

	gList(
		bleed_tiles,
		bleed_coins,
		cm_tiles,
		ps_tiles,
		ps_coins,
		ps_saucer,
		ch_coins,
		ch_saucer,
		vline
	)
}

# Dice, the belt, and the pawns are each cut out on their own and mounted on a
# cube, a cylinder, or folded into a token.  None is wrapped around the sheet's
# gutter fold, so none needs to line up with it and they all fit in one
# full-width row above the tile and coin blocks: six dice, the belt, then a
# pawn face / back pair whose tops touch so the pair folds into one token.
# `dx` / `dy` are the `size_bleed` left / bottom offsets.  `y_off` already
# carries `dy`, so only the page-frame coordinates below need shifting --
# without them the band would stay put while the tile and coin blocks moved,
# pulling the registration marks off the tile cut lines they sit on.
band_grob_grouped <- function(suit, cfg, y_off, dx = 0, dy = 0) {
	BLEED <- 1 / 8
	MARGIN <- 1 / 4
	tile_width <- cfg$get_width("tile_back")
	die_width <- cfg$get_width("die_face")
	pawn_width <- cfg$get_width("pawn_face")
	pawn_height <- cfg$get_height("pawn_face")
	belt_width <- cfg$get_width("belt_face")
	belt_height <- cfg$get_height("belt_face")

	band_bottom <- sep_line_y(cfg, y_off)
	band_top <- INTER_H - MARGIN + dy
	x_left <- MARGIN + dx
	x_right <- INTER_W - MARGIN + dx

	# Spare width is split into four equal gaps: two ends and two between groups.
	w_dice <- 6 * die_width + 2 * BLEED
	w_belt <- belt_width + 2 * BLEED
	w_pawn <- 2 * pawn_height + 2 * BLEED + PAWN_GUTTER
	gap <- (x_right - x_left - w_dice - w_belt - w_pawn) / 4
	x_dice <- x_left + gap
	x_belt <- x_dice + w_dice + gap
	x_pawn <- x_belt + w_belt + gap

	row_height <- max(die_width, belt_height, pawn_width)
	y_row <- band_top - BLEED - 0.5 * row_height
	row_bottom <- band_top - row_height - 2 * BLEED

	stopifnot(
		"band row is too wide for the page" = gap >= 0,
		"band row leaves no room above the separating rule" = row_bottom > band_bottom
	)

	dfd <- tibble(
		piece_side = "die_face",
		x = x_dice + BLEED + (seq(0, 5) + 0.5) * die_width,
		y = y_row,
		suit,
		rank = 1:6,
		angle = 0
	)
	dfb <- tibble(
		piece_side = "belt_face",
		x = x_belt + BLEED + 0.5 * belt_width,
		y = y_row,
		suit,
		rank = NA,
		angle = 0
	)
	# Rotated so the two heads face each other across the pawn gutter.
	x_pawn_gutter <- x_pawn + BLEED + pawn_height + 0.5 * PAWN_GUTTER
	dfp <- tibble(
		piece_side = c("pawn_face", "pawn_back"),
		x = x_pawn_gutter + c(-1, 1) * (0.5 * PAWN_GUTTER + 0.5 * pawn_height),
		y = y_row,
		suit,
		rank = NA,
		angle = c(270, 90)
	)

	bleed_rect <- function(xl, w, h, piece_side) {
		rectGrob(
			x = unit(xl + 0.5 * w, "in"),
			y = unit(y_row, "in"),
			width = unit(w, "in"),
			height = unit(h, "in"),
			just = "center",
			gp = gpar(fill = cfg$get_piece_opt(piece_side, suit, 1)$bleed_color, col = NA)
		)
	}
	bleeds <- gList(
		bleed_rect(x_dice, w_dice, die_width + 2 * BLEED, "die_face"),
		bleed_rect(x_belt, w_belt, belt_height + 2 * BLEED, "belt_face"),
		bleed_rect(x_pawn, w_pawn, pawn_width + 2 * BLEED, "pawn_face")
	)

	# The two heads meet at a fold, not a cut.  "2" and "7" sit beside the piece
	# at its top edge and so mark that edge as a cut line; "1" and "8" sit above
	# it and would print inside the gutter.  Drop all four, leaving "3456" to
	# mark the outer end and the two long sides.
	cm_pawns <- pmap_piece(
		dfp,
		cropmarkGrob,
		cfg = cfg,
		default.units = "in",
		bleed = TRUE,
		cm_select = "3456",
		draw = FALSE
	)
	# Dice needn't be cut on the shared lines -- a larger or smaller target die,
	# or an arch punch for rounded dice, is equally fine, and the face symbols are
	# placed to tolerate either.  So mark the row's top and bottom cut lines with
	# crop marks clear of the artwork: excluding the page top and bottom edges
	# leaves only the horizontal marks, and those only on the end dice.
	dfd_cm <- dfd
	dfd_cm$cm_select <- cm_select_outer(dfd, exclude = c(0L, 2L))
	cm_dice <- pmap_piece(
		dfd_cm,
		cropmarkGrob,
		cfg = cfg,
		default.units = "in",
		bleed = TRUE,
		draw = FALSE
	)
	ps <- pmap_piece(rbind(dfd, dfb, dfp), pieceGrob, cfg = cfg, default.units = "in", draw = FALSE)

	# Crosshairs sit on the corner itself, so where two dice share a cut line one
	# mark serves both.  They straddle the corner, so draw them over the pieces.
	df_ch <- rbind(dfd, dfb)
	df_ch$ch_width <- ch_width_for(df_ch, cfg)
	ch <- pmap_piece(df_ch, crosshairGrob, cfg = cfg, default.units = "in", draw = FALSE)

	# Solid rule dividing the row from the blocks below: cut here first, then fold
	# the lower sheet along the (dashed) gutter, which stops at this line.
	sep <- linesGrob(
		x = unit(c(x_left, x_right), "in"),
		y = unit(rep(band_bottom, 2), "in"),
		name = "pnp_separator"
	)

	# Dashed line down the middle of the pawn gutter, matching the sheet's own.
	# It runs a crop mark's length past the bleed zone at each end so it still
	# shows when the bleed is the same color as the line.
	pawn_gutter_line <- linesGrob(
		x = unit(rep(x_pawn_gutter, 2), "in"),
		y = unit(y_row + c(-1, 1) * (0.5 * pawn_width + 2 * BLEED), "in"),
		gp = gpar(lty = "dashed")
	)

	# Registration marks mirrored about the gutter, so a pair lands on top of
	# itself when the lower sheet is folded.  Both sit *below* the rule: cut the
	# rule to take off the top row, then use these to line up the gutter fold (or
	# to pair any two halves back to back).  Circled above, squared below, so the
	# sheet's orientation is never ambiguous.
	# Centred on the outermost tile cut line on each half.
	x_reg <- c(MARGIN + BLEED + dx, INTER_W - MARGIN - BLEED + dx)
	reg <- gList(
		circledSegmentsCrosshairGrob(
			x = inch(x_reg),
			y = inch(rep(reg_top_y(cfg, y_off), 2)),
			width = inch(REG_SIZE),
			height = inch(REG_SIZE)
		),
		squaredSegmentsCrosshairGrob(
			x = inch(x_reg),
			y = inch(rep(reg_bot_y(cfg, y_off), 2)),
			width = inch(REG_SIZE),
			height = inch(REG_SIZE)
		)
	)

	gList(bleeds, cm_pawns, cm_dice, ps, ch, pawn_gutter_line, sep, reg)
}

a5_inst_grob_grouped <- function(cfg, pieces, arrangement, size) {
	y_inst <- unit(1, "npc") - unit(0.2, "in")
	inst <- c("\u25cf See https://www.ludism.org/ppwiki/MakingPiecepacks for general advice")

	components <- paste(paste0('"', pieces, '"'), collapse = ", ")
	inst <- c(
		inst,
		"\u25cf This print-and-play layout was generated for:",
		sprintf('\t\u25cb %s components', components),
		sprintf('\t\u25cb "%s" arrangement with grouped bleed zones', arrangement),
		sprintf('\t\u25cb "%s" paper size', size)
	)

	inst <- c(
		inst,
		"● One page per suit, split in two by a solid rule:",
		'\t○ Above: 6 dice faces, a pawn belt, and a pawn face / back pair',
		'\t○ Below: tile faces (left) and backs (right) mirrored about the "gutter",',
		'\t\t  with coin and saucer backs / faces on either side of it'
	)

	inst <- c(
		inst,
		"● 1. Cut along the solid rule to take off the top row",
		"● 2. Put the lower part on both sides of the target material:",
		'\t○ Fold along the "gutter" over the target material\'s edge, or',
		'\t○ Cut the "gutter" too and line up the registration marks (circled',
		'\t\t  above, squared below); this also pairs any two halves back to back'
	)

	inst <- c(
		inst,
		"● 3. Cut out tiles, coins and saucers:",
		'\t○ Adjacent tiles share a cut line: use the "crop" marks around the block',
		'\t○ Coins and saucers are spaced so a circular punch clears its neighbor;',
		'\t\t  their crosshairs center the punch or guide (inferior) square cuts'
	)

	inst <- c(
		inst,
		"● 4. Cut out dice, belt and pawn from the top row:",
		'\t○ Crosshairs mark each die and belt corner; "crop" marks at the ends',
		'\t\t  of the dice row mark its top and bottom edges',
		'\t○ Cut dice wider or narrower, or punch rounded dice: the face symbols',
		'\t\t  are placed to suit a range of target dice',
		'\t○ Mount the faces on a cube and wrap the belt around a cylinder',
		'\t○ Cut around the pawn pair but not between the heads; fold there',
		'\t\t  around a core for a two-sided pawn'
	)

	inst <- paste(inst, collapse = "\n")

	gTree(
		name = "instructions",
		children = gList(
			textGrob(
				"Instructions",
				x = unit(0.5, "cm"),
				y = y_inst,
				just = "left",
				gp = gp_header
			),
			textGrob(
				inst,
				x = unit(0.5, "cm"),
				y = y_inst - unit(0.2, "in"),
				just = c(0, 1),
				gp = gp_text
			)
		)
	)
}
