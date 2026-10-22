print_and_play_paper_compact <- function(cfg, size, pieces, arrangement, quietly, size_bleed) {
	n_suits <- cfg$n_suits
	n_ranks <- cfg$n_ranks

	stopifnot(n_ranks <= 6)

	vps <- a5_panel_viewports(size, size_bleed)
	vpl <- vps$vpl
	vpr <- vps$vpr
	gl <- list()
	pl <- list()

	if ("piecepack" %in% pieces || "subpack" %in% pieces) {
		psuits <- seq(cfg$n_suits + 2L)
		lpgf <- lapply(
			psuits,
			a5_piecepack_grob,
			cfg = cfg,
			front = TRUE,
			arrangement = arrangement
		)
		lpgb <- lapply(
			psuits,
			a5_piecepack_grob,
			cfg = cfg,
			front = FALSE,
			arrangement = arrangement
		)
	}

	## Front Matter
	gl <- gappend(gl, a5_title_grob(cfg, pieces, quietly))
	gl <- gappend(gl, pnp_inst_grob(a5_inst_md_compact(cfg, pieces, arrangement, size)))
	if (arrangement == "double-sided" && size != "A5") {
		gl <- gappend(gl, blank_grob)
		gl <- gappend(gl, blank_grob)
		pl[["Front Matter"]] <- 2
	} else {
		pl[["Front Matter"]] <- 1
	}

	if ("piecepack" %in% pieces) {
		n_pages <- n_suits
		if (is_odd(n_suits) && arrangement == "double-sided" && size != "A5") {
			n_pages <- n_pages + 1
		}
		# The registration marks sit in the page margin, which is too narrow on A5
		reg_f <- if (size == "A5") NULL else a5_piecepack_reg_grob(cfg, front = TRUE)
		reg_b <- if (size == "A5") NULL else a5_piecepack_reg_grob(cfg, front = FALSE)
		# The gutter line divides the two halves of a page; an A5 page has one half
		gutter <- if (size == "A5") NULL else a5_piecepack_gutter_grob(cfg)
		rule <- a5_piecepack_rule_grob(cfg)
		for (suit in seq(n_pages)) {
			gl <- gappend(gl, gList(lpgf[[suit]], reg_f, rule))
			gl <- gappend(gl, gList(lpgb[[suit]], reg_b, rule, gutter))
		}
		pl$Piecepack <- n_pages
	}

	#### Fine-tune between pyramids, matchsticks, and misc.?
	#### Add misc, cards, dominoes accessories?
	if ("matchsticks" %in% pieces) {
		n_groups <- (n_suits - 1) %/% 4 + 1
		for (ii in seq(n_groups)) {
			ss <- seq(4 * ii - 3, 4 * ii)
			if (arrangement == "double-sided" && size == "A5") {
				gl <- gappend(gl, a5_matchsticks_grob(ss, cfg, TRUE))
				gl <- gappend(gl, a5_matchsticks_grob(ss, cfg, FALSE))
				gl <- gappend(gl, a5_matchsticks_grob2(ss, cfg, TRUE))
				gl <- gappend(gl, a5_matchsticks_grob2(ss, cfg, FALSE))
			} else {
				gl <- gappend(gl, a5_matchsticks_grob(ss, cfg, TRUE))
				gl <- gappend(gl, a5_matchsticks_grob2(ss, cfg, TRUE))
				if (arrangement == "double-sided") {
					gl <- gappend(gl, a5_matchsticks_grob2(ss, cfg, FALSE))
					gl <- gappend(gl, a5_matchsticks_grob(ss, cfg, FALSE))
				}
			}
		}
		pl$Matchsticks <- if (arrangement == "double-sided") 2 * n_groups else n_groups
	}
	if ("pyramids" %in% pieces) {
		n_groups <- (n_suits - 1) %/% 4 + 1
		for (ii in seq(n_groups)) {
			ss <- seq(4 * ii - 3, 4 * ii)
			s1 <- ss[1:2]
			s2 <- ss[3:4]
			if (arrangement == "double-sided" && size == "A5") {
				gl <- gappend(gl, a5_pyramids_grob(s1, cfg, TRUE))
				gl <- gappend(gl, blank_grob)
				gl <- gappend(gl, a5_pyramids_grob(s2, cfg, FALSE))
				gl <- gappend(gl, blank_grob)
			} else {
				gl <- gappend(gl, a5_pyramids_grob(s1, cfg, TRUE))
				gl <- gappend(gl, a5_pyramids_grob(s2, cfg, FALSE))
				if (arrangement == "double-sided") {
					gl <- gappend(gl, blank_grob)
					gl <- gappend(gl, blank_grob)
				}
			}
		}
		pl$Pyramids <- if (arrangement == "double-sided") 2 * n_groups else n_groups
	}
	if ("subpack" %in% pieces) {
		vpul <- viewport(x = 0.25, y = 0.75, width = inch(A5W / 2), height = inch(A5H / 2))
		vpur <- viewport(x = 0.75, y = 0.75, width = inch(A5W / 2), height = inch(A5H / 2))
		vpll <- viewport(x = 0.25, y = 0.25, width = inch(A5W / 2), height = inch(A5H / 2))
		vplr <- viewport(x = 0.75, y = 0.25, width = inch(A5W / 2), height = inch(A5H / 2))
		lsgf <- lapply(lpgf, as_picture, width = A5W, height = A5H)
		lsgb <- lapply(lpgb, as_picture, width = A5W, height = A5H)
		n_pages <- (n_suits - 1) %/% 4 + 1
		if (arrangement == "double-sided" && is_odd(n_pages) && size != "A5") {
			n_pages <- n_pages + 1
		}
		for (ii in seq(n_pages)) {
			ss <- seq(4 * ii - 3, 4 * ii)
			ss[which(ss > cfg$n_suits + 2L)] <- cfg$n_suits + 2L
			gf <- grobTree(
				grobTree(lsgf[[ss[1]]], vp = vpul),
				grobTree(lsgb[[ss[1]]], vp = vpur),
				grobTree(lsgf[[ss[2]]], vp = vpll),
				grobTree(lsgb[[ss[2]]], vp = vplr)
			)
			gb <- grobTree(
				grobTree(lsgf[[ss[3]]], vp = vpul),
				grobTree(lsgb[[ss[3]]], vp = vpur),
				grobTree(lsgf[[ss[4]]], vp = vpll),
				grobTree(lsgb[[ss[4]]], vp = vplr)
			)
			gl <- gappend(gl, gf)
			gl <- gappend(gl, gb)
		}
		pl$Subpack <- n_pages
	}
	if (size == "A5") {
		pl <- lapply(pl, function(x) 2 * x)
	}
	for (ii in seq(gl)) {
		if (is_odd(ii)) {
			grid.newpage()
			draw_a5_page(gl[[ii]], vpl)
		} else {
			if (size == "A5") {
				grid.newpage()
			}
			draw_a5_page(gl[[ii]], vpr)
		}
	}

	pl
}

a5_inst_md_compact <- function(cfg, pieces, arrangement, size) {
	a5 <- size == "A5"
	ds <- arrangement == "double-sided"
	piecepack <- "piecepack" %in% pieces
	subpack <- "subpack" %in% pieces
	matchsticks <- "matchsticks" %in% pieces
	pyramids <- "pyramids" %in% pieces
	# `a5_matchsticks_grob2()` fills the room beside the fourth rank of
	# matchsticks with fold-up die layouts when the dice are small enough
	die_layouts <- matchsticks && cfg$get_width("die_face") <= 0.6

	# A list item followed by a nested list of `items`; {marquee} needs the
	# blank line between them to nest the bullets
	nested <- function(item, items, indent = "  ") {
		paste0(item, "\n\n", paste0(indent, "* ", items, collapse = "\n"))
	}
	reg_marks <- "the registration marks (circled above, squared below)"
	pawn_fold <- paste(
		"in half along its dashed line so the face and back meet,",
		"glue, and fold out the two ends as a stand."
	)

	## Overview
	overview <- c(
		"* For general advice: <https://www.ludism.org/ppwiki/MakingPiecepacks>",
		pnp_generated_for_md(pieces, arrangement, size, "none")
	)
	if (piecepack) {
		top_row <- if (ds) {
			sprintf("on the %s only", if (a5) "page of fronts" else "left half")
		} else {
			sprintf("a copy on each %s", if (a5) "page" else "half")
		}
		below <- if (a5) {
			paste(
				"Below: tile faces, coin backs, and a (pawn) saucer face,",
				"with their backs mirrored on the page of backs."
			)
		} else {
			paste(
				'Below: tile faces (left) and backs (right) mirrored about the dashed "gutter" line,',
				"with coin and saucer backs / faces in between."
			)
		}
		overview <- c(
			overview,
			nested(
				if (a5) {
					"* For each suit a page of piecepack fronts then a page of backs, each split in two by a solid rule:"
				} else {
					"* One piecepack page per suit, split in two by a solid rule:"
				},
				c(
					sprintf(
						"Above: dice faces, a (standee) pawn layout, and a (pawn) belt (%s).",
						top_row
					),
					below,
					paste0(
						"Depending on your preferences you may only need one of the",
						" (standee) pawn, (pawn) belt, or (pawn) saucer."
					)
				)
			)
		)
	}
	if (matchsticks) {
		overview <- c(
			overview,
			paste0(
				if (a5) {
					"* Two pages of matchstick faces per four suits"
				} else {
					"* One page of matchstick faces per four suits"
				},
				if (die_layouts) ", beside spare fold-up dice layouts",
				if (ds && a5) ", each followed by a page of backs",
				if (ds && !a5) "; their backs are on the next page",
				"."
			)
		)
	}
	if (pyramids) {
		overview <- c(
			overview,
			paste0(
				"* One page of pyramid layouts per ",
				if (a5) "two suits" else "four suits (two per half)",
				", each suit topped by a spare pawn layout",
				if (ds) "; the back is left blank",
				"."
			)
		)
	}
	if (subpack) {
		overview <- c(
			overview,
			paste0(
				'* One "subpack" (half-size piecepack) page per ',
				if (a5) "two" else "four",
				" suits: each suit's fronts beside its backs, mirrored about the center of ",
				if (a5) "the page" else "each half",
				"."
			)
		)
	}

	## Instructions
	steps <- paste0(
		"1. Print ",
		if (ds) sprintf("double-sided (flip on the %s edge)", if (a5) "long" else "short"),
		if (!ds) "single-sided",
		" on cardstock or full-sheet label paper",
		if (ds && piecepack && !a5) {
			"; the registration marks beside the tiles should line up on both sides"
		},
		"."
	)
	material <- "the target material you'll be making your pieces from (if not using pre-cut pieces)"
	if (piecepack) {
		steps <- c(
			steps,
			"1. Cut along the solid rule to take off the top row.",
			if (ds) {
				sprintf("1. (optional) Mount the lower part of each page on %s.", material)
			} else if (a5) {
				sprintf(
					"1. (optional) Put the lower parts of each page of fronts and the page of backs after it on opposite sides of %s.",
					material
				)
			} else {
				paste0(
					"1. (optional) Put the lower part of each page on both sides of ",
					material,
					': fold it along the "gutter" over the material\'s edge,',
					' or cut the "gutter" too and line up ',
					reg_marks,
					"."
				)
			}
		)
	}
	if (subpack) {
		steps <- c(
			steps,
			paste0(
				"1. ",
				if (a5) "Fold each subpack page",
				if (!a5) 'Cut each subpack page along the "gutter" and fold each half',
				" in half so each suit's fronts land on its backs;",
				" then make its pieces like the full-size ones."
			)
		)
	}
	if (piecepack || subpack) {
		steps <- c(
			steps,
			nested(
				"1. Cut out tiles, coins, and saucers:",
				c(
					"Adjacent tiles share a cut line; (optional) round their corners.",
					"Cut out the coins and saucers with a circle punch (or scissors)."
				),
				indent = "   "
			),
			nested(
				paste0("1. Cut out dice, belt, and pawn", if (piecepack) " from the top row", ":"),
				c(
					"Affix the six dice faces onto a cube.",
					"Wrap the belt around a pawn / cylinder.",
					paste("Fold the pawn layout", pawn_fold)
				),
				indent = "   "
			)
		)
	}
	if (matchsticks) {
		steps <- c(
			steps,
			paste0(
				"1. Cut out the matchsticks",
				if (die_layouts) " and fold up the spare dice layouts into dice",
				"."
			)
		)
	}
	if (pyramids) {
		steps <- c(
			steps,
			paste0(
				"1. Cut out each pyramid layout, fold it up, and glue the overlapping faces.",
				if (!piecepack && !subpack) paste(" Fold each spare pawn layout", pawn_fold)
			)
		)
	}

	paste(c("## Overview", overview, "## Instructions", steps), collapse = "\n\n")
}

a5_matchsticks_grob <- function(suits = 1:4, cfg = pp_cfg(), front = TRUE) {
	n_suits <- length(suits)
	y1t <- A5H - 0.5 * MATCHSTICK_HEIGHTS[1]
	y1b <- A5H - 1.5 * MATCHSTICK_HEIGHTS[1]
	x1s <- (0.5 + 0:(3 * n_suits - 1)) * MATCHSTICK_WIDTHS[1]
	xs <- (0.5 + 0:(6 * n_suits - 1)) * MATCHSTICK_WIDTHS[2]
	y2 <- y1b - 0.5 * MATCHSTICK_HEIGHTS[1] - 0.5 * MATCHSTICK_HEIGHTS[2]
	y3 <- y2 - 0.5 * MATCHSTICK_HEIGHTS[2] - 0.5 * MATCHSTICK_HEIGHTS[3]
	y5 <- y3 - 0.5 * MATCHSTICK_HEIGHTS[3] - 0.5 * MATCHSTICK_HEIGHTS[5]
	y6 <- y5 - 0.5 * MATCHSTICK_HEIGHTS[5] - 0.5 * MATCHSTICK_HEIGHTS[6]

	x <- c(rep(x1s, each = 2), rep(xs, 4))
	y <- c(rep(c(y1t, y1b), 3 * n_suits), rep(c(y2, y3, y5, y6), each = 6 * n_suits))
	suit <- rep(rep(suits, each = 6), 5)
	rank <- rep(c(1:3, 5:6), each = 6 * n_suits)
	if (front) {
		piece_side <- "matchstick_face"
	} else {
		x <- A5W - x
		piece_side <- "matchstick_back"
	}
	df <- tibble(piece_side, x, y, suit, rank)

	pmap_piece(df, cfg = cfg, default.units = "inches", draw = FALSE)
}

a5_matchsticks_grob2 <- function(suits = 1:4, cfg = pp_cfg(), front = TRUE) {
	n_suits <- length(suits)
	y4s <- A5H - (0.5 + 0:3) * MATCHSTICK_HEIGHTS[4]
	x4s <- (0.5 + 0:5) * MATCHSTICK_WIDTHS[6]

	x <- rep(x4s, 4)
	y <- rep(c(y4s), each = 6)
	suit <- rep(suits, each = 6)
	rank <- 4
	if (front) {
		piece_side <- "matchstick_face"
	} else {
		x <- A5W - x
		piece_side <- "matchstick_back"
	}
	df <- tibble(piece_side, x, y, suit, rank)

	# add some extra dice if there is some space
	if (cfg$get_width("die_face") <= 0.6) {
		xd1 <- max(x4s) + 0.0 + c(0.6) * cfg$get_width("die_layoutLF")
		dfd <- tibble::tibble(
			piece_side = "die_layoutLF",
			x = xd1,
			y = unique(y),
			suit = suits,
			rank = NA
		)
		df <- rbind(df, dfd)
	}
	if (cfg$get_width("die_face") <= 0.55) {
		xd2 <- xd1 + c(0.7) * cfg$get_width("die_layoutLF")
		dfd <- tibble::tibble(
			piece_side = "die_layoutLF",
			x = xd2,
			y = unique(y),
			suit = suits,
			rank = NA
		)
		df <- rbind(df, dfd)
	}

	pmap_piece(df, cfg = cfg, default.units = "inches", draw = FALSE)
}

a5_piecepack_grob <- function(suit, cfg = pp_cfg(), front = TRUE, arrangement = "single-sided") {
	die_width <- cfg$get_width("die_width")
	pawn_width <- cfg$get_width("pawn_width")
	tile_width <- cfg$get_width("tile_back")
	xtl <- 1.5 * tile_width
	xtr <- 0.5 * tile_width
	ytb <- 0.5 * tile_width
	ytm <- 1.5 * tile_width
	ytt <- 2.5 * tile_width
	xc <- A5W - 0.25 * tile_width
	ycs <- rev((0.50 + seq(0, 5)) * 0.8)
	xdr <- c(0.5, 1.5, 2.5) * die_width
	ydt <- A5H - 0.5 * die_width
	ydb <- A5H - 1.5 * die_width
	xp <- A5W - 0.5 * cfg$get_height("pawn_layout")
	yp <- A5H - 0.5 * pawn_width
	xb <- A5W - 0.5 * cfg$get_width("belt_face")
	yb <- A5H - pawn_width - 0.5 * cfg$get_height("belt_face") - 0.25
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
	df <- rbind(dfc, dfd, dfp, dfb, dfs, dft)
	if (!front) {
		df$x <- A5W - df$x
		df$piece_side <- c(
			rep("coin_face", 6),
			rep("die_face", 6),
			"pawn_layout",
			"belt_face",
			"saucer_back",
			rep("tile_back", 6)
		)
		# Rotate tile backs to partially hide direction of face if tile back are not fully symmetric
		df$angle <- c(rep(0, 12), 270, 0, 0, 90 * ((df$suit[16:21] + df$rank[16:21]) %% 4))

		if (arrangement == "double-sided") {
			df <- df[df$piece_side %in% c("tile_back", "coin_face", "saucer_back"), ]
		}
	}
	pmap_piece(df, cfg = cfg, default.units = "inches", draw = FALSE)
}

# Registration marks in the page margin beside the tile block's outer edge,
# centered on its top and bottom edges so their crosshairs extend the tiles'
# cut lines: circled above and squared below, as in the bleed layouts.  Two
# marks three tile widths apart pin down the translation and rotation of a
# duplex print, and the margin keeps them clear of the pieces whatever their
# size.
a5_piecepack_reg_grob <- function(cfg = pp_cfg(), front = TRUE) {
	tile_width <- cfg$get_width("tile_back")
	side_x <- if (front) -REG_SIZE else A5W + REG_SIZE
	reg_marks_grob(
		side_x,
		3 * tile_width,
		side_x,
		0,
		REG_SIZE,
		name = "tile_registration_marks"
	)
}

# Bottom of the top row -- dice, (standee) pawn, and (pawn) belt -- which is
# printed on one side only.  The belt is the lowest piece on the right; large
# dice may come lower on the left.
a5_piecepack_band_bottom <- function(cfg) {
	die_width <- cfg$get_width("die_width")
	pawn_width <- cfg$get_width("pawn_width")
	belt_height <- cfg$get_height("belt_face")
	min(A5H - 2 * die_width, A5H - pawn_width - belt_height - 0.25)
}

# Solid rule between the top row and the tiles, coins, and saucer below it,
# which may be mounted on both sides of the material: cut along it first to
# take off the top row.  Midway between them, but never crossing the upper
# registration mark, which is centered on the tiles' top edge.
a5_piecepack_rule_y <- function(cfg) {
	tile_top <- 3 * cfg$get_width("tile_back")
	max(tile_top + REG_SIZE / 2, (tile_top + a5_piecepack_band_bottom(cfg)) / 2)
}

a5_piecepack_rule_grob <- function(cfg) {
	linesGrob(y = inch(rep(a5_piecepack_rule_y(cfg), 2)), name = "pnp_separator")
}

# Dashed gutter line between the two halves of a letter or A4 page, drawn on
# the back (right) half as in the bleed layouts.  It stops at the rule since
# only the part below it is folded around the material.
a5_piecepack_gutter_grob <- function(cfg) {
	linesGrob(
		x = unit(c(0, 0), "npc"),
		y = inch(c(0, a5_piecepack_rule_y(cfg))),
		gp = gpar(lty = "dashed"),
		name = "pnp_gutter"
	)
}

a5_pyramids_grob <- function(suit = 1:2, cfg = pp_cfg(), front = TRUE) {
	grobTree(pyramid_grob_helper(suit[1], cfg, 0), pyramid_grob_helper(suit[2], cfg, A5W / 2))
}

pyramid_grob_helper <- function(suit, cfg = pp_cfg(), xleft = 0) {
	rank <- c(2, 4, 5, 1, 3, 6)
	plh <- PYRAMID_LAYOUT_HEIGHTS[rank]
	y_up <- cumsum(plh) - 0.5 * plh
	x_up <- 0.5 * PYRAMID_LAYOUT_WIDTHS[rank]
	x <- xleft + c(x_up[1:3], A5W / 2 - x_up[4:6])
	y <- c(y_up[1:3], c(plh[5] + plh[6] + 0.5 * plh[4], plh[6] + 0.5 * plh[5], 0.5 * plh[6]))
	piece_side <- "pyramid_layout"
	angle <- c(rep(0, 3), rep(180, 3))
	df <- tibble::tibble(piece_side, x, y, suit, rank, angle)
	# space for a pawn layout on top
	dfp <- tibble::tibble(
		piece_side = "pawn_layout",
		x = xleft + 0.25 * A5W,
		y = A5H - 0.5 * cfg$get_width("pawn_layout"),
		suit,
		rank = 1,
		angle = 90
	)
	df <- rbind(dfp, df)
	pmap_piece(df, cfg = cfg, default.units = "inches", draw = FALSE)
}
