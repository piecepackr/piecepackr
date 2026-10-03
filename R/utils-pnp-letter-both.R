## Shared "paper" utilities

A5W <- 5 # 5.83"
A5H <- 7.5 # 8.27"
DIE_SLOT <- 3 / 4 # the bleed layouts center each die face in a slot this wide
a5_vp <- function() viewport(width = unit(A5W, "in"), height = unit(A5H, "in"))

draw_a5_page <- function(grob, vp) {
	pushViewport(vp)
	grid.draw(grob)
	upViewport()
}

blank_grob <- textGrob("Intentionally left blank")

is_odd <- function(x) as.logical(x %% 2)

gappend <- function(ll, g) {
	ll[[length(ll) + 1]] <- g
	ll
}

gp_title <- gpar(fontsize = 15, fontfamily = "sans", fontface = "bold")
gp_header <- gpar(fontsize = 12, fontfamily = "sans", fontface = "bold")
gp_text <- gpar(fontsize = 9, fontfamily = "sans")

htg <- function(label, x, y, just = "center", ...) {
	textGrob(label, x = inch(x), y = inch(y), just = just, gp = gp_header, ...)
}
tg <- function(label, x, y, just = "center", ...) {
	textGrob(label, x = inch(x), y = inch(y), just = just, gp = gp_text, ...)
}

n_indents <- function(x) {
	lines <- str_split(x, "\n")[[1L]]
	n_tabs <- str_count(lines, "\t")
	n_tabs <- n_tabs[n_tabs > 0L]
	if (length(n_tabs) == 0L) {
		return(0L)
	}
	min(n_tabs)
}

# Trims leading and trailing whitespace from a string and then removes the
# minimum number of leading tabs shared by all indented lines.
trim_multistring <- function(x) {
	x <- str_trim(x)
	lines <- str_split(x, "\n")[[1L]]
	lines <- str_replace(lines, str_glue("^\\t{{{n_indents(x)}}}"), "")
	str_flatten(lines, "\n")
}

# Wrapped so tests can mock a missing {marquee}
has_marquee <- function() {
	requireNamespace("marquee", quietly = TRUE)
}

# Wrapped so tests can mock an older R
r_supports_glyphs <- function() {
	getRversion() >= "4.3.0"
}

device_supports_glyphs <- function() {
	isTRUE(dev.capabilities()$glyphs)
}

# Converts the pre-markdown `cfg$credit` layout -- a "\u25cf " bullet line
# followed by tab-indented detail lines -- into markdown list items with line
# breaks, leaving any other markdown untouched.
legacy_credit_to_md <- function(credit) {
	lines <- unlist(str_split(paste(credit, collapse = "\n"), "\n"))
	if (all(str_trim(lines) == "")) {
		return(character(0L))
	}
	is_detail <- grepl("^\t", lines)
	lines <- str_replace(lines, "^\u25cf\\s*", "* ")
	lines[is_detail] <- paste0("  ", str_trim(lines[is_detail]))
	followed_by_detail <- c(is_detail[-1L], FALSE)
	lines[followed_by_detail] <- paste0(lines[followed_by_detail], "\\")
	lines
}

pnp_marquee_style <- function() {
	style <- marquee::classic_style(base_size = 9, body_font = "sans", header_font = "sans")
	style <- marquee::modify_style(style, "base", lineheight = 1.25)
	style <- marquee::modify_style(style, "p", margin = marquee::trbl(0))
	style <- marquee::modify_style(style, "ul", margin = marquee::trbl(0))
	style <- marquee::modify_style(style, "ol", margin = marquee::trbl(0))
	style <- marquee::modify_style(style, "a", color = "black")
	marquee::modify_style(
		style,
		"h2",
		size = 12,
		margin = marquee::trbl(0, 0, marquee::em(0.4)),
		padding = marquee::trbl(0),
		border_width = marquee::trbl(0)
	)
}

pnp_inst_grob <- function(md) {
	pnp_md_grob(
		md,
		"instructions",
		x = unit(0.5, "cm"),
		y = unit(1, "npc") - unit(0.1, "in"),
		width = unit(1, "npc") - unit(1, "cm")
	)
}

# Renders the markdown `md` for the print-and-play front matter or omits it
# (with a message) if {marquee} is missing or the device can't render glyphs.
pnp_md_grob <- function(md, name, x, y, width) {
	if (!has_marquee()) {
		reason <- "{marquee} is not installed"
	} else if (!r_supports_glyphs()) {
		reason <- "R >= 4.3 is required for {marquee} glyph rendering"
	} else if (!device_supports_glyphs()) {
		reason <- "the graphics device doesn't support {marquee} glyph rendering"
	} else {
		reason <- NULL
	}
	if (!is.null(reason)) {
		marquee_inform(name, reason)
		return(nullGrob(name = name))
	}
	marquee::marquee_grob(
		md,
		pnp_marquee_style(),
		x = x,
		y = y,
		width = width,
		name = name
	)
}

a5_title_grob <- function(cfg, pieces, quietly, extra_credit = TRUE, saucers = TRUE) {
	# Title
	y_title <- unit(1, "npc") - unit(0.2, "in")
	if (is.null(cfg$title)) {
		if (!quietly) {
			inform("`cfg$title` is `NULL`, omitting title", class = "piecepackr_missing_metadata")
		}
		grob_title <- nullGrob()
	} else {
		grob_title <- textGrob(
			cfg$title,
			y = y_title,
			just = "center",
			gp = gp_title,
			name = "title"
		)
	}

	# Description
	y_description <- y_title - grobHeight(grob_title) - unit(0.2, "in")
	if (is.null(cfg$description)) {
		if (!quietly) {
			inform(
				"`cfg$description` is `NULL`, omitting description",
				class = "piecepackr_missing_metadata"
			)
		}
		grob_description <- nullGrob()
	} else {
		dtext <- paste(strwrap(cfg$description, 72), collapse = "\n")
		grob_description <- textGrob(
			dtext,
			x = 0.1,
			y = y_description,
			just = c(0, 1),
			gp = gp_text,
			name = "description"
		)
	}

	# License
	y_license <- y_description - grobHeight(grob_description) - unit(0.2, "in")
	if (is.null(cfg$spdx_id)) {
		if (!quietly) {
			inform(
				"`cfg$spdx_id` is `NULL`, omitting license",
				class = "piecepackr_missing_metadata"
			)
		}
		grob_license <- grob_lh <- grob_l <- nullGrob()
	} else {
		stopifnot(cfg$spdx_id %in% piecepackr::spdx_license_list$id)
		url <- piecepackr::spdx_license_list[cfg$spdx_id, "url_alt"]
		if (is.na(url)) {
			url <- piecepackr::spdx_license_list[cfg$spdx_id, "url"]
		}
		full_name <- piecepackr::spdx_license_list[cfg$spdx_id, "name"]
		license <- paste(c(paste("\u25cf", full_name), paste("\t", url)), collapse = "\n")
		grob_lh <- textGrob("License", x = 0.1, y = y_license, just = "left", gp = gp_header)
		badge <- piecepackr::spdx_license_list[cfg$spdx_id, "badge"]
		if (is.na(badge)) {
			grob_cc <- nullGrob()
		} else {
			cc_file <- system.file(paste0("extdata/badges/", badge), package = "piecepackr")
			current_dev <- grDevices::dev.cur() # Workaround for {grImport2} v0.2-0 bug
			cc_picture <- grImport2::readPicture(cc_file)
			if (current_dev > 1) {
				grDevices::dev.set(current_dev)
			}
			grob_cc <- grImport2::symbolsGrob(cc_picture, x = 0.50, y = 0.05, size = inch(0.9))
		}
		grob_l <- textGrob(
			license,
			x = 0.1,
			y = y_license - unit(0.2, "in"),
			just = c(0, 1),
			gp = gp_text
		)

		grob_license <- grobTree(grob_lh, grob_l, grob_cc, name = "license")
	}

	# Copyright
	y_copyright <- y_license - grobHeight(grob_lh) - grobHeight(grob_l) - unit(0.3, "in")
	if (is.null(cfg$copyright) && !quietly) {
		if (!quietly) {
			inform(
				"`cfg$copyright` is `NULL`, omitting copyright",
				class = "piecepackr_missing_metadata"
			)
		}
	}
	if (is.null(cfg$copyright) || cfg$copyright == "") {
		grob_copyright <- grob_ch <- grob_c <- nullGrob()
	} else {
		copyright <- paste(cfg$copyright, collapse = "\n")
		grob_ch <- textGrob("Copyright", x = 0.1, y = y_copyright, just = "left", gp = gp_header)
		grob_c <- textGrob(
			copyright,
			x = 0.1,
			y = y_copyright - unit(0.2, "in"),
			just = c(0, 1),
			gp = gp_text
		)
		grob_copyright <- grobTree(grob_ch, grob_c, name = "copyright")
	}

	# Credits
	y_credits <- y_copyright - grobHeight(grob_ch) - grobHeight(grob_c) - unit(0.3, "in")
	if (is.null(cfg$credit) && !quietly) {
		inform(
			"`cfg$credit` is `NULL`, omitting custom credits",
			class = "piecepackr_missing_metadata"
		)
	}
	credit_item <- function(text, url) sprintf("* %s\\\n  <%s>", text, url)
	piecepack_credit <- credit_item(
		'The piecepack was invented by James "Kyle" Droscha. Public Domain.',
		"https://ludism.org/ppwiki/AnatomyOfAPiecepack"
	)
	credits <- credit_item(
		"This print-and-play layout was generated by piecepackr.",
		"https://github.com/piecepackr/piecepackr"
	)
	if (extra_credit) {
		if ("piecepack" %in% pieces) {
			credits <- c(credits, piecepack_credit)
		}
		if (saucers && "piecepack" %in% pieces) {
			credits <- c(
				credits,
				credit_item(
					"Pawn saucers were invented by Karol M. Boyle. Public Domain.",
					"https://web.archive.org/web/2018/http://www.piecepack.org/Accessories.html"
				)
			)
		}
		if ("pyramids" %in% pieces) {
			credits <- c(
				credits,
				credit_item(
					"Piecepack pyramids were invented by Tim Schutz. Public Domain.",
					"https://www.ludism.org/ppwiki/PiecepackPyramids"
				)
			)
		}
		if ("matchsticks" %in% pieces) {
			credits <- c(
				credits,
				credit_item(
					"Piecepack matchsticks were invented by Dan Burkey. Public Domain.",
					"https://www.ludism.org/ppwiki/PiecepackMatchsticks"
				)
			)
		}
	} else {
		credits <- c(credits, piecepack_credit)
	}
	md <- paste(credits, collapse = "\n")
	credit <- legacy_credit_to_md(cfg$credit)
	if (length(credit) > 0L) {
		# A custom list continues the built-in list, anything else follows it
		sep <- if (grepl("^[*+-] ", credit[1L])) "\n" else "\n\n"
		md <- paste0(md, sep, paste(credit, collapse = "\n"))
	}
	grob_credits <- pnp_md_grob(
		paste0("## Credits\n\n", md),
		"credits",
		x = unit(0.1, "npc"),
		y = y_credits + unit(0.1, "in"),
		width = unit(0.88, "npc")
	)

	grobTree(
		grob_title,
		grob_description,
		grob_license,
		grob_copyright,
		grob_credits,
		name = "title_page",
		vp = a5_vp()
	)
}
