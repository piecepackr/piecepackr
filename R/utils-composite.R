# pieces that are composites of other pieces
# earlier pieces (in df) are "beneath" later pieces (as viewed from `ref_side`)
CompositePiece <- R6Class(
	"pp_composite",
	public = list(
		initialize = function(df = tibble(), envir = list(), ref_side = "top", portrait = FALSE) {
			private$df <- df
			private$envir <- envir
			private$ref_side <- ref_side
			private$portrait <- portrait
		}
	),
	active = list(
		grob_fn = function() {
			function(piece_side, suit, rank, cfg) {
				df <- private$relative_df(piece_side)
				pmap_piece(df, suit = suit, envir = private$envir, draw = FALSE)
			}
		},
		obj_fn = function() {
			function(
				piece_side,
				suit,
				rank,
				cfg,
				x,
				y,
				z,
				angle,
				axis_x,
				axis_y,
				width,
				height,
				depth,
				filename,
				res
			) {
				suit <- ifelse(is.na(suit), 1, suit)
				rank <- ifelse(is.na(rank), 1, rank)
				if (is.na(angle)) {
					angle <- 0
				}
				if (is.na(axis_x)) {
					axis_x <- 0
				}
				if (is.na(axis_y)) {
					axis_y <- 0
				}
				if (is.na(width)) {
					width <- cfg$get_width(piece_side, suit, rank)
				}
				if (is.na(height)) {
					height <- cfg$get_height(piece_side, suit, rank)
				}
				if (is.na(depth)) {
					depth <- cfg$get_depth(piece_side, suit, rank)
				}
				if (is.na(z)) {
					z <- 0.5 * depth
				}

				df <- private$transform_df(
					piece_side,
					x,
					y,
					z,
					angle,
					width,
					height,
					depth,
					axis_x,
					axis_y
				)
				ext <- tools::file_ext(filename)
				filename <- gsub(paste0("\\.", ext, "$"), paste0("_%03d.", ext), filename)
				filename <- rep(filename, length.out = nrow(df))
				filename <- sprintf(filename, seq_along(filename))
				stopifnot("`filename` must not contain duplicates" = !anyDuplicated(filename))
				df$filename <- filename
				l <- pmap_piece(
					df,
					suit = suit,
					envir = private$envir,
					.f = save_piece_obj,
					res = res
				)
				do.call(rbind, l)
			}
		},
		op_grob_fn = function() {
			function(
				piece_side,
				suit,
				rank,
				cfg,
				x,
				y,
				z,
				angle,
				type,
				width,
				height,
				depth,
				op_scale,
				op_angle,
				scale = 1
			) {
				x <- convertX(x, "in", valueOnly = TRUE)
				y <- convertY(y, "in", valueOnly = TRUE)
				z <- convertX(z, "in", valueOnly = TRUE)
				width <- convertX(width, "in", valueOnly = TRUE)
				height <- convertY(height, "in", valueOnly = TRUE)
				depth <- convertX(depth, "in", valueOnly = TRUE)
				# pp_cfg() uses a "portrait" convention for pawn_face/back/left/right:
				# height = pawn physical height (tall), not the generic token convention
				# where height = piece depth. get_scaling_factors() reads h_sc from
				# `depth` for top/base relative sides and from `width` for left/right,
				# so we swap dimensions to put pawn height where h_sc is read.
				if (private$portrait) {
					side <- get_side(piece_side)
					if (side %in% c("face", "back")) {
						tmp <- height
						height <- depth
						depth <- tmp
					} else if (side %in% c("left", "right")) {
						tmp <- width
						width <- height
						height <- tmp
					}
				}
				df <- private$relative_df(piece_side)
				relative_side <- get_relative_side(piece_side, private$ref_side)
				df <- scale_df(df, relative_side, width, height, depth)
				df <- translate_df(df, relative_side, x, y, z, angle, width, height, depth)
				# For portrait pieces viewed from the side (face/back/left/right),
				# sort farthest-first using the oblique projection depth.  This is
				# equivalent to projecting world (x, y) onto (cos, sin) of op_angle:
				# larger projection = further from viewer = drawn first.  Adding the
				# x·cos term breaks ties when rotation moves depth differences from y
				# into x (e.g. angle=90° gives both pieces the same world y).
				if (private$portrait && side %in% c("face", "back", "left", "right")) {
					idx <- painter_order(
						as_coord2d(x = df$x, y = df$y),
						scale = 1,
						alpha = degrees(op_angle)
					)
					df <- df[idx, ]
				}
				# adjust scale for proper adjustment of `cex` / `lex`
				df <- adjust_scale_df(df, scale = scale)
				pmap_piece(
					df,
					suit = suit,
					envir = private$envir,
					draw = FALSE,
					default.units = "in",
					op_scale = op_scale,
					op_angle = op_angle,
					scale = scale
				)
			}
		}
	),
	private = list(
		df = NULL,
		envir = NULL,
		ref_side = NULL,
		portrait = NULL,
		relative_df = function(piece_side) {
			side <- get_relative_side(piece_side, private$ref_side)
			switch(
				side,
				face = private$df,
				back = back_df(private$df),
				right = right_df(private$df),
				left = left_df(private$df),
				top = top_df(private$df),
				base = base_df(private$df),
				abort(paste("`CompositePiece()$relative_df()` can't handle relative side", side))
			)
		},
		transform_df = function(
			piece_side,
			x,
			y,
			z,
			angle,
			width,
			height,
			depth,
			axis_x = 0,
			axis_y = 0,
			scale = 1
		) {
			side <- get_side(piece_side)
			relative_side <- get_relative_side(piece_side, private$ref_side)

			df <- scale_df(private$df, relative_side, width, height, depth, scale)
			whd <- get_scaling_factors(
				relative_side,
				width = scale * width,
				height = scale * height,
				depth = scale * depth
			)
			R <- side_R_rev(private$ref_side) %*% side_R(side) %*% AA_to_R(angle, axis_x, axis_y)
			xyz <- as_coord3d(x = df$x, y = df$y, z = df$z)$translate(-0.5, -0.5, -0.5)$scale(
				whd$width,
				whd$height,
				whd$depth
			)$transform(R)$translate(x, y, z)
			df$x <- xyz$x
			df$y <- xyz$y
			df$z <- xyz$z
			#### Doesn't yet handle cases where components in df have their own angle, axis_x, axis_y values
			df <- cbind(df, R_to_AA(R))
			df
		}
	)
)

# Get "relative" side of `piece_side` given "reference" `ref_side`
get_relative_side <- function(piece_side, ref_side = "top") {
	side <- get_side(piece_side)
	switch(
		ref_side,
		face = switch(
			side,
			face = "face",
			right = "right",
			back = "back",
			left = "left",
			top = "top",
			base = "base"
		),
		right = switch(
			side,
			face = "left",
			right = "face",
			back = "right",
			left = "back",
			top = "top",
			base = "base"
		),
		back = switch(
			side,
			face = "back",
			right = "left",
			back = "face",
			left = "right",
			top = "top",
			base = "base"
		),
		left = switch(
			side,
			face = "right",
			right = "back",
			back = "left",
			left = "face",
			top = "top",
			base = "base"
		),
		top = switch(
			side,
			face = "top",
			right = "right",
			back = "base",
			left = "left",
			top = "face",
			base = "back"
		),
		base = switch(
			side,
			face = "top",
			right = "left",
			back = "base",
			left = "right",
			top = "back",
			base = "face"
		)
	)
}

scale_df <- function(df, relative_side, width, height, depth, scale = 1) {
	whd <- get_scaling_factors(
		relative_side,
		width = scale * width,
		height = scale * height,
		depth = scale * depth
	)
	df$height <- whd$height * df$height
	df$width <- whd$width * df$width
	df$depth <- whd$depth * df$depth
	df
}

# Simple case with no `axis_x` or `axis_y`
translate_df <- function(df, relative_side, x, y, z, angle, width, height, depth, scale = 1) {
	whd <- get_scaling_factors(
		relative_side,
		width = scale * width,
		height = scale * height,
		depth = scale * depth
	)
	R <- R_z(angle)
	xyz <- as_coord3d(x = df$x, y = df$y, z = df$z)$translate(-0.5, -0.5, -0.5)$scale(
		whd$width,
		whd$height,
		whd$depth
	)$transform(R)$translate(x, y, z)
	df$x <- xyz$x
	df$y <- xyz$y
	df$z <- xyz$z
	df$angle <- angle
	df
}

adjust_scale_df <- function(df, scale = 1) {
	df$height <- df$height / scale
	df$width <- df$width / scale
	df$depth <- df$depth / scale
	df
}

map_piece_side <- function(piece_side, map) {
	pieces <- vapply(piece_side, get_piece, character(1))
	sides <- vapply(piece_side, get_side, character(1))
	new_sides <- vapply(sides, function(s) map[[s]], character(1))
	paste(pieces, new_sides, sep = "_")
}

back_df <- function(df) {
	df$z <- 1 - df$z
	df$piece_side <- map_piece_side(
		df$piece_side,
		list(
			face = "back",
			back = "face",
			left = "right",
			right = "left",
			top = "base",
			base = "top"
		)
	)
	sort_df(df)
}

# new_x=x, new_y=z, new_z=1-y; new_width=w, new_height=d, new_depth=h
top_df <- function(df) {
	old_y <- df$y
	old_z <- df$z
	old_height <- df$height
	old_depth <- df$depth
	df$y <- old_z
	df$z <- 1 - old_y
	df$height <- old_depth
	df$depth <- old_height
	df$piece_side <- map_piece_side(
		df$piece_side,
		list(
			face = "top",
			back = "base",
			left = "left",
			right = "right",
			top = "back",
			base = "face"
		)
	)
	sort_df(df)
}

# new_x=1-x, new_y=z, new_z=y; new_width=w, new_height=d, new_depth=h
base_df <- function(df) {
	old_x <- df$x
	old_y <- df$y
	old_z <- df$z
	old_height <- df$height
	old_depth <- df$depth
	df$x <- 1 - old_x
	df$y <- old_z
	df$z <- old_y
	df$height <- old_depth
	df$depth <- old_height
	df$piece_side <- map_piece_side(
		df$piece_side,
		list(
			face = "base",
			back = "top",
			left = "right",
			right = "left",
			top = "face",
			base = "back"
		)
	)
	sort_df(df)
}

# new_x=1-y, new_y=z, new_z=x; new_width=h, new_height=d, new_depth=w
right_df <- function(df) {
	old_x <- df$x
	old_y <- df$y
	old_z <- df$z
	old_width <- df$width
	old_height <- df$height
	old_depth <- df$depth
	df$x <- 1 - old_y
	df$y <- old_z
	df$z <- old_x
	df$width <- old_height
	df$height <- old_depth
	df$depth <- old_width
	df$piece_side <- map_piece_side(
		df$piece_side,
		list(
			face = "right",
			back = "left",
			left = "face",
			right = "back",
			top = "top",
			base = "base"
		)
	)
	sort_df(df)
}

# new_x=y, new_y=z, new_z=1-x; new_width=h, new_height=d, new_depth=w
left_df <- function(df) {
	old_x <- df$x
	old_y <- df$y
	old_z <- df$z
	old_width <- df$width
	old_height <- df$height
	old_depth <- df$depth
	df$x <- old_y
	df$y <- old_z
	df$z <- 1 - old_x
	df$width <- old_height
	df$height <- old_depth
	df$depth <- old_width
	df$piece_side <- map_piece_side(
		df$piece_side,
		list(
			face = "left",
			back = "right",
			left = "back",
			right = "face",
			top = "top",
			base = "base"
		)
	)
	sort_df(df)
}

sort_df <- function(df) {
	if (hasName(df, "order")) {
		df[order(df$z, df$order), ]
	} else {
		df[order(df$z), ]
	}
}

rev_df <- function(df) {
	if (nrow(df) > 1L) {
		df[nrow(df):1, ]
	} else {
		df
	}
}
