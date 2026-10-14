df_board <- data.frame(
	piece_side = "board_face",
	suit = 3,
	rank = 8,
	x = 4.5,
	y = 4.5,
	stringsAsFactors = FALSE
)
df_w <- data.frame(
	piece_side = "bit_back",
	suit = 6,
	rank = 1,
	x = rep(1:8, 2),
	y = rep(1:2, each = 8),
	stringsAsFactors = FALSE
)
df_b <- data.frame(
	piece_side = "bit_back",
	suit = 1,
	rank = 1,
	x = rep(1:8, 2),
	y = rep(7:8, each = 8),
	stringsAsFactors = FALSE
)
df <- rbind(df_board, df_w, df_b)
df$cfg <- "checkers1"

test_that("render_piece() works", {
	skip_on_ci()
	skip_if_not_installed("vdiffr")
	library("vdiffr")

	envir <- game_systems("sans")
	expect_doppelganger("render-piece", function() {
		render_piece(df, envir = envir, open_device = FALSE)
	})
	expect_doppelganger("render-piece-cartesian", function() {
		render_piece(df, envir = envir, annotate = TRUE, open_device = FALSE)
	})
	expect_doppelganger("render-piece-algebraic", function() {
		render_piece(
			df,
			envir = envir,
			op_scale = 0.5,
			annotate = "algebraic",
			trans = op_transform,
			open_device = FALSE
		)
	})

	skip_if_not_installed("rayvertex")
	library("rayvertex")
	envir3d <- game_systems("sans", border = FALSE)
	f <- tempfile(fileext = ".jpg")
	render_piece(
		df,
		file = f,
		.f = piece_mesh,
		envir = envir3d,
		op_scale = 0.5,
		trans = op_transform
	)
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not_installed("rayrender")
	library("rayrender")
	f <- tempfile(fileext = ".jpg")
	render_piece(
		df,
		file = f,
		.f = piece,
		envir = envir3d,
		op_scale = 0.5,
		trans = op_transform,
		samples = 5,
		clamp_value = 1,
		interactive = FALSE
	)
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not_installed("rgl")
	library("rgl")
	open3d()
	view3d(0, -30)
	f <- tempfile(fileext = ".tiff")
	render_piece(
		df,
		file = f,
		.f = piece3d,
		envir = envir3d,
		op_scale = 0.5,
		trans = op_transform,
		scale = 0.98
	)
	expect_true(file.exists(f))
	unlink(f)
	close3d()
})

test_that("render_piece() draws with `cfg` and defaults to the game systems' piecepack", {
	df <- data.frame(piece_side = "tile_face", x = 1, y = 1, suit = 1, rank = 2)
	drawn <- function(...) {
		grDevices::pdf(NULL)
		on.exit(invisible(grDevices::dev.off()))
		render_piece(df, open_device = FALSE, ...)
		pieces <- Filter(function(g) inherits(g, "pmap_piece"), grid::grid.grab()$children)
		pieces[[1]]$children[[1]]
	}
	opts <- function(cfg) Filter(Negate(is.function), cfg$as_list())
	expect_identical(opts(drawn()$cfg), opts(game_systems()$piecepack))
	cfg <- pp_cfg(list(suit_color = "red"))
	add_dice <- function(df, ...) {
		df$cfg <- "dice"
		df
	}
	expect_named(drawn(cfg = cfg, trans = add_dice)$envir, names(game_systems()))
	local_mocked_bindings(game_systems = function(...) stop("`game_systems()` was called"))
	expect_identical(drawn(cfg = cfg)$cfg, cfg)
})
