test_that("`cfg_lookups()` returns the configuration names in a `cfg` column", {
	cfg <- pp_cfg()
	df <- data.frame(piece_side = "tile_back", x = 1:2, y = 1)
	expect_identical(cfg_lookups(df), character())
	df$cfg <- rep(list(cfg), 2)
	expect_identical(cfg_lookups(df), character())
	df$cfg <- factor("piecepack")
	expect_identical(cfg_lookups(df), "piecepack")
	df$cfg <- c("piecepack", NA)
	expect_identical(cfg_lookups(df), "piecepack")
	df$cfg <- list(cfg, "dice")
	expect_identical(cfg_lookups(df), "dice")
})

test_that("`pmap_piece()` skips `game_systems()` when `cfg` covers every lookup", {
	local_mocked_bindings(game_systems = function(...) stop("`game_systems()` was called"))
	cfg <- pp_cfg()
	df <- data.frame(piece_side = "tile_back", x = 1:2, y = 1)
	expect_identical(default_cfg_envir(cfg, NULL, character())$envir, list(piecepack = cfg))
	expect_identical(default_cfg_envir(cfg, NULL, "piecepack")$envir, list(piecepack = cfg))
	expect_named(default_cfg_envir(NULL, NULL, character())$envir, "piecepack")
	expect_s3_class(pmap_piece(df, cfg = cfg, draw = FALSE), "pmap_piece")
	expect_s3_class(
		pmap_piece(df, cfg = cfg, trans = op_transform, op_scale = 0.5, draw = FALSE),
		"pmap_piece"
	)
})

test_that("`pmap_piece()` builds `game_systems()` to look up other configurations", {
	local_mocked_bindings(game_systems = function(...) list(dice = "dice"))
	cfg <- pp_cfg()
	expect_identical(default_cfg_envir(NULL, NULL, "piecepack")$envir, list(dice = "dice"))
	expect_identical(
		default_cfg_envir(cfg, NULL, c("piecepack", "dice"))$envir,
		list(dice = "dice", piecepack = cfg)
	)
	add_dice <- function(df, ...) {
		df$cfg <- "dice"
		df
	}
	df <- data.frame(piece_side = "tile_back", x = 1, y = 1)
	grob <- pmap_piece(df, cfg = cfg, trans = add_dice, draw = FALSE)
	expect_identical(grob$children[[1]]$envir, list(dice = "dice", piecepack = cfg))
})
