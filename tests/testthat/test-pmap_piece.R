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
	expect_s3_class(pmap_piece(df, cfg = cfg, draw = FALSE), "pmap_piece")
	df$cfg <- "piecepack"
	expect_s3_class(
		pmap_piece(df, cfg = cfg, trans = op_transform, op_scale = 0.5, draw = FALSE),
		"pmap_piece"
	)
})

test_that("`default_envir()` holds `cfg` plus just the game systems named", {
	calls <- character()
	local_mocked_bindings(
		game_systems = function(..., systems = NULL) {
			calls <<- c(calls, systems)
			setNames(as.list(systems), systems)
		}
	)
	cfg <- pp_cfg()
	expect_identical(default_cfg_envir(cfg, NULL, character())$envir, default_envir(cfg))
	expect_identical(default_cfg_envir(cfg, NULL, "piecepack")$envir, default_envir(cfg))
	expect_identical(
		default_cfg_envir(cfg, NULL, character(), systems = TRUE)$envir,
		default_envir(cfg)
	)
	expect_identical(default_cfg_envir(NULL, NULL, character())$envir, default_envir())
	expect_identical(calls, character())
	expect_identical(
		default_cfg_envir(cfg, NULL, c("piecepack", "dice", "unknown"))$envir,
		structure(list(piecepack = cfg, dice = "dice"), default = TRUE)
	)
	expect_identical(calls, "dice")
	ce <- default_cfg_envir(NULL, NULL, "checkers1", systems = TRUE)
	expect_identical(ce$cfg, "piecepack")
	expect_named(ce$envir, c("piecepack", "checkers1"))
	expect_identical(default_cfg_envir(cfg, list(dice = "dice"), "dice")$envir, list(dice = "dice"))
})

test_that("`pmap_piece()` adds the game systems that `trans` names", {
	local_mocked_bindings(
		game_systems = function(..., systems = NULL) setNames(as.list(systems), systems)
	)
	cfg <- pp_cfg()
	df <- data.frame(piece_side = "tile_back", x = 1, y = 1)
	add_cfg <- function(name) {
		function(df, ...) {
			df$cfg <- name
			df
		}
	}
	drawn_envir <- function(...) pmap_piece(df, draw = FALSE, ...)$children[[1]]$envir
	expect_identical(drawn_envir(cfg = cfg, trans = add_cfg("piecepack")), default_envir(cfg))
	expect_identical(drawn_envir(trans = add_cfg("piecepack")), default_envir(NULL, "piecepack"))
	expect_identical(
		drawn_envir(cfg = cfg, envir = default_envir(cfg), trans = add_cfg("dice")),
		default_envir(cfg, "dice")
	)
	expect_identical(drawn_envir(envir = list(), trans = add_cfg("dice")), list())
})
