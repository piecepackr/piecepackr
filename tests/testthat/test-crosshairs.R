test_that("crosshairs work as expected", {
	skip_if_not_installed("vdiffr")
	library("vdiffr")

	cfg <- pp_cfg(list(border_color = NA))
	df <- data.frame(
		piece_side = "tile_face",
		suit = 2,
		rank = 2,
		x = 2,
		y = 2,
		angle = 0,
		stringsAsFactors = FALSE
	)

	expect_doppelganger("crosshairs-circled-segments", {
		pmap_piece(df, grid.piece, cfg = cfg, default.units = "in")
		pmap_piece(
			df,
			grid.crosshair,
			cfg = cfg,
			default.units = "in",
			ch_grob = circledSegmentsCrosshairGrob()
		)
	})

	expect_doppelganger("crosshairs-segments", {
		pmap_piece(df, grid.piece, cfg = cfg, default.units = "in")
		pmap_piece(
			df,
			grid.crosshair,
			cfg = cfg,
			default.units = "in",
			ch_grob = segmentsCrosshairGrob()
		)
	})

	expect_doppelganger("crosshairs-squared-segments", {
		pmap_piece(df, grid.piece, cfg = cfg, default.units = "in")
		pmap_piece(
			df,
			grid.crosshair,
			cfg = cfg,
			default.units = "in",
			ch_grob = squaredSegmentsCrosshairGrob()
		)
	})

	expect_doppelganger("crosshairs-squares", {
		pmap_piece(df, grid.piece, cfg = cfg, default.units = "in")
		pmap_piece(df, grid.crosshair, cfg = cfg, default.units = "in")
	})
})
