cfg <- piecepackr::game_systems("sans3d")$piecepack
test_that("save_piece_obj works", {
    files <- save_piece_obj("tile_face", z = NA, cfg = cfg)
    expect_length(files, 3)
    files <- save_piece_obj("tile_back", cfg = cfg, angle = NA, axis_x = NA, axis_y = NA)
    expect_length(files, 3)
    files <- save_piece_obj("die_face", cfg = cfg)
    expect_length(files, 3)
    files <- save_piece_obj("pyramid_top", cfg = cfg)
    expect_length(files, 3)
    files <- save_piece_obj("pyramid_left", cfg = cfg)
    expect_length(files, 3)
    files <- save_piece_obj("pyramid_right", cfg = cfg)
    expect_length(files, 3)
    files <- save_piece_obj("pyramid_back", cfg = cfg)
    expect_length(files, 3)
    files <- save_piece_obj("pyramid_face", cfg = cfg)
    expect_length(files, 3)
    expect_error(save_piece_obj("belt_face"))
})

test_that("rgl works", {
    skip_on_cran()
    skip_on_os("mac")
    library("rgl")
    # go stone OBJ generation uses "rgl" in background
    files <- save_piece_obj("bit_face", cfg = game_systems("dejavu3d")$go)
    expect_length(files, 3)

    files <- save_piece_obj("pawn_top", cfg = game_systems("sans3d", pawn="peg-doll")$piecepack)
    expect_length(files, 3)

    rgl.open()
    df <- tibble::tibble(piece_side = "tile_face", scale = c(1, 0))
    pmap_piece(df, piece3d, cfg=cfg)
    f <- tempfile(fileext = ".png")
    rgl.snapshot(f)
    expect_true(file.exists(f))
    rgl.close()
    unlink(f)

    cfg <- game_systems("sans3d", pawn = "joystick")$piecepack
    rgl.clear()
    l <- piece3d("pawn_top", x=-1:1, suit=1:3, cfg = cfg, lit=TRUE)
    expect_true(length(l) > 2)
})

test_that("rayrender works", {
    skip_on_cran()
    library("rayrender")
    scene <- piece("coin_face", x=-1:1, rank=1:3, cfg = cfg)
    f <- tempfile(fileext = ".png")
    png(f)
    render_scene(scene, samples = 1)
    dev.off()
    expect_true(file.exists(f))
    unlink(f)

    expect_null(piece("coin_face", x=-1:1, rank=1:3, cfg = cfg, scale = 0))

    cfg <- game_systems("sans3d", pawn = "joystick")$piecepack
    scene <- piece("pawn_top", x=-1:1, suit=1:3, cfg = cfg)
    expect_equal(nrow(scene), 9)
    # render_scene(scene, samples = 1, lookfrom=c(0, -5, 5)) # nolint
})
