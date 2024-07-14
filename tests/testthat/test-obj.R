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
    skip_if_not_installed("rgl")
    skip_if_not_installed("systemfonts") # prevent buggy cairo interaction with "dejavu3d" style
    library("rgl", quietly = TRUE,
            include.only = c("clear3d", "close3d", "open3d", "view3d"))
    # go stone OBJ generation uses "rgl" in background
    files <- save_piece_obj("bit_face", cfg = game_systems("dejavu3d")$go)
    expect_length(files, 3)

    files <- save_piece_obj("pawn_top", cfg = game_systems("sans3d", pawn="peg-doll")$piecepack)
    expect_length(files, 3)

    open3d(useNULL = TRUE)
    df <- tibble::tibble(piece_side = "tile_face", scale = c(1, 0))
    pmap_piece(df, piece3d, cfg=cfg)

    clear3d()
    cfg <- game_systems("sans3d", pawn = "joystick")$piecepack
    l <- piece3d("pawn_top", x=-1:1, suit=1:3, cfg = cfg, lit=TRUE)
    expect_true(length(l) > 2)

    clear3d()
    cfg <- game_systems()$dice_d4
    l <- piece3d("die_face", x=1:4, suit=1:4, rank=1:4, cfg=cfg)
    expect_true(length(l) == 4)

    clear3d()
    l <- piece3d("die_face", x=1:8, suit=c(1:6, 1:2), rank=1:8, cfg=dice_d8())
    expect_true(length(l) == 8)

    clear3d()
    l <- piece3d("die_face", x=rep(1:5, 2), y=rep(1:2, each=5),
                 rank=1:10, suit=rep(1:5, 2), cfg=dice_d10())
    expect_true(length(l) == 10)

    clear3d()
    l <- piece3d("die_face", x=rep(1:5, 2), y=rep(1:2, each=5),
                 rank=1:10, suit=rep(1:5, 2), cfg=dice_d10_percentile())
    expect_true(length(l) == 10)

    clear3d()
    l <- piece3d("die_face", x=rep(1:6, 2), y=rep(1:2, each=6),
                 rank=1:12, suit=rep(1:6, length.out=12), cfg=dice_d12())
    expect_true(length(l) == 12)

    clear3d()
    l <- piece3d("die_face", x=rep(1:5, 4), y=rep(1:4, each=5),
                 rank=1:20, suit=rep(1:6, length.out=20), cfg=dice_d20())
    expect_true(length(l) == 20)

    close3d()
})

test_that("rayrender works", {
    skip_on_cran()
    skip_if_not_installed("rayrender")
    library("rayrender", include.only = "render_scene", quietly = TRUE)
    scene <- piece("coin_face", x=-1:1, rank=1:3, cfg = cfg)
    f <- tempfile(fileext = ".jpeg")
    png(f)
    render_scene(scene, samples = 1, interactive = FALSE)
    dev.off()
    expect_true(file.exists(f))
    unlink(f)

    expect_null(piece("coin_face", x=-1:1, rank=1:3, cfg = cfg, scale = 0))

    cfg <- game_systems("sans3d", pawn = "joystick")$piecepack
    skip_if_not_installed("rgl") # needed to generate joystick pawn obj
    scene <- piece("pawn_top", x=-1:1, suit=1:3, cfg = cfg)
    expect_equal(nrow(scene), 9)
    # render_scene(scene, samples = 1, lookfrom=c(0, -5, 5)) # nolint
})

test_that("rayvertex works", {
    skip_on_cran()
    skip_if_not_installed("rayvertex")
    library("rayvertex", quietly = TRUE,
            include.only = c("directional_light", "rasterize_scene"))
    scene <- piece_mesh("coin_face", x=-1:1, rank=1:3, cfg = cfg)
    f <- tempfile(fileext = ".png")
    png(f)
    rasterize_scene(scene,
                    light_info = directional_light(c(0, 0, 1)),
                    lookat = c(0, 0, 0))
    dev.off()
    expect_true(file.exists(f))
    unlink(f)

    expect_null(piece_mesh("coin_face", x=-1:1, rank=1:3, cfg = cfg, scale = 0))

    cfg <- game_systems("sans3d", pawn = "joystick")$piecepack
    skip_if_not_installed("rgl") # needed to generate joystick pawn obj
    scene <- piece_mesh("pawn_top", x=-1:1, suit=c(1,3,4), cfg = cfg)
    # rasterize_scene(scene, lookfrom=c(0, -5, 5),
    #                 light_info = directional_light(c(0, -5, 5))) # nolint
})
