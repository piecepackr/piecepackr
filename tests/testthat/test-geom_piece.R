test_that("geom_piece() works as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    skip_if_not_installed("ggplot2")
    library("ggplot2")
    library("tibble")

    envir <- piecepackr::game_systems("sans")

    df_board <- tibble(piece_side = "board_face", suit = 3, rank = 8,
                   x = 4.5, y = 4.5)
    df_w <- tibble(piece_side = "bit_face", suit = 6, rank = 1,
                   x = rep(1:8, 2), y = rep(1:2, each=8))
    df_b <- tibble(piece_side = "bit_face", suit = 1, rank = 1,
                   x = rep(1:8, 2), y = rep(7:8, each=8))
    df <- rbind(df_board, df_w, df_b)

    gg <- ggplot(df, aes_piece(df)) +
        geom_piece(cfg = "checkers1", envir = envir) +
        coord_fixed() +
        scale_x_piece() +
        scale_y_piece() +
        theme_minimal(32) +
        theme(panel.grid = element_blank())

    expect_doppelganger("geom_piece", gg)
})
