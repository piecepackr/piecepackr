context("no regressions in figures")
test_that("no regressions in figures", {

    skip_on_ci()
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    # standard d6 dice
    cfg <- game_systems()$dice
    expect_doppelganger("dice_d6", function()
        grid.piece("die_face", x=1:6, default.units="in", rank=1:6, suit=1:6, op_scale=0.5, cfg=cfg))

    # dominoes
    envir <- game_systems("dejavu")
    expect_doppelganger("dominoes", function()
        grid.piece("tile_face", x=rep(4:1, 3), y=rep(2*3:1, each=4), suit=1:12, rank=1:12+1,
                   cfg = paste0("dominoes_", rep(rev(c("black", "red", "green", "blue", "yellow", "white")), 2)),
                   default.units="in", envir=envir, op_scale=0.5)
        )
    expect_error(xya_pips_cards(11), "Don't know pip pattern for 11 pips")
    expect_error(xya_pips_dominoes(13), "Don't know pip pattern for 13 pips")
    for (i in 0:10) expect_equal(nrow(xya_pips_cards(i)), i)

    # checkers
    expect_doppelganger("checkers", function() {
        df2 <- tibble(piece_side = "board_face", x = c(4, 6), y=c(4, 6),
                     suit = c(2, 4), rank = c(4, 2), cfg = "checkers2")
        df1f <- tibble(piece_side = "board_face", x = c(1, 2), y=c(1, 6),
                      suit = c(1, 5), rank = c(2, 4), cfg = "checkers1")
        df1b <- tibble(piece_side = "board_back", x = 5, y=1,
                      suit = 1, rank = 2, cfg = "checkers1")
        df_bit <- tibble(piece_side = "bit_face", x = c(rep(1, 6), 2:7), y = c(rep(1, 6), 2:7),
                         suit = c(1:6, 6:1), rank = rep(1:6, 2), cfg = "checkers1")
        df_bit2 <- tibble(piece_side = "bit_back", x = 6, y = 6, suit = 4, rank = 4, cfg = "checkers2")
        df <- rbind(df2, df1f, df1b, df_bit2, df_bit)
        pmap_piece(df, default.units="in", envir=envir, op_scale=0.5, trans=op_transform)
    })

    # go
    expect_doppelganger("go", function() {
        dfb <- tibble(piece_side = "board_face", x=10, y=10, suit=2)
        dfB <- tibble(piece_side = "bit_face",
                      x = c(2,2,2, 3,3, 4,4, 5,5, 6,6,6, 7,7, 8),
                      y = c(11,13,14, 12,15, 13,16, 12,16, 12,13,16, 15,17, 16),
                      suit = 4)
        dfR <- tibble(piece_side = "bit_face",
                      x = c(3,3, 4, 5, 6, 7, 8,8, 9),
                      y = c(2:3, 4, 3, 4, 4, 2:3, 2),
                      suit = 1)
        dfY <- tibble(piece_side = "bit_face",
                      x = 20 - c(3, 4, 5,5, 7, 8, 9,9,9),
                      y = 20 - c(3, 2, 2:3, 2, 5, 2:4),
                      suit = 5)
        dfG <- tibble(piece_side = "bit_face",
                      x = 20 - c(3,3,3,3,3, 4,4,4,4, 5,5,5, 6,6),
                      y = c(4,5,7,12,13, 6,7, 10,11, 7,9,10, 8,9),
                      suit = 3)
        dfK <- tibble(piece_side = "bit_face",
                      x = c(rep(2,5), rep(3,5), rep(4,4), rep(5,5),
                            rep(6,2), rep(7,4), rep(8,7), rep(9,5),
                            rep(10,5), rep(11,4), rep(12,5),
                            rep(13,6), rep(14,2), rep(15,6),
                            rep(16,6), rep(17,2)),
                      y = c(2:3,8:9,12, 4:5,10,13:14, 5,10,14:15, 6,8,10:11,14, 5,11,
                            5,7,12,14, 4,7,9,11:13,15, 3:4,11,14,16, 8:9,14:16,
                            3,6,8,15, 9:11,16,18, 6,8:9,11:12,16, 6:7,
                            5:6,8,11:12,16, 3,5,12,14,16:17, 3,16),
                      suit = 2)
        dfW <- tibble(piece_side = "bit_face",
                      x = c(3:5, rep(6,3), rep(7,3), rep(8,2), rep(9,5),
                            rep(10,3), rep(11,4), rep(12,4), rep(13,2), 14),
                      y = c(9,9,9, 7:8,10, 8,10:11, 8,10, 8:10,12:13, 10:11,13,
                            9,11,13:14, 7:8,12:13, 7,10, 11),
                      suit = 6)
        df <- rbind(dfb, dfB, dfR, dfY, dfG, dfK, dfW)

        cfg <- game_systems("sans")$go
        pmap_piece(df, grid.piece, cfg=cfg, trans=op_transform, default.units="in", op_scale=0.5)
    })

    # playing cards
    cfg <- game_systems("sans3d")$playing_cards_colored
    expect_doppelganger("ten_of_clubs", function()
        grid.piece("card_face", suit = 3, rank = 10, cfg = cfg))
    expect_doppelganger("king_of_stars", function()
        grid.piece("card_face", suit = 5, rank = 13, cfg = cfg))
    expect_doppelganger("red_joker", function()
        grid.piece("card_face", suit = 1, rank = 14, cfg = cfg))
    cfg <- game_systems("sans3d")$playing_cards_tarot
    expect_doppelganger("black_joker_no_star", function()
        grid.piece("card_face", suit = 3, rank = 15, cfg = cfg))
    expect_doppelganger("knight_of_diamonds", function()
        grid.piece("card_face", suit = 4, rank = 12, cfg = cfg))
})
