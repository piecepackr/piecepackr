library("vdiffr")
context("no regressions in figures")
test_that("no regressions in figures", {

    skip_on_ci()
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

    # playing cards
    cfg <- game_systems("sans3d")$playing_cards_colored
    expect_doppelganger("ten_of_clubs", function()
        grid.piece("card_face", suit = 3, rank = 10, cfg = cfg))
    expect_doppelganger("king_of_stars", function()
        grid.piece("card_face", suit = 5, rank = 13, cfg = cfg))
    expect_doppelganger("red_joker", function()
        grid.piece("card_face", suit = 1, rank = 14, cfg = cfg))
})
