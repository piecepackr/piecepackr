test_that("no regressions in figures", {

    skip_on_ci()
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    # d6 dice
    envir <- game_systems()
    suppressMessages({
      expect_doppelganger("dice_d6", function() {
        grid.piece("die_face", x=1:6, y=1, default.units="in", rank=1:6, suit=1:6, op_scale=0.5, cfg=envir$dice)
        grid.piece("die_face", x=1:6, y=2, default.units="in", rank=1:6, suit=1:6, op_scale=0.5, cfg=envir$dice_numeral)
        grid.piece("die_face", x=1:6, y=3, default.units="in", rank=1:6, suit=1:6, op_scale=0.5, cfg=envir$dice_fudge)
    })}, classes="piecepackr_affine_transformation")

    # standard d4 dice
    cfg <- envir$dice_d4
    suppressMessages({
      expect_doppelganger("dice_d4", function() {
        grid.piece("die_face", x=1:4, y=1, default.units="in", rank=1:4, suit=1:4, op_scale=0.0, cfg=cfg)
        grid.piece("die_face", x=1:4, y=2, default.units="in", rank=1:4, suit=1:4, op_scale=0.5, cfg=cfg)
    })}, classes="piecepackr_affine_transformation")

    # standard d8 dice
    cfg <- envir$dice_d8
    suppressMessages({
      expect_doppelganger("dice_d8", function() {
        grid.piece("die_face", x=1:8, y=1, default.units="in", rank=1:8, suit=c(1:6, 1:2), op_scale=0.0, cfg=cfg)
        grid.piece("die_face", x=1:8, y=2, default.units="in", rank=1:8, suit=c(1:6, 1:2), op_scale=0.5, cfg=cfg)
    })}, classes="piecepackr_affine_transformation")

    # standard d10 dice
    cfg <- envir$dice_d10
    suppressMessages({
      expect_doppelganger("dice_d10", function() {
        grid.piece("die_face", x=rep(1:5, 2), y=rep(1:2, each=5),
                   default.units="in", rank=1:10, suit=rep(1:5, 2),
                   op_scale=0.5, cfg=cfg)
    })}, classes="piecepackr_affine_transformation")

    # percentile d10 dice
    cfg <- envir$dice_d10_percentile
    suppressMessages({
      expect_doppelganger("dice_d10_percentile", function() {
        grid.piece("die_face", x=rep(1:5, 2), y=rep(1:2, each=5),
                   default.units="in", rank=1:10, suit=rep(1:5, 2),
                   op_scale=0.5, cfg=cfg)
    })}, classes="piecepackr_affine_transformation")

    # standard d12 dice
    cfg <- envir$dice_d12
    suppressMessages({
      expect_doppelganger("dice_d12", function() {
        grid.piece("die_face", x=rep(1:6, 2), y=rep(1:2, each=6),
                   default.units="in", rank=1:12, suit=rep(1:6, 2),
                   op_scale=0.5, cfg=cfg)
    })}, classes="piecepackr_affine_transformation")

    # standard d20 dice
    cfg <- envir$dice_d20
    suppressMessages({
      expect_doppelganger("dice_d20", function() {
        grid.piece("die_face", x=rep(1:5, 4),
                   y=rep(1:4, each=5), default.units="in", rank=1:20,
                   suit=rep(1:6, length.out=20),
                   op_scale=0.5, cfg=cfg)
    })}, classes="piecepackr_affine_transformation")

    # dominoes
    skip_if_not_installed("systemfonts") # prevent buggy cairo interaction with "dejavu" style
    envir <- game_systems("dejavu")
    expect_doppelganger("dominoes", function()
        grid.piece("tile_face", x=rep(6:1, 3), y=rep(2*3:1, each=6), suit=1:18, rank=1:18+1,
                   cfg = paste0("dominoes_", rep(rev(c("black", "red", "green", "blue", "yellow", "white")), 2)),
                   default.units="in", envir=envir, op_scale=0.5)
        )
    expect_error(xya_pips_cards(11), "Don't know pip pattern for 11 pips")
    expect_error(xya_pips_dominoes(19), "Don't know pip pattern for 19 pips")
    for (i in 0:10) expect_equal(nrow(xya_pips_cards(i)), i)

    # Chinese dominoes
    suppressMessages({
      expect_doppelganger("dominoes_chinese", function() {
        df1 <- tibble(piece_side = "tile_face",
                      suit = c(rep(1, 6L), 2L, rep(2, 4L), rep(3, 3L), 3, rep(4, 3), 5, 5, 6),
                      rank = c(1:6, 2L, 3:6, 3:5, 6, 4:6, 5:6, 6),
                      cfg = c(rep(c("dominoes_chinese", "dominoes_chinese_black", "dominoes_chinese"), each = 7L)),
                      x = rep(1:7, 3L),
                      y = c(rep(c(1.5, 4.0, 6.5), each = 7L)))
        df2 <- tibble(piece_side = "die_face",
                      suit = 1, rank = 1:6,
                      cfg = rep(c("dominoes_chinese", "dominoes_chinese_black", "dominoes_chinese"), each = 2L),
                      x = 8.5, y = c(1, 2.25, 3.5, 4.75, 6.0, 7.25))
        df <- rbind(df1, df2)
        pmap_piece(df, envir = game_systems(), default.units = "in",
                   trans = op_transform, op_scale = 0.5)
        })
    }, classes="piecepackr_affine_transformation")

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

    # joystick pawns
    cfg <- game_systems(pawn = "joystick")$piecepack
    df <- tibble(piece_side = "pawn_top", x = 1:4, y=1:4, suit=1:4)
    expect_doppelganger("joystick", function() {
        pmap_piece(df, default.units="in", cfg=cfg)
    })
    expect_doppelganger("joystick-op", function() {
        pmap_piece(df, default.units="in", cfg=cfg, op_scale=0.5)
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
        grid.piece("card_face", suit = 3, rank = 10, cfg = cfg,
                   default.units = "npc"))
    expect_doppelganger("king_of_stars", function()
        grid.piece("card_face", suit = 5, rank = 13, cfg = cfg,
                   default.units = "npc"))
    expect_doppelganger("red_joker", function()
        grid.piece("card_face", suit = 1, rank = 14, cfg = cfg,
                   default.units = "npc"))
    cfg <- game_systems("sans3d")$playing_cards_tarot
    expect_doppelganger("black_joker_no_star", function()
        grid.piece("card_face", suit = 3, rank = 15, cfg = cfg,
                   default.units = "npc"))
    expect_doppelganger("knight_of_diamonds", function()
        grid.piece("card_face", suit = 4, rank = 12, cfg = cfg,
                   default.units = "npc"))

    # morris
    cfg <- envir$morris
    expect_doppelganger("two_morris", function() {
        grid.piece("board_face", x=4, y=4, suit=3, rank=2, cfg = cfg, default.units = "in")
        grid.piece("bit_back", x=0:5, y=0:5, suit=1:6, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("three_morris", function() {
        grid.piece("board_face", x=4, y=4, suit=3, rank=3, cfg = cfg, default.units = "in")
        grid.piece("bit_back", x=0:5, y=0:5, suit=1:6, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("six_morris", function() {
        grid.piece("board_face", x=4, y=4, suit=3, rank=6, cfg = cfg, default.units = "in")
        grid.piece("bit_back", x=0:5, y=0:5, suit=1:6, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("seven_morris", function() {
        grid.piece("board_face", x=4, y=4, suit=3, rank=7, cfg = cfg, default.units = "in")
        grid.piece("bit_back", x=0:5, y=0:5, suit=1:6, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("nine_morris", function() {
        grid.piece("board_face", x=4, y=4, suit=3, rank=9, cfg = cfg, default.units = "in")
        grid.piece("bit_back", x=0:5, y=0:5, suit=1:6, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("twelve_morris", function() {
        grid.piece("board_face", x=4, y=4, suit=3, rank=12, cfg = cfg, default.units = "in")
        grid.piece("bit_back", x=0:5, y=0:5, suit=1:6, cfg = cfg, default.units = "in")
    })

    # alquerque
    cfg <- envir$alquerque
    expect_doppelganger("alquerque", function() {
        grid.piece("board_face", x=3, y=3, suit=3, cfg = cfg, default.units = "in")
        grid.piece("bit_back", x=1:5, y=1:5, suit=1:5, cfg = cfg, default.units = "in")
    })

    # reversi
    expect_doppelganger("reversi", function() {
        envir <- game_systems()
        df_board <- tibble(piece_side = "board_face", x = 4.5, y = 4.5,
                     suit = 4, rank = 8, cfg = "reversi")
        df_bit_face <- tibble(piece_side = "bit_face", x = 1:6, y = 1:6,
                              suit = 1:6, rank = NA, cfg = "reversi")
        df_bit_back <- tibble(piece_side = "bit_back", x = 1:6 + 1L, y = 1:6,
                              suit = 1:6, rank = NA, cfg = "reversi")
        df <- rbind(df_board, df_bit_face, df_bit_back)
        pmap_piece(df, default.units="in", envir=envir, op_scale=0.5, trans=op_transform)
    })

})
