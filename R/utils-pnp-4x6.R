print_and_play_4x6 <- function(cfg, pieces, quietly, bleed, size_bleed) {
    n_suits <- cfg$n_suits
    n_ranks <- cfg$n_ranks

    stopifnot(n_ranks <= 6)
    stopifnot(!bleed)

    if ("matchsticks" %in% pieces)
        abort('"matchsticks" `pieces` not currently supported for `size = "4x6"`')
    if ("pyramids" %in% pieces)
        abort('"pyramids" `pieces` not currently supported for `size = "4x6"`')
    if ("subpack" %in% pieces)
        abort('"subpack" `pieces` not currently supported for `size = "4x6"`')

    pl <- list()

    vp <- viewport(x = inch(3 + size_bleed$left),
                   y = inch(2 + size_bleed$bottom),
                   width = inch(6), height = inch(4))

    draw_4x6_title(cfg, pieces, quietly, vp)

    n_pages_tiles <- 2 * n_suits
    for (i_suit in seq.int(n_suits))
        draw_4x6_tiles(cfg, i_suit, vp)

    n_pages_coins <- (n_suits-1) %/%4 + 1
    for (i_page in seq.int(n_pages_coins)) {
        suits <- seq(4*i_page-3, 4*i_page)
        draw_4x6_coins(cfg, suits, vp)
    }

    n_pages_dice <- (n_suits-1) %/%4 + 1
    for (i_page in seq.int(n_pages_dice)) {
        suits <- seq(4*i_page-3, 4*i_page)
        draw_4x6_dice(cfg, suits, vp)
    }

    n_pages_pawns <- (n_suits-1) %/%4 + 1
    for (i_page in seq.int(n_pages_pawns)) {
        suits <- seq(4*i_page-3, 4*i_page)
        draw_4x6_pawns(cfg, suits, vp)
    }

    n_pages <- 1 + n_pages_tiles + 2 * n_pages_coins + n_pages_dice + n_pages_pawns

    pl$Piecepack <- n_pages
    pl
}

draw_4x6_title <- function(cfg, pieces, quietly, vp) {
    grob <- a5_title_grob(cfg, pieces, quietly, FALSE)
    grob <- editGrob(grob, vp = vp)
    grid.draw(grob)
}

draw_4x6_tiles <- function(cfg, i_suit, vp) {
    df <- tibble::tibble(piece_side = "tile_face",
                         x = rep(1:3 / 3 - 1/6, each = 2),
                         y = rep(2:1 / 2 - 1/4, 3),
                         suit = i_suit,
                         rank = 1:6)
    grid.newpage()
    pushViewport(vp)
    pmap_piece(df, cfg = cfg, default.units = "npc")
    popViewport()

    df$piece_side = "tile_back"
    # Rotate tile backs to partially hide direction of face if tile back are not fully symmetric
    df$angle <- 90 * ((df$suit + df$rank) %% 4)
    grid.newpage()
    pushViewport(vp)
    pmap_piece(df, cfg = cfg, default.units = "npc")
    popViewport()
}

draw_4x6_coins <- function(cfg, suits, vp) {
    df <- tibble::tibble(piece_side = "coin_back",
                         x = rep(1:6 / 6 - 1/12, 4),
                         y = rep(4:1 / 4 - 1/8, each = 6),
                         suit = rep(suits, each = 6),
                         rank = rep(1:6, 4))

    grid.newpage()
    pushViewport(vp)
    pmap_piece(df, cfg = cfg, default.units = "npc")
    popViewport()

    df$piece_side = "coin_face"
    grid.newpage()
    pushViewport(vp)
    pmap_piece(df, cfg = cfg, default.units = "npc")
    popViewport()
}

draw_4x6_dice <- function(cfg, suits, vp) {
    df <- tibble::tibble(piece_side = "die_face",
                         x = rep(1:6 / 6 - 1/12, 4),
                         y = rep(4:1 / 4 - 1/8, each = 6),
                         suit = rep(suits, each = 6),
                         rank = rep(1:6, 4))

    grid.newpage()
    pushViewport(vp)
    pmap_piece(df, cfg = cfg, default.units = "npc")
    popViewport()
}

draw_4x6_pawns <- function(cfg, suits, vp) {

    dfp <- tibble::tibble(piece_side = "pawn_layout",
                          x = 0.25, y = 4:1 / 4 - 1/8,
                          angle = -90, suit = suits)
    #### Saucers in middle?
    dfb <- tibble::tibble(piece_side = "belt_face",
                          x = 0.75, y = 4:1 / 4 - 1/8,
                          angle = 0, suit = suits)
    df <- rbind(dfp, dfb)

    grid.newpage()
    pushViewport(vp)
    pmap_piece(df, cfg = cfg, default.units = "npc")
    popViewport()
}
