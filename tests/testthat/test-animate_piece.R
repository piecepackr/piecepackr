test_that("animate_piece()", {
    # Prevent weird bug if checked with R compiled with cairo/X11 but X server not started
    skip_if_not(all(capabilities(c("cairo", "png", "X11"))))
    skip_if_not_installed("gifski")
    skip_if_not_installed("tweenr")

    dfs <- list()
    d.frame <- function(piece_side = "bit_back", ..., rank = 1L) {
                   data.frame(piece_side = piece_side, ..., rank = rank,
                              cfg = "checkers1", stringsAsFactors = FALSE)
    }
    df <- d.frame("board_back", suit = 2L, rank = 3L, x = 2, y = 2, id = "1")
    dfs[[1L]] <- df
    df <- rbind(df, d.frame(suit = 1L, x = 2, y = 2, id = "2"))
    dfs[[2L]] <- df
    df <- rbind(df, d.frame(suit = 2L, x = 1, y = 2, id = "3"))
    dfs[[3L]] <- df
    df <- rbind(df, d.frame(suit = 1L, x = 3, y = 1, id = "4"))
    dfs[[4L]] <- df
    df <- rbind(df, d.frame(suit = 2L, x = 1, y = 3, id = "5"))
    dfs[[5L]] <- df
    df <- rbind(df, d.frame(suit = 1L, x = 1, y = 1, id = "6"))
    dfs[[6L]] <- df
    df <- rbind(df, d.frame(suit = 2L, x = 3, y = 3, id = "7"))
    dfs[[7L]] <- df
    df <- rbind(df, d.frame(suit = 1L, x = 2, y = 1, id = "8"))
    dfs[[8L]] <- df

    gif_filename <- tempfile(fileext = ".gif")
    on.exit(unlink(gif_filename))
    animate_piece(dfs, file = gif_filename, n_transitions = 1L, n_pauses = 1L)
    expect_true(file.exists(gif_filename))
})
