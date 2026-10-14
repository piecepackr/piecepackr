# pp_cfg querying variables work as expected

    Code
      cfg$has_piecepack <- 3
    Condition
      Error:
      ! `has_piecepack` must be a logical value

# `pp_cfg()` warns about unrecognized configuration names

    Code
      cfg <- pp_cfg(list(bogus = 1, cex.s1.r1.tile.x = 2, suit_color = "red"))
    Condition
      Warning:
      bogus is not a recognized configuration
      Warning:
      cex.s1.r1.tile.x is not a recognized configuration

