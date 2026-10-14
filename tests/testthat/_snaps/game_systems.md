# `game_systems(systems = )` builds just the named systems

    Code
      game_systems(systems = c("piecepack", "boobear"))
    Condition
      Error in `game_systems()`:
      ! Don't have a game system named "boobear"

# removed `style` argument errors

    Code
      game_systems(style = "dejavu3d")
    Condition
      Error in `game_systems()`:
      ! `...` must be empty.
      x Problematic argument:
      * style = "dejavu3d"

