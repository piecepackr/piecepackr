# We use `<<-` below to modify the package's namespace.
# We don't modify the global environment.
# This is so {ggplot2} can be Suggests instead of Imports
# Define object at build time
GeomPiece <- NULL

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
      # Modify object at load time
      GeomPiece <<- create_GeomPiece()
  }
}
