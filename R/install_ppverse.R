#' Install additonal packages from the piecepackr universe
#'
#' Use [utils::install.packages()] to install additional R packages
#' from the piecepackr universe <https://piecepackr.r-universe.dev/builds>.
#' `pkgs_ppverse()` returns a character vector of R packages
#' in the piecepackr universe.
#' @param pkgs Packages to install. Passed to [utils::install.packages()].
#' @param ... Passed to [utils::install.packages()].
#' @param dependencies Logical indicating whether to install dependencies of `pkgs`. Passed to [utils::install.packages()].
#' @examples
#' pkgs_ppverse()
#' \dontrun{# Installs non-CRAN packages from the piecepackr universe
#'   install_ppverse()
#' }
#' @export
install_ppverse <- function(pkgs = pkgs_ppverse(free_libre_only), 
                            ..., 
                            dependencies = TRUE, 
                            free_libre_only = TRUE) {
    repos <- c(piecepackr = "https://piecepackr.r-universe.dev/",
               getOption("repos"))
    utils::install.packages(pkgs, ..., repos = repos, dependencies = dependencies)
}

#' @rdname install_ppverse
#' @param free_libre_only Flag to only include packages that are Free/Libre Open Source.
#' @export
pkgs_ppverse <- function(free_libre_only = TRUE) {
    pkgs <- c("piecepackr", "pprules", "ppcli", "ppgamer", "ppn", "ppdf")
    if (isFALSE(free_libre_only)) {
        pkgs <- append(pkgs, c("piecenikr"))
    }
    pkgs <- sort(pkgs)
    pkgs
}
