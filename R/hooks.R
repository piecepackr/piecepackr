.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "suppressWarnings")
  backports::import(pkgname, "hasName", force = TRUE)
}
