# Utilities to help use R 4.2's group affine transformation feature
# https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html

#' Affine transformation grob
#'
#' `transformationGrob()` is a grid grob function to facilitate
#' using the group affine transformation features introduced in R 4.2.
#' @param grob A grid grob to perform affine transformations on.
#' @param vp.define Viewport to define grid group in.
#' @param transform An affine transformation function.
#' @inheritParams pieceGrob
#' @examples
#' if (getRversion() >= '4.2.0' && require("grid")) {
#'   grob <- grobTree(circleGrob(gp=gpar(fill="yellow", col="blue")),
#'                    textGrob("RSTATS", gp=gpar(fontsize=32)))
#'
#'   vp.define <- viewport(width=unit(2, "in"), height=unit(2, "in"))
#'   transformation <- transformationGrob(grob, vp.define=vp.define)
#'
#'   # Only works if active graphics device supports affine transformations
#'   # such as `X11(type="cairo")` on R 4.2+
#'   \dontrun{
#'      grid.newpage()
#'      pushViewport(viewport(width=unit(3, "in"), height=unit(2, "in")))
#'      grid.draw(grob)
#'      popViewport()
#'
#'      grid.newpage()
#'      pushViewport(viewport(width=unit(3, "in"), height=unit(2, "in")))
#'      grid.draw(transformation)
#'      popViewport()
#'   }
#' }
#'
#' @noRd
transformationGrob <- function(grob,
                               vp.define = NULL,
                               transform = NULL,
                               name=NULL, gp = gpar(), vp = NULL) {
    stopifnot(getRversion() >= '4.2.0')
    if (is.null(transform))
        transform <- viewportTransform
    gTree(grob=grob, vp.define=vp.define, transform=transform,
          name = NULL, gp = gpar(), vp = NULL, cl = "pp_transformation")
}

#' @export
makeContent.pp_transformation <- function(x) {
    stopifnot(isTRUE(dev.capabilities()$transformations))
    define <- defineGrob(x$grob, vp=x$vp.define)
    use <- useGrob(define$name, transform=x$transform)
    gl <- gList(define, use)
    setChildren(x, gl)
}

has_transformations <- function() {
    getRversion() >= '4.2.0' && isTRUE(dev.capabilities()$transformations)
}
