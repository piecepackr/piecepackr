# unlike `grid::editGrob()` *updates* previous cex, lex, alpha values
# by multiplying new values with previous values.
update_gp <- function(grob, gp = gpar()) {
    stopifnot(all(names(gp) %in% c("alpha", "cex", "lex")))
    if(is.null(grob$gp)) {
        grob$gp <- gp
    } else {
        for (name in c("alpha", "cex", "lex")) {
            if(hasName(grob$gp, name) && hasName(gp, name)) {
                grob$gp[[name]] <- grob$gp[[name]] * gp[[name]]
            } else if (hasName(gp, name)) {
                grob$gp[[name]] <- gp[[name]]
            }
        }
    }
    grob
}
