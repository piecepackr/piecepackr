opt_cache_key <- function(component_side, i_s, i_r) {
    paste(component_side, i_s, i_r, sep=".")
}

#' Create piecepack configuration list with a cache of component opts
#'
#' Adds a \code{cache} and \code{signature} attribute and a \code{pp_cfg} class
#' to a list of configuration options.  
#' The cache stores pre-computed component opt lists.
#' Once done this significantly speeds up the drawing of piecepack components
#' with that configuration list.  However if you later change the configuration list you
#' should run this again to re-compute the cache otherwise \code{draw_component} may 
#' not work as intended.
#' @param cfg List of configuration options
#' @examples
#'  \donttest{
#'    cfg <- list()
#'    system.time(replicate(500, draw_component("tile_face", cfg, 4, 4)))
#'    system.time(cfg <- pp_cfg(cfg))
#'    system.time(replicate(500, draw_component("tile_face", cfg, 4, 4)))
#'  }
#'   
#' @exportClass pp_cfg
#' @export
pp_cfg <- function(cfg=list()) {
    signature <- paste(unlist(cfg), collapse='')
    if (!is.null(attr(cfg, "signature"))) {
        if (attr(cfg, "signature") == signature) return(cfg)
    }
    attr(cfg, "signature") <- signature
    attr(cfg, "cache") <- list()
    class(cfg) <- "pp_cfg"

    n_ranks <- get_n_ranks(cfg)
    n_suits <- get_n_suits(cfg)
    i_unsuit <- n_suits + 1

    key <- opt_cache_key("tile_face", i_unsuit, n_ranks+1)
    attr(cfg, "cache")[[key]] <- get_component_opt("tile_face", i_unsuit, n_ranks+1, cfg) #### joker tile

    for (cs in COMPONENT_AND_SIDES_UNSUITED_UNRANKED) {
        key <- opt_cache_key(cs, i_unsuit, 0)
        attr(cfg, "cache")[[key]] <- get_component_opt(cs, i_unsuit, 0, cfg)
    }
    for (i_r in 1:n_ranks) {
        for (cs in COMPONENT_AND_SIDES_UNSUITED_RANKED) {
            key <- opt_cache_key(cs, i_unsuit, i_r)
            attr(cfg, "cache")[[key]] <- get_component_opt(cs, i_unsuit, i_r, cfg)
        }
    }
    for (i_s in 1:n_suits) {
        for (cs in COMPONENT_AND_SIDES_SUITED_UNRANKED) {
        key <- opt_cache_key(cs, i_s, 0)
        attr(cfg, "cache")[[key]] <- get_component_opt(cs, i_s, 0, cfg)
        }
        for (i_r in 1:n_ranks) {
            for (cs in COMPONENT_AND_SIDES_SUITED_RANKED) {
                key <- opt_cache_key(cs, i_s, i_r)
                attr(cfg, "cache")[[key]] <- get_component_opt(cs, i_s, i_r, cfg)
            }
        }
    }
    for (i_s in i_unsuit:(i_unsuit+1)) {
        for (i_r in 1:n_ranks) {
            for (cs in c("die_face")) {
                key <- opt_cache_key(cs, i_s, i_r)
                attr(cfg, "cache")[[key]] <- get_component_opt(cs, i_s, i_r, cfg)
            }
        }
        key <- opt_cache_key("suitdie_face", i_s, 0)
        attr(cfg, "cache")[[key]] <- get_component_opt("suitdie_face", i_s, 0, cfg)
    }
    cfg
}


#' @export
print.pp_cfg <- function(x, ...) {
    for(name in names(x)) {
        cat(paste0("$", name, " : ", x[[name]]), "\n")
    }
}

#' @export
as.list.pp_cfg <- function(x, ...) {
    attr(x, "cache") <- NULL
    attr(x, "signature") <- NULL
    x <- unclass(x)
    x
}

#' @export
`$<-.pp_cfg` <- function(x, name, value) {
    x <- as.list(x)
    x[[name]] <- value
    x
}

#' @export
`[[<-.pp_cfg` <- function(x, name, value) {
    x <- as.list(x)
    x[[name]] <- value
    x
}

