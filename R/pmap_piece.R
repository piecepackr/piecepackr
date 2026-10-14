#' Create graphics using data frame input
#'
#' `pmap_piece()` operates on the rows of a data frame
#'     applying `.f` to each row (usually `grid.piece`).
#'
#' `pmap_piece()` differs from `purrr::pmap()` in a few ways:
#' \enumerate{
#'    \item{If `cfg` and/or `envir` are missing attempts to set reasonable
#'          defaults.  A missing `envir` is built with [game_systems()] only if
#'          `.l` has a `cfg` column of configuration names to look up (other
#'          than `"piecepack"` when `cfg` is given); otherwise it is just
#'          `list(piecepack = cfg)`.}
#'    \item{If not `NULL` will first apply function `trans` to `.l`.}
#'    \item{If the output of `.f` is a grid grob object then `pmap_piece`
#'          will return a `gTree` object with
#'          specified `name`, `gp`, and `vp` values and if `draw` is true draw it.}
#'    \item{If `.l` lacks a `name` column or if `name` column is non-unique
#'          attempts to generate a reasonable new default `name` column
#'          and use that to name the return `gTree` children  or `list` values.}
#'  }
#' @inheritParams grid.piece
#' @param .l A list of vectors, such as a data frame. The length of `.l`
#'           determines the number of arguments that `.f`
#'           will be called  with. List names will be used if present.
#' @param .f Function to be applied to `.l` after adjustments to
#'        `cfg` and `envir` and the application of `trans`.
#'        Usually [grid.piece()], [pieceGrob()], [piece3d()], or [piece()].
#' @param trans Function to modify `.l` before drawing.
#'        Default (`NULL`) is to not modify `.l`.  `op_transform`
#'        can help with using an oblique projection (i.e. `op_scale` over 0).
#' @param ... Extra arguments to pass to `.f`.
#' @examples
#'   if (requireNamespace("grid", quietly = TRUE) && piecepackr:::device_supports_unicode()) {
#'        dark_colorscheme <- list(suit_color="darkred,black,darkgreen,darkblue,black",
#'                             invert_colors.suited=TRUE, border_color="black", border_lex=2)
#'        traditional_ranks <- list(use_suit_as_ace=TRUE, rank_text=",a,2,3,4,5")
#'        cfg3d <- list(width.pawn=0.75, height.pawn=0.75, depth.pawn=1,
#'                           dm_text.pawn="", shape.pawn="convex6", invert_colors.pawn=TRUE,
#'                           edge_color.coin="tan", edge_color.tile="tan")
#'        cfg <- pp_cfg(c(dark_colorscheme, traditional_ranks, cfg3d))
#'        grid::grid.newpage()
#'        df_tiles <- data.frame(piece_side="tile_back", x=0.5+c(3,1,3,1), y=0.5+c(3,3,1,1),
#'                               suit=NA, angle=NA, z=NA, stringsAsFactors=FALSE)
#'        df_coins <- data.frame(piece_side="coin_back", x=rep(4:1, 4), y=rep(4:1, each=4),
#'                               suit=1:16%%2+rep(c(1,3), each=8),
#'                               angle=rep(c(180,0), each=8), z=1/4+1/16, stringsAsFactors=FALSE)
#'        df <- rbind(df_tiles, df_coins)
#'        pmap_piece(df, cfg=cfg, op_scale=0.5, default.units="in")
#'   }
#' @seealso [render_piece()] is a higher-level function that wraps this function.
#' @export
pmap_piece <- function(
	.l,
	.f = pieceGrob,
	...,
	cfg = getOption("piecepackr.cfg"),
	envir = getOption("piecepackr.envir"),
	trans = getOption("piecepackr.trans"),
	draw = TRUE,
	name = NULL,
	gp = NULL,
	vp = NULL
) {
	if (length(.l) == 0L || (!is.null(nrow(.l)) && nrow(.l) == 0L)) {
		return(list())
	}
	ce <- default_cfg_envir(cfg, envir, cfg_lookups(.l))

	.l <- update_name(.l)
	if (is.function(trans)) {
		.l <- trans(.l, ..., cfg = ce$cfg, envir = ce$envir)
		.l <- update_name(.l)
		# `trans` may add configuration names that a stand-in `envir` lacks
		lookups <- cfg_lookups(.l)
		if (is_standin_envir(ce$envir) && needs_game_systems(cfg, lookups)) {
			ce <- default_cfg_envir(cfg, NULL, lookups)
		}
	}
	cfg <- ce$cfg
	envir <- ce$envir
	if (hasName(.l, "cfg")) {
		ll <- purrr::pmap(.l, .f, ..., envir = envir, draw = FALSE)
	} else {
		ll <- purrr::pmap(.l, .f, ..., cfg = cfg, envir = envir, draw = FALSE)
	}
	if (length(ll) && all(sapply(ll, is.grob))) {
		grob <- gTree(children = as.gList(ll), name = name, gp = gp, vp = vp, cl = "pmap_piece")
		if (draw) {
			grid.draw(grob)
		}
		invisible(grob)
	} else {
		names(ll) <- .l$name
		invisible(ll)
	}
}

update_name <- function(.l) {
	if (hasName(.l, "name")) {
		if (!is.character(.l$name)) {
			.l$name <- as.character(.l$name)
		}
		if (anyDuplicated(.l$name)) {
			warn("the name column in .l is not unique, generating new name column")
		} else {
			return(.l)
		}
	}
	if (hasName(.l, "id")) {
		.l$name <- paste0("piece.", as.character(.l$id))
		if (anyDuplicated(.l$name)) {
			warn("the id column in .l is not unique, generating new name column")
		} else {
			return(.l)
		}
	}
	.l$name <- paste0("piece.", as.character(seq(length(.l[[1]]))))
	.l
}

#' @export
grobPoints.pmap_piece <- function(x, closed, ...) {
	grobCoords(x, closed)
}

#' @export
grobCoords.pmap_piece <- function(x, closed, ...) {
	gc <- lapply(x$children, grobCoords, closed = closed)
	f <- function(x, y) gridGeometry::polyclip(x, y, "union")
	Reduce(f, gc)
}

as.gList <- function(ll) {
	gl <- gList()
	for (ii in seq(ll)) {
		gl[[ii]] <- ll[[ii]]
	}
	gl
}

# The names of the configurations a `cfg` column of `.l` refers to by name
# (as opposed to by `pp_cfg()` object), which must be looked up in `envir`
cfg_lookups <- function(.l) {
	cfgs <- .l[["cfg"]]
	if (is.list(cfgs)) {
		cfgs <- unlist(Filter(is.character, cfgs))
	}
	cfgs <- as.character(cfgs)
	unique(cfgs[!is.na(cfgs)])
}

# Must a defaulted `envir` be built with `game_systems()` to resolve `lookups`
# (see `cfg_lookups()`)?  A defaulted `cfg` is `pp_cfg()` rather than
# `game_systems()$piecepack`, so without `cfg` any lookup, even "piecepack",
# needs `game_systems()`.
needs_game_systems <- function(cfg, lookups) {
	length(lookups) > 0L && (is.null(cfg) || !all(lookups == "piecepack"))
}

# Building `game_systems()` takes about 0.1 seconds, so when `lookups` needs
# nothing but `cfg` a defaulted `envir` is this cheap stand-in instead.
# `pmap_piece()` replaces it with `game_systems()` if `trans` adds lookups.
standin_envir <- function(cfg) {
	structure(list(piecepack = cfg), standin = TRUE)
}

is_standin_envir <- function(envir) {
	isTRUE(attr(envir, "standin"))
}

# If `systems` is `TRUE` a defaulted `cfg` is `game_systems()$piecepack`,
# the look preferred for game diagrams, instead of `pp_cfg()`.
default_cfg_envir <- function(cfg = NULL, envir = NULL, lookups, systems = FALSE) {
	if (is.null(cfg) && is.null(envir)) {
		if (systems) {
			envir <- game_systems()
			cfg <- envir[["piecepack"]]
		} else {
			cfg <- pp_cfg()
			envir <- if (needs_game_systems(NULL, lookups)) game_systems() else standin_envir(cfg)
		}
	} else if (is.null(cfg)) {
		# and !is.null(envir)
		if (hasName(envir, "piecepack")) {
			cfg <- envir[["piecepack"]]
		} else {
			cfg <- pp_cfg()
		}
	} else if (is.null(envir)) {
		# and !is.null(cfg)
		cfg <- get_cfg(cfg)
		if (needs_game_systems(cfg, lookups)) {
			envir <- game_systems()
			envir[["piecepack"]] <- cfg
		} else {
			envir <- standin_envir(cfg)
		}
	}
	list(cfg = cfg, envir = envir)
}
