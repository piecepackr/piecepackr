#' Create graphics using data frame input
#'
#' `pmap_piece()` operates on the rows of a data frame
#'     applying `.f` to each row (usually `grid.piece`).
#'
#' `pmap_piece()` differs from `purrr::pmap()` in a few ways:
#' \enumerate{
#'    \item{If `cfg` and/or `envir` are missing attempts to set reasonable
#'          defaults.  A missing `envir` holds `cfg` (if given) under the name
#'          `"piecepack"` plus just the game systems from [game_systems()]
#'          that a `cfg` column of `.l` names.}
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
		# `trans` may add configuration names that a default `envir` lacks
		if (is_default_envir(ce$envir)) {
			ce$envir <- add_game_systems(ce$envir, cfg_lookups(.l))
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

# A default `envir` holds `cfg` (if any) under the name "piecepack" plus the
# game systems that `lookups` (see `cfg_lookups()`) name.  Building all of
# `game_systems()` takes tens of milliseconds so we only build the ones named.
# It is marked so that `pmap_piece()` can add any more that `trans` names.
default_envir <- function(cfg = NULL, lookups = character()) {
	envir <- if (is.null(cfg)) list() else list(piecepack = cfg)
	add_game_systems(structure(envir, default = TRUE), lookups)
}

is_default_envir <- function(envir) {
	isTRUE(attr(envir, "default"))
}

# Add the game systems that `lookups` name but a default `envir` lacks.
# Unknown names are left for `get_cfg()` to fall back to a default `pp_cfg()`.
add_game_systems <- function(envir, lookups) {
	systems <- setdiff(intersect(lookups, names(game_systems_exprs)), names(envir))
	if (length(systems)) {
		envir <- structure(c(envir, game_systems(systems = systems)), default = TRUE)
	}
	envir
}

# If `systems` is `TRUE` a defaulted `cfg` is `game_systems()$piecepack`,
# the look preferred for game diagrams, instead of `pp_cfg()`.
default_cfg_envir <- function(cfg = NULL, envir = NULL, lookups, systems = FALSE) {
	if (is.null(cfg) && is.null(envir)) {
		if (systems) {
			envir <- default_envir(NULL, c("piecepack", lookups))
			cfg <- envir[["piecepack"]]
		} else {
			cfg <- pp_cfg()
			envir <- default_envir(NULL, lookups)
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
		envir <- default_envir(cfg, lookups)
	}
	list(cfg = cfg, envir = envir)
}
