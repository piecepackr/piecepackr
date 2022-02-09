#' SPDX License List data
#'
#' `spdx_license_list` is a data frame of SPDX License List data.
#'
#' @seealso See \url{https://spdx.org/licenses/} for more information.
#' @format a data frame with eight variables:
#'   \describe{
#'     \item{id}{SPDX Identifier.}
#'      \item{name}{Full name of license.
#'           For Creative Commons licenses these have been tweaked from the SPDX version
#'           to more closely match the full name used by Creative Commons Foundation. }
#'      \item{url}{URL for copy of license located at `spdx.org`}
#'      \item{fsf}{Is this license considered Free/Libre by the FSF?}
#'      \item{osi}{Is this license OSI approved?}
#'      \item{deprecated}{Has this SPDFX Identifier been deprecated by SPDX?}
#'      \item{badge}{Filename of appropriate \dQuote{button mark} badge (if any)
#'                   located in `system.file("extdata/badges", package = "piecepackr")`.}
#'      \item{url_alt}{Alternative URL for license.
#'                    Manually created for a subset of Creative Commons licenses.
#'                    Others taken from \url{https://github.com/sindresorhus/spdx-license-list}.
#'                    }
#'   }
"spdx_license_list"
