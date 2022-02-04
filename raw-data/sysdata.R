if (FALSE) {
    # re-build meeple coordinates
    library("grid")
    library("rsvg")
    library("tibble")

    svg_file <- tempfile(fileext = ".svg")
    rsvg::rsvg_svg("raw-data/meeple.svg", file = svg_file)

    p = grImport2::readPicture(svg_file)
    xy = grobCoords(grImport2::pictureGrob(p), closed=T)[[2]]

    x <- xy$x
    x <- (x - min(x)) / (max(x) - min(x))

    y <- xy$y
    y <- (y - min(y)) / (max(y) - min(y))

    df <- tibble::tibble(x, y)
    df <- df[!duplicated(df), ]
    df <- df[c(266:530, 1:265),]

    df$c <- "C1"
    df[530, "c"] <- "C0"
    df[137, "c"] <- "C0"
    df[232, "c"] <- "C0"
    df[300, "c"] <- "C0"
    df[395, "c"] <- "C0"

    # f <- function(i) grid.points(df$x[i], df$y[i], default.units = "npc")
    # grid.newpage()
    # grid.polygon(x=df$x, y=df$y, gp=gpar(fill="lightblue"))

    meeple_xy <- as.data.frame(df)

} else {
    load("R/sysdata.rda")
}

if (FALSE) {
    # git clone https://github.com/creativecommons/cc-assets
    library("rsvg")
    compress <- function(svg_filename) {
        svgz_filename <- paste0(svg_filename, 'z')
        system(paste("cat", svg_filename, "| gzip >", svgz_filename))
    }
    rsvg::rsvg_svg("raw-data/cc-assets/license_badges/big/by.svg", "inst/extdata/badges/by.svg")
    compress("inst/extdata/badges/by.svg")
    rsvg::rsvg_svg("raw-data/cc-assets/license_badges/big/by_nc.svg", "inst/extdata/badges/by-nc.svg")
    compress("inst/extdata/badges/by-nc.svg")
    rsvg::rsvg_svg("raw-data/cc-assets/license_badges/big/by_nd.svg", "inst/extdata/badges/by-nd.svg")
    compress("inst/extdata/badges/by-nd.svg")
    rsvg::rsvg_svg("raw-data/cc-assets/license_badges/big/by_nc_nd.svg", "inst/extdata/badges/by-nc-nd.svg")
    compress("inst/extdata/badges/by-nc-nd.svg")
    rsvg::rsvg_svg("raw-data/cc-assets/license_badges/big/by_nc_sa.svg", "inst/extdata/badges/by-nc-sa.svg")
    compress("inst/extdata/badges/by-nc-sa.svg")
    rsvg::rsvg_svg("raw-data/cc-assets/license_badges/big/by_sa.svg", "inst/extdata/badges/by-sa.svg")
    compress("inst/extdata/badges/by-sa.svg")
    rsvg::rsvg_svg("raw-data/cc-assets/license_badges/big/cc_zero.svg", "inst/extdata/badges/cc-zero.svg")
    compress("inst/extdata/badges/cc-zero.svg")
    rsvg::rsvg_svg("raw-data/cc-assets/license_badges/big/publicdomain.svg", "inst/extdata/badges/publicdomain.svg")
    compress("inst/extdata/badges/publicdomain.svg")

}

# SPDX License List data
# Update CC urls and add CC badge info
if (FALSE) {
    library("dplyr")
    library("rvest")

    spdx_file <-"raw-data/spdx_license_list.html"

    if (!file.exists(spdx_file))
        download.file("https://spdx.org/licenses/", spdx_file)

    spdx <- read_html(spdx_file) |> html_table() |> bind_rows()

    names(spdx) <- c("name", "id", "fsf", "osi", "deprecated")

    spdx <- mutate(spdx,
                     fsf = ifelse(fsf == "Y", TRUE, fsf),
                     fsf = ifelse(fsf == "", FALSE, fsf),
                     fsf = as.logical(fsf),
                     osi = ifelse(osi == "Y", TRUE, fsf),
                     osi = ifelse(osi == "", FALSE, osi),
                     osi = as.logical(osi),
                     url = paste0("https://spdx.org/licenses/", id, ".html"),
                     url = ifelse(id == "CC-BY-1.0", "https://creativecommons.org/licenses/by/1.0/", url),
                     url = ifelse(id == "CC-BY-2.0", "https://creativecommons.org/licenses/by/2.0/", url),
                     url = ifelse(id == "CC-BY-2.5", "https://creativecommons.org/licenses/by/2.5/", url),
                     url = ifelse(id == "CC-BY-3.0", "https://creativecommons.org/licenses/by/3.0/", url),
                     url = ifelse(id == "CC-BY-4.0", "https://creativecommons.org/licenses/by/4.0/", url),
                     url = ifelse(id == "CC-BY-NC-1.0", "https://creativecommons.org/licenses/by-nc/1.0/", url),
                     url = ifelse(id == "CC-BY-NC-2.0", "https://creativecommons.org/licenses/by-nc/2.0/", url),
                     url = ifelse(id == "CC-BY-NC-2.5", "https://creativecommons.org/licenses/by-nc/2.5/", url),
                     url = ifelse(id == "CC-BY-NC-3.0", "https://creativecommons.org/licenses/by-nc/3.0/", url),
                     url = ifelse(id == "CC-BY-NC-4.0", "https://creativecommons.org/licenses/by-nc/4.0/", url),
                     url = ifelse(id == "CC-BY-NC-ND-1.0", "https://creativecommons.org/licenses/by-nc-nd/1.0/", url),
                     url = ifelse(id == "CC-BY-NC-ND-2.0", "https://creativecommons.org/licenses/by-nc-nd/2.0/", url),
                     url = ifelse(id == "CC-BY-NC-ND-2.5", "https://creativecommons.org/licenses/by-nc-nd/2.5/", url),
                     url = ifelse(id == "CC-BY-NC-ND-3.0", "https://creativecommons.org/licenses/by-nc-nd/3.0/", url),
                     url = ifelse(id == "CC-BY-NC-ND-4.0", "https://creativecommons.org/licenses/by-nc-nd/4.0/", url),
                     url = ifelse(id == "CC-BY-NC-SA-1.0", "https://creativecommons.org/licenses/by-nc-sa/1.0/", url),
                     url = ifelse(id == "CC-BY-NC-SA-2.0", "https://creativecommons.org/licenses/by-nc-sa/2.0/", url),
                     url = ifelse(id == "CC-BY-NC-SA-2.5", "https://creativecommons.org/licenses/by-nc-sa/2.5/", url),
                     url = ifelse(id == "CC-BY-NC-SA-3.0", "https://creativecommons.org/licenses/by-nc-sa/3.0/", url),
                     url = ifelse(id == "CC-BY-NC-SA-4.0", "https://creativecommons.org/licenses/by-nc-sa/4.0/", url),
                     url = ifelse(id == "CC-BY-ND-1.0", "https://creativecommons.org/licenses/by-nd/1.0/", url),
                     url = ifelse(id == "CC-BY-ND-2.0", "https://creativecommons.org/licenses/by-nd/2.0/", url),
                     url = ifelse(id == "CC-BY-ND-2.5", "https://creativecommons.org/licenses/by-nd/2.5/", url),
                     url = ifelse(id == "CC-BY-ND-3.0", "https://creativecommons.org/licenses/by-nd/3.0/", url),
                     url = ifelse(id == "CC-BY-ND-4.0", "https://creativecommons.org/licenses/by-nd/4.0/", url),
                     url = ifelse(id == "CC-BY-SA-1.0", "https://creativecommons.org/licenses/by-sa/1.0/", url),
                     url = ifelse(id == "CC-BY-SA-2.0", "https://creativecommons.org/licenses/by-sa/2.0/", url),
                     url = ifelse(id == "CC-BY-SA-2.5", "https://creativecommons.org/licenses/by-sa/2.5/", url),
                     url = ifelse(id == "CC-BY-SA-3.0", "https://creativecommons.org/licenses/by-sa/3.0/", url),
                     url = ifelse(id == "CC-BY-SA-4.0", "https://creativecommons.org/licenses/by-sa/4.0/", url),
                     url = ifelse(id == "CC0-1.0", "https://creativecommons.org/publicdomain/zero/4.0/", url),
                     badge = NA_character_,
                     badge = ifelse(id == "CC-BY-4.0" | id == "CC-BY-3.0", "by.svgz", badge),
                     badge = ifelse(id == "CC-BY-NC-4.0" | id == "CC-BY-NC-3.0", "by-nc.svgz", badge),
                     badge = ifelse(id == "CC-BY-NC-ND-4.0" | id == "CC-BY-NC-ND-3.0", "by-nc-nd.svgz", badge),
                     badge = ifelse(id == "CC-BY-NC-SA-4.0" | id == "CC-BY-NC-SA.3.0", "by-nc-sa.svgz", badge),
                     badge = ifelse(id == "CC-BY-ND-4.0" | id == "CC-BY-ND-3.0", "by-nd.svgz", badge),
                     badge = ifelse(id == "CC-BY-SA-4.0" | id == "CC-BY-SA-3.0", "by-sa.svgz", badge),
                     badge = ifelse(id == "CC0-1.0", "cc-zero.svgz", badge)
    )
    which_cc <- grep("^Creative Commons", spdx$name)
    spdx$name[which_cc] <- gsub(" Non Commercial", "-NonCommercial", spdx$name[which_cc])
    spdx$name[which_cc] <- gsub(" Share Alike", "-ShareAlike", spdx$name[which_cc])
    spdx$name[which_cc] <- gsub(" No Deriv", "-NoDeriv", spdx$name[which_cc])

    spdx <- select(spdx, name, id, url, badge, deprecated) |> as.data.frame()
    rownames(spdx) <- spdx$id

}

save(meeple_xy, spdx, file="R/sysdata.rda", version=2)
