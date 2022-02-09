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
# CC0 licensed
# Update CC urls and add CC badge info
if (TRUE) {
    library("dplyr")
    library("jsonlite")
    library("rvest")

    spdx_file <-"raw-data/spdx_license_list.html"

    if (!file.exists(spdx_file))
        download.file("https://spdx.org/licenses/", spdx_file)

    spdx <- read_html(spdx_file) |> html_table() |> bind_rows()

    names(spdx) <- c("name", "id", "fsf", "osi", "deprecated")

    spdx_json_file <- "raw-data/spdx.json"
    if (!file.exists(spdx_json_file))
        download.file("https://raw.githubusercontent.com/sindresorhus/spdx-license-list/main/spdx.json",
                      spdx_json_file)

    spdx_list <- jsonlite::fromJSON(spdx_json_file)
    spdx_json <- spdx_list |> bind_rows() |> select(url_alt = url)
    spdx_json$id <- names(spdx_list)

    spdx_license_list <- left_join(spdx, spdx_json, by = "id")

    spdx_license_list <- mutate(spdx_license_list,
                     fsf = ifelse(fsf == "Y", TRUE, fsf),
                     fsf = ifelse(fsf == "", FALSE, fsf),
                     fsf = as.logical(fsf),
                     osi = ifelse(osi == "Y", TRUE, fsf),
                     osi = ifelse(osi == "", FALSE, osi),
                     osi = as.logical(osi),
                     deprecated = ifelse(is.na(deprecated), FALSE, TRUE),
                     url = paste0("https://spdx.org/licenses/", id, ".html"),
                     url_alt = ifelse(id == "CC-BY-1.0", "https://creativecommons.org/licenses/by/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-2.0", "https://creativecommons.org/licenses/by/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-2.5", "https://creativecommons.org/licenses/by/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-3.0", "https://creativecommons.org/licenses/by/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-4.0", "https://creativecommons.org/licenses/by/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-1.0", "https://creativecommons.org/licenses/by-nc/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-2.0", "https://creativecommons.org/licenses/by-nc/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-2.5", "https://creativecommons.org/licenses/by-nc/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-3.0", "https://creativecommons.org/licenses/by-nc/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-4.0", "https://creativecommons.org/licenses/by-nc/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-1.0", "https://creativecommons.org/licenses/by-nc-nd/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-2.0", "https://creativecommons.org/licenses/by-nc-nd/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-2.5", "https://creativecommons.org/licenses/by-nc-nd/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-3.0", "https://creativecommons.org/licenses/by-nc-nd/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-4.0", "https://creativecommons.org/licenses/by-nc-nd/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-1.0", "https://creativecommons.org/licenses/by-nc-sa/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-2.0", "https://creativecommons.org/licenses/by-nc-sa/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-2.5", "https://creativecommons.org/licenses/by-nc-sa/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-3.0", "https://creativecommons.org/licenses/by-nc-sa/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-4.0", "https://creativecommons.org/licenses/by-nc-sa/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-1.0", "https://creativecommons.org/licenses/by-nd/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-2.0", "https://creativecommons.org/licenses/by-nd/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-2.5", "https://creativecommons.org/licenses/by-nd/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-3.0", "https://creativecommons.org/licenses/by-nd/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-4.0", "https://creativecommons.org/licenses/by-nd/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-1.0", "https://creativecommons.org/licenses/by-sa/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-2.0", "https://creativecommons.org/licenses/by-sa/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-2.5", "https://creativecommons.org/licenses/by-sa/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-3.0", "https://creativecommons.org/licenses/by-sa/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-4.0", "https://creativecommons.org/licenses/by-sa/4.0/", url_alt),
                     url_alt = ifelse(id == "CC0-1.0", "https://creativecommons.org/publicdomain/zero/4.0/", url_alt),
                     badge = NA_character_
    )
    which_cc <- grep("^Creative Commons", spdx_license_list$name)
    spdx_license_list$name[which_cc] <- gsub(" Non Commercial", "-NonCommercial", spdx_license_list$name[which_cc])
    spdx_license_list$name[which_cc] <- gsub(" Share Alike", "-ShareAlike", spdx_license_list$name[which_cc])
    spdx_license_list$name[which_cc] <- gsub(" No Deriv", "-NoDeriv", spdx_license_list$name[which_cc])
    spdx_license_list <- as.data.frame(spdx_license_list)

    spdx_license_list <- select(spdx_license_list, id, name, url, fsf, osi, deprecated, badge, url_alt) |> as.data.frame()
    rownames(spdx_license_list) <- spdx_license_list$id

    # manually assign badge data
    spdx_license_list["CC-BY-1.0", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-2.0", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-2.5", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-2.5-AU", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-3.0", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-3.0-AT", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-3.0-DE", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-3.0-NL", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-3.0-US", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-4.0", "badge"] <- "by.svgz"
    spdx_license_list["CC-BY-NC-1.0", "badge"] <- "by-nc.svgz"
    spdx_license_list["CC-BY-NC-2.0", "badge"] <- "by-nc.svgz"
    spdx_license_list["CC-BY-NC-2.5", "badge"] <- "by-nc.svgz"
    spdx_license_list["CC-BY-NC-3.0", "badge"] <- "by-nc.svgz"
    spdx_license_list["CC-BY-NC-3.0-DE", "badge"] <- "by-nc.svgz"
    spdx_license_list["CC-BY-NC-4.0", "badge"] <- "by-nc.svgz"
    spdx_license_list["CC-BY-NC-ND-1.0", "badge"] <- "by-nc-nd.svgz"
    spdx_license_list["CC-BY-NC-ND-2.0", "badge"] <- "by-nc-nd.svgz"
    spdx_license_list["CC-BY-NC-ND-2.5", "badge"] <- "by-nc-nd.svgz"
    spdx_license_list["CC-BY-NC-ND-3.0", "badge"] <- "by-nc-nd.svgz"
    spdx_license_list["CC-BY-NC-ND-3.0-DE", "badge"] <- "by-nc-nd.svgz"
    spdx_license_list["CC-BY-NC-ND-3.0-IGO", "badge"] <- "by-nc-nd.svgz"
    spdx_license_list["CC-BY-NC-SA-4.0", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-1.0", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-2.0", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-2.0-FR", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-2.0-UK", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-2.5", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-3.0", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-3.0-DE", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-3.0-IGO", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-NC-SA-4.0", "badge"] <- "by-nc-sa.svgz"
    spdx_license_list["CC-BY-ND-1.0", "badge"] <- "by-nd.svgz"
    spdx_license_list["CC-BY-ND-2.0", "badge"] <- "by-nd.svgz"
    spdx_license_list["CC-BY-ND-2.5", "badge"] <- "by-nd.svgz"
    spdx_license_list["CC-BY-ND-3.0", "badge"] <- "by-nd.svgz"
    spdx_license_list["CC-BY-ND-3.0-DE", "badge"] <- "by-nd.svgz"
    spdx_license_list["CC-BY-ND-4.0", "badge"] <- "by-nd.svgz"
    spdx_license_list["CC-BY-SA-1.0", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-BY-SA-2.0", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-BY-SA-2.0-UK", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-BY-SA-2.1-JP", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-BY-SA-2.5", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-BY-SA-3.0", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-BY-SA-3.0-AT", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-BY-SA-3.0-DE", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-BY-SA-4.0", "badge"] <- "by-sa.svgz"
    spdx_license_list["CC-PDDC", "badge"] <- "publicdomain.svgz"
    spdx_license_list["CC0-1.0", "badge"] <- "cc-zero.svgz"
}

save(spdx_license_list, file="data/spdx_license_list.rda", version=2)
