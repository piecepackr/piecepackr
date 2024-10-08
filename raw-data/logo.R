library("grid")
library("piecepackr")
library("tibble")
pp <- game_systems("dejavu")$piecepack
ppi <- game_systems("dejavu")$piecepack_inverted

cfg <- as.list(pp)
# cfgi <- as.list(ppi)

# adjust colors
cfg$invert_colors = TRUE
cfg$invert_colors.tile_face = FALSE
cfg$edge_color = ppi$get_suit_color(2)
cfg$invert_colors.die_face = TRUE
cfg$suit_color.s2 = ppi$get_suit_color(2)
cfg$suit_color.s3 = pp$get_suit_color(5)

# adjust symbol sizes
cfg$suit_cex.tile_face = 1.8
cfg$suit_cex.die_face = 1.5
cfg$suit_cex.pawn_face = 1.08
cfg$suit_cex.coin_back = 1.3
cfg$dm_cex.coin_back = 0.8

# adjust shapes/sizes
cfg$shape.pawn_face = "meeple"
cfg$width.pawn_face = 0.70
cfg$height.pawn_face = 0.70
cfg$width.die_face = 0.53
cfg$dm_text.pawn_face = ""
cfg$dm_text.die_face = ""

# adjust location of suits
cfg$dm_t.tile_face = 132
cfg$dm_r.tile_face = 0.33
cfg$ps_t.r2.die_face = 90
cfg$ps_r.r2.die_face = 0.1
cfg$die_arrangement = "6,5,4,2,3,1"
cfg$ps_t.coin_back = 90
cfg$ps_r.coin_back = -0.1
cfg$ps_t.pawn_face = 90
cfg$ps_r.pawn_face = 0.03
cfg <- pp_cfg(cfg)

w <- 4.5

fill <- pp$get_suit_color(3)
draw_logo <- function(border = TRUE, cut = FALSE) {
    # make hex
    hex <- pp_shape("convex6")
    grid.draw(hex$shape(gp = gpar(fill=fill, col="transparent")))
    if (border)
        grid.draw(hex$mat(mat_width=0.015, gp = gpar(fill="black")))
    if (cut) {
        pushViewport(viewport(width=unit(8/9, "npc"), height=unit(8/9, "npc")))
        grid.draw(hex$shape(gp = gpar(fill="transparent", col="white")))
        popViewport()
        pushViewport(viewport(width=unit(5/6, "npc"), height=unit(5/6, "npc")))
        grid.draw(hex$shape(gp = gpar(fill="transparent", col="white")))
        popViewport()
    }

    pushViewport(viewport(width=unit(w, "in"), height=unit(w, "in")))
    # logo with pieces
    x0 <- 0.48 * w
    y0 <- 0.59 * w
    df <- tibble(piece_side = c("tile_face", "die_face", "pawn_face", "coin_back"),
                 x = c(x0, x0 + 0.45, x0 + 0.5, x0 - 0.5),
                 y = c(y0, y0 + 0.45, y0 - 0.5, y0 - 0.5),
                 suit = 1:4, rank = c(1, 2, NA, 4))
    pmap_piece(df, default.units="in", trans=op_transform, op_scale = 0.5, cfg=cfg)

    # name
    grid.text("piecepackr", x = 0.51 * w, y = 0.32 * w, default.units = "in",
              gp = gpar(fontsize = 42, fontfamily = "Dejavu Sans",
                        col = "white", fontface = 2))
    popViewport()
}
svg("man/figures/logo.svg", width = w, height = w, bg = "transparent")
draw_logo(border=TRUE)
dev.off()

png("man/figures/logo.png", width = w, height = w, units = "in", res = 72, bg = "transparent")
draw_logo(border=TRUE)
dev.off()

# https://www.stickermule.com/support/full-bleed
png("raw-data/sticker_with_cutline.png", width = 5.125, height = 5.125, units = "in", res = 150, bg = fill)
draw_logo(border=FALSE, cut = TRUE)
dev.off()

# https://www.stickermule.com/support/full-bleed
png("raw-data/sticker.png", width = 5.125, height = 5.125, units = "in", res = 150, bg = fill)
draw_logo(border=FALSE, cut = FALSE)
dev.off()
