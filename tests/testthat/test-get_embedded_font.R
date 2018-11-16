context("get embedded font work as expected")
skip_if(Sys.which("pdffont") == "", "Doesn't have pdffont binary")
df <- get_embedded_font("sans", c("♥","♠","♣","♦","🌞","🌜","꩜"))
