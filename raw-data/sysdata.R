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

save(meeple_xy, file="R/sysdata.rda", version=2)

