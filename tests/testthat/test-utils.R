expect_equal(to_x(90, 1), 0)
expect_equal(to_y(90, 1), 1)
expect_equal(to_t(0, 1), 90)
expect_equal(to_r(0, 1), 1)

f <- function(x) assert_suggested("imaginaryPackage")
expect_error(f(), "You need to install the suggested package")

expect_true(grepl(simplify_font("Dotaro Ranks"), simplify_font("dotaro-ranks.ttf")))

skip_on_cran()
skip_if_not_installed("pdftools")
skip_if(!capabilities("cairo"))
skip_if(is_cairo_maybe_buggy())

df <- get_embedded_font("sans", c("♥", "♠", "♣", "♦", "🌞", "🌜", "꩜"))
expect_false(has_font("thisshouldnotexist"))
