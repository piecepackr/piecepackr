expect_equal(to_x(90, 1), 0)
expect_equal(to_y(90, 1), 1)
expect_equal(to_t(0, 1), 90)
expect_equal(to_r(0, 1), 1)

f <- function(x) assert_suggested("imaginaryPackage")
expect_error(f(), "You need to install the suggested package")

skip_if(Sys.which("pdffonts") == "", "Doesn't have pdffont binary")
df <- get_embedded_font("sans", c("♥", "♠", "♣", "♦", "🌞", "🌜", "꩜"))
skip_if(!capabilities("cairo"))
expect_false(has_font("thisshouldnotexist"))
