pdf_file <- tempfile(fileext = ".pdf")
svg_file <- tempfile(fileext = ".svg")

old_dev <- dev.cur()
pdf(pdf_file)
expect_false(device_supports_unicode())
dev.off()
if (old_dev > 1L) {
	dev.set(old_dev)
}
unlink(pdf_file)

skip_if(!capabilities("cairo"))
old_dev <- dev.cur()
svg(svg_file)
expect_true(device_supports_unicode())
dev.off()
if (old_dev > 1L) {
	dev.set(old_dev)
}
unlink(svg_file)

test_that("`cleave()` keeps empty elements", {
	expect_equal(cleave("a,b,"), c("a", "b", ""))
	expect_equal(cleave(""), "")
	expect_equal(cleave(","), c("", ""))
	expect_equal(cleave(NA_character_), NA_character_)
	expect_equal(col_cleave("red,"), c("red", "transparent"))
	expect_equal(numeric_cleave(135), 135)
	expect_equal(numeric_cleave(c(1, 2.5)), c(1, 2.5))
	expect_equal(cleave("a.b", sep = "\\."), c("a", "b"))
})
