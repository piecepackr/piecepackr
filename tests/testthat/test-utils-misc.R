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
