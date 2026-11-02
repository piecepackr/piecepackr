# Open two null `pdf()` devices, make the first one current,
# and close them both when `env` exits.
local_null_pdfs <- function(env = parent.frame()) {
	grDevices::pdf(NULL)
	dev1 <- grDevices::dev.cur()
	grDevices::pdf(NULL)
	dev2 <- grDevices::dev.cur()
	devs <- unname(c(dev1, dev2))
	withr::defer(
		for (d in devs) {
			if (d %in% grDevices::dev.list()) grDevices::dev.off(d)
		},
		envir = env
	)
	grDevices::dev.set(devs[1L])
	devs
}

# The first of `devs` is current and exactly `devs` are open.
expect_devs <- function(devs) {
	expect_equal(unname(grDevices::dev.cur()), devs[1L])
	expect_equal(unname(grDevices::dev.list()), devs)
}
