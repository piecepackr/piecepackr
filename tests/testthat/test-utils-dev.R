test_that("`with_temp_dev()` closes its device and restores the old one", {
	skip_if_not_installed("withr")
	devs <- local_null_pdfs()
	expect_equal(with_temp_dev(function() grDevices::pdf(NULL), 42L), 42L)
	expect_devs(devs)

	expect_s3_class(
		try(with_temp_dev(function() grDevices::pdf(NULL), stop("boom")), silent = TRUE),
		"try-error"
	)
	expect_devs(devs)

	expect_s3_class(try(with_temp_dev(function() NULL, 42L), silent = TRUE), "try-error")
	expect_devs(devs)

	opens_then_errors <- function() {
		grDevices::pdf(NULL)
		stop("boom")
	}
	expect_s3_class(try(with_temp_dev(opens_then_errors, 42L), silent = TRUE), "try-error")
	expect_devs(devs)

	# NB. `expect_snapshot()` itself changes the current device
	expect_snapshot(with_temp_dev(function() grDevices::pdf(NULL), stop("boom")), error = TRUE)
	expect_snapshot(with_temp_dev(function() NULL, 42L), error = TRUE)
})

test_that("`with_temp_dev()` leaves no device open if none was open before", {
	skip_if(grDevices::dev.cur() > 1L)
	with_temp_dev(function() grDevices::pdf(NULL), NULL)
	expect_null(grDevices::dev.list())
})

test_that("`close_temp_dev()` is safe to call twice", {
	skip_if_not_installed("withr")
	devs <- local_null_pdfs()
	grDevices::pdf(NULL)
	temp <- grDevices::dev.cur()
	close_temp_dev(temp, devs[1L])
	close_temp_dev(temp, devs[1L])
	expect_devs(devs)
})
