# Make `dev` the active device again, unless it is the null device (1),
# no longer open, or already active.
# NB. `dev.set()` calls the device's (de)activate callbacks even if `dev` is
# already current, and `dev.set(1L)` would open a new device.
restore_dev <- function(dev) {
	if (dev > 1L && dev %in% grDevices::dev.list() && grDevices::dev.cur() != dev) {
		grDevices::dev.set(dev)
	}
	invisible(NULL)
}

# Close the temporary device `temp` and return to `old` (the result of
# `dev.cur()` before `temp` was opened).  `old` is selected *first* so that
# closing `temp` doesn't make some other device current in between.
# Safe to call more than once.
close_temp_dev <- function(temp, old) {
	force(temp) # e.g. `close_temp_dev(dev.cur(), old)` must not see `old`
	restore_dev(old)
	if (temp > 1L && temp != old && temp %in% grDevices::dev.list()) {
		grDevices::dev.off(temp)
	}
	invisible(NULL)
}

# Open a device with `open()`, evaluate `code` on it, then close it and
# return to the previously active device (also on error).
# Returns the value of `code`.
with_temp_dev <- function(open, code) {
	old <- grDevices::dev.cur()
	# If `open()` opens a device and then errors, close that device and
	# return to `old`.  A calling handler doesn't catch the error, so it
	# reaches the caller unchanged.
	withCallingHandlers(open(), error = function(e) close_temp_dev(grDevices::dev.cur(), old))
	temp <- grDevices::dev.cur()
	if (temp == old) {
		stop("`open()` did not open a new graphics device")
	}
	on.exit(close_temp_dev(temp, old), add = TRUE)
	code
}
