# `with_temp_dev()` closes its device and restores the old one

    Code
      with_temp_dev(function() grDevices::pdf(NULL), stop("boom"))
    Condition
      Error in `with_temp_dev()`:
      ! boom

---

    Code
      with_temp_dev(function() NULL, 42L)
    Condition
      Error in `with_temp_dev()`:
      ! `open()` did not open a new graphics device

