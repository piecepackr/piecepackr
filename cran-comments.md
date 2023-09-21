* Wraps `get_embedded_font()` example with `try()` and skips test on CRAN.

  - This prevents R CMD check ERRORS on CRAN machines with buggy versions of `cairo` installed.
  - In particular `cairo` versions 1.17.4 to 1.17.8 may be buggy.
    We'll now also raise a warning if a user tries to use `get_embedded_font()` with these `cairo` versions.
    These warnings have class `"piecepackr_buggy_cairo"` and can also be suppressed by setting
    `options(piecepackr.check.cairo = FALSE)`.

**Test environments**

* local (linux, R 4.3.1)
* win-builder (windows, R devel)
* mac-builder (osx, R release)
* Github Actions (linux, R devel)
* Github Actions (linux, R release)
* Github Actions (linux, R oldrel)
* Github Actions (windows, R release)

**R CMD check --as-cran results**

1 NOTE generated on a subset of platforms:


```
found 3 marked UTF-8 strings 
```

The `spdx_license_list` data set contains details about 478 open source license
from the "SPDX License List" <https://spdx.org/licenses/>.
The "name" column of this data set includes the official name of the licenses.
Three of these official license names use non-ASCII characters.
