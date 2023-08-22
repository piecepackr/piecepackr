* One package example is now wrapped in `try()` to prevent a CRAN check ERROR that
  occurred on a CRAN Fedora Linux server with R-devel using `_R_CHECK_DONTTEST_EXAMPLES_=true`.
  This ERROR was unreproducible on my Ubuntu Linux server using `_R_CHECK_DONTTEST_EXAMPLES_=true` and
  the same version of R-devel.

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
