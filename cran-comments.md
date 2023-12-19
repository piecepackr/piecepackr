* We fix an Rd formatting error in `game_systems.Rd`
  that was triggering a CRAN check NOTE
* Bumps version of `{rayvertex}` in the `DESCRIPTION` to v0.10.4 to avoid a bug
  introduced in v0.10.3 that briefly caused a CRAN check ERROR
  in the `\donttest` "Additional issues"

**Test environments**

* local (linux, R 4.3.2)
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
