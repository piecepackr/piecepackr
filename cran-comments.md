The `donttest` "Additional issue" example ERROR
did not replicate locally or
in R-hub's `donttest` docker container.

However we now wrap the `\donttest` `piece_mesh()` examples
with `try()` so they should theoretically not throw an
ERROR on that CRAN machine.

**Test environments**

* local (linux, R 4.5.1)
* win-builder (windows, R devel)
* mac-builder (osx, R release)
* Github Actions (linux, R devel)
* Github Actions (linux, R release)
* Github Actions (linux, R oldrel)
* Github Actions (windows, R release)

**R CMD check --as-cran results**

OK
