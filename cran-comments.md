We make the following tweaks to `\donttest{}` examples requested by Dr. Ligges:

* Each `\donttest{}` example now has the comment "May take more than 5 seconds on CRAN servers"

  I thought longer-running examples that work were SUPPOSED to be wrapped in
 `\donttest{}` instead of `\dontrun{}`.
  Instead of `\donttest{}` should I instead do something like:

    if (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")){}

  So these "Examples with CPU (user + system) or elapsed time > 5s" NOTES 
  don't arise when CRAN checks `\donttest{}`?

* The `\donttest{}` examples using `{rayrender}` (i.e. "piece") and
  `{rayvertex}` (i.e. "piece_mesh") packages now have:

     opt <- options(cores = getOption("Ncpus"))

  and:

     options(opt)

  According to the documentation this should limit the number of cores used by
  `{rayrender}` / `{rayvertex}` to what CRAN is happy with and then restore
  the options to how they were before the example was run.

* Several `\donttest{}` examples modified so they shouldn't throw:

      Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x,
x$  y,  :
     conversion failure on '♥' in 'mbcsToSbcs': dot substituted

  style WARNINGS on certain CRAN servers either by switching to a different example
  or by wrapping in an `if()` statement so it only runs if the active graphics
  device should be expected to support Unicode.

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
