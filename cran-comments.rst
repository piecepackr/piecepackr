* ``vdiffr`` now used conditionally as requested by Prof. Ripley
* To fix CRAN check error for ``r-oldrel`` we now use ``backports`` to supply ``suppressWarnings()``
* To fix CRAN check warning for ``r-oldrel-windows`` we now skip on CRAN
  any tests that assume the presence of the Dejavu Sans font

**Test environments**

* local (linux, R 4.0.3) 
* win-builder (windows, R devel)
* win-builder (windows, R oldrel)
* appveyor (windows, R devel) 
* appveyor (windows, R release) 
* travis-ci (OSX, R release) 
* travis-ci (linux, R devel) 
* travis-ci (linux, R release) 

**R CMD check --as-cran results**

Status: OK
