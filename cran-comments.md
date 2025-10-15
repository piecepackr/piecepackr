* {piecepackr} was archived on 2025-10-15 since
  the 2025-10-09 submission that eliminated the **donttest** example
  ERROR was not processed by CRAN in six days.

* Further changes in this submission to try to reduce false positive NOTES in auto-check:

  + Fewer URL links to Wikipedia in the README
  + Skip running `piece()` and `piece_mesh()` examples on CRAN

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
