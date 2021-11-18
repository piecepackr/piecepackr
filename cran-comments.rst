* Tweaks example so it should not cause an R CMD check ERROR
  on the 'M1mac' platform (example no longer needs Dejavu Sans font).

**Test environments**

* local (linux, R 4.1.1) 
* win-builder (windows, R devel)
* Github Actions (linux, R devel)
* Github Actions (linux, R release)
* Github Actions (windows, R release)
* Github Actions (osx, R release)

**R CMD check --as-cran results**

Status: OK
