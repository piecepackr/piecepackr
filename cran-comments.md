**Test environments**

* local (linux, R 4.1.3) 
* win-builder (windows, R devel)
* Github Actions (linux, R devel)
* Github Actions (linux, R release)
* Github Actions (linux, R oldrel)
* Github Actions (windows, R release)

**R CMD check --as-cran results**

1 NOTE on 'oldrel' and 'release':

```
Undefined global functions or variables:
  emptyGrobCoords gridCoords gridGrobCoords
```

The `emptyGrobCoords()`, `gridCoords()`, and `gridGrobCoords` functions
are only available in the `{grid}` package bundled with R 4.2 and later.
This package checks the R version and if the R version is too old to support
these new functions it falls back to an alternative.
Hence this package should not depend on R (>= 4.2).
