# cache which mimics API of cachem: https://cachem.r-lib.org/
# a variation using `fastmap::fastmap()` was not faster in testing
Cache <- R6Class("pp_cache",
                 public = list(
                               exists = function(key) hasName(private$cache, key),
                               get = function(key, default = key_missing())
                                   if (self$exists(key)) private$cache[[key]] else default,
                               keys = function() names(private$cache),
                               remove = function(key) private$cache[[key]] <- NULL,
                               reset = function() for (key in self$keys()) self$remove(key),
                               set = function(key, value) private$cache[[key]] <- value,
                               size = function() length(private$cache)
                               ),
                 private = list(cache = list()))

key_missing <- function() structure(list(), class = "key_missing")
is.key_missing <- function(x) inherits(x, "key_missing")
