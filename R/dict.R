#' Dictionary
#' 
#' @description A dictionary (hash map) data structure.
#' The keys must be strings and the values can be any R object.
#' 
#' @details This data type is a thin wrapper around a standard R environment.
#' It exists because of the packages currently on CRAN, \code{hashmap} does
#' not allow arbitrary value types and \code{hash} is GPL-ed.
dict <- function(...) {
  structure(list2env(list(...), hash=TRUE, parent=emptyenv()),
            class="dict")
}

`[[.dict` <- function(d, k) {
  if (!has_key(d, k))
    stop(paste("missing key:", k))
  get(k, envir=d)
}

#' @rdname dict
keys <- function(d) UseMethod("keys")
keys.dict <- function(d) names(d)
keys.list <- function(l) names(l)

#' @rdname dict
values <- function(d) UseMethod("values")
values.dict <- function(d) lapply(names(d), function(k) d[[k]])
values.list <- function(l) unname(l)

#' @rdname dict
has_key <- function(d, k) UseMethod("has_key")
has_key.dict <- function(d, k) exists(k, envir=d)
has_key.list <- function(l, k) k %in% names(l)

#' @rdname dict
del <- function(d, k) UseMethod("del")
del.dict <- function(d, k) remove(list=k, envir=d)
del.list <- function(l, k) stop("lists are immutable")

#' @rdname dict
get_default <- function(d, k, default=NULL) UseMethod("get_default")
get_default.default <- function(d, k, default=NULL) {
  if (has_key(d, k)) d[[k]] else default
}

#' @rdname dict
set_default <- function(d, k, default=NULL) UseMethod("set_default")
set_default.default <- function(d, k, default=NULL) {
  if (has_key(d, k)) d[[k]] else d[[k]] <- default
}
set_default.list <- function(d, k, default=NULL) stop("lists are immutable")

print.dict <- function(d, ...) {
  cat(class(d)[[1]], "with", length(d), "keys")
}