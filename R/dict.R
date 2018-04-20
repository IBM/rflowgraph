# Copyright 2018 IBM Corp.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Dictionary
#' 
#' @description A dictionary (hash map) data structure.
#' The keys must be strings and the values can be any R object.
#' 
#' @details The \code{dict} data type is a thin wrapper around an R environment.
#' Unlike most R types, it is mutable and has pass-by-reference semantics.
#' 
#' The \code{dict} interface is also implemented for lists, which can serve as
#' immutable dictionaries (with linear time lookup).
dict <- function(...) {
  as_dict(list(...))
}

#' @rdname dict
as_dict <- function(x) UseMethod("as_dict")
as_dict.dict <- identity
as_dict.list <- function(x) {
  structure(list2env(x, hash=TRUE, parent=emptyenv()),
            class="dict")
}

`[[.dict` <- function(d, k) {
  if (!has_key(d, k))
    stop("missing key: ", k)
  get(k, envir=d)
}

#' @rdname dict
keys <- function(d) UseMethod("keys")
keys.dict <- function(d) names(d)
keys.list <- function(l) names(l)

#' @rdname dict
values <- function(d) UseMethod("values")
values.dict <- function(d) map(names(d), function(k) d[[k]])
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
