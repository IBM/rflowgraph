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

#' Class system
#' 
#' @description Which class system does the object or function use?
#' 
#' @return Returns one of "S3", "S4", "R5", "R6", or, in the case of functions,
#' possibly \code{NULL}.
class_system <- function(x) {
  if (is.function(x)) {
    if (is.primitive(x))
      NULL
    else if (is.R6(environment(x)$self))
      "R6"
    else if (is(x, "refMethodDef"))
      "R5"
    else if (is(x, "standardGeneric") || is(x, "nonstandardGenericFunction"))
      "S4"
    else if (isS3stdGeneric(x))
      # XXX: Doesn't handle S3 methods. However, isS3method() operates on
      # function names, not functions, and is in any case terribly hackish.
      "S3"
    else
      NULL
  } else {
    if (is.R6(x))
      "R6"
    else if (!isS4(x))
      "S3"
    else if (is(x, "refClass"))
      "R5"
    else
      "S4"
  }
}

#' Get arguments of function
#' 
#' @description Get the formal arguments of a function (primitive or closure).
#' 
#' @return A named list.
fun_args <- function(f) {
  # Use args() for primitive functions: https://stackoverflow.com/q/25978301
  stopifnot(is.function(f))
  as.list(formals(if (is.primitive(f)) args(f) else f))
}

#' Get package of function
#' 
#' @description In which package is the function defined?
#' 
#' @details This function reliably determines the definining package of a
#' function object, including for primitive functions, anonymous functions,
#' and methods.
fun_package <- function(f) {
  stopifnot(is.function(f))
  if (is.primitive(f))
    # Special case: primitive functions do not belong to an environment.
    return("base")
  
  env = environment(f)
  if (is(f, "refMethodDef"))
    # Special case: class method of R5 class.
    return(attr(env$def, "package"))
  
  else if (is.R6Class(env))
    # Special case: class method of R6 class.
    env = env$parent_env

  # General case: find the nearest named environment.
  name = ""
  while (name == "") {
    name = environmentName(env)
    env = parent.env(env)
  }
  name
}

#' Match arguments of function call
#' 
#' @description A reimplementation of R's algorithm for argument matching,
#' including partial matching.
fun_args_match <- function(fun_args, call_args) {
  stopifnot(is.character(fun_args))
  call_args_names = names2(call_args)
  
  # Assign named call arguments using R's partial matching convention.
  named = call_args_names != ""
  i = pmatch(call_args_names[named], fun_args)
  if (any(is.na(i)))
      stop("Argument match failed: ",
           "non-matching, multiply partially matching, or duplicate names")
  fun_args_named = fun_args[i]
  fun_args = if (is_empty(i)) fun_args else fun_args[-i]
  
  # Assign unnamed call arguments, taking ellipsis into account.
  fun_args = dplyr::recode(fun_args, `...`="")
  n_unnamed = sum(!named)
  i = seq2(1, n_unnamed)
  ell = match("", fun_args)
  if (!is.na(ell))
    # An ellipsis absorbs all following unnamed call arguments.
    i[seq2(ell, n_unnamed)] = ell
  fun_args_unnamed = fun_args[i]
  
  matched = c(call_args[!named], call_args[named])
  names = c(fun_args_unnamed, fun_args_named)
  if (all(names == "")) unname(matched) else set_names(matched, names)
}

#' @rdname fun_args_match
call_args_match <- function(call, env=rlang::caller_env()) {
  fun_args_match(names(fun_args(rlang::call_fn(call, env=env))),
                 rlang::call_args(call))
}

#' Inspect call
#' 
#' @description A convenience function for inspecting calls, which supports
#' functions (closures and primitives) and methods (S3, S4, R5, R6).
#' 
#' @return A named list with items possibly including:
#' \itemize{
#' \item \code{name}: Name of function
#' \item \code{package}: Package where function is defined
#' \item \code{system}: Class system used by function, if it's a method
#' }
#' 
#' @seealso \code{\link{inspect_obj}}
inspect_call <- function(call, env=rlang::caller_env(), fun=NULL) {
  stopifnot(is.call(call))
  if (is.null(fun))
    fun = rlang::call_fn(call, env)
  
  head = call[[1]]
  rlang::switch_lang(call, named = {
    pkg = fun_package(fun)
    name = as.character(head)
  }, namespaced = {
    pkg = as.character(head[[2]]) # == fun_package(fun)
    name = as.character(head[[3]])
  }, recursive = {
    stop("Not implemented: method calls")
  })
  system = class_system(fun)
  compact(list(name=name, package=pkg, system=system))
}

#' Inspect object
#' 
#' @description A convenience function for inspecting function objects.
#' 
#' @return A named list with items including:
#' \itemize{
#' \item \code{class}: Classes of object.
#' \item \code{system}: Class system used by object.
#' }
#' 
#' @seealso \code{\link{inspect_call}}
inspect_obj <- function(x) {
  cls = class(x)
  attributes(cls) = NULL
  compact(list(class=cls, system=class_system(x)))
}