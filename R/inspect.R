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

#' Class system of object
#' 
#' @description Which class system does the object use?
#' @return Returns one of "S3", "S4", "R5", "R6".
class_system <- function(x) {
  if (is.R6(x))
    "R6"
  else if (isS4(x))
    tryCatch({ x$getRefClass; "R5"}, error=function(e) "S4")
  else
    "S3"
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
#' 
#' @seealso \code{\link{obj_package}}
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

#' Get package of object
#' 
#' @description In which package is the object's class defined?
#' 
#' @details This function tries to determine the defining package of the
#' object's class. In general, this is not possible because R has an informal
#' class system.
#' 
#' Currently, this function works reliably on S4 and R5 instances, but not on
#' S3 or R6 instances. When it fails, it returns \code{NULL}.
#' 
#' @seealso \code{\link{fun_package}}
obj_package <- function(x) {
  # Obviously not possible for S3 classes, which are completely informal.
  # In general, not possible for R6 classes either:
  # https://github.com/r-lib/R6/issues/144
  attr(class(x), "package")
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
  
  # Remove named call arguments, then assign remaining call arguments by order.
  matched = c(call_args[!named], call_args[named])
  matched_names = if (is_empty(i))
    fun_args[seq_along(call_args)]
  else
    c(fun_args[-i][seq_along(call_args[!named])], fun_args[i])
  set_names(matched, matched_names)
}

#' @rdname fun_args_match
call_args_match <- function(call, env=rlang::caller_env()) {
  fun_args_match(names(fun_args(rlang::call_fn(call, env=env))),
                 rlang::call_args(call))
}
