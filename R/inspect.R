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

#' Get package of function
#' 
#' @description In which package is the function defined?
#' 
#' @details This function reliably determines the definining package of a
#' function object, including for primitive functions, anonymous functions,
#' and methods.
#' 
#' @seealso \code{\link{obj_package}}
fun_package <- function(fun) {
  stopifnot(is.function(fun))
  if (is.primitive(fun))
    # Special case: primitive functions do not belong to an environment.
    return("base")
  
  env = environment(fun)
  if (is(fun, "refMethodDef"))
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