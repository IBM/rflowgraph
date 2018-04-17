
record_deref <- function(x, context=NULL) {
  name = substitute(x)
  stopifnot(is.name(name))
  cat("Deref", name, "at", paste(context,collapse=","), "\n")
  return(x)
}

record_call <- function(x, context=NULL) {
  expr = substitute(x)
  stopifnot(is.call(expr))
  name = rlang::call_name(expr)
  cat("Begin call", name, "at", paste(context,collapse=","), "\n")
  x
  cat("End call", name, "\n")
  return(x)
}