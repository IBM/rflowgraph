#' Record as flow graph
#' 
#' @description Record the evaluation of an R expression as a flow graph.
#' 
#' @export
record <- function(expr, env=rlang::caller_env(), db=NULL) {
  expr = substitute(expr)
  state = record_state$new(db=db)
  env$`__literal__` = function(...) record_literal(..., state)
  env$`__deref__` = function(...) record_deref(..., state)
  env$`__call__` = function(...) record_call(..., state)
  tryCatch({
    eval(record_in_ast(expr), envir=env)
  }, finally={
    rm(`__literal__`, `__deref__`, `__call__`, envir=env)
  })
  state$graph
}

record_in_ast <- function(expr, context=NULL) {
  if (is.atomic(expr)) {
    # Case 1: Literal values.
    rlang::call2("__literal__", expr, context)
  }
  else if (is.name(expr)) {
    # Case 2: Names.
    rlang::call2("__deref__", expr, context)
  }
  else if (is.pairlist(expr)) {
    # Case 3: Pair lists.
    # Used only for formal arguments of functions. Ignore.
    return(expr)
  }
  else if (is.call(expr)) {
    # Case 4: Function calls.
    name = rlang::call_name(expr)
    if (name %in% skip_calls) {
      # Special case 1: Ignore certain calls, like `library`.
      return(expr)
    } else {
      # Main case: ordinary function call.
      args = rlang::call_args(expr)
      new_args = map2(args, seq_along(args), record_in_ast)
      new_expr = rlang::call2(name, !!! new_args)
      return(rlang::call2("__call__", new_expr, context))
    }
  }
  else {
    stop("AST transform: don't know how to handle type ",
         typeof(expr), call.=FALSE)
  }
}
skip_calls <- c(
  "function",
  "library",
  "loadNamespace",
  "require",
  "requireNamespace"
)

record_literal <- function(x, context, state) {
  cat("Literal", x, "at", context, "\n")
  return(x)
}

record_deref <- function(x, context, state) {
  name = substitute(x)
  stopifnot(is.name(name))
  cat("Deref", name, "at", context, "\n")
  return(x)
}

record_call <- function(x, context, state) {
  expr = substitute(x)
  stopifnot(is.call(expr))
  name = rlang::call_name(expr)
  cat("Begin call", name, "at", context, "\n")
  x
  cat("End call", name, "\n")
  return(x)
}

record_state = R6Class("record_state",
  public = list(
    annotator = NULL,
    graph = NULL,
    eval_stack = NULL,
    initialize = function(db=NULL) {
      self$annotator = annotator$new(db)
      self$graph = wiring_diagram()
      self$eval_stack = dequer::stack()
    }
  )
)