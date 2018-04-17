
#' Add recording to R program by transforming the AST.
record_in_ast <- function(expr, context=NULL) {
  if (is.atomic(expr)) {
    # Case 1: Literal values.
    # Ideally we should record these as nullary functions but for now we won't,
    # for agreement with Python flow graphs.
    return(expr)
  }
  else if (is.name(expr)) {
    # Case 2: Names.
    rlang::call2("record_deref", expr, context)
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
      new_args = mapply(
        function(arg, i, name) {
          record_in_ast(arg, list(i,name))
        },
        args, seq_along(args), names2(args), SIMPLIFY=FALSE, USE.NAMES=TRUE)
      new_expr = rlang::call2(name, !!! new_args)
      return(rlang::call2("record_call", new_expr, context))
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