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
  state$graph()
}

record_in_ast <- function(expr, index=NULL) {
  if (is.atomic(expr)) {
    # Case 1: Literal values.
    rlang::call2("__literal__", index, expr)
  }
  else if (is.name(expr)) {
    # Case 2: Names.
    rlang::call2("__deref__", index, expr)
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
    }
    else if (name == "function") {
      # Special case 2: User-defined function.
      stop("Not implemented: recording user functions")
    }
    else {
      # Main case: ordinary function call.
      args = rlang::call_args(expr)
      new_args = map2(args, seq_along(args), record_in_ast)
      new_expr = rlang::call2(name, !!! new_args)
      return(rlang::call2("__call__", index, new_expr))
    }
  }
  else {
    stop("AST transform: don't know how to handle type ",
         typeof(expr), call.=FALSE)
  }
}
skip_calls <- c(
  "library",
  "loadNamespace",
  "require",
  "requireNamespace"
)

record_literal <- function(index, value, state) {
  #cat("Literal", value, "at", index, "\n")
  node = add_node(state, class(value), list(), list(value=value))
  graph_state = state$graph_state()
  graph_state$call_state()$push_arg(index, value, list(node,1L))
  value
}

record_deref <- function(index, value, state) {
  name = substitute(value)
  stopifnot(is.name(name))
  #cat("Deref", name, "at", index, "\n")
  
  graph_state = state$graph_state()
  origin = get_default(graph_state$output_table, name, list(NULL,NULL))
  graph_state$call_state()$push_arg(index, value, origin)
  value
}

record_call <- function(index, value, state) {
  call = substitute(value)
  stopifnot(is.call(call))
  name = rlang::call_name(call)

  graph_state = state$graph_state()
  call_state = graph_state$push_call()
  cat("Begin call", name, "at", index, "\n")
  value
  cat("End call", name, "\n")
  graph_state$pop_call()
  
  return(value)
}

# Add node with name unique across all flow graphs.
add_node.record_state = function(state, name, ...) {
  i = get_default(state$node_names, name, 0L) + 1L
  state$node_names[[name]] = i
  node = paste(name, i, sep=":")
  add_node(state$graph(), node, ...)
  node
}

# Data structures

record_state = R6Class("record_state",
  public = list(
    annotator = NULL,
    node_names = NULL,
    initialize = function(db=NULL) {
      self$annotator = annotator$new(db)
      self$node_names = dict()
      private$stack = stack$new()
      self$push_graph()
    },
    push_graph = function() private$stack$push(graph_state$new()),
    pop_graph = function() private$stack$pop(),
    graph_state = function() private$stack$peek(),
    graph = function() self$graph_state()$graph
  ),
  private = list(
    stack = NULL
  )
)

graph_state = R6Class("graph_state",
  public = list(
    graph = NULL,
    output_table = NULL,
    initialize = function() {
      self$graph = wiring_diagram()
      self$output_table = dict()
      private$stack = stack$new()
    },
    push_call = function() private$stack$push(call_state$new()),
    pop_call = function() private$stack$pop(),
    call_state = function() private$stack$peek()
  ),
  private = list(
    stack = NULL
  )
)

call_state = R6Class("call_state",
  public = list(
    initialize = function() {
      private$queue = dequer::queue()
    },
    args = function() as.list(private$queue),
    push_arg = function(index, value, src) {
      if (is.null(index)) return()
      dequer::pushback(private$queue, list(index=index, value=value, src=src))
    }
  ),
  private = list(
    queue = NULL
  )
)

# XXX: Wrap dequer::stack to work around lack of peeking.
stack = R6Class("stack",
  public = list(
    initialize = function() {
      private$stack = dequer::stack()
    },
    push = function(x) {
      dequer::push(private$stack, x)
      private$current = x
    },
    peek = function() private$current,
    pop = function() {
      popped = dequer::pop(private$stack)
      private$current = if (is_empty(private$stack)) {
        NULL
      } else {
        x = dequer::pop(private$stack)
        dequer::push(private$stack, x)
        x
      }
      popped
    }
  ),
  private = list(
    current = NULL,
    stack = NULL
  )
)