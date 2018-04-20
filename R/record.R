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

#' Record as flow graph
#' 
#' @description Record the evaluation of an R expression as a flow graph.
#' 
#' @export
record <- function(expr, env=rlang::caller_env(), db=NULL) {
  expr = substitute(expr)
  state = record_state$new(env=env, db=db)
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
    expr
  }
  else if (is.call(expr)) {
    # Case 4: Function calls.
    name = rlang::call_name(expr)
    if (name %in% SKIP_FUNS) {
      # Special case: Completely ignore certain calls, like `library`.
      expr
    } else {
      # Recursively transform call arguments.
      args = rlang::call_args(expr)
      if (name %in% ASSIGN_FUNS) {
        # Special case: Variable assignment.
        stopifnot(length(args) == 2)
        new_expr = rlang::call2(name, args[[1]], record_in_ast(args[[2]], 1L))
      } else {
        # Main case: Recorded function call.
        new_args = map2(args, seq_along(args), record_in_ast)
        new_expr = rlang::call2(name, !!! new_args)
      }
      rlang::call2("__call__", index, new_expr)
    }
  }
  else {
    stop("AST transform: don't know how to handle type ",
         typeof(expr), call.=FALSE)
  }
}

record_literal <- function(index, value, state) {
  # Create nullary node for literal.
  node = add_node(state, class(value), list(), c("__return__"))
  
  # Attach value to node.
  graph_state = state$graph_state()
  graph_state$observe(index, list(node,"__return__"), value)
  
  return(value)
}

record_deref <- function(index, value, state) {
  name = substitute(value)
  stopifnot(is.name(name))
  name = as.character(name)
  
  # Dereference the name.
  value
  
  # Attach value to node in output table, if any.
  graph_state = state$graph_state()
  source = get_default(graph_state$output_table, name, list(NULL,NULL))
  graph_state$observe(index, source, value)
  
  return(value)
}

record_call <- function(index, value, state) {
  call = substitute(value)
  stopifnot(is.call(call))
  name = rlang::call_name(call)
  if (is.null(name))
    stop("Not implemented: recording methods")
  else if (name == "function")
    stop("Not implemented: recording user functions")

  # Evaluate function call.
  graph_state = state$graph_state()
  graph_state$push_call(call)
  value # Evaluate!
  call_state = graph_state$pop_call()
  observed = call_state$observed_args()
  
  # Create or retrieve node for call.
  if (name == "(" || name == "{") {
    # Special case: Don't create nodes for parentheses and braces,
    # which are represented as calls in R.
    # FIXME: This won't handle control flow changes through return().
    c(node, out_port) %<-% dplyr::last(observed)$source
  }
  else if (name %in% ASSIGN_FUNS) {
    # Special case: Don't create nodes for assignments, but do update the
    # output table.
    c(node, out_port) %<-% observed[[1]]$source
    varname = as.character(call[[2]])
    graph_state$output_table[[varname]] = list(node,out_port)
  }
  else {
    # Match observed values to arguments.
    fun = rlang::call_fn(call, env=state$env)
    names(observed) = rlang::call_args_names(call)
    matched = fun_args_match(names(fun_args(fun)), observed) %>%
      discard(rlang::is_missing)
    
    # Create call node and edges to observed argument nodes.
    out_port = "__return__"
    node = add_node(state, name, names(matched), out_port)
    iwalk(matched, function(data, port) {
      c(src_node, src_port) %<-% data$source
      if (is.null(node)) {
        # TODO: Add edge to input port of diagram.
      } else {
        add_edge(graph_state$graph, src_node, node, src_port, port)
      }
    })
  }
  
  # Attach call value to node.
  graph_state$observe(index, list(node,out_port), value)
  
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
    env = NULL,
    annotator = NULL,
    node_names = NULL,
    initialize = function(env, db=NULL) {
      self$env = env
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
    push_call = function(call) private$stack$push(call_state$new(call)),
    pop_call = function() private$stack$pop(),
    call_state = function() private$stack$peek(),
    observe = function(...) {
      call_state = self$call_state()
      if (!is.null(call_state))
        call_state$observe_arg(...)
    }
  ),
  private = list(
    stack = NULL
  )
)

call_state = R6Class("call_state",
  public = list(
    initialize = function(call) {
      nargs = length(call) - 1
      private$args = rep(list(rlang::missing_arg()), nargs)
    },
    observed_args = function() private$args,
    observe_arg = function(index, source, value) {
      private$args[[index]] = list(source=source, value=value)
    }
  ),
  private = list(
    args = NULL
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

# Constants

ASSIGN_FUNS <- c(
  "<-",
  "<<-",
  "="
)
SKIP_FUNS <- c(
  "library",
  "loadNamespace",
  "require",
  "requireNamespace"
)