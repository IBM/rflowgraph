#' Wiring diagrams
#' 
#' @description Wiring diagrams (aka string diagrams) with arbitrary node/edge
#' data.
#' 
#' @details The implementation is a simple wrapper around \code{multigraph}.
#'
#' @seealso \code{\link{multigraph}}, \code{\link{graph}}
#' @export
wiring_diagram <- function(in_ports=list(), out_ports=list(), data=list()) {
  g = wiring_diagram_class$new(box_data$new(in_ports, out_ports, data))
  add_node.multigraph(g, input_node(g))
  add_node.multigraph(g, output_node(g))
  g
}

#' @rdname wiring_diagram
#' @export
input_node <- function(g) UseMethod("input_node")
input_node.wiring_diagram <- function(g) g$input_node

#' @rdname wiring_diagram
#' @export
output_node <- function(g) UseMethod("output_node")
output_node.wiring_diagram <- function(g) g$output_node

#' @rdname wiring_diagram
add_node.wiring_diagram <- function(g, node, in_ports, out_ports, data=list()) {
  add_node.multigraph(g, node, box_data$new(in_ports, out_ports, data))
}

#' @rdname wiring_diagram
#' @export
input_ports <- function(g, node=NULL) UseMethod("input_ports")
input_ports.wiring_diagram <- function(g, node=NULL) {
  if (is.null(node))
    graph_data.multigraph(g)$input_ports
  else
    node_data.multigraph(g, node)$input_ports
}

#' @rdname wiring_diagram
#' @export
output_ports <- function(g, node=NULL) UseMethod("output_ports")
output_ports.wiring_diagram <- function(g, node=NULL) {
  if (is.null(node))
    graph_data.multigraph(g)$output_ports
  else
    node_data.multigraph(g, node)$output_ports
}

#' @rdname wiring_diagram
add_edge.wiring_diagram <- function(g, src, tgt, src_port, tgt_port, data=list()) {
  add_edge.multigraph(g, src, tgt, wire_data$new(src_port, tgt_port, data))
}

#' @rdname wiring_diagram
#' @export
source_port <- function(g, src, tgt, ind) UseMethod("source_port")
source_port.wiring_diagram <- function(g, src, tgt, ind) {
  edge_data.multigraph(g, src, tgt, ind)$source_port
}

#' @rdname wiring_diagram
#' @export
target_port <- function(g, src, tgt, ind) UseMethod("target_port")
target_port.wiring_diagram <- function(g, src, tgt, ind) {
  edge_data.multigraph(g, src, tgt, ind)$target_port
}

graph_data.wiring_diagram <- function(g) {
  graph_data.multigraph(g)$data
}
`graph_data<-.wiring_diagram` <- function(g, value) {
  data = graph_data.multigraph(g)
  data$data <- value
  g
}

node_data.wiring_diagram <- function(g, node) {
  node_data.multigraph(g, node)$data
}
`node_data<-.wiring_diagram` <- function(g, node, value) {
  data = node_data.multigraph(g, node)
  data$data <- value
  g
}

edge_data.wiring_diagram <- function(g, src, tgt, ind) {
  edge_data.multigraph(g, src, tgt, ind)$data
}
`edge_data<-.wiring_diagram` <- function(g, src, tgt, ind, value) {
  data = edge_data.multigraph(g, src, tgt, ind)
  data$data <- value
  g
}

wiring_diagram_class = R6Class(
  classname = "wiring_diagram",
  inherit = multigraph_class,
  public = list(
    input_node = "__in__",
    output_node = "__out__"
  )
)

box_data <- R6Class("box_data",
  public = list(
    input_ports = NULL,
    output_ports = NULL,
    data = NULL,
    initialize = function(in_ports, out_ports, data=list()) {
      self$input_ports = in_ports
      self$output_ports = out_ports
      self$data = data
    }
  )
)

wire_data <- R6Class("wire_data",
  public = list(
    source_port = character(),
    target_port = character(),
    data = NULL,
    initialize = function(src_port, tgt_port, data=list()) {
      self$source_port = src_port
      self$target_port = tgt_port
      self$data = data
    }
  )
)