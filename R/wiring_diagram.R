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

#' Wiring diagrams
#' 
#' @description Wiring diagrams (aka string diagrams) with arbitrary
#' node/port/edge data.
#' 
#' @details The implementation is a simple wrapper around \code{multigraph}.
#'
#' @seealso \code{\link{multigraph}}, \code{\link{graph}}
#' @export
wiring_diagram <- function(input_ports=list(), output_ports=list(),
                           data=list()) {
  g = wiring_diagram_class$new(box_data$new(input_ports, output_ports, data))
  add_node.multigraph(g, input_node(g))
  add_node.multigraph(g, output_node(g))
  g
}
wiring_diagram_class <- R6Class(
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
    initialize = function(in_ports=list(), out_ports=list(), data=list()) {
      self$input_ports = coerce_ports(in_ports)
      self$output_ports = coerce_ports(out_ports)
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

#' @rdname wiring_diagram
#' @export
input_node <- function(g) UseMethod("input_node")

#' @export
input_node.wiring_diagram <- function(g) g$input_node

#' @rdname wiring_diagram
#' @export
output_node <- function(g) UseMethod("output_node")

#' @export
output_node.wiring_diagram <- function(g) g$output_node

#' @export
nnodes.wiring_diagram <- function(g) nnodes.multigraph(g) - 2

#' @export
nodes.wiring_diagram <- function(g) {
  skip = c(input_node(g), output_node(g))
  discard(nodes.multigraph(g), function(n) n %in% skip)
}

#' @rdname wiring_diagram
#' @export
add_node.wiring_diagram <- function(g, node, input_ports=list(),
                                    output_ports=list(), data=list()) {
  add_node.multigraph(g, node, box_data$new(input_ports, output_ports, data))
}

#' @rdname wiring_diagram
#' @export
input_ports <- function(g, node) UseMethod("input_ports")

#' @export
input_ports.wiring_diagram <- function(g, node=NULL) {
  keys(graph_or_node_data(g, node)$input_ports)
}

#' @rdname wiring_diagram
#' @export
`input_ports<-` <- function(g, node, value) UseMethod("input_ports<-")

#' @export
`input_ports<-.wiring_diagram` <- function(g, node=NULL, value) {
  data = graph_or_node_data(g, node)
  data$input_ports = coerce_ports(value)
  g
}

#' @rdname wiring_diagram
#' @export
output_ports <- function(g, node) UseMethod("output_ports")

#' @export
output_ports.wiring_diagram <- function(g, node=NULL) {
  keys(graph_or_node_data(g, node)$output_ports)
}

#' @rdname wiring_diagram
#' @export
`output_ports<-` <- function(g, node, value) UseMethod("output_ports<-")

#' @export
`output_ports<-.wiring_diagram` <- function(g, node=NULL, value) {
  data = graph_or_node_data(g, node)
  data$output_ports = coerce_ports(value)
  g
}

graph_or_node_data <- function(g, node=NULL) {
  if (is.null(node) || node == input_node(g) || node == output_node(g))
    graph_data.multigraph(g)
  else
    node_data.multigraph(g, node)
}

coerce_ports <- function(ports) {
  if (is.character(ports))
    ports = set_names(map(seq_along(ports), ~list()), ports)
  as_ordered_dict(ports)
}

#' @rdname wiring_diagram
#' @export
add_edge.wiring_diagram <- function(g, src, tgt, src_port, tgt_port, data=list()) {
  add_edge.multigraph(g, src, tgt, wire_data$new(src_port, tgt_port, data))
}

#' @rdname wiring_diagram
#' @export
edges.wiring_diagram <- function(g, src, tgt, src_port, tgt_port) {
  if (missing(src) && missing(tgt)) {
    edges.multigraph(g)
  } else {
    edges = edges.multigraph(g, src, tgt)
    if (!missing(src_port))
      edges = keep(edges, ~ source_port(g,.) == src_port)
    if (!missing(tgt_port))
      edges = keep(edges, ~ target_port(g,.) == tgt_port)
    edges
  }
}

#' @rdname wiring_diagram
#' @export
nedges.wiring_diagram <- function(g, src, tgt, src_port, tgt_port) {
  if (missing(src) && missing(tgt))
    nedges.multigraph(g)
  else if (missing(src_port) && missing(tgt_port))
    nedges.multigraph(g, src, tgt)
  else
    length(edges(g, src, tgt, src_port, tgt_port))
}

#' @rdname wiring_diagram
#' @export
source_port <- function(g, src, tgt, ind) UseMethod("source_port")

#' @export
source_port.wiring_diagram <- function(...) edge_data.multigraph(...)$source_port

#' @rdname wiring_diagram
#' @export
target_port <- function(g, src, tgt, ind) UseMethod("target_port")

#' @export
target_port.wiring_diagram <- function(...) edge_data.multigraph(...)$target_port

# Graph data

#' @export
graph_data.wiring_diagram <- function(g) {
  graph_data.multigraph(g)$data
}

#' @export
`graph_data<-.wiring_diagram` <- function(g, value) {
  data = graph_data.multigraph(g)
  data$data <- value
  g
}

#' @export
node_data.wiring_diagram <- function(g, node) {
  node_data.multigraph(g, node)$data
}

#' @export
`node_data<-.wiring_diagram` <- function(g, node, value) {
  data = node_data.multigraph(g, node)
  data$data <- value
  g
}

#' @export
edge_data.wiring_diagram <- function(...) edge_data.multigraph(...)$data

#' @export
`edge_data<-.wiring_diagram` <- function(g, src, tgt, ind, value) {
  data = edge_data.multigraph(g, src, tgt, ind)
  data$data <- value
  g
}

#' Wiring diagram data
#' 
#' @description In addition to the usual graph/node/edge data, wiring diagrams
#' allow arbitrary data on input and output ports.
#' 
#' @name wiring_diagram_data
#' @seealso \code{\link{graph_data}}
NULL

#' @rdname wiring_diagram_data
#' @export
input_port_data <- function(g, node, port) UseMethod("input_port_data")

#' @export
input_port_data.wiring_diagram <- function(g, node, port) {
  graph_or_node_data(g, node)$input_ports[[port]]
}

#' @rdname wiring_diagram_data
#' @export
`input_port_data<-` <- function(g, node, port, value)
  UseMethod("input_port_data<-")

#' @export
`input_port_data<-.wiring_diagram` <- function(g, node, port, value) {
  data = graph_or_node_data(g, node)$input_ports
  data[[port]] <- value
  g
}

#' @rdname wiring_diagram_data
#' @export
input_port_attr <- function(g, node, port, key) UseMethod("input_port_attr")

#' @export
input_port_attr.wiring_diagram <- function(g, node, port, key) {
  graph_or_node_data(g, node)$input_ports[[port]][[key]]
}

#' @rdname wiring_diagram_data
#' @export
`input_port_attr<-` <- function(g, node, port, key, value)
  UseMethod("input_port_attr<-")

#' @export
`input_port_attr<-.wiring_diagram` <- function(g, node, port, key, value) {
  data = graph_or_node_data(g, node)$input_ports
  data[[port]][[key]] = value
  g
}

#' @rdname wiring_diagram_data
#' @export
output_port_data <- function(g, node, port) UseMethod("output_port_data")

#' @export
output_port_data.wiring_diagram <- function(g, node, port) {
  graph_or_node_data(g, node)$output_ports[[port]]
}

#' @rdname wiring_diagram_data
#' @export
`output_port_data<-` <- function(g, node, port, value)
  UseMethod("output_port_data<-")

#' @export
`output_port_data<-.wiring_diagram` <- function(g, node, port, value) {
  data = graph_or_node_data(g, node)$output_ports
  data[[port]] <- value
  g
}

#' @rdname wiring_diagram_data
#' @export
output_port_attr <- function(g, node, port, key) UseMethod("output_port_attr")

#' @export
output_port_attr.wiring_diagram <- function(g, node, port, key) {
  graph_or_node_data(g, node)$output_ports[[port]][[key]]
}

#' @rdname wiring_diagram_data
#' @export
`output_port_attr<-` <- function(g, node, port, key, value)
  UseMethod("output_port_attr<-")

#' @export
`output_port_attr<-.wiring_diagram` <- function(g, node, port, key, value) {
  data = graph_or_node_data(g, node)$output_ports
  data[[port]][[key]] = value
  g
}