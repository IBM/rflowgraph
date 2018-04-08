#' Graphs
#' 
#' @description Generic interface for graphs as S3 objects
#' (directed or undirected, simple or multi-edged).
graph <- function(...) stop("Abstract interface for graphs")

#' @rdname graph
#' @export
nodes <- function(g) UseMethod("nodes")

#' @rdname graph
#' @export
edges <- function(g, ...) UseMethod("edges")

#' @rdname graph
#' @export
nnodes <- function(g) UseMethod("nnodes")

#' @rdname graph
#' @export
nedges <- function(g, ...) UseMethod("nedges")

#' @rdname graph
#' @export
add_node <- function(g, node, ...) UseMethod("add_node")

#' @rdname graph
#' @export
add_nodes <- function(g, nodes) UseMethod("add_nodes")
add_nodes.default <- function(g, nodes) {
  for (node in nodes)
    add_node(g, node)
}

#' @rdname graph
#' @export
has_node <- function(g, node) UseMethod("has_node")

#' @rdname graph
#' @export
rem_node <- function(g, node) UseMethod("rem_node")

#' @rdname graph
#' @export
add_edge <- function(g, src, tgt, ...) UseMethod("add_edge")

#' @rdname graph
#' @export
has_edge <- function(g, src, tgt) UseMethod("has_edge")

#' @rdname graph
#' @export
rem_edge <- function(g, src, tgt, ...) UseMethod("rem_edge")

#' @rdname graph
neighbors <- function(g, node) UseMethod("neighbors")
neighbors.default <- function(g, node) successors(g, node)

#' @rdname graph
#' @export
successors <- function(g, node) UseMethod("successors")
successors.default <- function(g, node) neighbors(g, node)

#' @rdname graph
#' @export
predecessors <- function(g, node) UseMethod("predecessors")
predecessors.default <- function(g, node) neighbors(g, node)

#' Graph data
#' 
#' @description Arbitrary metadata can be attached to graphs, nodes, and edges.
#' By default, the data is a dictionary of attributes (key-value pairs).
#' 
#' @export
graph_data <- function(g) UseMethod("graph_data")

#' @rdname graph_data
#' @export
graph_attr <- function(g, key) UseMethod("graph_attr")
graph_attr.default <- function(g, key) graph_data(g)[[key]]

#' @rdname graph_data
#' @export
`graph_attr<-` <- function(g, key, value) UseMethod("graph_attr<-")
`graph_attr<-.default` <- function(g, key, value) {
  data = graph_data(g)
  data[[key]] = value
  g
}

#' @rdname graph_data
#' @export
node_data <- function(g, node) UseMethod("node_data")

#' @rdname graph_data
#' @export
node_attr <- function(g, node, key) UseMethod("node_attr")
node_attr.default <- function(g, node, key) node_data(g, node)[[key]]

#' @rdname graph_data
#' @export
`node_attr<-` <- function(g, node, key, value) UseMethod("node_attr<-")
`node_attr<-.default` <- function(g, node, key, value) {
  data = node_data(g, node)
  data[[key]] = value
  g
}

#' @rdname graph_data
#' @export
edge_data <- function(g, src, tgt, ...) UseMethod("edge_data")

#' @rdname graph_data
#' @export
edge_attr <- function(g, src, tgt, ..., key) UseMethod("edge_attr")
edge_attr.default <- function(g, src, tgt, key) edge_data(g, src, tgt)[[key]]

#' @rdname graph_data
#' @export
`edge_attr<-` <- function(g, src, tgt, ..., key, value) UseMethod("edge_attr<-")
`edge_attr<-.default` <- function (g, src, tgt, key, value) {
  data = edge_data(g, src, tgt)
  data[[key]] = value
  g
}