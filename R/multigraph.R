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

#' Multigraphs
#' 
#' @description A directed, attributed multigraph.
#' 
#' @details The internal data structure uses adjacency lists for successors
#' and predecessors. It is modeled on the \code{MultiDiGraph} class in the
#' Python library NetworkX.
#' 
#' @seealso \code{\link{graph}}
#' @export
multigraph <- function(data=list()) multigraph_class$new(data=data)
multigraph_class <- R6Class(
  classname = c("multigraph", "graph"),
  public = list(
    nodes = NULL,
    succ = NULL,
    pred = NULL,
    data = NULL,
    initialize = function(data=NULL) {
      self$nodes = ordered_dict()
      self$succ = ordered_dict()
      self$pred = ordered_dict()
      self$data = data
    }
  )
)

#' @rdname multigraph
#' @export
multiedge <- function(src, tgt, ind) {
  structure(list(src=src, tgt=tgt, ind=ind),
            class=c("multiedge", "edge"))
}

is_directed.multigraph <- function(g) TRUE

nodes.multigraph <- function(g) keys(g$nodes)
edges.multigraph <- function(g, src, tgt) {
  if (missing(src) && missing(tgt)) {
    flatten(map(keys(g$succ), function(src) {
      succ = g$succ[[src]]
      flatten(map(keys(succ), function(tgt) {
        map(seq_along(succ[[tgt]]), function(ind) multiedge(src,tgt,ind))
      }))
    }))
  } else {
    map(seq_len(nedges(g,src,tgt)), function(ind) multiedge(src,tgt,ind))
  }
}

nnodes.multigraph <- function(g) length(g$nodes)
nedges.multigraph <- function(g, src, tgt) {
  if (missing(src) && missing(tgt)) {
    sum(map_int(values(g$succ), function(succ) {
      sum(map_int(values(succ), length))
    }))
  } else {
    length(get_default(g$succ[[src]], tgt, list()))
  }
}

has_node.multigraph <- function(g, n) has_key(g$nodes, n)
has_edge.multigraph <- function(g, src, tgt) has_key(g$succ[[src]], tgt)

add_node.multigraph <- function(g, n, data=list()) {
  if (has_key(g$nodes, n))
    stop("node already exists: ", n)
  g$nodes[[n]] = data
  g$succ[[n]] = ordered_dict()
  g$pred[[n]] = ordered_dict()
  NULL
}

rem_node.multigraph <- function(g, n) {
  stopifnot(has_node(g, n))
  del(g$nodes, n)
  for (u in keys(g$succ[[n]]))
    del(g$pred[[u]], n)
  del(g$succ, n)
  for (u in keys(g$pred[[n]]))
    del(g$succ[[u]], n)
  del(g$pred, n)
}

add_edge.multigraph <- function(g, src, tgt, data=list()) {
  stopifnot(has_node(g, src) && has_node(g, tgt))
  succ = g$succ[[src]]
  pred = g$pred[[tgt]]
  pred[[src]] = TRUE
  edges = get_default(succ, tgt, list())
  length(succ[[tgt]] <- c(edges, list(data)))
}

rem_edge.multigraph <- function(g, src, tgt, ind) {
  edges = edges(g, src, tgt)
  if (missing(ind))
    ind = length(edges)
  if (!(1 <= ind && ind <= length(edges)))
    stop("missing edge ", ind)
  edges = edges[-ind]
  if (is_empty(edges)) {
    del(g$succ[[src]], tgt)
    del(g$pred[[tgt]], src)
  } else {
    g$succ[[src]][[tgt]] <- edges
  }
  ind
}

successors.multigraph <- function(g, node) keys(g$succ[[node]])
predecessors.multigraph <- function(g, node) keys(g$pred[[node]])
  
graph_data.multigraph <- function(g) g$data
`graph_data<-.multigraph` <- function(g, value) {
  g$data <- value
  g
}

node_data.multigraph <- function(g, n) g$nodes[[n]]
`node_data<-.multigraph` <- function(g, n, value) {
  stopifnot(has_node(g, n))
  g$nodes[[n]] <- value
  g
}

edge_data.multigraph <- function(g, src, tgt, ind) g$succ[[src]][[tgt]][[ind]]
`edge_data<-.multigraph` <- function(g, src, tgt, ind, value) {
  stopifnot(nedges(g, src, tgt) >= ind)
  g$succ[[src]][[tgt]][[ind]] <- value
  g
}

edge_attr.multigraph <- function(g, src, tgt, ind, key) {
  edge_data(g, src, tgt, ind)[[key]]
}
`edge_attr<-.multigraph` <- function(g, src, tgt, ind, key, value) {
  edge_data(g, src, tgt, ind)[[key]] <- value
  g
}