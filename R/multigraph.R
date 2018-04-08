#' Multigraphs
#' 
#' @description A directed, attributed multigraph.
#' The internal data structure uses adjacency lists for successors and predecessors.
#' 
#' @details TODO
#' @export
multigraph <- function(data=dict()) {
  structure(list(nodes=ordered_dict(),
                 succ=ordered_dict(),
                 pred=ordered_dict(),
                 data=data),
            class=c("multigraph", "graph"))
}

#' @rdname multigraph
#' @export
multiedge <- function(src, tgt, ind) {
  structure(list(src=src, tgt=tgt, ind=ind),
            class=c("multiedge", "edge"))
}

nodes.multigraph <- function(g) keys(g$nodes)
edges.multigraph <- function(g, src, tgt) {
  if (missing(src) && missing(tgt)) {
    as.list(do.call(c, lapply(keys(g$succ), function(src) {
      succ = g$succ[[src]]
      do.call(c, lapply(keys(succ), function(tgt) {
        lapply(seq_along(succ[[tgt]]), function(ind) multiedge(src,tgt,ind))
      }))
    })))
  } else {
    lapply(seq_len(nedges(g,src,tgt)), function(ind) multiedge(src,tgt,ind))
  }
}

nnodes.multigraph <- function(g) length(g$nodes)
nedges.multigraph <- function(g, src, tgt) {
  if (missing(src) && missing(tgt)) {
    sum(vapply(values(g$succ), function(succ) {
      sum(vapply(values(succ), length, 0L))
    }, 0L))
  } else {
    length(get_default(g$succ[[src]], tgt, list()))
  }
}

has_node.multigraph <- function(g, n) has_key(g$nodes, n)
has_edge.multigraph <- function(g, src, tgt) has_key(g$succ[[src]], tgt)

add_node.multigraph <- function(g, n, data=dict()) {
  g$nodes[[n]] = data
  g$succ[[n]] = ordered_dict()
  g$pred[[n]] = ordered_dict()
  NULL
}

rem_node.multigraph <- function(g, n, data=dict()) {
  stopifnot(has_node(g, n))
  del(g$nodes, n)
  for (u in keys(g$succ[[n]]))
    del(g$pred[[u]], n)
  del(g$succ, n)
  for (u in keys(g$pred[[n]]))
    del(g$succ[[u]], n)
  del(g$pred, n)
}

add_edge.multigraph <- function(g, src, tgt, data=dict()) {
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
    stop(paste("missing edge", ind))
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
node_data.multigraph <- function(g, n) g$nodes[[n]]
edge_data.multigraph <- function(g, src, tgt, ind) g$succ[[src]][[tgt]][[ind]]

edge_attr.multigraph <- function(g, src, tgt, ind, key) {
  edge_data(g, src, tgt, ind)[[key]]
}
`edge_attr<-.multigraph` <- function(g, src, tgt, ind, key, value) {
  data = edge_data(g, src, tgt, ind)
  data[[key]] = value
  g
}