#' Ordered dictionary
#' 
#' @description An ordered dictionary data structure, ala \code{OrderedDict}
#' in Python or Julia.
#' 
#' @details TODO
ordered_dict <- function() {
  root = OrderedDictNode$new()
  root$prev = root
  root$next_ = root
  structure(list(root=root,
                 dict=dict()),
            class=c("ordered_dict", "dict"))
}

`[[.ordered_dict` <- function(od, k) {
  od$dict[[k]]$value
}

`[[<-.ordered_dict` <- function(od, k, value) {
  if (!has_key(od, k)) {
    root = od$root
    last = root$prev
    last$next_ = root$prev = od$dict[[k]] = OrderedDictNode$new(last, root, k, value)
  }
  od$dict[[k]]$value = value
  od
}

del.ordered_dict <- function(od, k) {
  link = od$dict[[k]]
  link$prev$next_ = link$next_
  link$next_$prev = link$prev
  del(od$dict, k)
}

length.ordered_dict <- function(od) {
  length(od$dict)
}

has_key.ordered_dict <- function(od, k) {
  has_key(od$dict, k)
}

keys.ordered_dict <- function(od, ...) {
  root = od$root
  sapply(as.list(root$next_, root$prev), function(link) link$key)
}

values.ordered_dict <- function(od, ...) {
  root = od$root
  sapply(as.list(root$next_, root$prev), function(link) link$value)
}

OrderedDictNode <- R6Class("OrderedDictNode",
  public = list(
    prev = NULL,
    next_ = NULL,
    key = NULL,
    value = NULL,
    initialize = function(prev=NULL, next_=NULL, key=NULL, value=NULL) {
      self$prev = prev
      self$next_ = next_
      self$key = key
      self$value = value
    }
  )
)

as.list.OrderedDictNode <- function(first, last) {
  nodes = list()
  node = first
  while (!identical(node, last)) {
    nodes = c(nodes, node)
    node = node$next_
  }
  c(nodes, node)
}
