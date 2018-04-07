#' Ordered dictionary
#' 
#' @description Data structure for ordered dictionaries,
#' ala \code{OrderedDict} in Python or Julia.
#' 
#' @details The implementation combines an ordinary dictionary (hashmap)
#' with a circular doubly linked list, exactly as in Python's
#' \code{OrderedDict}.
#' 
#' @seealso \code{\link{dict}}
ordered_dict <- function() {
  structure(ordered_dict_impl$new(), class=c("ordered_dict", "dict"))
}

`[[.ordered_dict` <- function(d, k) d$get(k)$value
`[[<-.ordered_dict` <- function(d, k, value) {
  node = if (d$has(k)) d$get(k) else d$add(k)
  node$value = value
  d
}

has_key.ordered_dict <- function(d, k) d$has(k)
del.ordered_dict <- function(d, k) d$del(k)
length.ordered_dict <- function(d) d$length()
keys.ordered_dict <- function(d) sapply(d$as_list(), function(n) n$key)
values.ordered_dict <- function(d) sapply(d$as_list(), function(n) n$value)

ordered_dict_impl = R6Class("ordered_dict_impl",
  private = list(
    dict = NULL,
    root = NULL
  ),
  public = list(
    initialize = function() {
      private$dict = dict()
      root = ordered_dict_node$new()
      root$prev = root
      root$next_ = root
      private$root = root
    },
    length = function() length(private$dict),
    get = function(key) private$dict[[key]],
    has = function(key) has_key(private$dict, key),
    add = function(key) {
      stopifnot(!self$has(key))
      root = private$root
      last = root$prev
      node = ordered_dict_node$new(last, root, key)
      last$next_ = root$prev = private$dict[[key]] = node
    },
    del = function(key) {
      stopifnot(self$has(key))
      node = self$get(key)
      node$prev$next_ = node$next_
      node$next_$prev = node$prev
      node$prev = node$next_ = NULL
      del(private$dict, key)
    },
    next_ = function(node=NULL) {
      if (is.null(node))
        node = private$root
      node = node$next_
      if (identical(node, private$root)) NULL else node
    },
    as_list = function() {
      n = self$length()
      nodes = vector(mode="list", length=n)
      node = NULL
      for (i in seq2(1,n))
        nodes[[i]] = node = self$next_(node)
      nodes
    }
  )
)

ordered_dict_node <- R6Class("ordered_dict_node",
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