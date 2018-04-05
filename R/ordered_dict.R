#' Ordered dictionary
#' 
#' @description An ordered dictionary data structure, ala \code{OrderedDict}
#' in Python or Julia.
#' 
#' @details TODO
ordered_dict <- function() {
  root = ordered_dict_link()
  root$prev = root
  root$next_ = root
  structure(list(root=root,
                 dict=dict()),
            class=c("ordered_dict", "dict"))
}

ordered_dict_link <- function(prev=NULL, next_=NULL, key=NULL, value=NULL) {
  link = dict()
  link$prev = prev; link$next_ = next_; link$key = key; link$value = value
  link
}

ordered_dict_links <- function(od) {
  links = list()
  link = od$root$next_
  while (!identical(link, od$root)) {
    links = c(links, link)
    link = link$next_
  }
  links
}

`[[.ordered_dict` <- function(od, k) {
  od$dict[[k]]$value
}

`[[<-.ordered_dict` <- function(od, k, value) {
  if (!has_key(od, k)) {
    root = od$root
    last = root$prev
    last$next_ = root$prev = od$dict[[k]] = ordered_dict_link(last, root, k, value)
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
  sapply(ordered_dict_links(od), function(link) link$key)
}

values.ordered_dict <- function(od, ...) {
  sapply(ordered_dict_links(od), function(link) link$value)
}
