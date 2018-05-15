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

#' Annotate a flow graph
#' 
#' @description Annotate a flow graph, as created by \code{record}.
#' 
#' @param g flow graph to annotate
#' @param db annotation database (by default, the Data Science Ontology)
#' @param nodes whether to annotate nodes
#' @param ports whether to annotate ports
#' 
#' @details Normally, this function need not be called directly. Instead,
#' call \code{record} with the switch \code{annotate=TRUE}.
#' 
#' This function modifies the flow graph in place.
#'
#' @return A flow graph with annotated nodes and/or ports.
#' 
#' @seealso \code{\link{record}}
#' @export
annotate <- function(g, db=NULL, nodes=TRUE, ports=TRUE) {
  # Load package annotations, if not already loaded.
  #
  # Note: Packages cannot be assigned to classes due to R's informal class
  # system. When S3 class names conflict among loaded packages, there is
  # no reliable strategy to disambiguate them. In this case, which annotation
  # is selected is undefined.
  packages = nodes(g) %>% map(function(node) node_attr(g, node, "package")) %>%
    compact() %>% unique()
  annotator = annotator$new(db)
  annotator$load_packages(packages)
  
  # Annotate input and output ports.
  if (ports) {
    for (node in nodes(g)) {
      for (port in input_ports(g, node)) {
        key = annotate_port(annotator, input_port_data(g, node, port))
        if (!is.null(key))
          input_port_attr(g, node, port, "annotation") <- key
      }
      for (port in output_ports(g, node)) {
        key = annotate_port(annotator, output_port_data(g, node, port))
        if (!is.null(key))
          output_port_attr(g, node, port, "annotation") <- key
      }
    }
  }
    
  # Annotate nodes, possibly overriding the default port annotations.
  # Note: Because the nodes in a wiring diagram are ordered, the nodes will be
  # traversed in order of recording (which is also a topological ordering).
  if (nodes) {
    for (node in nodes(g)) {
      annotate_node(annotator, g, node)
    }
  }
  g
}

annotate_node <- function(annotator, g, node) {
  kind = get_default(node_data(g, node), "kind", "function")
  dispatch = switch(kind,
    `function` = annotate_function,
    literal = annotate_literal,
    stop("Unknown node kind: ", kind)
  )
  dispatch(annotator, g, node)
}

annotate_function <- function(annotator, g, node) {
  data = node_data(g, node)
  in_ports = input_ports(g, node)
  out_ports = output_ports(g, node)
  
  # Special case: Dispatch slot annotation.
  if (!is.null(data$slot))
    return(annotate_slot(annotator, g, node))
  
  # Look up annotation for function.
  first_data = if (is_empty(in_ports)) NULL else
    input_port_data(g, node, in_ports[[1]])
  key = annotator$annotate_function(data$`function`, data$package,
                                    first_data$class, first_data$system)
  if (is.null(key)) return()
  
  # Attach annotation to node.
  node_attr(g, node, "annotation") <- key
  
  # Align input and outport ports to domain and codomain of annotation.
  note = annotator$annotation(key)
  align_ports(in_ports, note$domain) %>%
    iwalk(function(port, i) {
      if (is.na(port)) return()
      input_port_attr(g, node, port, "annotation_index") <- i
    })
  align_ports(out_ports, note$codomain) %>%
    iwalk(function(port, i) {
      if (is.na(port)) return()
      key = note$codomain[[i]]$annotate
      if (!is.null(key)) {
        # Override default output port annotation.
        output_port_attr(g, node, port, "annotation") <- key
        for (succ in successors(g, node)) {
          for (edge in edges(g, node, succ, port))
            input_port_attr(g, succ, target_port(g, edge), "annotation") <- key
        }
      }
      output_port_attr(g, node, port, "annotation_index") <- i
    })
}

annotate_literal <- function(annotator, g, node) {
  key = output_port_attr(g, node, return_port, "annotation")
  if (is.null(key)) return()
  
  node_attr(g, node, "annotation") <- key
  node_attr(g, node, "annotation_kind") <- "construct"
  output_port_attr(g, node, return_port, "annotation_index") <- 1L
}

annotate_slot <- function(annotator, g, node) {
  first_port = input_ports(g, node)[[1]]
  key = input_port_attr(g, node, first_port, "annotation")
  if (is.null(key)) return()
  
  note = annotator$annotation(key)
  slots = get_default(note, "slots", list())
  slot = node_attr(g, node, "slot")
  i = detect_index(slots, ~ .$slot == slot)
  if (i == 0L) return()
  
  node_attr(g, node, "annotation") <- key
  node_attr(g, node, "annotation_kind") <- "slot"
  node_attr(g, node, "annotation_index") <- i
}

annotate_port <- function(annotator, data) {
  if (!is_empty(data)) {
    annotator$annotate_type(data$class, data$system)
  }
}

align_ports <- function(ports, obs) {
  map_chr(obs, function(ob) {
    slot = ob$slot
    if (is.character(slot))
      if (slot %in% ports) slot else NA_character_
    else if (is.numeric(slot))
      ports[[slot]]
    else
      stop("Unknown slot type: ", class(slot))
  })
}