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
  annotator = annotator$new(db)
  # Note: Because the nodes in a wiring diagram are ordered, the nodes will be
  # traversed in order of recording (which is also a topological ordering).
  # Thus package annotations will be loaded in order of package use,
  # not package loading through `library` or `require`.
  # 
  # This difference can affect the final result only for object annotations,
  # where packages cannot be assigned to classes due to R's informal class 
  # system. But when S3 class names conflict among loaded packages, there is
  # no reliable strategy to disambiguate anyway, only heuristics. For now
  # we just punt on the whole issue.
  for (node in nodes(g)) {
    # Annotate the node itself.
    key = if (nodes) annotate_node(annotator, g, node)
    note = if (!is.null(key)) annotator$annotation(key)

    # Annotate input and output ports of node.
    if (ports) {
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
    
    # Align input and outport ports to domain and codomain of node annotation.
    if (nodes && !is.null(note) && note$kind == "morphism") {
      align_ports(input_ports(g, node), note$domain) %>%
        iwalk(function(port, i) {
          if (!is.na(port))
            input_port_attr(g, node, port, "annotation_index") <- i
        })
      align_ports(output_ports(g, node), note$codomain) %>%
        iwalk(function(port, i) {
          if (!is.na(port))
            output_port_attr(g, node, port, "annotation_index") <- i
        })
    }
  }
  g
}

annotate_node <- function(annotator, g, node) {
  key = NULL
  data = node_data(g, node)
  kind = get_default(data, "kind", "function")
  switch(kind,
    `function`={
      key = annotator$annotate_function(data$`function`, data$package)
      if (!is.null(key))
        node_attr(g, node, "annotation") <- key
    },
    literal={
      key = annotate_port(annotator, output_port_data(g, node, return_port))
      if (!is.null(key))
        node_data(g, node) <- c(node_data(g, node), list(
          annotation=key, annotation_kind="construct"))
    },
    stop("Unknown node kind: ", kind)
  )
  key
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