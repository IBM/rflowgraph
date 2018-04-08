#' Read graph from GraphML
#' 
#' @import xml2
#' @export
read_graphml <- function(xml) {
  # Read XML document and top-level elements.
  if (!("xml_document" %in% class(xml)))
    xml = read_xml(xml)
  if (xml_name(xml) != "graphml")
    stop("Root element of GraphML document must be <graphml>")
  xgraphs = xml_find_all(xml, "graph")
  if (length(xgraphs) != 1)
    stop("Root element of GraphML document must contain exactly one <graph>")
  xgraph = xgraphs[[1]]
  
  # Read nodes.
  graph = multigraph()
  for (xnode in xml_find_all(xgraph, "node")) {
    node = xml_required_attr(xnode, "id")
    add_node(graph, node, read_graphml_data(xnode))
  }
  
  # Read edges.
  for (xedge in xml_find_all(xgraph, "edge")) {
    src = xml_required_attr(xedge, "source")
    tgt = xml_required_attr(xedge, "target")
    add_edge(graph, src, tgt, read_graphml_data(xedge))
  }
  
  graph
}

read_graphml_data <- function(xnode) {
  # TODO
  dict()
}

#' Write graph to GraphML
#' 
#' @import xml2
#' @export
write_graphml <- function(graph, file=NULL) {
  # Create XML document and root node.
  xml = xml_new_root("graphml",
    xmlns = "http://graphml.graphdrawing.org/xmlns",
    `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
    `xsi:schemaLocation` = "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd"
  )
  
  # Create top-level graph element.
  xgraph = xml_add_child(xml, "graph", edgedefault="directed")
  write_graphml_data(xgraph, "graph", graph_data(graph, node))
  
  # Create node elements.
  for (node in nodes(graph)) {
    xnode = xml_add_child(xgraph, "node", id=node)
    write_graphml_data(xnode, "node", node_data(graph, node))
  }
  
  # Create edge elements.
  for (edge in edges(graph)) {
    c(src, tgt, ind) %<-% unclass(edge)
    xedge = xml_add_child(xgraph, "edge", source=src, target=tgt)
    write_graphml_data(xedge, "edge", edge_data(graph, src, tgt, ind))
  }

  # Write XML to file or return XML document.
  if (!is.null(file))
    write_xml(xml, file)
  else
    xml
}

write_graphml_data <- function(xgraph, scope, data) {
  # TODO
}

xml_required_attr <- function(x, attr) {
  stopifnot(xml_has_attr(x, attr))
  xml_attr(x, attr)
}