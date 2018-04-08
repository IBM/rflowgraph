#' Read graph from GraphML
#' 
#' @import xml2
#' @export
read_graphml <- function(xml) {
  if (!class(xml) == "xml_document")
    xml = read_xml(xml)
  stopifnot(xml_name(xml) == "graphml")
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
    print(edge)
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