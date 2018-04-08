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
  list()
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
  graphml_keys = ordered_dict()
  xgraph = xml_add_child(xml, "graph", edgedefault="directed")
  write_graphml_data(graphml_keys, xgraph, graph_data(graph))
  
  # Create node elements.
  for (node in nodes(graph)) {
    xnode = xml_add_child(xgraph, "node", id=node)
    write_graphml_data(graphml_keys, xnode, node_data(graph, node))
  }
  
  # Create edge elements.
  for (edge in edges(graph)) {
    c(src, tgt, ind) %<-% unclass(edge)
    xedge = xml_add_child(xgraph, "edge", source=src, target=tgt)
    write_graphml_data(graphml_keys, xedge, edge_data(graph, src, tgt, ind))
  }
  
  # Write key elements, based on attribute keys (data declarations) collected
  # while writing graph, node, and edge data.
  for (key in values(graphml_keys)) {
    xkey = xml_add_sibling(xgraph, "key", .where="before")
    xml_attrs(xkey) = key
  }

  # Write XML to file or return XML document.
  if (!is.null(file))
    write_xml(xml, file)
  else
    xml
}

write_graphml_data <- function(graphml_keys, xelem, data) {
  if (is.null(data)) return()
  scope = xml_name(xelem)
  for (attr_name in keys(data)) {
    attr_value = data[[attr_name]]
    
    # Get or create GraphML key for the data attribute.
    graphml_key = set_default(graphml_keys, paste(attr_name, scope, sep=":"), {
      id = paste0("d", length(graphml_keys) + 1)
      attr_type = write_graphml_data_type(typeof(attr_value))
      list(id=id, `for`=scope, attr.name=attr_name, attr.type=attr_type)
    })
    
    # Write data attribute to <data> element.
    xdata = xml_add_child(xelem, "data", key=graphml_key$id)
    xml_text(xdata) <- write_graphml_data_value(attr_value)
  }
}

write_graphml_data_type <- function(type) {
  if (type == "logical") "boolean"
  else if (type == "integer") "int"
  else if (type == "double") "double"
  else if (type == "character") "string"
  else stop(paste("No GraphML data type for R type:", type))
}

write_graphml_data_value <- function(x) {
  type = typeof(x)
  if (type == "logical")
    tolower(toString(x))
  else if (type == "integer" || type == "double" || type == "character")
    toString(x)
  else
    stop(paste("No GraphML data type for R type:", type))
}

xml_required_attr <- function(x, attr) {
  stopifnot(xml_has_attr(x, attr))
  xml_attr(x, attr)
}