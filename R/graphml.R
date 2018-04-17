#' Read graph from GraphML
#' 
#' @import xml2
#' @export
read_graphml <- function(xml, graph=NULL) {
  # Read XML document and top-level elements.
  if (!("xml_document" %in% class(xml)))
    xml = read_xml(xml)
  if (xml_name(xml) != "graphml")
    stop("Root element of GraphML document must be <graphml>")
  xgraphs = xml_find_all(xml, "graph")
  if (length(xgraphs) != 1)
    stop("Root element of GraphML document must contain exactly one <graph>")
  xgraph = xgraphs[[1]]
  
  # Read keys (data attribute declarations).
  graphml_keys = dict()
  for (xkey in xml_find_all(xml, "key")) {
    id = xml_required_attr(xkey, "id")
    scope = xml_attr(xkey, "for", default="all")
    attr_name = xml_required_attr(xkey, "attr.name")
    attr_type = xml_attr(xkey, "attr.type", default="string")
    graphml_keys[[id]] = list(attr_name=attr_name, attr_type=attr_type)
  }
  
  # Read graph.
  if (is.null(graph))
    graph = multigraph()
  read_graphml_graph(graph, graphml_keys, xgraph)
}

read_graphml_graph <- function(graph, graphml_keys, xgraph)
  UseMethod("read_graphml_graph")

read_graphml_graph.graph <- function(graph, graphml_keys, xgraph) {
  # Read graph data.
  graph_data(graph) <- read_graphml_data(graphml_keys, xgraph)
  
  # Read nodes.
  for (xnode in xml_find_all(xgraph, "node")) {
    node = xml_required_attr(xnode, "id")
    add_node(graph, node, read_graphml_data(graphml_keys, xnode))
  }
  
  # Read edges.
  for (xedge in xml_find_all(xgraph, "edge")) {
    src = xml_required_attr(xedge, "source")
    tgt = xml_required_attr(xedge, "target")
    add_edge(graph, src, tgt, read_graphml_data(graphml_keys, xedge))
  }
  
  graph
}

read_graphml_graph.wiring_diagram <- function(graph, graphml_keys, xgraph) {
  # Read top-level node.
  xnodes = xml_find_all(xgraph, "node")
  if (length(xnodes) != 1)
    stop("Root graph of GraphML document must contain exactly one <node>")
  xparent = xnodes[[1]]
  parent = xml_required_attr(xparent, "id")
  
  # Read subgraph of top-level node.
  xgraphs = xml_find_all(xparent, "graph")
  if (length(xgraphs) != 1)
    stop("Node element can contain at most one <graph> (subgraph element)")
  xgraph = xgraphs[[1]]
  
  # Read diagram ports and graph data.
  c(in_ports, out_ports) %<-% read_graphml_ports(graphml_keys, xparent)
  input_ports(graph) <- in_ports
  output_ports(graph) <- out_ports
  graph_data(graph) <-read_graphml_data(graphml_keys, xgraph)
  
  # Read nodes.
  for (xnode in xml_find_all(xgraph, "node")) {
    # TODO: Support nested wiring diagrams.
    node = xml_required_attr(xnode, "id")
    c(in_ports, out_ports) %<-% read_graphml_ports(graphml_keys, xnode)
    data = read_graphml_data(graphml_keys, xnode)
    add_node(graph, node, in_ports, out_ports, data)
  }
  
  # Read edges.
  for (xedge in xml_find_all(xgraph, "edge")) {
    src = xml_required_attr(xedge, "source")
    tgt = xml_required_attr(xedge, "target")
    src_port = xml_required_attr(xedge, "sourceport")
    tgt_port = xml_required_attr(xedge, "targetport")
    data = read_graphml_data(graphml_keys, xedge)
    if (src == parent) src = input_node(graph)
    if (tgt == parent) tgt = output_node(graph)
    add_edge(graph, src, tgt, src_port, tgt_port, data)
  }
  
  graph
}

read_graphml_ports <- function(graphml_keys, xnode) {
  in_ports = ordered_dict()
  out_ports = ordered_dict()
  for (xport in xml_find_all(xnode, "port")) {
    name = xml_required_attr(xport, "name")
    data = read_graphml_data(graphml_keys, xport)
    i = match("portkind", names2(data))
    if (is.na(i))
      stop("Port elements must have 'portkind' data")
    
    portkind = data[[i]]
    data = data[-i]
    if (is_empty(data)) data = list()
    if (portkind == "input")
      in_ports[[name]] = data
    else if (portkind == "output")
      out_ports[[name]] = data
    else
      stop("Port element has invalid 'portkind' data: ", portkind)
  }
  list(input_ports=in_ports, output_ports=out_ports)
}

read_graphml_data <- function(graphml_keys, xelem) {
  xdatas = xml_find_all(xelem, "data")
  data = vector(mode="list", length=length(xdatas))
  for (i in seq_along(xdatas)) {
    xdata = xdatas[[i]]
    key = graphml_keys[[xml_required_attr(xdata, "key")]]
    names(data)[[i]] = key$attr_name
    data[[i]] = read_graphml_data_value(key$attr_type, xml_text(xdata))
  }
  data
}

read_graphml_data_value <- function(attr_type, s) {
  if (attr_type == "boolean") as.logical(s)
  else if (attr_type == "int" || attr_type == "long") as.integer(s)
  else if (attr_type == "float" || attr_type == "double") as.numeric(s)
  else if (attr_type == "string") s
  else stop("Invalid GraphML data type: ", attr_type)
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
  xgraph = xml_add_child(xml, "graph",
    edgedefault = if (is_directed(graph)) "directed" else "undirected")
  write_graphml_graph(graph, graphml_keys, xgraph)
  
  # Write key elements, based on attribute names and types collected while
  # writing graph, node, and edge data.
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

write_graphml_graph <- function(graph, graphml_keys, xelem)
  UseMethod("write_graphml_graph")

write_graphml_graph.graph <- function(graph, graphml_keys, xgraph) {
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
}

write_graphml_graph.wiring_diagram <- function(graph, graphml_keys, xgraph,
                                               parent=NULL) {
  # Create top-level node and graph.
  if (is.null(parent))
    parent = "__root__"
  xparent = xml_add_child(xgraph, "node", id=parent)
  write_graphml_ports(graph, graphml_keys, xparent)
  
  xgraph = xml_add_child(xparent, "graph")
  write_graphml_data(graphml_keys, xgraph, graph_data(graph))
  
  # Create node elements.
  skip_nodes = c(input_node(graph), output_node(graph))
  for (node in nodes(graph)) {
    # TODO: Support nested wiring diagrams.
    if (node %in% skip_nodes) next
    xnode = xml_add_child(xgraph, "node", id=node)
    write_graphml_data(graphml_keys, xnode, node_data(graph, node))
    write_graphml_ports(graph, graphml_keys, xnode, node)
  }
  
  # Create edge elements.
  for (edge in edges(graph)) {
    c(src, tgt, ind) %<-% unclass(edge)
    xedge = xml_add_child(xgraph, "edge",
      source = if (src %in% skip_nodes) parent else src,
      target = if (tgt %in% skip_nodes) parent else tgt,
      sourceport = source_port(graph, src, tgt, ind),
      targetport = target_port(graph, src, tgt, ind)
    )
    write_graphml_data(graphml_keys, xedge, edge_data(graph, src, tgt, ind))
  }
}

write_graphml_ports <- function(graph, graphml_keys, xnode, node=NULL) {
  for (name in input_ports(graph, node)) {
    xport = xml_add_child(xnode, "port", name=name)
    write_graphml_data(graphml_keys, xport, list(portkind="input"))
    write_graphml_data(graphml_keys, xport, input_port_data(graph, node, name))
  }
  for (name in output_ports(graph, node)) {
    xport = xml_add_child(xnode, "port", name=name)
    write_graphml_data(graphml_keys, xport, list(portkind="output"))
    write_graphml_data(graphml_keys, xport, output_port_data(graph, node, name))
  }
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
  else stop("No GraphML data type for R type: ", type)
}

write_graphml_data_value <- function(x) {
  type = typeof(x)
  if (type == "logical")
    tolower(toString(x))
  else if (type == "integer" || type == "double" || type == "character")
    toString(x)
  else
    stop("No GraphML data type for R type: ", type)
}

xml_required_attr <- function(x, attr) {
  stopifnot(xml_has_attr(x, attr))
  xml_attr(x, attr)
}