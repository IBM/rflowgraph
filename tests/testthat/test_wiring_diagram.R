context("wiring_diagram")

test_that("add boxes and wires in a wiring diagram", {
  g = wiring_diagram(c("x1","x2"), c("y1","y2"))
  expect_true(is_directed(g))
  expect_true(has_node(g, input_node(g)))
  expect_true(has_node(g, output_node(g)))
  expect_equal(names(input_ports(g)), c("x1","x2"))
  expect_equal(names(output_ports(g)), c("y1","y2"))
  
  add_node(g, "foo", c("w1","w2"), c("z1","z2"))
  expect_true(has_node(g, "foo"))
  expect_equal(names(input_ports(g, "foo")), c("w1","w2"))
  expect_equal(names(output_ports(g, "foo")), c("z1","z2"))
  
  add_edge(g, input_node(g), "foo", "x1", "w1")
  add_edge(g, input_node(g), "foo", "x2", "w2")
  add_edge(g, "foo", output_node(g), "z1", "y1")
  add_edge(g, "foo", output_node(g), "z2", "y2")
  expect_equal(source_port(g, input_node(g), "foo", 1), "x1")
  expect_equal(target_port(g, input_node(g), "foo", 1), "w1")
  expect_equal(source_port(g, "foo", output_node(g), 2), "z2")
  expect_equal(target_port(g, "foo", output_node(g), 2), "y2")
})

test_that("get and set graph/node/port/edge attributes in a wiring diagram", {
  g = wiring_diagram(data=list(foo=TRUE))
  expect_true(has_node(g, input_node(g)))
  expect_true(has_node(g, output_node(g)))
  expect_equal(graph_attr(g, "foo"), TRUE)
  expect_equal(graph_attr(g, "bar"), NULL)
  graph_attr(g, "bar") <- FALSE
  expect_equal(graph_attr(g, "foo"), TRUE)
  expect_equal(graph_attr(g, "bar"), FALSE)
  
  add_node(g, "f", list(x=list(biz=TRUE)), list(y=list(biz=FALSE)),
           data=list(foo=TRUE))
  add_node(g, "g", "y", "z")
  expect_equal(input_ports(g, "f"), list(x=list(biz=TRUE)))
  expect_equal(output_ports(g, "f"), list(y=list(biz=FALSE)))
  expect_equal(input_ports(g, "g"), list(y=list()))
  expect_equal(output_ports(g, "g"), list(z=list()))
  expect_equal(node_attr(g, "f", "foo"), TRUE)
  expect_equal(node_attr(g, "g", "foo"), NULL)
  node_attr(g, "g", "foo") <- TRUE
  node_attr(g, "g", "bar") <- FALSE
  expect_equal(node_attr(g, "g", "foo"), TRUE)
  expect_equal(node_attr(g, "g", "bar"), FALSE)
  
  add_edge(g, "f", "g", "y", "y", data=list(baz="baz"))
  edge_attr(g, "f", "g", 1, "bar") <- TRUE
  expect_equal(edge_attr(g, "f", "g", 1, "foo"), NULL)
  expect_equal(edge_attr(g, "f", "g", 1, "bar"), TRUE)
  expect_equal(edge_attr(g, "f", "g", 1, "baz"), "baz")
})