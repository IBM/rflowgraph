context("multigraph")

test_that("add and remove nodes in a multigraph", {
  g = multigraph()
  expect_equal(nodes(g), character())
  expect_equal(nnodes(g), 0)
  expect_false(has_node(g, "u"))
  
  add_node(g, "u")
  expect_equal(nodes(g), c("u"))
  expect_true(has_node(g, "u"))
  
  add_node(g, "v")
  expect_equal(nodes(g), c("u","v"))
  expect_equal(nnodes(g), 2)
  
  rem_node(g, "u")
  expect_equal(nodes(g), c("v"))
})

test_that("add and remove edges in a multigraph", {
  g = multigraph()
  add_nodes(g, c("u","v","w"))
  expect_equal(edges(g), list())
  expect_equal(edges(g,"u","v"), list())
  expect_equal(nedges(g), 0)
  expect_equal(nedges(g,"u","v"), 0)
  expect_false(has_edge(g,"u","v"))
  
  add_edge(g, "u", "v")
  add_edge(g, "v", "w")
  expect_equal(edges(g), list(multiedge("u","v",1), multiedge("v","w",1)))
  expect_equal(edges(g,"u","v"), list(multiedge("u","v",1)))
  expect_equal(nedges(g), 2)
  expect_equal(nedges(g,"u","v"), 1)
  expect_true(has_edge(g,"u","v"))
  
  add_edge(g, "u", "v")
  expect_equal(nedges(g), 3)
  expect_equal(nedges(g,"u","v"), 2)
  
  rem_edge(g, "u", "v")
  expect_equal(nedges(g), 2)
  expect_equal(nedges(g,"u","v"), 1)
  
  add_edge(g, "u", "w")
  rem_node(g, "u")
  expect_equal(nodes(g), c("v","w"))
  expect_equal(edges(g), list(multiedge("v","w",1)))
})

test_that("get successors and predecessors in a multigraph", {
  g = multigraph()
  add_nodes(g, c("u","v","w"))
  add_edge(g, "u", "v")
  add_edge(g, "u", "w")
  add_edge(g, "v", "w")
  expect_equal(neighbors(g,"u"), c("v","w"))
  expect_equal(successors(g,"u"), c("v","w"))
  expect_equal(predecessors(g,"u"), character())
  expect_equal(successors(g,"v"), "w")
  expect_equal(predecessors(g,"v"), "u")
})

test_that("get and set graph, node, and edge attributes in a multigraph", {
  g = multigraph()
  expect_error(graph_attr(g, "foo"))
  graph_attr(g, "foo") <- TRUE
  graph_attr(g, "bar") <- FALSE
  expect_equal(graph_attr(g, "foo"), TRUE)
  expect_equal(graph_attr(g, "bar"), FALSE)
  
  add_nodes(g, c("u","v"))
  expect_error(node_attr(g, "v", "foo"))
  node_attr(g, "v", "foo") <- TRUE
  node_attr(g, "v", "bar") <- FALSE
  expect_equal(node_attr(g, "v", "foo"), TRUE)
  expect_equal(node_attr(g, "v", "bar"), FALSE)
  
  add_edge(g, "u", "v")
  add_edge(g, "u", "v")
  edge_attr(g, "u", "v", 1, "foo") <- TRUE
  edge_attr(g, "u", "v", 1, "bar") <- FALSE
  expect_equal(edge_attr(g, "u", "v", 1, "foo"), TRUE)
  expect_equal(edge_attr(g, "u", "v", 1, "bar"), FALSE)
  expect_error(edge_attr(g, "u", "v", 2, "foo"))
})