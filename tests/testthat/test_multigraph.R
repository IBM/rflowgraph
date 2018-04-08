context("multigraph")

test_that("adding and removing nodes in a multigraph", {
  g = multigraph()
  expect_equal(nodes(g), character())
  expect_equal(nnodes(g), 0)
  
  add_node(g, "u")
  expect_equal(nodes(g), c("u"))
  
  add_node(g, "v")
  expect_equal(nodes(g), c("u","v"))
  expect_equal(nnodes(g), 2)
  
  rem_node(g, "u")
  expect_equal(nodes(g), c("v"))
})

test_that("adding and removing edges in a multigraph", {
  g = multigraph()
  add_nodes(g, c("u","v","w"))
  expect_equal(edges(g), list())
  expect_equal(edges(g,"u","v"), list())
  expect_equal(nedges(g), 0)
  expect_equal(nedges(g,"u","v"), 0)
  
  add_edge(g, "u", "v")
  add_edge(g, "v", "w")
  expect_equal(edges(g), list(multiedge("u","v",1), multiedge("v","w",1)))
  expect_equal(edges(g,"u","v"), list(multiedge("u","v",1)))
  expect_equal(nedges(g), 2)
  expect_equal(nedges(g,"u","v"), 1)
  
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