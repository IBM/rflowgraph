context("graphml")

test_that("round trip graph with no attributes through GraphML", {
  g = multigraph()
  add_nodes(g, c("u","v","w"))
  add_edge(g, "u", "v")
  add_edge(g, "u", "w")
  add_edge(g, "v", "w")
  xml = write_graphml(g)
  h = read_graphml(xml)
  expect_equal(h, g)
})

test_that("round trip attributed graph through GraphML", {
  g = multigraph(list(name="g"))
  add_node(g, "u", list(bar=TRUE, baz=FALSE))
  add_nodes(g, c("v","w"))
  add_edge(g, "u", "v", list(foo=1L))
  add_edge(g, "u", "w", list(foo=2L))
  add_edge(g, "v", "w", list(foo=3L))
  xml = write_graphml(g)
  h = read_graphml(xml)
  expect_equal(h, g)
})