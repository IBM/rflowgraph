scope("graphml")

test_that("round trip graph with no attributes", {
  g = multigraph()
  add_nodes(g, c("u","v","w"))
  add_edge(g, "u", "v")
  add_edge(g, "u", "w")
  add_edge(g, "v", "w")
  xml = write_graphml(g)
})