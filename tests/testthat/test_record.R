context("record")

ret = "__return__"

test_that("record simple arithemetic expressions", {
  h = record(1+1)
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), ret)
  add_node(g, "numeric:2", list(), ret)
  add_node(g, "+:1", c("e1","e2"), ret)
  add_edge(g, "numeric:1", "+:1", ret, "e1")
  add_edge(g, "numeric:2", "+:1", ret, "e2")
  expect_equal(h, g)
})