context("record")

ret = "__return__"

test_that("record trivial arithemetic expression", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), ret)
  add_node(g, "numeric:2", list(), ret)
  add_node(g, "+:1", c("e1","e2"), ret)
  add_edge(g, "numeric:1", "+:1", ret, "e1")
  add_edge(g, "numeric:2", "+:1", ret, "e2")
  
  h = record(1+1)
  expect_equal(h, g)
  
  h = record({
    x = 1
    y = 1
    z = x+y
  })
  expect_equal(h, g)
})

test_that("record parenthesized arithmetic expression", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), ret)
  add_node(g, "numeric:2", list(), ret)
  add_node(g, "numeric:3", list(), ret)
  add_node(g, "+:1", c("e1","e2"), ret)
  add_edge(g, "numeric:2", "+:1", ret, "e1")
  add_edge(g, "numeric:3", "+:1", ret, "e2")
  add_node(g, "*:1", c("e1","e2"), ret)
  add_edge(g, "numeric:1", "*:1", ret, "e1")
  add_edge(g, "+:1", "*:1", ret, "e2")
  
  h = record(2*(1+1))
  expect_equal(h, g)
})