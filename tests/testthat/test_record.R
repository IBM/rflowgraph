# Copyright 2018 IBM Corp.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

context("record")

ret = "__return__"

test_that("record literal values", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), ret)
  h = record(1)
  expect_equal(h, g)
})

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