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
  expect_equal(record(1), g)
  
  g = wiring_diagram()
  add_node(g, "formula:1", list(), ret)
  expect_equal(record(y~x-1), g)
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

test_that("record namespaced calls", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), ret)
  add_node(g, "exp:1", "x", ret)
  add_edge(g, "numeric:1", "exp:1", ret, "x")
  expect_equal(record(exp(1)), g)
  expect_equal(record(base::exp(1)), g)
})

test_that("record calls with ellipses", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), ret)
  add_node(g, "numeric:2", list(), ret)
  add_node(g, "sum:1", c("1","2"), ret)
  add_edge(g, "numeric:1", "sum:1", ret, "1")
  add_edge(g, "numeric:2", "sum:1", ret, "2")
  expect_equal(record(sum(1,1)), g)
})

test_that("record slot access", {
  value = function (v) list(value=v)
  out_value = function(v) set_names(list(value(v)), ret)
  
  x = list(foo=1, bar=2)
  g = wiring_diagram()
  add_node(g, "character:1", list(), out_value("foo"))
  add_node(g, "$:1", list(`1`=value(x), `2`=value("foo")), out_value(1))
  add_edge(g, "character:1", "$:1", ret, "2")
  
  h = record(x$foo, values=TRUE)
  expect_equal(h, g)
})

test_that("record and store node data", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), ret, list(kind="literal"))
  add_node(g, "numeric:2", list(), ret, list(kind="literal"))
  add_node(g, "+:1", c("e1","e2"), ret, list(name="+", package="base"))
  add_edge(g, "numeric:1", "+:1", ret, "e1")
  add_edge(g, "numeric:2", "+:1", ret, "e2")
  h = record(1+1, node_data=TRUE)
  expect_equal(h, g)
})

test_that("record and store port data", {
  num_data = list(class="numeric", system="S3")
  num_out = set_names(list(num_data), ret)
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), num_out)
  add_node(g, "numeric:2", list(), num_out)
  add_node(g, "+:1", list(e1=num_data, e2=num_data), num_out)
  add_edge(g, "numeric:1", "+:1", ret, "e1")
  add_edge(g, "numeric:2", "+:1", ret, "e2")
  h = record(1+1, port_data=TRUE)
  expect_equal(h, g)
})

test_that("record and store object values", {
  out_value = function(v) set_names(list(list(value=v)), ret)
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), out_value(1))
  add_node(g, "numeric:2", list(), out_value(1))
  add_node(g, "+:1", list(e1=list(value=1), e2=list(value=1)), out_value(2))
  add_edge(g, "numeric:1", "+:1", ret, "e1")
  add_edge(g, "numeric:2", "+:1", ret, "e2")
  h = record(1+1, values=TRUE)
  expect_equal(h, g)
})