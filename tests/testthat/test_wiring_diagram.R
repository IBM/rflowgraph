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

context("wiring_diagram")

test_that("add boxes and wires in a wiring diagram", {
  g = wiring_diagram(c("x1","x2"), c("y1","y2"))
  expect_true(is_directed(g))
  expect_equal(nodes(g), character())
  expect_equal(nnodes(g), 0)
  expect_true(has_node(g, input_node(g)))
  expect_true(has_node(g, output_node(g)))
  expect_equal(input_ports(g), c("x1","x2"))
  expect_equal(output_ports(g), c("y1","y2"))
  
  add_node(g, "foo", c("w1","w2"), c("z1","z2"))
  expect_equal(nodes(g), c("foo"))
  expect_equal(nnodes(g), 1)
  expect_equal(edges(g), list())
  expect_equal(nedges(g), 0)
  expect_true(has_node(g, "foo"))
  expect_equal(input_ports(g, "foo"), c("w1","w2"))
  expect_equal(output_ports(g, "foo"), c("z1","z2"))
  
  add_edge(g, input_node(g), "foo", "x1", "w1")
  add_edge(g, input_node(g), "foo", "x2", "w2")
  add_edge(g, "foo", output_node(g), "z1", "y1")
  add_edge(g, "foo", output_node(g), "z2", "y2")
  expect_equal(nedges(g), 4)
  edge = edges(g)[[1]]
  expect_equal(source_port(g, edge), "x1")
  expect_equal(target_port(g, edge), "w1")
  expect_equal(source_port(g, input_node(g), "foo", 1), "x1")
  expect_equal(target_port(g, input_node(g), "foo", 1), "w1")
  expect_equal(source_port(g, "foo", output_node(g), 2), "z2")
  expect_equal(target_port(g, "foo", output_node(g), 2), "y2")
})

test_that("get and set graph/node/port/edge attributes in a wiring diagram", {
  g = wiring_diagram("x", "z", data=list(foo=TRUE))
  expect_equal(graph_attr(g, "foo"), TRUE)
  expect_equal(graph_attr(g, "bar"), NULL)
  expect_equal(input_port_attr(g, input_node(g), "x", "foo"), NULL)
  expect_equal(output_port_attr(g, output_node(g), "z", "foo"), NULL)
  
  input_port_attr(g, input_node(g), "x", "foo") <- TRUE
  output_port_attr(g, output_node(g), "z", "foo") <- FALSE
  graph_attr(g, "bar") <- FALSE
  expect_equal(graph_attr(g, "foo"), TRUE)
  expect_equal(graph_attr(g, "bar"), FALSE)
  expect_equal(input_port_attr(g, input_node(g), "x", "foo"), TRUE)
  expect_equal(output_port_attr(g, output_node(g), "z", "foo"), FALSE)
  
  add_node(g, "f", "x", "y", data=list(foo=TRUE))
  add_node(g, "g", "y", "z")
  input_port_data(g, "f", "x") <- list(biz=TRUE)
  output_port_data(g, "f", "y") <- list(biz=FALSE)
  expect_equal(input_port_data(g, "f", "x"), list(biz=TRUE))
  expect_equal(output_port_data(g, "f", "y"), list(biz=FALSE))
  expect_equal(input_port_data(g, "g", "y"), list())
  expect_equal(output_port_data(g, "g", "z"), list())
  expect_equal(input_port_attr(g, "f", "x", "biz"), TRUE)
  expect_equal(output_port_attr(g, "f", "y", "biz"), FALSE)
  expect_equal(node_attr(g, "f", "foo"), TRUE)
  expect_equal(node_attr(g, "g", "foo"), NULL)
  
  input_port_attr(g, "f", "x", "biz") <- FALSE
  output_port_attr(g, "f", "y", "biz") <- TRUE
  node_attr(g, "g", "foo") <- TRUE
  node_attr(g, "g", "bar") <- FALSE
  expect_equal(input_port_attr(g, "f", "x", "biz"), FALSE)
  expect_equal(output_port_attr(g, "f", "y", "biz"), TRUE)
  expect_equal(node_attr(g, "g", "foo"), TRUE)
  expect_equal(node_attr(g, "g", "bar"), FALSE)
  
  add_edge(g, "f", "g", "y", "y", data=list(baz="baz"))
  edge_attr(g, "f", "g", 1, "bar") <- TRUE
  expect_equal(edge_attr(g, "f", "g", 1, "foo"), NULL)
  expect_equal(edge_attr(g, "f", "g", 1, "bar"), TRUE)
  expect_equal(edge_attr(g, "f", "g", 1, "baz"), "baz")
})