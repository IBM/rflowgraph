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

context("annotate")

db = annotation_db$new()
db$load_json(file.path("data", "annotations.json"))

ret = "__return__"

test_that("annotate nodes of flow graph", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), ret, list(kind="literal"))
  add_node(g, "numeric:2", list(), ret, list(kind="literal"))
  add_node(g, "+:1", c("e1","e2"), ret, list(
    name="+", package="base", annotation="r/base/plus"))
  add_edge(g, "numeric:1", "+:1", ret, "e1")
  add_edge(g, "numeric:2", "+:1", ret, "e2")
  
  h = record(1+1, node_data=TRUE, port_data=FALSE) %>%
    annotate(db=db, nodes=TRUE, ports=FALSE)
  expect_equal(h, g)
})

test_that("annotate ports of flow graph", {
  num_port = list(class="numeric", system="S3", annotation="r/base/numeric")
  num_out = set_names(list(num_port), ret)
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), num_out)
  add_node(g, "numeric:2", list(), num_out)
  add_node(g, "+:1", list(e1=num_port, e2=num_port), num_out)
  add_edge(g, "numeric:1", "+:1", ret, "e1")
  add_edge(g, "numeric:2", "+:1", ret, "e2")
  
  h = record(1+1, node_data=FALSE, port_data=TRUE) %>%
    annotate(db=db, nodes=FALSE, ports=TRUE)
  expect_equal(h, g)
})

test_that("annotate nodes and ports of flow graph", {
  num_port = list(class="numeric", system="S3", annotation="r/base/numeric")
  num_out = set_names(list(num_port), ret)
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), num_out, list(
    annotation="r/base/numeric", annotation_kind="construct"
  ))
  add_node(g, "numeric:2", list(), num_out, list(
    annotation="r/base/numeric", annotation_kind="construct"
  ))
  add_node(g, "+:1", list(e1=num_port, e2=num_port), num_out, list(
    name="+", package="base", annotation="r/base/plus"
  ))
  add_edge(g, "numeric:1", "+:1", ret, "e1")
  add_edge(g, "numeric:2", "+:1", ret, "e2")
  
  h = record(1+1, data=TRUE, values=FALSE) %>% annotate(db=db)
  expect_equal(h, g)
})