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

num = list(class="numeric", system="S3", annotation="r/base/numeric")
int = list(class="integer", system="S3", annotation="r/base/integer")

port = function(data=list(), i)
  if (missing(i)) data else c(data, list(annotation_index=as.integer(i)))
out_port = function(...) set_names(list(port(...)), return_port)


test_that("annotate nodes of flow graph", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), out_port(), list(kind="literal"))
  add_node(g, "numeric:2", list(), out_port(), list(kind="literal"))
  add_node(g, "+:1", list(e1=port(i=1), e2=port(i=2)), out_port(i=1),
           list(`function`="+", package="base", annotation="r/base/plus"))
  add_edge(g, "numeric:1", "+:1", return_port, "e1")
  add_edge(g, "numeric:2", "+:1", return_port, "e2")
  
  h = record(1+1, node_data=TRUE, port_data=FALSE) %>%
    annotate(db=db, nodes=TRUE, ports=FALSE)
  expect_equal(h, g)
})

test_that("annotate ports of flow graph", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), out_port(num))
  add_node(g, "numeric:2", list(), out_port(num))
  add_node(g, "+:1", list(e1=num, e2=num), out_port(num))
  add_edge(g, "numeric:1", "+:1", return_port, "e1")
  add_edge(g, "numeric:2", "+:1", return_port, "e2")
  
  h = record(1+1, node_data=FALSE, port_data=TRUE) %>%
    annotate(db=db, nodes=FALSE, ports=TRUE)
  expect_equal(h, g)
})

test_that("annotate nodes and ports of flow graph", {
  g = wiring_diagram()
  add_node(g, "integer:1", list(), out_port(int), list(
    kind="literal", annotation="r/base/integer", annotation_kind="construct"))
  add_node(g, "integer:2", list(), out_port(int), list(
    kind="literal", annotation="r/base/integer", annotation_kind="construct"))
  add_node(g, "*:1", list(e1=port(int,1), e2=port(int,2)), out_port(int,1), list(
    `function`="*", package="base", annotation="r/base/times"))
  add_edge(g, "integer:1", "*:1", return_port, "e1")
  add_edge(g, "integer:2", "*:1", return_port, "e2")
  
  h = record(1L*2L, data=TRUE, values=FALSE) %>% annotate(db=db)
  expect_equal(h, g)
})