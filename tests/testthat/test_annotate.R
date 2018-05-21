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
chr = list(class="character", system="S3")

literal = function(key) {
  if (missing(key))
    list(kind="literal")
  else
    list(kind="literal", annotation=key, annotation_kind="construct")
}

port = function(data=list(), i)
  if (missing(i)) data else c(data, list(annotation_index=as.integer(i)))
out_port = function(...) set_names(list(port(...)), return_port)


test_that("annotate nodes of flow graph", {
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), out_port(), literal())
  add_node(g, "numeric:2", list(), out_port(), literal())
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
  add_node(g, "integer:1", list(), out_port(int,1), literal("r/base/integer"))
  add_node(g, "integer:2", list(), out_port(int,1), literal("r/base/integer"))
  add_node(g, "*:1", list(e1=port(int,1), e2=port(int,2)), out_port(int,1),
           list(`function`="*", package="base", annotation="r/base/times"))
  add_edge(g, "integer:1", "*:1", return_port, "e1")
  add_edge(g, "integer:2", "*:1", return_port, "e2")
  
  h = record(1L*2L, data=TRUE) %>% annotate(db=db)
  expect_equal(h, g)
})

test_that("annotate slot access in flow graph", {
  htest = list(class="htest", system="S3", annotation="r/stats/htest")
  g = wiring_diagram()
  add_node(g, "character:1", list(), out_port(chr), literal())
  add_node(g, "$:1", list(`1`=htest, `2`=chr), out_port(num),
           list(`function`="$", package="base", slot="p.value",
                annotation="r/stats/htest", annotation_kind="slot", annotation_index=1L))
  add_edge(g, "character:1", "$:1", return_port, "2")
  
  htest = binom.test(28, 50)
  h = record(htest$p.value, data=TRUE) %>% annotate(db=db)
  expect_equal(h, g)
})

test_that("override output port annotation from function annotation", {
  summary_obj = list(class=c("summaryDefault", "table"), system="S3", 
                     annotation="r/base/summary-object")
  g = wiring_diagram()
  add_node(g, "numeric:1", list(), out_port(num,1), literal("r/base/numeric"))
  add_node(g, "summary:1", list(object=port(num,1)), out_port(summary_obj,1),
           list(`function`="summary", package="base", system="S3", 
                annotation="r/base/summary"))
  add_node(g, "class:1", list(x=summary_obj), out_port(chr),
           list(`function`="class", package="base"))
  add_edge(g, "numeric:1", "summary:1", return_port, "object")
  add_edge(g, "summary:1", "class:1", return_port, "x")
  
  h = record(class(summary(0)), data=TRUE) %>% annotate(db)
  expect_equal(h, g)
})

test_that("annotate is idempotent", {
  g = record(1+1, data=TRUE) %>% annotate(db=db)
  h = record(1+1, data=TRUE) %>% annotate(db=db) %>% annotate(db=db)
  expect_equal(h, g)
})