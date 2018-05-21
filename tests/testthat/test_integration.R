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

context("integration")

df = list(class="data.frame", system="S3", annotation="r/base/data-frame")
mat = list(class="matrix", system="S3", annotation="r/base/matrix")
num = list(class="numeric", system="S3", annotation="r/base/numeric")
int = list(class="integer", system="S3", annotation="r/base/integer")
chr = list(class="character", system="S3", annotation="r/base/character")
lgl = list(class="logical", system="S3", annotation="r/base/logical")

literal = function(key) {
  if (missing(key))
    list(kind="literal")
  else
    list(kind="literal", annotation=key, annotation_kind="construct")
}

port = function(data=list(), i)
  if (missing(i)) data else c(data, list(annotation_index=as.integer(i)))
out_port = function(...) set_names(list(port(...)), return_port)


test_that("record k-means clustering on Iris data", {
  h = record_expr(file(file.path("data", "clustering_kmeans.R")),
                  cwd="data", annotate=TRUE)
  write_graphml(h, file.path("data", "clustering_kmeans.xml"))
  expect_equal(names(iris), c("SepalLength","SepalWidth","PetalLength","PetalWidth"))
  expect_is(centroids, "matrix")
  expect_is(clusters, "integer")
  
  kmeans = list(class="kmeans", system="S3", annotation="r/stats/k-means")
  g = wiring_diagram()
  add_node(g, "character:1", list(), out_port(chr,1), literal("r/base/character"))
  add_node(g, "logical:1", list(), out_port(lgl,1), literal("r/base/logical"))
  add_node(g, "read.csv:1",
           list(file=port(chr,1), stringsAsFactors=port(lgl)), out_port(df,1),
           list(`function`="read.csv", package="utils", annotation="r/utils/read-csv"))
  add_node(g, "names:1", list(x=df), out_port(chr),
           list(`function`="names", package="base"))
  add_node(g, "character:2", list(), out_port(chr,1), literal("r/base/character"))
  add_node(g, "!=:1", list(e1=chr, e2=chr),
           out_port(lgl), list(`function`="!=", package="base"))
  add_node(g, "[:1", list(`1`=df, `3`=lgl),
           out_port(df), list(`function`="[", package="base"))
  add_node(g, "numeric:1", list(), out_port(num,1), literal("r/base/numeric"))
  add_node(g, "kmeans:1",
           list(x=port(df,2), centers=port(num,1)), out_port(kmeans,1),
           list(`function`="kmeans", package="stats", annotation="r/stats/fit-k-means"))
  add_node(g, "character:3", list(), out_port(chr,1), literal("r/base/character"))
  add_node(g, "$:1", list(`1`=kmeans, `2`=chr), out_port(mat),
           list(`function`="$", package="base", slot="centers",
                annotation="r/stats/k-means", annotation_kind="slot", annotation_index=2L))
  add_node(g, "character:4", list(), out_port(chr,1), literal("r/base/character"))
  add_node(g, "$:2", list(`1`=kmeans, `2`=chr), out_port(int),
           list(`function`="$", package="base", slot="cluster",
                annotation="r/stats/k-means", annotation_kind="slot", annotation_index=1L))
  add_edge(g, "character:1", "read.csv:1", return_port, "file")
  add_edge(g, "logical:1", "read.csv:1", return_port, "stringsAsFactors")
  add_edge(g, "read.csv:1", "names:1", return_port, "x")
  add_edge(g, "read.csv:1", "[:1", return_port, "1")
  add_edge(g, "names:1", "!=:1", return_port, "e1")
  add_edge(g, "character:2", "!=:1", return_port, "e2")
  add_edge(g, "!=:1", "[:1", return_port, "3")
  add_edge(g, "[:1", "kmeans:1", return_port, "x")
  add_edge(g, "numeric:1", "kmeans:1", return_port, "centers")
  add_edge(g, "kmeans:1", "$:1", return_port, "1")
  add_edge(g, "kmeans:1", "$:2", return_port, "1")
  add_edge(g, "character:3", "$:1", return_port, "2")
  add_edge(g, "character:4", "$:2", return_port, "2")
  expect_equal(h, g)
})