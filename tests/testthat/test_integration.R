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

df_port = list(class="data.frame", system="S3", annotation="r/base/data-frame")
num_port = list(class="numeric", system="S3", annotation="r/base/numeric")
int_port = list(class="integer", system="S3", annotation="r/base/integer")
chr_port = list(class="character", system="S3", annotation="r/base/character")
lgl_port = list(class="logical", system="S3", annotation="r/base/logical")
out_port = function(data) set_names(list(data), return_port)


test_that("record k-means clustering on Iris data", {
  h = record_expr(file(file.path("data", "clustering_kmeans.R")),
                  cwd="data", annotate=TRUE, values=FALSE)
  write_graphml(h, file.path("data", "clustering_kmeans.xml"))
  expect_equal(names(iris), c("SepalLength","SepalWidth","PetalLength","PetalWidth"))
  expect_is(centroids, "matrix")
  expect_is(clusters, "integer")
  
  kmeans_port = list(class="kmeans", system="S3", annotation="r/stats/k-means")
  g = wiring_diagram()
  add_node(g, "character:1", list(), out_port(chr_port),
           list(annotation="r/base/character", annotation_kind="construct"))
  add_node(g, "logical:1", list(), out_port(lgl_port),
           list(annotation="r/base/logical", annotation_kind="construct"))
  add_node(g, "read.csv:1",
           list(file=chr_port, stringsAsFactors=lgl_port), out_port(df_port),
           list(`function`="read.csv", package="utils", annotation="r/utils/read-csv"))
  add_node(g, "names:1", list(x=df_port), out_port(chr_port),
           list(`function`="names", package="base"))
  add_node(g, "character:2", list(), out_port(chr_port),
           list(annotation="r/base/character", annotation_kind="construct"))
  add_node(g, "!=:1", list(e1=chr_port, e2=chr_port),
           out_port(lgl_port), list(`function`="!=", package="base"))
  add_node(g, "[:1", list(`1`=df_port, `3`=lgl_port),
           out_port(df_port), list(`function`="[", package="base"))
  add_node(g, "numeric:1", list(), out_port(num_port),
           list(annotation="r/base/numeric", annotation_kind="construct"))
  add_node(g, "kmeans:1",
           list(x=df_port, centers=num_port), out_port(kmeans_port),
           list(`function`="kmeans", package="stats", annotation="r/stats/fit-k-means"))
  add_node(g, "character:3", list(), out_port(chr_port),
           list(annotation="r/base/character", annotation_kind="construct"))
  add_node(g, "$:1", list(`1`=kmeans_port, `2`=chr_port), 
           out_port(list(class="matrix", system="S3", annotation="r/base/matrix")),
           list(`function`="$", package="base"))
  add_node(g, "character:4", list(), out_port(chr_port),
           list(annotation="r/base/character", annotation_kind="construct"))
  add_node(g, "$:2", list(`1`=kmeans_port, `2`=chr_port), out_port(int_port),
           list(`function`="$", package="base"))
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