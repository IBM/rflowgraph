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

test_that("record k-means clustering on Iris data", {
  path = system.file("extdata", "tests", "datasets", "iris.csv",
                     package="opendisc", mustWork=TRUE)
  h = record({
    iris = read.csv(path, stringsAsFactors=FALSE)
    iris = iris[, names(iris) != "Species"]
    
    km = kmeans(iris, 3)
    centroids = km$centers
    clusters = km$cluster
  }, annotate=TRUE, values=FALSE)
  expect_equal(names(iris), c("SepalLength","SepalWidth","PetalLength","PetalWidth"))
  expect_is(centroids, "matrix")
  expect_is(clusters, "integer")
  # TODO: Check flow graph!
})