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

context("annotator")

annotator = annotator$new(file.path("data", "annotations.json"))

test_that("annotate objects by class", {
  annotate = function(x) annotator$annotate_object(x)
  
  # Builtin S3 classes.
  expect_equal(annotate(0L), "r/base/integer")
  expect_equal(annotate(0), "r/base/numeric")
  
  # Inheritance in S3.
  expect_equal(annotate(structure(0,class="lm")), "r/stats/lm")
  expect_equal(annotate(structure(0,class=c("glm","lm"))), "r/stats/glm")
  expect_equal(annotate(structure(0,class=c("my-lm","lm"))), "r/stats/lm")
})

test_that("annotate functions", {
  annotate = function(x, env=rlang::caller_env()) {
    info = inspect_call(substitute(x), env)
    annotator$annotate_function(info$name, info$package)
  }
  
  # Named call.
  expect_equal(annotate(lm(y~x-1, df)), "r/stats/fit-lm")
  
  # Namespaced call.
  expect_equal(annotate(stats::lm(y~x-1, df)), "r/stats/fit-lm")
})

test_that("annotate methods", {
  annotate = annotator$annotate_function
  
  expect_equal(annotate("predict", "stats"), "r/stats/predict")
  expect_equal(annotate("predict", "stats", class="lm"), "r/stats/predict-lm")
})