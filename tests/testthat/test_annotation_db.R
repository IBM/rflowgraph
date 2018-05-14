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

context("annotation_db")

library(dplyr)

test_that("annotations are loaded from a local file", {
  db = annotation_db$new()
  expect_equal(db$tbl() %>% collect %>% nrow, 0)
  
  db$load_json(file.path("data", "annotations.json"))
  base_tbl = db$tbl() %>% filter(package=="base") %>% collect
  expect_true("integer" %in% base_tbl$class)
  expect_true("numeric" %in% base_tbl$class)
  
  note = db$annotation("r/base/integer")
  expect_equal(note$class, "integer")
})

test_that("annotations are loaded from the remote DB", {
  db = remote_annotation_db$new()
  expect_equal(db$tbl() %>% collect %>% nrow, 0)
  
  packages = db$list_packages()
  expect_true("base" %in% packages)
  expect_true("stats" %in% packages)
  expect_false("sklearn" %in% packages)
  
  db$load_packages("base")
  note = db$annotation("r/base/data-frame")
  expect_equal(note$class, "data.frame")
  
  base_tbl = db$tbl() %>% filter(package=="base") %>% collect
  expect_gt(nrow(base_tbl), 0)
  notes = db$annotations(base_tbl)
  expect_equal(length(notes), nrow(base_tbl))
})