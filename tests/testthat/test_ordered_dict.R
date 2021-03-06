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

context("ordered_dict")

test_that("getting, setting, and deleting individual keys on ordered dict", {
  d = ordered_dict()
  expect_equal(length(d), 0)
  expect_false(has_key(d, "foo"))
  expect_error(d[["foo"]], "key")
  expect_equal(get_default(d, "foo"), NULL)
  
  d[["foo"]] = 1
  expect_equal(length(d), 1)
  expect_true(has_key(d, "foo"))
  expect_equal(d[["foo"]], 1)
  expect_equal(get_default(d, "foo"), 1)
  
  d[["bar"]] = 2
  expect_equal(length(d), 2)
  expect_true(has_key(d, "bar"))
  expect_equal(d[["bar"]], 2)
  
  del(d, "bar")
  expect_true(has_key(d, "foo"))
  expect_false(has_key(d, "bar"))
})

test_that("order is preserved in ordered dict", {
  d = ordered_dict()
  expect_equal(keys(d), character())
  expect_equal(values(d), list())
  
  d[["foo"]] = 1
  expect_equal(keys(d), c("foo"))
  expect_equal(values(d), list(1))
  
  d[["bar"]] = 2
  d[["baz"]] = 3
  expect_equal(keys(d), c("foo", "bar", "baz"))
  expect_equal(values(d), list(1, 2, 3))
  
  del(d, "foo")
  expect_equal(keys(d), c("bar", "baz"))
  expect_equal(values(d), list(2, 3))
  d[["bar"]] = 0
  d[["foo"]] = 4
  expect_equal(keys(d), c("bar", "baz", "foo"))
  expect_equal(values(d), list(0, 3, 4))
  
  d = ordered_dict(foo=1, bar=2, baz=3)
  expect_equal(keys(d), c("foo", "bar", "baz"))
  expect_equal(values(d), list(1, 2, 3))
})