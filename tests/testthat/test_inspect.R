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

context("inspect")

ExampleClass = setClass("ExampleClass", representation(name="character"))
ExampleRefClass = setRefClass("ExampleRefClass")

test_that("get class system of object", {
  expect_equal(class_system(1), "S3")
  expect_equal(class_system(NULL), "S3")
  expect_equal(class_system(data.frame(x=1, y=2)), "S3")
  
  expect_equal(class_system(ExampleClass()), "S4")
  expect_equal(class_system(ExampleRefClass$new()), "R5")
  
  expect_equal(class_system(dict()), "S3")
  expect_equal(class_system(ordered_dict()), "R6")
})

test_that("get arguments of function", {
  expect_equal(names(fun_args(is.function)), "x") # primitive
  expect_equal(names(fun_args(is.primitive)), "x") # not primitive
})

test_that("get package of function", {
  expect_equal(fun_package(is.function), "base") # primitive
  expect_equal(fun_package(is.primitive), "base") # not primitive
  expect_equal(fun_package(lm), "stats")
  
  pkg = packageName()
  expect_equal(fun_package(ordered_dict), pkg)
  expect_equal(fun_package(ordered_dict_class$new), pkg)
  expect_equal(fun_package(ordered_dict_class$new()$clone), pkg)
  expect_equal(fun_package(ExampleClass), pkg)
  expect_equal(fun_package(ExampleRefClass$new), pkg)
})

test_that("get package of object", {
  pkg = packageName()
  expect_equal(obj_package(dict()), NULL)
  expect_equal(obj_package(ExampleClass()), pkg)
  expect_equal(obj_package(ExampleRefClass$new()), pkg)
})

test_that("match arguments of function call", {
  matched = list(formula=y~x, data=quote(df))
  expect_equal(call_args_match(quote(lm(y~x, df))), matched)
  
  matched = list(formula=y~x, data=quote(df), method="qr")
  expect_equal(call_args_match(quote(lm(y~x, df, method="qr"))), matched)
  expect_equal(call_args_match(quote(lm(y~x, method="qr", df))), matched)
  expect_equal(call_args_match(quote(lm(data=df, method="qr", y~x))), matched)
})