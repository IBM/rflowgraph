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