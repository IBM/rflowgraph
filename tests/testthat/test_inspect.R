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