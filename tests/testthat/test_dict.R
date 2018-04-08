context("dict")

test_that("getting, setting, and deleting individual keys on dict", {
  d = dict()
  expect_equal(length(d), 0)
  expect_false(has_key(d, "foo"))
  expect_error(d[["foo"]], "key")
  expect_equal(get_default(d, "foo"), NULL)
  
  d[["foo"]] = 1
  expect_equal(length(d), 1)
  expect_true(has_key(d, "foo"))
  expect_equal(d[["foo"]], 1)
  expect_equal(get_default(d, "foo"), 1)
  
  d$bar = 2
  expect_equal(length(d), 2)
  expect_true(has_key(d, "bar"))
  expect_equal(d$bar, 2)
  
  del(d, "bar")
  expect_true(has_key(d, "foo"))
  expect_false(has_key(d, "bar"))
})

test_that("getting all keys or values of dict", {
  d = dict()
  expect_equal(keys(d), character())
  expect_equal(values(d), list())
  
  d[["foo"]] = 1
  d[["bar"]] = 2
  d[["baz"]] = 3
  expect_equal(sort(keys(d)), c("bar","baz","foo"))
  expect_equal(sort(as.numeric(values(d))), c(1,2,3))
})