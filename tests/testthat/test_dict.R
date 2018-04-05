context("dict")

test_that("getting, setting, and deleting individual keys on dict", {
  d = dict()
  expect_equal(length(d), 0)
  expect_false(has_key(d, "foo"))
  expect_equal(d[["foo"]], NULL)
  
  d[["foo"]] = 1
  expect_equal(length(d), 1)
  expect_true(has_key(d, "foo"))
  expect_equal(d[["foo"]], 1)
  
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
  d[["foo"]] = 1
  d[["bar"]] = 2
  d[["baz"]] = 3
  
  ks = sort(keys(d))
  expect_equal(ks, c("bar","baz","foo"))
  
  vs = values(d)[ks]
  expect_equal(length(vs), 3)
  expect_equal(vs[ks], c(bar=2, baz=3, foo=1))
})