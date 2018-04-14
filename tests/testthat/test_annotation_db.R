context("annotation_db")

library(dplyr)

test_that("annotations are loaded from the remote DB", {
  db = remote_annotation_db$new()
  expect_equal(db$tbl() %>% collect %>% nrow, 0)
  
  packages = db$list_packages()
  expect_true("base" %in% packages)
  expect_true("stats" %in% packages)
  expect_false("sklearn" %in% packages)
  
  db$load_package("base")
  note = db$annotation("base/data-frame")
  expect_equal(note$class, "data.frame")
  
  base_tbl = db$tbl() %>% filter(package=="base") %>% collect
  expect_gt(nrow(base_tbl), 0)
  notes = db$annotations(base_tbl)
  expect_equal(length(notes), nrow(base_tbl))
})