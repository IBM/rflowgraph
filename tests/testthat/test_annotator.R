context("annotator")

db = annotation_db$new()
db$load_json(system.file("extdata", "tests", "annotations.json",
                         package="opendisc", mustWork=TRUE))

test_that("annotate objects by class", {
  notator = annotator$new(db)
  notate = function(x) notator$annotate_object(x)
  
  # Builtin S3 classes.
  expect_equal(notate(0L)$id, "integer")
  expect_equal(notate(0)$id, "numeric")
  
  # Inheritance in S3.
  expect_equal(notate(structure(0,class="lm"))$id, "lm")
  expect_equal(notate(structure(0,class=c("glm","lm")))$id, "glm")
  expect_equal(notate(structure(0,class=c("my-lm","lm")))$id, "lm")
})