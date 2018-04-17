context("annotator")

db = annotation_db$new()
db$load_json(system.file("extdata", "tests", "annotations.json",
                         package="opendisc", mustWork=TRUE))

test_that("annotate objects by class", {
  annotator = annotator$new(db)
  annotate = function(x) annotator$annotate_object(x)
  
  # Builtin S3 classes.
  expect_equal(annotate(0L)$id, "integer")
  expect_equal(annotate(0)$id, "numeric")
  
  # Inheritance in S3.
  expect_equal(annotate(structure(0,class="lm"))$id, "lm")
  expect_equal(annotate(structure(0,class=c("glm","lm")))$id, "glm")
  expect_equal(annotate(structure(0,class=c("my-lm","lm")))$id, "lm")
})

test_that("annotation calls by function name and package", {
  annotator = annotator$new(db)
  annotate = function(x) annotator$annotate_call(substitute(x))
  
  # Named call.
  expect_equal(annotate(lm(y~x-1, df))$id, "lm-fit")
  
  # Namespaced call.
  expect_equal(annotate(stats::lm(y~x-1, df))$id, "lm-fit")
})