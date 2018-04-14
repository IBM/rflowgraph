#' Annotator
#' 
#' @description Assign annotations to R classes, functions, and methods.
#' 
#' @name annotator
#' @seealso \code{\link{annotation_db}}, \code{\link{remote_annotation_db}}
NULL

#' @export
annotator <- R6Class("annotator",
  public = list(
    initialize = function(db=NULL) {
      private$db = db = if (is.null(db)) remote_annotation_db$new() else db
      private$loaded = loaded = dict()
      
      stopifnot(inherits(db, "annotation_db"))
      if (inherits(db, "remote_annotation_db")) {
        for (package in db$list_packages())
          loaded[[package]] = FALSE
      }
    }
  ),
  private = list(
    db = NULL,
    loaded = NULL,
    load = function(package) {
      if (!get_default(private$loaded, package, TRUE)) {
        private$db$load_package(package)
        private$loaded[[package]] = TRUE
      }
    }
  )
)