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
    },
    annotate_object = function(x) {
      self$annotate_type(class(x), class_system(x))
    },
    annotate_type = function(classes, system="S3") {
      # Query DB for annotations matching any of the classes.
      db = private$db
      df = db$tbl() %>%
        dplyr::filter(kind=="object", system==system, class %in% classes) %>%
        dplyr::collect()

      # Return annotation for the most specific annotated class, if any.
      for (cls in classes) {
        match = df %>% dplyr::filter(class==cls)
        if (nrow(match) > 0) {
          if (nrow(match) > 1) {
            # When multiple annotations match the same class, return the first one.
            warning(paste("Multiple annotations matching class:", cls))
          }
          return(db$annotation(match[[1,"key"]]))
        }
      }
    }
  ),
  private = list(
    db = NULL,
    loaded = NULL,
    load = function(package) {
      loaded = private$loaded
      if (!get_default(loaded, package, TRUE)) {
        private$db$load_package(package)
        loaded[[package]] = TRUE
      }
    }
  )
)