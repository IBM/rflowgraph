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
    annotate_call = function(call) {
      stopifnot(rlang::is_call(call))
      switch(rlang::lang_type_of(call),
        named = {
          package = fun_package(rlang::call_fn(call))
          name = rlang::call_name(call)
        },
        namespaced = {
          c(package, name) %<-% map(rlang::call_args(call[[1]]), as.character)
        },
        stop("Only named or namespaced calls can be annotated")
      )
      self$annotate_function(name, package)
    },
    annotate_function = function(name, package) {
      # Load annotations for package, if not already loaded.
      private$load_package(package)
      
      # Query DB for annotations matching package and function name.
      db = private$db
      match = db$tbl() %>%
        dplyr::filter(kind=="morphism", package==package, `function`==name) %>%
        dplyr::collect()
      if (nrow(match) > 0) {
        if (nrow(match) > 1) {
          warning("Multiple annotations match function: ", package, "::", name)
        }
        db$annotation(match[[1,"key"]])
      }
    },
    annotate_object = function(x) {
      self$annotate_type(class(x), class_system(x))
    },
    annotate_type = function(classes, system="S3") {
      # Query DB for annotations matching any of the classes.
      db = private$db
      matches = db$tbl() %>%
        dplyr::filter(kind=="object", system==system, class %in% classes) %>%
        dplyr::collect()

      # Return annotation for the most specific annotated class, if any.
      for (cls in classes) {
        match = matches %>% dplyr::filter(class==cls)
        if (nrow(match) > 0) {
          if (nrow(match) > 1) {
            # When multiple annotations match the same class, return the first one.
            warning("Multiple annotations match class: ", cls)
          }
          return(db$annotation(match[[1,"key"]]))
        }
      }
    }
  ),
  private = list(
    db = NULL,
    loaded = NULL,
    load_package = function(package) {
      loaded = private$loaded
      if (!get_default(loaded, package, TRUE)) {
        private$db$load_package(package)
        loaded[[package]] = TRUE
      }
    }
  )
)