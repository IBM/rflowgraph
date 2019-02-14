# Copyright 2018 IBM Corp.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
    initialize = function(annotations=NULL) {
      if (inherits(annotations, "annotation_db")) {
        db = annotations
      } else if (is.null(annotations)) {
        db = remote_annotation_db$new()
      } else {
        db = annotation_db$new()
        db$load_json(annotations)
      }
      private$db = db
      private$loaded = loaded = dict()
      
      if (inherits(db, "remote_annotation_db")) {
        for (package in db$list_packages())
          loaded[[package]] = FALSE
      }
    },
    annotation = function(...) private$db$annotation(...),
    annotate_function = function(name, package, class=NULL, system=NULL) {
      # Query DB for any annotations matching package and function.
      match = private$db$tbl() %>%
        dplyr::filter(kind == "function",
                      package == !! package, `function` == name) %>%
        dplyr::collect()
      
      # First, try to match a method.
      class_na = is.na(match$class)
      key = if (!(all(class_na) || is.null(class))) {
        match[!class_na,] %>%
          private$filter_best_class(class, system) %>%
          private$select_first()
      }
      
      # If that fails, try to match an ordinary function.
      key %||% (
        match[class_na,] %>%
          private$select_first()
      )
    },
    annotate_object = function(x) {
      # Ideally, we would filter by package but it's generally not possible
      # to determine the package where an R class is defined, because R has
      # an informal class system.
      #
      # It's certainly not possible for S3, which is completely informal.
      # Apparently, it's not possible for R6 either, though for no good reason:
      # https://github.com/r-lib/R6/issues/144
      #
      # It is possible for S4 and R5 via the prescription
      #   attr(class(x), "package")
      # but the benefit of handling this case seems marginal, given how rare
      # S4 classes are in practice.
      self$annotate_type(class(x), class_system(x))
    },
    annotate_type = function(class, system=NULL) {
      private$db$tbl() %>%
        dplyr::filter(kind == "type",
                      system == !! (system %||% "S3"), class %in% !! class) %>%
        dplyr::collect() %>%
        private$filter_best_class(class, system) %>%
        private$select_first()
    },
    load_packages = function(pkgs) {
      loaded = private$loaded
      pkgs = discard(pkgs, function(pkg) get_default(loaded, pkg, TRUE))
      if (!is_empty(pkgs)) {
        private$db$load_packages(pkgs)
        for (pkg in pkgs)
          loaded[[pkg]] = TRUE
      }
    }
  ),
  private = list(
    db = NULL,
    loaded = NULL,
    filter_best_class = function(df, class, system=NULL) {
      # Return annotation for the most specific annotated class, if any.
      for (cls in class) {
        filtered = df %>%
          dplyr::filter(system == !! (system %||% "S3"), class == !! cls)
        if (nrow(filtered) > 0)
          return(filtered)
      }
      df[FALSE,]
    },
    select_first = function(df) {
      if (nrow(df) > 0) {
        key = df[[1,"key"]]
        if (nrow(df) > 1)
          warning("Ambiguously selected annotation: ", key)
        key
      }
    }
  )
)
