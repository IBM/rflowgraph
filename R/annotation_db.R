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

#' Annotation DB
#' 
#' @description Database of class, function, and method annotations.
#' 
#' @section Methods:
#' \code{load_documents(docs)} \cr
#' Load a list of (parsed) JSON documents conforming to the JSON schema for
#' annotations.
#' 
#' \code{load_json(txt)} \cr
#' Load documents from a JSON string, URL, or file.
#' 
#' @name annotation_db
NULL

#' @importFrom dbplyr sql
#' @export
annotation_db <- R6Class("annotation_db",
  public = list(
    initialize = function() {
      private$conn = conn = DBI::dbConnect(RSQLite::SQLite(), path=":memory:")
      df = data_frame(
        key = character(),
        package = character(),
        id = character(),
        kind = character(),
        system = character(),
        class = character(),
        `function` = character()
      )
      DBI::dbWriteTable(conn, "annotations", df)
      dplyr::db_create_indexes(conn, "annotations", list(
        # Index for type annotations.
        c("kind", "system", "class"),
        # Index for function annotations.
        c("kind", "package", "function")
      ))
      private$notes = dict()
    },
    finalize = function() {
      DBI::dbDisconnect(private$conn)
    },
    annotation = function(key) {
      private$notes[[key]]
    },
    annotations = function(x) {
      keys = if (is.data.frame(x)) x$key else x
      stopifnot(is.character(keys))
      map(keys, ~ private$notes[[.]])
    },
    tbl = function() {
      dplyr::tbl(private$conn, "annotations")
    },
    load_documents = function(docs) {
      df = self$prepare_table(docs)
      notes = private$notes
      walk2(docs, df$key, function(doc, key) {
        if (has_key(notes, key))
          stop("Annotation already loaded: ", key)
        notes[[key]] = doc
      })
      DBI::dbWriteTable(private$conn, "annotations", df, append=TRUE)
    },
    load_json = function(txt) {
      docs = jsonlite::fromJSON(txt, simplifyDataFrame=FALSE)
      self$load_documents(docs)
    },
    prepare_table = function(docs) {
      # Create data frame for annotation lookup.
      required = set_names(c("package", "id", "kind"))
      optional = set_names(c("system", "class", "function"))
      df = map_dfr(docs, function(doc) {
        stopifnot(doc$schema == "annotation" && doc$language == "r")
        key = paste(doc$language, doc$package, doc$id, sep="/")
        c(list(key=key),
          map(required, ~ get_default(doc,.,stop("Bad annotation: ", key))),
          map(optional, ~ get_default(doc,.,NA_character_)))
      })
      
      # Schema: Default class system is S3.
      df[!is.na(df$class) & is.na(df$system), "system"] = "S3"
      
      # Schema: Function names may be namespaced (e.g., "stats::predict"),
      # overriding the annotation's declared package. Useful for generics.
      split_fun = strsplit(df$`function`, "::", fixed=TRUE)
      is_namespaced = map_lgl(split_fun, ~ length(.) == 2)
      df[is_namespaced,"package"] = map_chr(split_fun[is_namespaced], ~.[[1]])
      df[is_namespaced,"function"] = map_chr(split_fun[is_namespaced], ~.[[2]])
      df
    }
  ),
  private = list(
    conn = NULL,
    notes = NULL
  )
)

#' Remote annotation DB
#' 
#' @description Annotation database that fetches annotations from a remote 
#' CouchDB database.
#' 
#' @section Methods:
#' \code{load_all_packages()} \cr
#' Load annotations for all R packages.
#' 
#' \code{load_package(package)} \cr
#' Load annotations for a specific R package.
#' 
#' @name remote_annotation_db
#' @seealso \code{\link{annotation_db}}
NULL

#' @export
remote_annotation_db <- R6Class("remote_annotation_db",
  inherit = annotation_db,
  public = list(
    initialize = function(api_url=NULL) {
      if (is.null(api_url))
        api_url = "https://api.datascienceontology.org"
      private$api_client = crul::HttpClient$new(url=api_url)
      super$initialize()
    },
    list_packages = function() {
      counts = private$api_get("/count/annotation/r")
      names(counts)
    },
    load_all_packages = function() {
      docs = private$api_get("/annotations/r")
      self$load_documents(docs)
    },
    load_packages = function(pkgs) {
      for (pkg in pkgs) {
        docs = private$api_get(paste0("/annotations/r/", pkg))
        self$load_documents(docs)
      }
    }
  ),
  private = list(
    api_client = NULL,
    api_get = function(endpoint) {
      response = private$api_client$get(endpoint)
      jsonlite::fromJSON(response$parse(), simplifyVector=FALSE)
    }
  )
)