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
        method = character(),
        `function` = character()
      )
      DBI::dbWriteTable(conn, "annotations", df)
      dplyr::db_create_indexes(conn, "annotations", list(
        c("package", "kind", "system", "class", "method"),
        c("package", "kind", "function")
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
      notes = private$notes
      required = c("package", "id", "kind")
      optional = c("system", "class", "method", "function")
      df = map_dfr(docs, function(doc) {
        stopifnot(doc$schema == "annotation" && doc$language == "r")
        key = paste(doc$package, "/", doc$id, sep="")
        if (has_key(notes, key))
          stop(paste("Annotation already loaded:", key))
        notes[[key]] = doc
        
        c(list(key=key),
          set_names(map(required, ~ get_default(doc,.,stop("bad note"))), required),
          set_names(map(optional, ~ get_default(doc,.,NA_character_)), optional))
      })
      DBI::dbWriteTable(private$conn, "annotations", df, append=TRUE)
    },
    load_json = function(txt) {
      self$load_documents(jsonlite::fromJSON(txt))
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
    initialize = function(config=NULL) {
      if (is.null(config)) config = default_remote_annotation_db_config
      i = match("dbname", names2(config))
      stopifnot(!is.na(i))
      private$cushion = invoke(sofa::Cushion$new, config[-i])
      private$dbname = config[[i]]
      super$initialize()
    },
    list_packages = function() {
      # Row keys are triples (schema, language, package).
      result = sofa::design_search(private$cushion, private$dbname, "query",
                                   "annotation_index", list(group=TRUE))
      result$rows %>%
        keep(function(row) row$key[[2]] == "r") %>%
        map_chr(function(row) row$key[[3]])
    },
    load_all_packages = function() {
      result = sofa::db_query(private$cushion, private$dbname, selector=list(
        schema = "annotation",
        language = "r"
      ))
      self$load_documents(result$docs)
    },
    load_package = function(package) {
      result = sofa::db_query(private$cushion, private$dbname, selector=list(
        schema = "annotation",
        language = "r",
        package = package
      ))
      self$load_documents(result$docs)
    }
  ),
  private = list(
    cushion = NULL,
    dbname = character()
  )
)

default_remote_annotation_db_config <- list(
  host = "d393c3b5-9979-4183-98f4-7537a5de15f5-bluemix.cloudant.com",
  port = NULL,
  transport = "https",
  dbname = "data-science-ontology"
)