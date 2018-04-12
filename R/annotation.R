#' Annotation DB
#' 
#' @description Database of class, function, and method annotations.
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
      dbplyr:::db_create_indexes(conn, "annotatations", list(
        c("package", "kind", "system", "class", "method"),
        c("package", "kind", "function")
      ))
      private$notes = dict()
    },
    finalize = function() {
      DBI::dbDisconnect(private$conn)
    },
    annotation = function(key) {
      privates$notes[[key]]
    },
    annotations = function(keys) {
      if (is.data.frame(keys)) keys = keys$keys
      stopifnot(is.character(keys))
      map(keys, ~ private$notes[[.]])
    },
    table = function() {
      dplyr::tbl(conn, "annotations")
    },
    load_documents = function(docs) {
      required = c("package", "id", "kind")
      optional = c("system", "class", "method", "function")
      df = map_dfr(docs, function(doc) {
        key = paste(doc$package, "/", doc$id, sep="")
        private$notes[[key]] = doc
        c(list(key=key),
          set_names(map(required, ~ get_default(doc,.,stop("bad note")), required)),
          set_names(map(optional, ~ get_default(doc,.,NA_character_), optional)))
      })
      DBI::dbWriteTable(conn, "annotations", df, append=TRUE)
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