
#' Class system of object
#' 
#' @description Which class system does the object use?
#' @return Returns one of "S3", "S4", "R5", "R6".
class_system <- function(x) {
  if (is.R6(x))
    "R6"
  else if (isS4(x))
    tryCatch({ x$getRefClass; "R5"}, error=function(e) "S4")
  else
    "S3"
}