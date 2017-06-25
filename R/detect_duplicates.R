#' Title
#'
#' @param DT data.table; table to inspect
#' @param cols vector of characters; columns with primary key (for sure? :D)
#'
#' @return
#' @export
#'
#' @examples
detect_duplicates <- function(DT, cols) {
  tryCatch({
    stopifnot(nrow(DT) == nrow(unique(DT, by = cols)))    
  }
  , error = function(err) {
    pkg_logerror(return_all_duplicated_rows(DT = DT, cols = cols))
    pkg_logerror('Duplicates in table detected.')
    stop('Duplicates in table detected.')
  })
}