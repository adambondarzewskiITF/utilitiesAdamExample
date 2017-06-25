#' Title
#'
#' @param DT data.table; table for check if there are duplicated cols present
#'
#' @return
#' @export
#'
#' @examples
stop_if_duplicated_cols <- function(DT) {
  tryCatch({
    stopifnot(sum(duplicated(names(DT))) == 0)
  }, error =function(err) {
    loggingUtilities:::pkg_logerror(sprintf('Duplicated cols in data.table %s detected', deparse(substitute(DT))))
    stop(err)
  })

}