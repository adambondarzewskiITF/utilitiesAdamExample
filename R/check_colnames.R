#' Function checks if two tables has the same columns and returns any columns that are not present in one of the tables
#'
#' @param DT1 data.table; first table to inspect
#' @param DT2 data.table; the second table to inspect
#'
#' @return
#' @export
#'
#' @examples
check_colnames <- function(DT1, DT2) {
  
  dif1 <- setdiff(names(DT1), names(DT2))
  pkg_loginfo(sprintf('Cols in %s which are not present in %s: %s', deparse(substitute(DT1)), deparse(substitute(DT2)), dif1))
  dif2 <- setdiff(names(DT2), names(DT1))
  pkg_loginfo(sprintf('Cols in %s which are not present in %s: %s', deparse(substitute(DT2)), deparse(substitute(DT1)), dif2))
}