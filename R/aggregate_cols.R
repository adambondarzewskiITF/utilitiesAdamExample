#' Title
#'
#' @param DT data.table; table to aggregate
#' @param functions list of functions; functions to apply to DT
#' @param variables_function list of characters; variables functions would be applied to, accordingly (1st function to the first element of list, etc)
#' @param variables_aggregation vector of characters; variables for aggregation
#'
#' @import data.table
#' @return DT_out data.table; aggregated table
#' @export
#'
#' @examples
aggregate_cols <- function(  DT
                             , functions
                             , variables_function
                             , variables_aggregation) {
  
  
  apply_function <- function(fun, variables, DT, aggregation) {
    DT[, lapply(.SD, fun), by = eval(aggregation), 
       .SDcols = variables]
  }
  
  # applying functions to variables
  DTS <- mapply(apply_function, fun = functions, variables = variables_function, MoreArgs = list(DT = DT, aggregation = variables_aggregation)
                , SIMPLIFY = FALSE)

  # DT_out <- as.data.table(DT_out)
  
  DT_out <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = variables_aggregation, all = TRUE),
                   DTS)
  
  return(DT_out)
}