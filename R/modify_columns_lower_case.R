#' Title
#'
#' @param DT 
#' @param aggregation_columns 
#' @param aggregation_key 
#' @param funs_chosen 
#' @param aggregation_functions 
#' @param cols_to_modify 
#'
#' @return
#' @export
#'
#' @examples
modify_columns_and_aggregate <- function( DT
                          , cols_to_modify
                          , funs_chosen
                          , aggregation_functions
                          , aggregation_columns
                          , aggregation_key) {
  
  
  tryCatch({
    cols_absent <- setdiff(  unlist(aggregation_columns)
                             , names(DT)
    )
    stopifnot(length(cols_absent) == 0)
  }, error = function(err) {
    pkg_logerror(sprintf('Col: %s not present in data table', cols_absent))
    stop(err)
  })
  
  tryCatch({
    cols_absent <- setdiff(  unlist(aggregation_key)
                             , names(DT)
    )
    stopifnot(length(cols_absent) == 0)
  }, error = function(err) {
    pkg_logerror(sprintf('Col: %s not present in data table', cols_absent))
    stop(err)
  })

  DT[, col_modified := FALSE]
  
  for (col_it in cols_to_modify) {
    
    DT[, col_old := get(col_it)]
    
    for (fun_chosen in funs_chosen) {
      DT[, eval(col_it) := fun_chosen(get(col_it))]
      DT[col_old != get(col_it), col_modified := TRUE]      
    }
    
    if (nrow(DT[col_old != get(col_it)]) > 0) {
      DT_logs <- unique(DT[col_old != get(col_it), .(get(col_it), col_old)])
      pkg_loginfo('Data modified for col %s: , full value: %s, shrinked value %s'
                  , col_it
                  , DT_logs[, col_old]
                  , DT_logs[, V1]
      )
      rm(DT_logs)      
    }
    
    DT[, col_old := NULL]
  }
  
  pkg_loginfo('Rows modified: %s.', nrow(DT[col_modified == TRUE]))
  
  # grouping
  
  pkg_loginfo('Aggregating data before shrinking....')
  
  
  DT[, col_modified := NULL]
  
  # Dividing data into two parts - duplicated and not duplicated
  DTS_parts <- divide_data_table_to_duplicated_and_not_duplicated(DT, cols = aggregation_key)

  DT_unique <- DTS_parts$unique_rows
  
  DT_duplicated <- DTS_parts$duplicated_rows
  
  DT_duplicated <- aggregate_cols(  DT_duplicated
                                  , functions = aggregation_functions
                                  , variables_function = aggregation_columns
                                  , variables_aggregation = aggregation_key)
  
  
  # combining both parts to get final output

  # some logging
  check_colnames(DT_duplicated, DT_unique)
  
  DT <- rbindlist(  list(DT_unique, DT_duplicated)
                    , use.names = TRUE)
  
  pkg_loginfo('Data aggregated after shrinking.')
  
  return(DT)
}

#' Title
#'
#' @param DT 
#' @param cols_to_modify 
#' @param aggregation_columns 
#' @param aggregation_key 
#' @param aggregation_functions 
#'
#' @return
#' @export
#'
#' @examples
modify_columns_lower_case <- function( DT
                                          , cols_to_modify
                                          , aggregation_functions
                                          , aggregation_columns
                                          , aggregation_key) {
  
  modify_columns_and_aggregate( DT
                                , cols_to_modify
                                , funs_chosen = list(tolower)
                                , aggregation_functions = aggregation_functions
                                , aggregation_columns = aggregation_columns
                                , aggregation_key = aggregation_key)
  
}

#' Title
#'
#' @param DT 
#' @param cols_to_modify 
#' @param aggregation_functions 
#' @param aggregation_columns 
#' @param aggregation_key 
#'
#' @return
#' @export
#'
#' @examples
modify_columns_clean <- function( DT
                                       , cols_to_modify
                                       , aggregation_functions
                                       , aggregation_columns
                                       , aggregation_key) {
  
  # creating auxiliary functions to run...
  stri_trans_general_auxiliary <- function(x) {stri_trans_general(  x
                                                                  , id = "latin-ascii")}
  # making use of prepared functions...
  modify_columns_and_aggregate( DT
                                , cols_to_modify
                                , funs_chosen = list(str_trim, stri_trans_general_auxiliary)
                                , aggregation_functions = aggregation_functions
                                , aggregation_columns
                                , aggregation_key)
  
}