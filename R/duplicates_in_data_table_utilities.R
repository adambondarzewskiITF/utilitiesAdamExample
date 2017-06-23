#' Title
#'
#' @param DT 
#' @param cols 
#'
#' @return
#' @export
#'
#' @examples
return_all_duplicated_rows <- function(DT, cols) {
  
  duplicate_entries <- DT[duplicated(DT, by = cols), cols, with = FALSE]
  
  duplicate_entries <- unique(duplicate_entries, by = cols)
  
  merge(DT, duplicate_entries, by = cols)
  
}

#' Title
#'
#' @param DT 
#' @param cols 
#'
#' @return
#' @export
#'
#' @examples
divide_data_table_to_duplicated_and_not_duplicated <- function(DT, cols) {
  
  not_duplicate_entries <- DT[!duplicated(DT, by = cols), cols, with = FALSE]
  
  duplicate_entries <- DT[duplicated(DT, by = cols), cols, with = FALSE]
  
  duplicate_entries <- unique(duplicate_entries, by = cols)
  
  duplicate_entries[, column_auxiliary := 'not_null']
  
  DT_out <- merge(  DT, duplicate_entries, by = cols
                  , all.x = TRUE)
  
  stopifnot(nrow(DT) == nrow(DT_out))
  
  return(list(  duplicated_rows = DT_out[!is.na(column_auxiliary), !'column_auxiliary', with = FALSE]
              , unique_rows = DT_out[is.na(column_auxiliary), !'column_auxiliary', with = FALSE]
              ))
  
}

