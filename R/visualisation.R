#' Title
#'
#' @param cols_colour 
#' @param DT 
#' @param cols_hidden 
#' @param cols_bold 
#' @param page_length 
#' @param row_call_back 
#'
#' @return
#' @export
#'
#' @examples
produce_datatable <- function(DT, cols_colour = NULL, cols_hidden = c()
                              , cols_bold = c(), page_length = 100
                              , row_call_back = NULL
                              , footer_call_back = NULL
                              , filter = list(position = "top", clear = T, plain = F)
                              , ...) {
  
  container <- htmltools::withTags(table(
    tableHeader(DT),
    tableFooter(DT)
  ))
  
  options_prepared <- list(dom = "Bfrtip", buttons = c("colvis", 
                                                       "csv"), columnDefs = list(list(targets = cols_hidden, visible = FALSE)), 
                           pageLength = page_length)
  
  if (!is.null(row_call_back)) {
    options_prepared <- c(options_prepared, list(rowCallback = row_call_back))
  }
  
  if (!is.null(footer_call_back)) {
    options_prepared <- c(options_prepared, list(footerCallback = footer_call_back))
  }
  
  datatable(  DT, escape = FALSE, extensions = c("Buttons", "ColReorder")
            # , container = container
            , options = options_prepared
            , filter = filter
            , colnames = names(DT)
            , ...) %>% 
    formatStyle(columns = cols_bold, fontWeight = "bold") %>% formatStyle(columns = cols_colour, 
                                                                    backgroundColor = "  #efbbc5")
}
