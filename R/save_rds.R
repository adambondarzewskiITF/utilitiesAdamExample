#' Title
#'
#' @param x R object; object to save
#' @param file_path character; path to save R object
#'
#' @return
#' @export
#'
#' @examples
save_rds <- function(x, file_path) {
  saveRDS(x, file.path(file_path, paste0(deparse(substitute(x), '.rds'))))
}
