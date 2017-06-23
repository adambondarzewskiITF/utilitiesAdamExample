#' Title
#'
#' @param x 
#' @param file_path 
#'
#' @return
#' @export
#'
#' @examples
save_rds <- function(x, file_path) {
  saveRDS(x, file.path(file_path, paste0(deparse(substitute(x), '.rds'))))
}
