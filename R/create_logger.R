#' @param name 
#'
#' @param logging_level 
#' @param log_file 
#'
#' @title Creates logger
#' @export
#'
#'

create_logger <- function(name, logging_level, log_file) {
  logger <- getLogger(name)
  setLevel(level = logging_level, container = logger)
  #addHandler(writeToConsole, level = logging_level, logger=name)
  addHandler(writeToFile, level = logging_level, logger = name, file = log_file)
  return(getLogger(name))
}
