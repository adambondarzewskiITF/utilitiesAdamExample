#' Title
#'
#' @param fun 
#' @param args 
#' @param condition 
#' @param tries 
#' @param wait 
#'
#' @return
#' @export
#'
#' @examples
try_several_times <- function(  fun
                              , args
                              , condition
                              , tries = 15
                              , wait = 5) {
  
  it <- 1
  try_again <- TRUE
  
  while (it <= tries & try_again == TRUE) {
    
    try_again <- FALSE

    tryCatch({
      
      out <- do.call(fun, args) # if it causes error operation is repeated
      stopifnot(eval(condition)) # when this condition is not met the operation is also repeated
      
    }, error = function(err) {
      
      try_again <<- TRUE
      
      pkg_loginfo(  'Condition not met while runnig function at try %s.'
                  , it)
      it <<- it + 1
      Sys.sleep(wait)
      }
    )
  }
  
  tryCatch({
    stopifnot(eval(condition))
  },
  error = function(err) {
    pkg_logerror(sprintf('Running function failed for %s times. Script stopped.', tries))
    stop(err)
  }
  )
  
  return(out)
}