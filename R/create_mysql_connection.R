#' Title
#'
#' @param host_name character; name of host to connect
#' @param db_name character; name of data base to connect;
#' @param creds_path character; path to credentials
#'
#' @return db_bi; mysql connection
#' @export
#'
#' @examples
create_mysql_connection <- function(  host_name = config$host_name
                                   , db_name = config$db_name
                                   , creds_path = config$creds_path) {
  
    tryCatch({
    
    creds <- read_credentials(creds_path)
    
    loggingUtilities:::pkg_loginfo('Creating connection with bi, dbname bu_adv_sem...')
    
    db_bi <- src_mysql(    host = host_name
                           , dbname = db_name
                           , user = creds$user
                           , password = creds$password)
    
    db_bi
    
  }, error = function(err) {
    loggingUtilities:::pkg_logerror(sprintf('Error while creating connection with bi, dbname bu_adv_sem: %s', err))
    stop()
  }, warning = function(war) {
    loggingUtilities:::pkg_logerror(sprintf('Warning while creating connection with bi, dbname bu_adv_sem: %s', war))
  })
}