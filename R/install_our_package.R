#' Title
#'
#' @param creds_path 
#' @param package_name 
#' @param user 
#' @param domain 
#'
#' @return
#' @export
#'
#' @examples
install_our_package <- function(  creds_path = '~/pass/dcaplan_ab.txt'
                                , package_name, user = 'analitycy_reklamy'
                                , domain = 'gitlab.iiit.pl') {
  install_packages_https(pkg_https_link = sprintf('https://%s/%s/%s.git', domain, user, package_name),
                         credentials_path = creds_path)
}