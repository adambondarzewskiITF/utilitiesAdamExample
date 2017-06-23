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
install_our_package <- function(  creds_path = '~/pass/git.txt'
                                , package_name, user = 'analitycy_reklamy'
                                , domain) {
  install_packages_https(pkg_https_link = sprintf('https://%s/%s/%s.git', domain, user, package_name),
                         credentials_path = creds_path)
}