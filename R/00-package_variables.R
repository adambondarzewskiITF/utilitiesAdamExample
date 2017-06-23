pkg_env <- new.env()

# logger

assign("pkg_logger_info", value = logging::getLogger("eff_i"), envir = pkg_env)
assign("pkg_logger_warn", value = logging::getLogger("eff_w"), envir = pkg_env)
assign("pkg_logger_debug", value = logging::getLogger("eff_d"), envir = pkg_env)
assign("pkg_logger_error", value = logging::getLogger("eff_e"), envir = pkg_env)

pkg_logger <- function(x) {
  get(x = x, envir = pkg_env)
}

registerLogger <- function(x, logger) {
  assign(x = x, value = logger, envir = pkg_env)
}

pkg_loginfo <- function(msg, ...) { logging::loginfo(msg,  logger = pkg_logger("pkg_logger_info"), ...)}
pkg_logdebug <- function(msg, ...) { logging::logdebug(msg,  logger = pkg_logger("pkg_logger_debug"), ...)}
pkg_logerror <- function(msg, ...) { logging::logerror(msg,  logger = pkg_logger("pkg_logger_error"), ...)}
pkg_logwarn <- function(msg, ...) { logging::logwarn(msg,  logger = pkg_logger("pkg_logger_warn"), ...)}
