#' @rdname parse_log_bin
#' @keywords internal
dev_bin_type1 <- function(log, tz, verbose, include, info) {

  packets <-
    cpp_include(include) %>%
    bin_dev1_initialize(log, verbose, .)

}
