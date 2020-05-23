#' Verify \code{\link{read_gt3x}} gives equivalent output using legacy and dev
#' parsers
#'
#' @param legacy legacy output
#' @param dev dev output
#'
#' @return A logical scalar indicating whether the outputs matched
#' @export
#'
#' @examples
#' \donttest{
#' file_3x <- system.file(
#'   "extdata", "example.gt3x", package = "AGread"
#' )
#' l <- read_gt3x(file_3x, parser = "legacy")
#' d <- read_gt3x(file_3x, parser = "dev")
#' legacy_dev_compare(l, d)
#' }
legacy_dev_compare <- function(legacy, dev) {

  stopifnot(
    all(names(legacy) %in% names(dev)),
    all(names(dev) %in% names(legacy))
  )
  dev %<>% .[names(legacy)]

  test_log <- logical()

  ## Account for known (and functionally meaningless) differences

  ## EVENT

    legacy$EVENT$other_events$index <- NULL
    row.names(legacy$EVENT$other_events) <- NULL
    dev$EVENT$other_events$index <- NULL
    class(dev$EVENT) <- class(legacy$EVENT)
    class(dev$EVENT$other_events) <- class(legacy$EVENT$other_events)
    dev$EVENT$other_events$type %<>% as.character(.)

  ## PARAMETERS

    legacy$PARAMETERS %<>% .$Payload
    class(dev$PARAMETERS) <- class(legacy$PARAMETERS)

  all.equal(legacy, dev, scale = 1, tolerance = 0.001)

}
