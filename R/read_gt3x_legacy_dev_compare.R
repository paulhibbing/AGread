#' Verify \code{\link{read_gt3x}} gives equivalent output using legacy and dev
#' parsers
#'
#' @param file path to the gt3x file for use in comparing the parsers
#' @param dev time logical. Should timing information be returned?
#' @param verbose logical. Print updates to console?
#'
#' @return If \code{time = FALSE} (default), a logical scalar is returned,
#'   indicating whether the outputs matched. If \code{time = TRUE}, run times
#'   are returned for both parsers.
#' @export
#'
#' @examples
#' \donttest{
#' file_3x <- system.file(
#'   "extdata", "example.gt3x", package = "AGread"
#' )
#' legacy_dev_compare(file)
#' }
legacy_dev_compare <- function(file, time = FALSE, verbose = FALSE) {

  if (verbose) cat("\nComparing parsers for", basename(file))

  ## Run and time the parsers

    if (verbose) cat("\n...Running with legacy parser")
    legacy_time <- proc.time()
    legacy <- read_gt3x(file, parser = "legacy")
    legacy_time %<>%
      {proc.time() - .} %>%
      {.[3]} %>%
      stats::setNames("legacy_runtime_s")

    if (verbose) cat("\n...Running with dev parser")
    dev_time <- proc.time()
    dev <- read_gt3x(file, parser = "dev")
    dev_time %<>%
      {proc.time() - .} %>%
      {.[3]} %>%
      stats::setNames("dev_runtime_s")

    times <- c(legacy_time, dev_time)

  ## Order dev the same as legacy

    all_names <- names(legacy)
    dev <-
      all_names %T>%
      {stopifnot(
        setequal(., names(dev)),
        length(.) == length(names(dev)),
        length(legacy) == length(dev)
      )} %>%
      {dev[.]}

  ## Account for known (and functionally meaningless) differences

  ## EVENT
  if ("EVENT" %in% all_names) {

    if (verbose) cat("\n...Checking EVENT format")
    legacy$EVENT$other_events$index <- NULL
    row.names(legacy$EVENT$other_events) <- NULL
    dev$EVENT$other_events$index <- NULL
    class(dev$EVENT) <- class(legacy$EVENT)
    class(dev$EVENT$other_events) <- class(legacy$EVENT$other_events)
    dev$EVENT$other_events$type %<>% as.character(.)

    if (!rlang::is_true(all.equal(legacy$EVENT, dev$EVENT))) {
      stop("EVENT packets differ")
    }

  }

  ## PARAMETERS
  if ("PARAMETERS" %in% all_names) {

    if (verbose) cat("\n...Checking PARAMETERS format")
    legacy$PARAMETERS %<>% .$Payload
    class(dev$PARAMETERS) <- class(legacy$PARAMETERS)

    if (!rlang::is_true(all.equal(legacy$PARAMETERS, dev$PARAMETERS))) {
      stop("PARAMETERS packets differ")
    }

  }

  all.equal(legacy, dev, scale = 1, tolerance = 0.001) %>%
  rlang::is_true(.) %T>%
  stopifnot(.) %T>%
  {if (verbose) cat("\n...Success!")} %>%
  {if (time) times else .}

}
