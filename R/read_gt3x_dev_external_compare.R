#' Verify \code{\link{read_gt3x}} gives equivalent output using dev and
#' external parsers
#'
#' @inheritParams legacy_dev_compare
#'
#' @return If \code{time = FALSE} (default), a logical scalar is returned,
#'   indicating whether the outputs matched. If \code{time = TRUE}, run times
#'   are returned for both parsers.
#' @export
#'
#' @note This is intended to be run after verifying that output from the dev
#'   parser matches the legacy parser (see \code{\link{legacy_dev_compare}}).
#'   The latter is very slow, so this setup allows only having to run it once.
#'
#' @examples
#' \donttest{
#' file_3x <- system.file(
#'   "extdata", "example.gt3x", package = "AGread"
#' )
#' dev_external_compare(file_3x)
#' }
dev_external_compare <- function(file, time = FALSE, verbose = FALSE) {


  if (verbose) cat("\nComparing DEV and EXTERNAL for", basename(file))


  ## Run and time the parsers

    if (verbose) cat("\n...Running with external parser")
    external_time <- proc.time()
    external <- read_gt3x(file, parser = "external")
    external_time %<>%
      {proc.time() - .} %>%
      {.[3]} %>%
      stats::setNames("external_runtime_s")

    if (verbose) cat("\n...Running with dev parser")
    dev_time <- proc.time()
    dev <- read_gt3x(file, parser = "dev", include = c("ACTIVITY2", "EVENT"))
    dev_time %<>%
      {proc.time() - .} %>%
      {.[3]} %>%
      stats::setNames("dev_runtime_s")

    times <- c(dev_time, external_time)


  ## Order external the same as dev

    all_names <- names(dev)
    external <-
      all_names %T>%
      {stopifnot(
        setequal(., names(external)),
        length(.) == length(names(external)),
        length(dev) == length(external)
      )} %>%
      {external[.]}


  ## Account for known (and functionally meaningless) differences

    ## INFO
    if ("info" %in% all_names) {
      dev$info <- NULL
      external$info <- NULL
    }

    ## EVENT
    if ("EVENT" %in% all_names) {

      if (verbose) cat("\n...Checking EVENT format")

      external$EVENT$other_events %<>% dplyr::slice(
        ., which(.$payload_size != 1)
      )

      if (nrow(external$EVENT$other_events) == 0) {
        external$EVENT$other_events <- data.frame()
      }

      row.names(external$EVENT$other_events) <-
        row.names(dev$EVENT$other_events) <-
          NULL

      if (!rlang::is_true(all.equal(dev$EVENT, external$EVENT))) {
        stop("EVENT packets differ")
      }

    }


  ## Test and return

    test <- all.equal(dev, external, scale = 1, tolerance = 0.001)

    if (!rlang::is_true(test)) stop(
      "\n", paste(test, collapse = "\n"), call. = FALSE
    )

    if (verbose) cat("\n...Success!")
    if (time) times else test


}
