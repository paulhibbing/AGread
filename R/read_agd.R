# Main function -----------------------------------------------------------

#' Read an ActiGraph agd file
#'
#' @param file character. Path to the agd file
#' @param tz character. The desired timezone
#' @param return character. The desired elements to return (see details)
#'
#' @details The agd is made up of a \code{settings} element and a \code{data}
#'   element. The former is likely only necessary for debugging, and so the
#'   default is to return only the \code{data} element. Both \code{settings} and
#'   \code{data} are in data frame format. If both are returned, they are given
#'   as a list.
#'
#' @return the desired output
#' @seealso \url{ActiGraph documentation}{https://github.com/actigraph/ActiLifeManual/blob/master/docs/appendix.rst}
#' @export
#'
#' @examples
read_agd <- function(file, tz = "UTC", return = c("data", "settings", "both")) {

  return <- match.arg(return)

  conn <-
    RSQLite::SQLite() %>%
    DBI::dbConnect(file)

  mapply(
    DBI::dbReadTable,
    c("settings", "data"),
    MoreArgs = list(conn = conn),
    SIMPLIFY = FALSE
  ) %T>%
  {DBI::dbDisconnect(conn)} %>%
  agd_format(tz, return)

}

# Supporting code ---------------------------------------------------------

.agd_names <- data.frame(
  agd_name = c(
    "dataTimestamp", "axis1", "axis2", "axis3", "steps", "lux", "inclineOff",
    "inclineStanding", "inclineSitting", "inclineLying"
  ),
  csv_name = c(
    "Timestamp", "Axis1", "Axis2", "Axis3", "Steps", "Lux", "Inclinometer.Off",
    "Inclinometer.Standing", "Inclinometer.Sitting", "Inclinometer.Lying"
  ),
  stringsAsFactors = FALSE
)

#' Format ActiGraph agd data after initial reading
#'
#' @param AG list. The result of initial reading
#' @inheritParams read_agd
#'
#' @rdname agd_format
#'
#' @keywords internal
agd_format <- function(AG, tz, return) {

  ## Checks

    # Continuity
    diff(AG$data$dataTimestamp) %>%
    unique(.) %>%
    length(.) %>%
    {. == 1} %>%
    stopifnot(.)

    # Variable names
    names(AG$data) %>%
    {. %in% .agd_names$agd_name} %>%
    all(.) %>%
    stopifnot(., "dataTimestamp" %in% names(AG$data))

  ## Format data

    AG$data %<>%
      names(.) %>%
      match(.agd_names$agd_name) %>%
      .agd_names$csv_name[.] %>%
      stats::setNames(AG$data, .) %>%
      within({
        Timestamp = tick_to_posix(Timestamp, tz)
        Date = agd_date_string(Timestamp, tz)
        Time = strftime(Timestamp, "%H:%M:%S", tz)
      }) %>%
      agd_order_columns(.) %>%
      agd_vector_magnitude(.)

  ## Finish up

    switch(
      return,
      "data" = AG$data,
      "settings" = AG$settings,
      "both" = AG
    )

}

#' @rdname agd_format
#' @keywords internal
agd_date_string <- function(timestamp, tz) {
  strftime(timestamp, "%m/%d/%Y", tz) %>%
  strsplit("/") %>%
  lapply(., gsub, pattern = "^0", replacement = "") %>%
  lapply(paste, collapse = "/") %>%
  do.call(c, .)
}

#' @rdname agd_format
#' @param target character. The target column after which the new variables
#'   should be inserted
#' @param new character. The new column names that should be inserted
agd_order_names <- function(AG, target = "Timestamp", new = c("Date", "Time")) {
  target %T>%
  {stopifnot(. %in% names(AG))} %>%
  match(names(AG)) %>%
  append(names(AG), new, .)
}

#' @rdname agd_format
#' @param ... arguments passed to \code{agd_order_names}
#' @keywords internal
agd_order_columns <- function(AG, ...) {
  agd_order_names(AG, ...) %>%
  AG[ ,.]
}

#' @rdname agd_format
#' @param expected character. The expected names of the triaxial variables
#' @keywords internal
agd_vector_magnitude <- function(AG, expected = paste0("Axis", 1:3)) {
  ## This will calculate vector magnitude if possible, regardless of whether
  ## the data already includes it. Not a big deal because the operation runs
  ## quickly.
  if (all(expected %in% names(AG))) {
    new_names <-
      length(expected) %T>%
      {stopifnot(. == 3)} %>%
      expected[.] %>%
      match(names(AG)) %>%
      append(names(AG), "Vector.Magnitude", .)
    AG[ ,expected] %>%
    as.list(.) %>%
    stats::setNames(c("x", "y", "z")) %>%
    do.call(get_VM_C, .) %>%
    round(2) %>%
    {within(AG, {
      Vector.Magnitude = .
    })} %>%
    agd_order_columns(
      expected[length(expected)],
      "Vector.Magnitude"
    )
  } else {
    AG
  }
}

