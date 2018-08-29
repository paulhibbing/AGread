#' Get the size of a file in GB
#'
#' @inheritParams read_AG_raw
#'
#' @keywords internal
#'
get_file_size__gb <- function(file) {

  size  <- structure(
    file.size(file),
    class = "object_size"
  )

  as.numeric(
    gsub(" .*", "", format(size, "GB"))
  )

}
