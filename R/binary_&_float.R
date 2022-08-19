# Binary operations -------------------------------------------------------

  which_bits <- function(value, n) {
    rawToBits(value) %>%
    .[1:n] %>%
    as.logical(.) %>%
    which(.)
  }


  logical_bits <- function(value) {
    rawToBits(value) %>%
    rev(.) %>%
    as.logical(.)
  }


# Float operations --------------------------------------------------------

  get_float_value <- function(value) {
    n_bytes <- length(value)
    n_bits <- n_bytes * 8
    exponent <- get_exponent(value, n_bytes)
    significand <- get_significand(value, n_bits, n_bytes)
    significand * (2^exponent)
  }


  get_exponent <- function(value, n_bytes) {

    binx <-
      dplyr::last(value) %>%
      rawToBits(.)

    is_negative <-
      dplyr::last(binx) %>%
      as.logical(.)

    exponent <-
      rep(0, 3) %>%
      as.raw(.) %>%
      rawToBits(.) %>%
      c(binx, .) %>%
      packBits("integer")

    if (is_negative) {
      exponent <- exponent * -1
    }

    return(as.double(exponent))

  }


  get_significand <- function(value, n_bits, n_bytes) {

    FLOAT_MAXIMUM <- 2^((n_bits - 8) - 1)

    binx <-
      utils::head(value, n = -1) %>%
      rawToBits(.)

    is_negative <-
      dplyr::last(binx) %>%
      as.logical(.)

    significand <-
      as.raw(0) %>%
      rawToBits(.) %>%
      c(binx, .) %>%
      packBits("integer")

    if (is_negative) {
      significand <- significand * -1
    }

    as.double(significand) / FLOAT_MAXIMUM

  }
