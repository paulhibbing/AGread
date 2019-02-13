AG_round <- function(x, digits = 6) {
  # x <- c(7.5555555, -7.5555555)
  # digits <- 6
  
  signs <- sign(x)
  
  DescTools::RoundTo(
    abs(x), 5 / (10 ^ digits), "ceiling"
  ) * signs
  
}

bin_int <- function(value_bin) {
  sum(2 ^ (length(value_bin) - which(value_bin)))
}

AG_binary <- function(value, n = 8) {
  binaryLogic::as.binary(
    unlist(binaryLogic::as.binary(rev(value), n = n)),
    logic = TRUE
  )
}

get_value <- function(type, value) {
  switch(
    type,
    "float" = get_float_value(value),
    "int" = readBin(value, "integer", 4, 4)
  )
}

get_float_value <- function(value) {
  exponent <- get_exponent(value)
  significand <- get_significand(value)
  significand * (2^exponent)
}

get_exponent <- function(value) {
  
  x <- value[4]
  binx <- binaryLogic::as.binary(x, n = 8)
  is_negative <- binx[1] == binaryLogic::as.binary(1)
  
  exponent <- as.integer(binx)
  if (is_negative) {
    magnitude <- binaryLogic::binAdd(
      !binx, binaryLogic::as.binary(1)
    )
    exponent <- as.integer(binx) * -1
  }
  
  return(as.double(exponent))
  
}

get_significand <- function(value) {
  
  FLOAT_MAXIMUM <- 2^23
  x <- value[3:1]
  
  binx <- binaryLogic::as.binary(
    unlist(binaryLogic::as.binary(x, n = 8)),
    logic = TRUE
  )
  is_negative <- binx[1] == binaryLogic::as.binary(1)
  
  significand <- as.integer(binx)
  if (is_negative) {
    magnitude <- binaryLogic::binAdd(
      !binx, binaryLogic::as.binary(1)
    )
    significand <- as.integer(binx) * -1
  }
  
  as.double(significand) / FLOAT_MAXIMUM
  
}

update_key <- function(key, value) {
  key <- key[1, ]
  key$Range <- NA
  key$value <- value
  return(key)
}

capability_collapse <- function(caps) {
  cap_length <- length(caps)
  if (cap_length == 1) return (caps)
  last_cap <- caps[length(caps)]
  caps <- paste(caps[-length(caps)], collapse = ", ")
  caps <- paste(c(caps, ", and ", last_cap), collapse = "")
  if (cap_length == 2) caps <- gsub(", and", " and", caps)
  return(caps)
}

test_val <- function(hex) {
  wkb::hex2raw(hex)
}
