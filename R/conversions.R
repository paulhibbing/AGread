hex_convert <- function(values, endian = "little") {

  message("Avoid using the hex_convert function if possible.")
  print(sys.call())
  if (endian == "little") values <- rev(values)

  values <- paste(values, collapse = "")
  values <- paste("0x", values, sep = "")
  base::strtoi(values)

}

tick_to_posix <- function(x, tz = "UTC", ...) {
  
  x <- as.numeric(as.character(x)) / 10000000
  
  as.POSIXct(x, tz, origin = "0001-01-01", ...)
  
}
