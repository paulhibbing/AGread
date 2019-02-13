checksum_calculate <- function(record, final_index) {
  
  target_value <- record[final_index]
  record <- record[-final_index]
  
  checksum <- as.raw(0)
  
  for (i in seq(record)) {
    checksum <- xor(checksum, record[i])
  }
  
  checksum_ok <- (!checksum) == target_value
  stopifnot(checksum_ok)
  
  invisible()
  
}
