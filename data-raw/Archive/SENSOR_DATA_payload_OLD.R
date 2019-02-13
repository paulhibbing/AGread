# rm(list = ls())
# source("Helpers/Utils_3x_read.R")
# load("example_sensor_payload.RData")

payload_parse_sensor_data_25 <- function(
  payload, info, parameters, schema, record_header
) {
  
  id <- readBin(payload[1:2], "integer", 2, 2, FALSE, "little")
  stopifnot(id == schema$Payload$id)
  
  BITS_PER_BYTE <- 8
  offset <- 3
  
  tick <- schema$Payload$samples
  if (tick == 0) {
    tick <- 100
    schema$Payload$samples <- 100
  }
  rawSecondCounter <- 0
  
  firstSample <- record_header$timestamp
  sensor_packets <- data.frame()
  
  columns <- split(
    schema$Payload$sensorColumns,
    seq(nrow(schema$Payload$sensorColumns))
  )
  
  for (i in seq(schema$Payload$samples) - 1) {
    
    # new_packet <- data.frame(
    #   Timestamp = firstSample + (rawSecondCounter / 1000),
    #   label = 
    # )
    Timestamp <- firstSample +
      (rawSecondCounter / schema$Payload$samples)
    
    for (column in columns) {
      # column <- schema$Payload$sensorColumns[1, ]
      sensorValue <- 0
      bytesInValue <- column$size/BITS_PER_BYTE
      stopifnot(bytesInValue %in% 1:2)
      
      endianness <- "little"
      if (column$is_big_endian) endianness <- "big"
      sensorValue <- readBin(
          payload[offset:sum(offset, bytesInValue - 1)], "integer",
          bytesInValue, bytesInValue,
          column$is_signed, endianness
      )
      
      if (column$scale_factor != 0) {
        result <- sensorValue / column$scale_factor
      }
      
      if (
        grepl("temperature", column$label, ignore.case = TRUE)
      ) {
        result <- result + 21
      }
      
      if (column$label != "") {
        new_packet <- data.frame(
          Timestamp = Timestamp,
          label = gsub(" ", "_", column$label),
          result = result
        )
        sensor_packets <- rbind(
          sensor_packets, new_packet
        )
      }
    
    }
    
    offset <- offset + (column$size / BITS_PER_BYTE)
    rawSecondCounter <- rawSecondCounter + 1
    
  }
  
  reshape2::dcast(sensor_packets, Timestamp~label, value.var = "result")

}
