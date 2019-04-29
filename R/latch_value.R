#' Latch a data packet to the last payload sample of the previous packet
#'
#' @inheritParams zero_fill
#'
#' @keywords internal
latch_value <- function(records, index) {

  previous_record <- records[[index - 1]]
  missing_record <- records[[index]]

  ## Repeat last row of previous payload n times, where
  ## n is the number of rows in the missing payload
  new_payload <- previous_record$Payload[
    rep(
      nrow(previous_record$Payload),
      nrow(missing_record$Payload)
    ),

    ]
  missing_record$Payload <- data.frame(
    new_payload, row.names = NULL
  )

  return(missing_record)

}
