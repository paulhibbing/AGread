rm(list = ls())
devtools::load_all()

## Step 1: Set starter variables ####

  file <- "data-raw/3x-error/FLPAY002S1D2LW (2017-01-28).gt3x"
  # test <- read_gt3x(file)
  tz <- "UTC"
  verbose <- TRUE
  give_timestamp <- TRUE
  include <- c(
    "METADATA", "PARAMETERS",
    "SENSOR_SCHEMA", "SENSOR_DATA"
  )

## Step 2: read_gt3x ####

  timer <- proc.time()
  if (verbose) cat("\nProcessing", basename(file), "\n")

  #1) Verify .gt3x file is a zip file
  file_3x <- try(
    utils::unzip(file, list = TRUE),
    TRUE
  )

  if (class(file_3x) == "try-error") {
    stop(paste(
      deparse(substitute(file)),
      "is not a valid gt3x file."
    ))
  } else {
    row.names(file_3x) <- file_3x$Name
  }

  #2) Verify .gt3x file has log.bin file
  #3) Verify .gt3x file has info.txt file
  stopifnot(all(c("info.txt", "log.bin") %in% file_3x$Name))

  #4) Extract info.txt
  info_con <- unz(file, "info.txt")

  #5) Parse and save the sample rate from the info.txt file (it's stored in Hz)
  #6) Parse and save the start date from the info.txt file (it's stored in .NET
  #Ticks)
  info <- parse_info_txt(info_con, verbose)
  close(info_con)

  #7) Extract log.bin
  #8) Parse log.bin
  # n_records <- file_3x["log.bin", "Length"]
  log_file  <- utils::unzip(file, "log.bin", exdir = tempdir())

  n_records <- file_3x["log.bin", "Length"]

## Step 3: parse_log_bin ####

  ## Read the bin file
  if (verbose) cat("\n  Reading log.bin")
  log <- readBin(log_file, "raw", n_records)
  if (verbose) cat("  ............. COMPLETE")

  ## Get headers
  record_headers <- get_headers(
    log, n_records, tz, verbose = verbose
  )

  stopifnot(
    all(
      sum(record_headers$type == "21") <= 1,
      sum(record_headers$type == "24") <= 1
    )
  )

  ## Get parameters (if applicable)
  par_index <- which(record_headers$type == "21")
  if (!!length(par_index)) {
    parameters <- process_record_set(
      record_headers[par_index, ],
      log, tz, info, give_timestamp,
      verbose = verbose, do_post_process = FALSE
    )
    record_headers <- record_headers[-par_index, ]
  }

  ## Get schema (if applicable)
  schema_index <- which(record_headers$type == "24")
  if (!!length(schema_index)) {
    schema <- process_record_set(
      record_headers[schema_index, ],
      log, tz, info, give_timestamp,
      verbose = verbose, do_post_process = FALSE
    )
    record_headers <- record_headers[-schema_index, ]
  }

  ## Process the remaining packets
  record_headers <- sort_records(record_headers)
  record_headers <- select_records(record_headers, include)

  save.image("data-raw/3x-error/Step3.RData")

  # rm(list = ls())
  # load("data-raw/3x-error/Step3.RData")

## Step 4: process_record_set ####

  rm(list = ls())
  devtools::load_all()
  load("data-raw/3x-error/Step3.RData")
  record_set <- record_headers[[2]]

  ## Setup
  n_vals <- nrow(record_set)
  label <- RECORDS$Type[match(
    record_set$type[1],
    as.character(RECORDS$ID)
  )]

  if (verbose) cat(
    "\n  Parsing", label, "packet(s)"
  )

  records <- lapply(
    seq(n_vals),
    function(i) {

      if (verbose & (i != n_vals)) cat(
        "\r  Parsing", label, "packet(s)",
        "  .............",
        paste(
          c(round(i/n_vals * 100, 0), "%"),
          collapse = ""
        )
      )

      result <- read_record(
        record_set[i, ], log, tz,
        info, give_timestamp, parameters, schema,
        is_last_packet = i == n_vals
      )

      if (is.null(result$Payload)) return(NULL)
      return(result)
    }
  )

  records[sapply(records, is.null)] <- NULL

  save.image("data-raw/3x-error/Step4.RData")

## Step 5: record_set_extras ####

  rm(list = ls())
  devtools::load_all()
  load("data-raw/3x-error/Step4.RData")

  if (verbose) cat(
    "\r  Parsing", label, "packet(s)",
    "-- applying extra processes"
  )

  records <- record_set_extras(
    records, label, do_post_process,
    info, schema, tz, verbose
  )
