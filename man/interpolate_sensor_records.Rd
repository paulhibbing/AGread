% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_gt3x_Interpolation.R
\name{interpolate_sensor_records}
\alias{interpolate_sensor_records}
\title{Perform linear interpolation to fill in SENSOR_DATA payloads that are missing
one or more samples}
\usage{
interpolate_sensor_records(record_set, schema, verbose)
}
\arguments{
\item{record_set}{data frame. Information about the SENSOR_DATA packets (one
row per packet)}

\item{schema}{a SENSOR_SCHEMA object}

\item{verbose}{logical. Print updates to console?}
}
\description{
Perform linear interpolation to fill in SENSOR_DATA payloads that are missing
one or more samples
}
\keyword{internal}
