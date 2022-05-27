if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".", "block", "Date", "Last_Sample_Time", "Sample_Rate", "Start_Date",
  "Time", "Timestamp", "values", "Vector.Magnitude", "where"
))

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "package 'AGread' was built under R version 4.0.5"
  )

}
