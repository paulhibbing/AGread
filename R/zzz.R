if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".", "Last_Sample_Time", "Sample_Rate", "Start_Date",
  "values", "block"
))

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "package 'AGread' was built under R version 3.5.0"
  )

}
