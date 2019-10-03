#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// DataFrame get_headersC(CharacterVector filename, bool verbose, IntegerVector max_samples) {

// [[Rcpp::export]]
DataFrame get_headersC(RawVector x, bool verbose) {
  //Retrieve information for first record
  int max_samples = round(x.size()/1.5);
  IntegerVector index (max_samples, NA_INTEGER);
  IntegerVector type (max_samples, NA_INTEGER);
  IntegerVector timestamp (max_samples, NA_INTEGER);
  IntegerVector payload_size (max_samples, NA_INTEGER);

  //Setup for the loop
  int next_index = 0;
  int this_row = 0;
  int x_len = x.size();

  //Set up printing
  if (verbose) {
    Rcout << "\n x_len " << x_len;
  }
  //Run the loop
  while ( next_index < x_len ) {

    index[this_row] = next_index;
    type[this_row] = x[next_index + 1];
    timestamp[this_row] = (unsigned int)(
      (unsigned int)(x[next_index + 5]) << 24 |
        (unsigned int)(x[next_index + 4]) << 16 |
        (unsigned int)(x[next_index + 3]) << 8 |
        (unsigned int)(x[next_index + 2]));
    payload_size[this_row] = (unsigned int)(
      (unsigned int)(x[next_index + 7]) << 8 |
        (unsigned int)(x[next_index + 6]));


    next_index = index[this_row] + 9 +
      payload_size[this_row];
    this_row += 1;
  }

  LogicalVector l1 = !is_na(index);
  index = index[l1];
  type = type[l1];
  timestamp = timestamp[l1];
  payload_size = payload_size[l1];

  //Wrapup
  return DataFrame::create( Named("index") = index ,
                            Named("type") = type ,
                            Named("timestamp") = timestamp ,
                            Named("payload_size") = payload_size);
}
