#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame get_headersC(RawVector x) {

  //Retrieve information for first record
    IntegerVector index = (unsigned int)(0);
    IntegerVector type = (unsigned int)(0);
    IntegerVector timestamp = (unsigned int)(0);
    IntegerVector payload_size = (unsigned int)(0);

  //Setup for the loop
    int next_index = 0;
    int this_row = 0;
    int x_len = x.size();

  //Run the loop
    while ( next_index < x_len ) {

      index.push_back(next_index);
      type.push_back((unsigned int)(x[next_index + 1]));
      timestamp.push_back((unsigned int)(
        (unsigned int)(x[next_index + 5]) << 24 |
        (unsigned int)(x[next_index + 4]) << 16 |
        (unsigned int)(x[next_index + 3]) << 8 |
        (unsigned int)(x[next_index + 2])));
      payload_size.push_back((unsigned int)(
        (unsigned int)(x[next_index + 7]) << 8 |
        (unsigned int)(x[next_index + 6])));

      next_index = index[this_row] + 9 +
        payload_size[this_row];
      this_row += 1;

    }

  //Wrapup
    return DataFrame::create( Named("index") = index ,
      Named("type") = type ,
      Named("timestamp") = timestamp ,
        Named("payload_size") = payload_size);
}
