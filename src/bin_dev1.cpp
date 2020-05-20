#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' @rdname dev_bin_type1
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List bin_dev1_initialize(RawVector log, bool verbose) {

  // Set up preallocated null list
    double max_packets = log.size()/double(10);
    Rcpp::List packets(ceil(max_packets));

  // Declare loop variables
    int type;
    int timestamp;
    int size;

  // Define counter variables
    int packet_number = 0;
    int current_index = 0;

  while(current_index < log.size()) {

    //Set up printing
    if (verbose) {
      Rcout << "\r  Parsing log.bin " <<
      " ............. " <<
      floor(double(current_index)/log.size()*100) << "%";
    }

    type = log[current_index + 1];

    timestamp = (unsigned int)(
      (unsigned int)(log[current_index + 5]) << 24 |
      (unsigned int)(log[current_index + 4]) << 16 |
      (unsigned int)(log[current_index + 3]) << 8 |
      (unsigned int)(log[current_index + 2])
    );

    size = (unsigned int)(
      (unsigned int)(log[current_index + 7]) << 8 |
      (unsigned int)(log[current_index + 6])
    );

    packets[packet_number] = List::create(
      Named("type") = type, Named("timestamp") = timestamp
    );
    ++packet_number;

    current_index = next_separator(log, current_index + 9 + size);
    if (current_index == NA_INTEGER) {
      break;
    }

  }

  if (verbose) {
    Rcout << "\r  Getting record headers " <<
    " ............. COMPLETE";
  }

  return packets;

}
